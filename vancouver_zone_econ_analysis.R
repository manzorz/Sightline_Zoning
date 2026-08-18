library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(terra)
library(exactextractr)

# =========================================================================
# 1. LOAD DATA & SET PROJECTION
# =========================================================================
target_crs <- 2927 # NAD83 / Washington South (ftUS)

lots   <- st_read("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/TaxlotsPublic.shp") %>%
  st_transform(target_crs) %>%
  st_make_valid()

zoning <- st_read("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/Zoning.shp") %>%
  st_transform(target_crs) %>%
  st_make_valid()

ugabnd <- st_read("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/Ugabnd.shp") %>%
  st_transform(target_crs) %>%
  st_make_valid()

# Clean up any identical stacked zoning polygons
zoning_cleaned <- zoning %>% 
  filter(!duplicated(st_geometry(.))) %>%
  mutate(
    Zoning_ID = 1:n(),
    Zone_Acres = as.numeric(st_area(geometry)) / 43560,
    MaxZoneUnits = round(Zone_Acres * UnitsPerAc)
  )

# =========================================================================
# 2. SPATIAL JOIN BY LARGEST OVERLAP (One-to-One: Lots inherit Zoning rules)
# =========================================================================
intersections <- st_intersection(lots, zoning_cleaned)
intersections$intersect_area <- st_area(intersections)

lots_joined <- intersections %>%
  group_by(prop_id) %>%                       
  arrange(desc(intersect_area), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

# =========================================================================
# 3. ASSIGN LOCAL DIMENSIONAL RULES TO THE LOT LEVEL
# =========================================================================
lots_with_rules <- lots_joined %>%
  mutate(
    # Isolate Zone Code to map Title 40 dimensional rules
    Zone_Code = sub(".*\\((.*)\\).*", "\\1", desc_),
    
    Max_Height_Ft = case_when(
      grepl("R1-|R-6|R-7.5|R-10|RLD", Zone_Code) ~ 35,
      grepl("R-12|R-18|R-22", Zone_Code)        ~ 35,
      grepl("R-30|R-43", Zone_Code)             ~ 45,
      grepl("OR-15|OR-18|OR-22", Zone_Code)     ~ 45,
      grepl("OR-30|OR-43", Zone_Code)           ~ 60,
      grepl("IH|IL|ML|IR", Zone_Code)           ~ 100, 
      TRUE                                      ~ 35   
    ),
    
    Front_Setback_Ft = case_when(
      grepl("R1-20|R1-10", Zone_Code)           ~ 20,
      grepl("R1-5|R1-6|R1-7.5|R-6|R-7.5", Zone_Code) ~ 10, 
      grepl("R-12|R-18|R-22|R-30|R-43", Zone_Code)   ~ 10,
      grepl("RLD", Zone_Code)                   ~ 15,
      TRUE                                      ~ 15   
    )
  )

# =========================================================================
# 4. ENVIRONMENTAL WETLAND IMPACT (Raster Extraction at Lot Level)
# =========================================================================
gdb_path <- "C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/Wetlands.gdb"
wetlands_raster <- rast(gdb_path, lyrs = "Your_Wetlands_Raster_Name")

if (crs(wetlands_raster, proj = TRUE) != crs(lots_with_rules, proj = TRUE)) {
  wetlands_raster <- project(wetlands_raster, crs(lots_with_rules), method = "near")
}

# Extract values directly across individual property parcel configurations
extracted_data <- exact_extract(wetlands_raster, lots_with_rules, include_cols = "prop_id")

lot_wetland_summary <- bind_rows(extracted_data) %>%
  mutate(
    Is_Wetland = value %in% c(13:18, 22, 23),
    Wetland_Coverage_Weight = coverage_fraction * Is_Wetland
  ) %>%
  group_by(prop_id) %>%
  summarise(
    Total_Lot_Pixels = sum(coverage_fraction),
    Total_Wetland_Pixels = sum(Wetland_Coverage_Weight),
    Pct_Wetland = (Total_Wetland_Pixels / Total_Lot_Pixels) * 100
  )

# =========================================================================
# 5. PARCEL BUILD_OUT CAP MODEL (Volumetric Calculation per Lot)
# =========================================================================
ASSUMED_AVG_STORY_HEIGHT <- 11  
ASSUMED_AVG_UNIT_SIZE    <- 1200 

lots_capacity_model <- lots_with_rules %>%
  left_join(lot_wetland_summary, by = "prop_id") %>%
  mutate(
    # Clean up empty records and calculate physical land margins
    Pct_Wetland = ifelse(is.na(Pct_Wetland), 0, Pct_Wetland),
    
    # Calculate acreage using live geometry field: Shape_Area
    Lot_Acres = as.numeric(Shape_Area) / 43560,
    Wetland_Acres = Lot_Acres * (Pct_Wetland / 100),
    Net_Lot_Acres = pmax(0, Lot_Acres - Wetland_Acres),
    
    # Setback reductions
    Setback_Reduction_Factor = case_when(
      Front_Setback_Ft >= 20 ~ 0.70,  
      Front_Setback_Ft == 15 ~ 0.75,  
      Front_Setback_Ft <= 10 ~ 0.85,  
      TRUE                    ~ 0.80
    ),
    Net_Footprint_SqFt = (Net_Lot_Acres * 43560) * Setback_Reduction_Factor,
    Max_Stories = floor(Max_Height_Ft / ASSUMED_AVG_STORY_HEIGHT),
    
    Max_Lot_Coverage_Pct = case_when(
      grepl("R1-|R-6|R-7.5|R-10|RLD", desc_) ~ 0.40, 
      grepl("R-|MF|OR", desc_)                ~ 0.60, 
      TRUE                                   ~ 0.50
    ),
    
    Max_Potential_Floor_Area = (Net_Footprint_SqFt * Max_Lot_Coverage_Pct) * Max_Stories,
    Physical_Unit_Capacity = floor(Max_Potential_Floor_Area / ASSUMED_AVG_UNIT_SIZE),
    Regulatory_Density_Cap = floor(Net_Lot_Acres * UnitsPerAc),
    
    # Total Gross Allowed Units on this specific parcel shape
    MaxPossibleConstruction = pmin(Physical_Unit_Capacity, Regulatory_Density_Cap),
    
    # NET HEADROOM: Realistic new homes that can be built given currently existing units
    # Accounts for the live "Units" column in your dataset summary
    Net_Realizable_Homes = pmax(0, MaxPossibleConstruction - Units),
    
    Is_Useless_Upzone = ifelse(Regulatory_Density_Cap > Units & MaxPossibleConstruction <= Units, TRUE, FALSE)
  )

# =========================================================================
# 6. CONSOLIDATE LOGICAL HOUSING TYPE ASSIGNMENT 
# =========================================================================
res_keywords <- "Residential|Resid|Single-family|Single Family|Multifamily|Multiple-family|Mobile Home|MHP|MDR|LDR|HDR|RLD|Mixed Use|Mixed-Use|WMU|Office Residential|Downtown|Town Center|Village"

zoning_binary <- lots_capacity_model %>%
  mutate(
    Is_Residential = ifelse(
      grepl(res_keywords, desc_, ignore.case = TRUE) & !grepl("Airport/Residential", desc_, ignore.case = TRUE), 
      "Residential Zoning", 
      "Non-Residential / Commercial / Other"
    )
  )

# =========================================================================
# 7. MAP COMPONENT: BOUNDARY OVERLAY CALCULATION
# =========================================================================
city_outlines <- ugabnd %>%
  filter(!is.na(City), City != "", !grepl("Unincorporated|County", City, ignore.case = TRUE)) %>%
  group_by(City) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup()

city_centroids <- st_centroid(city_outlines)
label_x_coords <- st_coordinates(city_centroids)[, "X"]
label_y_coords <- sapply(st_geometry(city_outlines), function(geom) st_bbox(geom)[["ymax"]])

city_labels <- city_outlines %>%
  st_drop_geometry() %>%
  mutate(X = label_x_coords, Y = label_y_coords + 4000) %>%
  st_as_sf(coords = c("X", "Y"), crs = target_crs)

# =========================================================================
# 8. VISUALIZATION AND GRAPHIC OUTPUT GENERATION
# =========================================================================
output_dir <- "C:\\Users\\gmann\\Documents\\ClarkCountyZoning"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# --- GRAPHIC 1: Zone-Level Growth Potential Summary ---
zone_summary_layer <- lots_capacity_model %>%
  group_by(Zoning_ID, Zoning_Status) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup()

map_categorical <- ggplot(data = zone_summary_layer) +
  geom_sf(aes(fill = Zoning_Status), color = NA) + 
  scale_fill_manual(
    values = c("Under Zoned Limit (Has Room)" = "#21918c", "At/Over Zoned Limit" = "#440154"),
    name = "Zoning Limitations"
  ) +
  labs(title = "Zoning Limitations & Growth Potential", subtitle = "Pre-Environmental Footprint Baseline") +
  theme_minimal() + theme(panel.grid = element_blank(), axis.text = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "bottom")

ggsave(filename = file.path(output_dir, "Zoning_Limitations_Categorical.png"), plot = map_categorical, width = 10, height = 8, dpi = 300, bg = "white")

# --- GRAPHIC 2: Lot-Level Realizable Headroom Gradient Map ---
lots_gradient <- lots_capacity_model %>%
  mutate(Headroom_Display = ifelse(Zoning_Status == "At/Over Zoned Limit" | Net_Realizable_Homes == 0, NA, Net_Realizable_Homes))

map_gradient <- ggplot() +
  geom_sf(data = lots_gradient, fill = "#D3D3D3", color = NA) +
  geom_sf(data = filter(lots_gradient, !is.na(Headroom_Display)), aes(fill = Headroom_Display), color = NA) +
  geom_sf(data = city_outlines, fill = NA, color = "#4A4A4A", size = 0.5) +
  geom_sf_text(data = city_labels, aes(label = City), color = "black", fontface = "bold", size = 3, alpha = 0.6, check_overlap = TRUE) +
  scale_fill_gradient(low = "#FFFFE0", high = "#FF0000", name = "Potential\nnew homes\nunder current\nzoning limitation", na.value = "#D3D3D3") +
  theme_minimal() + theme(panel.grid = element_blank(), axis.text = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "right")

ggsave(filename = file.path(output_dir, "Zoning_Headroom_Gradient.png"), plot = map_gradient, width = 10, height = 8, dpi = 300, bg = "white")

# --- GRAPHIC 3: Residential Footprint Matrix ---
map_residential <- ggplot() +
  geom_sf(data = zoning_binary, aes(fill = Is_Residential), color = NA) +
  geom_sf(data = city_outlines, fill = NA, color = "#4A4A4A", size = 0.5) +
  geom_sf_text(data = city_labels, aes(label = City), color = "black", fontface = "bold", size = 3, alpha = 0.6, check_overlap = TRUE) +
  scale_fill_manual(values = c("Residential Zoning" = "#CCCCFF", "Non-Residential / Commercial / Other" = "#B19FF1"), name = "Zoning Type") +
  theme_minimal() + theme(panel.grid = element_blank(), axis.text = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "bottom")

ggsave(filename = file.path(output_dir, "Zoning_Residential_Footprint.png"), plot = map_residential, width = 10, height = 8, dpi = 300, bg = "white")