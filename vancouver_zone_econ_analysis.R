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
# 4 & 5. ENVIRONMENTAL WETLAND IMPACT & CACHED PARCEL MODEL
# =========================================================================
output_dir <- "C:\\Users\\gmann\\Documents\\ClarkCountyZoning"
cache_file <- file.path(output_dir, "processed_lots_capacity.rds")

# Check if the fully processed capacity file already exists on your disk
if (file.exists(cache_file)) {
  
  cat("Found cached capacity model file. Loading instantly...\n")
  lots_capacity_model <- readRDS(cache_file)
  
} else {
  
  cat("No cached file found. Loading and processing wetlands raster (this may take a few minutes)...\n")
  
  # 1. Load the Raster Layer from the Geodatabase
  gdb_path <- "C:/Users/gmann/Downloads/SEA_BIO_WetlandsInventory/Wetlands_Inventory.gdb"
  wetlands_raster <- rast(gdb_path, lyrs = "wetlands_inventory_2016")
  
  # 2. Match Projections
  if (crs(wetlands_raster, proj = TRUE) != crs(lots_with_rules, proj = TRUE)) {
    wetlands_raster <- project(wetlands_raster, crs(lots_with_rules), method = "near")
  }
  
  # 3. Extract values across individual property configurations
  cat("Extracting raster values across parcel boundaries...\n")
  extracted_data <- exact_extract(wetlands_raster, lots_with_rules, include_cols = "prop_id")
  
  # 4. Process and group pixel attributes
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
  
  # 5. Volumetric Calculation per Lot (Merged directly into the pipeline)
  ASSUMED_AVG_STORY_HEIGHT <- 11  
  ASSUMED_AVG_UNIT_SIZE    <- 1200 
  
  lots_capacity_model <- lots_with_rules %>%
    left_join(lot_wetland_summary, by = "prop_id") %>%
    mutate(
      Pct_Wetland = ifelse(is.na(Pct_Wetland), 0, Pct_Wetland),
      Lot_Acres = as.numeric(Shape_Area) / 43560,
      Wetland_Acres = Lot_Acres * (Pct_Wetland / 100),
      Net_Lot_Acres = pmax(0, Lot_Acres - Wetland_Acres),
      
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
      
      MaxPossibleConstruction = pmin(Physical_Unit_Capacity, Regulatory_Density_Cap),
      Net_Realizable_Homes = pmax(0, MaxPossibleConstruction - Units),
      Is_Useless_Upzone = ifelse(Regulatory_Density_Cap > Units & MaxPossibleConstruction <= Units, TRUE, FALSE)
    )
  
  # 6. Save the resulting file to disk to avoid processing the raster next time
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  cat("Saving processed capacity dataset to cache file...\n")
  saveRDS(lots_capacity_model, file = cache_file)
}

# =========================================================================
# 6. CONSOLIDATE LOGICAL HOUSING TYPE ASSIGNMENT (Expanded)
# =========================================================================
# Expanded to catch Commercial, Downtown, and Town Center designations that allow housing
res_keywords <- "Residential|Resid|Single-family|Single Family|Multifamily|Multiple-family|Mobile Home|MHP|MDR|LDR|HDR|RLD|Mixed Use|Mixed-Use|WMU|Office Residential|Downtown|Town Center|Village|Commercial|Neighborhood Center|Community Center"

zoning_binary <- lots_capacity_model %>%
  mutate(
    Is_Residential = ifelse(
      grepl(res_keywords, desc_, ignore.case = TRUE) & 
        !grepl("Airport/Residential|Heavy Industrial|Light Industrial", desc_, ignore.case = TRUE), # Safe exclusions
      "Residential / Mixed-Use Zoning", 
      "Pure Commercial / Industrial / Parks"
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
# EXTRA STEP: STRIP OUT CEMETERY FOOTS TO PREVENT ARTIFICIAL CAPACITY
# =========================================================================
# Load your local cemetery shapefile
cemeteries <- st_read("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/Cemetery.shp") %>%
  st_transform(target_crs) %>%
  st_make_valid()

# Find any tax parcels that overlap a cemetery and completely erase the overlapping area
# (Or remove those specific lots entirely from the development pool)
lots_capacity_model <- lots_capacity_model %>%
  mutate(
    # Check if a parcel intersects a cemetery shape
    Intersects_Cemetery = lgcl <- as.logical(st_intersects(geometry, st_union(cemeteries))),
    Intersects_Cemetery = ifelse(is.na(Intersects_Cemetery), FALSE, Intersects_Cemetery),
    
    # If it is a cemetery parcel, force its realizable housing headroom to zero
    Net_Realizable_Homes = ifelse(Intersects_Cemetery == TRUE, 0, Net_Realizable_Homes),
    Headroom_Display = ifelse(Intersects_Cemetery == TRUE, NA, Headroom_Display)
  )

# =========================================================================
# 8. VISUALIZATION AND GRAPHIC OUTPUT GENERATION
# =========================================================================
output_dir <- "C:\\Users\\gmann\\Documents\\ClarkCountyZoning"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# --- GRAPHIC 1: Zone-Level Growth Potential Summary with City Limits (Fixed) ---
# Render the clean, dissolved zone-level baseline map with city boundaries and text labels
map_categorical <- ggplot() +
  # 1. Base Layer: Solid color filled zones with NO outlines (color = NA)
  geom_sf(data = zone_summary_layer, aes(fill = Zoning_Status), color = NA) + 
  
  # 2. City Outlines Layer: Thin dark grey borders for actual urban growth boundaries
  geom_sf(data = city_outlines, fill = NA, color = "#4A4A4A", size = 0.5, linetype = "solid") +
  
  # 3. City Labels Layer: Positioned safely above northern borders and semi-transparent
  geom_sf_text(data = city_labels, aes(label = City), 
               color = "black", fontface = "bold", size = 3, alpha = 0.6, check_overlap = TRUE) +
  
  # Custom categorical fill colors matching your updated color selections
  scale_fill_manual(
    values = c(
      "Under Zoned Limit (Has Room)" = "#21918c",
      "At/Over Zoned Limit"          = "#CCCCFF"
    ),
    name = "Zoning Limitations"
  ) +
  
  # Structural titles anchoring the layout context
  labs(
    title = "Zoning Limitations & Growth Potential", 
    subtitle = "Zone-level status aggregated from individual parcel headroom analysis",
    caption = "City borders outlined in dark grey. Internal individual parcel lines removed for clarity."
  ) +
  
  # Minimal theme adjustments stripping out raw background coordinate metrics
  theme_minimal() + 
  theme(
    panel.grid = element_blank(), 
    axis.text = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "#4A4A4A", size = 10)
  )

# Save the finalized graphic to your specified documents directory
ggsave(
  filename = file.path(output_dir, "Zoning_Limitations_Categorical.png"), 
  plot = map_categorical, 
  width = 10, 
  height = 8, 
  dpi = 300, 
  bg = "white"
)


# --- GRAPHIC 2: Lot-Level Realizable Headroom Gradient Map (Fixed) ---
# Create the display column directly based on whether a lot has realizable homes left
lots_gradient <- lots_capacity_model %>%
  mutate(
    Headroom_Display = ifelse(Net_Realizable_Homes == 0, NA, Net_Realizable_Homes)
  )

map_gradient <- ggplot() +
  # 1. Base layer: Everything is drawn in light grey first (handles lots with 0 headroom)
  geom_sf(data = lots_gradient, fill = "#D3D3D3", color = NA) +
  
  # 2. Overlay layer: Only draws color over lots that have positive realizable headroom
  geom_sf(data = filter(lots_gradient, !is.na(Headroom_Display)), aes(fill = Headroom_Display), color = NA) +
  
  # 3. City Outlines Layer
  geom_sf(data = city_outlines, fill = NA, color = "#4A4A4A", size = 0.5) +
  
  # 4. City Labels Layer
  geom_sf_text(data = city_labels, aes(label = City), color = "black", fontface = "bold", size = 3, alpha = 0.6, check_overlap = TRUE) +
  
  # Custom continuous gradient from light yellow to red
  scale_fill_gradient(
    low = "#FFFFE0", 
    high = "#FF0000", 
    name = "Potential\nnew homes\nunder current\nzoning limitation", 
    na.value = "#D3D3D3"
  ) +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(), 
    axis.text = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    legend.position = "right"
  )

ggsave(filename = file.path(output_dir, "Zoning_Headroom_Gradient.png"),
       plot = map_gradient, width = 10, height = 8, dpi = 300, bg = "white")


# --- GRAPHIC 3: Residential Footprint Matrix (Fixed Zoning-Level Render) ---

# Define the comprehensive keywords to catch all policy frameworks allowing housing
res_keywords <- paste0("Residential|Resid|Single-family|Single Family|Multifamily|Multiple-family|",
                       "Mobile Home|MHP|MDR|LDR|HDR|RLD|Mixed Use|Mixed-Use|WMU|Office Residential|",
                       "Downtown|Town Center|Village|Commercial|Neighborhood Center|Community Center")

# Apply the binary classification directly to the macro zoning layer instead of heavy lot files
zoning_matrix_data <- zoning_joined %>%
  mutate(
    Is_Residential = ifelse(
      grepl(res_keywords, desc_, ignore.case = TRUE) & 
        !grepl("Airport/Residential|Heavy Industrial|Light Industrial", desc_, ignore.case = TRUE), 
      "Residential / Mixed-Use Zoning", 
      "Pure Commercial / Industrial / Parks"
    )
  )

# Render the clean zoning framework matrix map
map_residential <- ggplot() +
  # Plot using the clean macro zoning layer shapes (color = NA removes boundaries for a solid matrix look)
  geom_sf(data = zoning_matrix_data, aes(fill = Is_Residential), color = NA) +
  
  # Overlay city limits to provide structural geographic anchors
  geom_sf(data = city_outlines, fill = NA, color = "#4A4A4A", size = 0.5) +
  
  # Add semi-transparent city headers pushed 4,000 feet above boundaries
  geom_sf_text(data = city_labels, aes(label = City), 
               color = "black", fontface = "bold", size = 3, alpha = 0.6, check_overlap = TRUE) +
  
  # Explicit color formatting using your preferred light periwinkle and violet tones
  scale_fill_manual(
    values = c(
      "Residential / Mixed-Use Zoning"       = "#B19FF1",  # Light Violet
      "Pure Commercial / Industrial / Parks" = "#CCCCFF"   # Light Periwinkle
    ),
    name = "Regulatory Framework"
  ) +
  
  # Clear titles anchoring the policy context of the analysis map
  labs(
    title = "Residential vs Non-Residential Zoning Footprint Matrix",
    subtitle = "Consolidated view of regulatory allowances for housing development across Clark County",
    caption = "City borders outlined in dark grey. Internal individual parcel lines removed for clarity."
  ) +
  
  # Apply layout cleanup parameters to strip out raw coordinates and gridlines
  theme_minimal() + 
  theme(
    panel.grid = element_blank(), 
    axis.text = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "#4A4A4A", size = 10)
  )

# Save the finalized clean graphic to your specified documents folder
ggsave(
  filename = file.path(output_dir, "Zoning_Residential_Footprint_Matrix.png"), 
  plot = map_residential, 
  width = 10, 
  height = 8, 
  dpi = 300, 
  bg = "white"
)


# =========================================================================
# 9. SUMMARY TABLE: POTENTIAL NEW HOMES BY CITY URBAN GROWTH BOUNDARY
# =========================================================================

# Intersect the final parcel capacity model with your clean city outlines
city_intersections <- st_intersection(lots_capacity_model, city_outlines)
city_intersections$city_intersect_area <- st_area(city_intersections)

# Assign each lot strictly to the city UGB where it has the largest overlap
lots_with_city <- city_intersections %>%
  group_by(prop_id) %>%
  arrange(desc(city_intersect_area), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

# Aggregate the total net realizable homes grouped by City boundary
city_ugb_table <- lots_with_city %>%
  st_drop_geometry() %>%
  group_by(City) %>%
  summarise(
    Potential_New_Homes = sum(Net_Realizable_Homes, na.rm = TRUE)
  ) %>%
  arrange(desc(Potential_New_Homes))

# Display the resulting table in your R console
print(city_ugb_table)

# Export the summary table as a CSV file to your local documents directory
write.csv(city_ugb_table, 
          file = file.path(output_dir, "City_UGB_Housing_Potential.csv"), 
          row.names = FALSE)
