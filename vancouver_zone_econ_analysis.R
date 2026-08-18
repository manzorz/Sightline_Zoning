rm(list = ls())

library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(terra)
library(exactextractr)

# =========================================================================
# 1. LOAD BASE DATA & SET PROJECTION
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
# 4. ENVIRONMENT INTEGRATION & EXTRACT GEOMETRIC HAZARD SHAPE MASKS
# =========================================================================
output_dir <- "C:\\Users\\gmann\\Documents\\ClarkCountyZoning"
cache_file <- file.path(output_dir, "processed_lots_capacity.rds")

# Check if the fully consolidated capacity model file already exists on your disk
if (file.exists(cache_file)) {
  
  cat("Found fully consolidated capacity model file. Loading cache instantly...\n")
  lots_capacity_model <- readRDS(cache_file)
  
} else {
  
  cat("No cache found. Processing 12 vector constraint layers simultaneously...\n")
  
  # -------------------------------------------------------------------------
  # STEP A: RAW VECTOR DISK IMPORTATION
  # -------------------------------------------------------------------------
  load_raw_shp <- function(file_path) {
    if (file.exists(file_path)) {
      st_read(file_path, quiet = TRUE) %>% st_transform(target_crs) %>% st_make_valid()
    } else {
      NULL
    }
  }
  
  cat("Importing environmental, geological, and jurisdictional vector frames...\n")
  slopes_df       <- load_raw_shp("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/Slopes.shp")
  wet_vec_df      <- load_raw_shp("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/WetInv.shp")
  erosion_df      <- load_raw_shp("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/ErosionHazard.shp")
  habitat_df      <- load_raw_shp("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/Habitat.shp")
  hyd_poly_df     <- load_raw_shp("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/HydPoly.shp")
  liq_df          <- load_raw_shp("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/Liquefaction.shp")
  landslid_df     <- load_raw_shp("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/Lndslid.shp")
  landslp_df      <- load_raw_shp("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/Lndslp.shp")
  mines_df        <- load_raw_shp("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/Mines.shp")
  tribal_df       <- load_raw_shp("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/TribalLands.shp")
  aquifer_df      <- load_raw_shp("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/Aquifer.shp")
  wui_proposed_df <- load_raw_shp("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/WildlandUrbanInterfaceProposed.shp")
  
  # -------------------------------------------------------------------------
  # STEP B: SEGREGATING GEOMETRIC RULES BY POLICY SEVERITY (HARD MASKS)
  # -------------------------------------------------------------------------
  cat("Executing layer-specific architectural logic evaluations...\n")
  
  # 1. Topography: Isolate ONLY extreme steep slopes (>= 40% based on Clark County Title 40 rules)
  hard_slope_mask <- slopes_df %>%
    filter(grepl("40 - 100|greater than 100", desc_, ignore.case = TRUE)) %>%
    st_geometry() %>% st_union()
  
  # Isolate the mitigable slope layers for downstream financial risk mapping
  slope_15_25_mask <- slopes_df %>% filter(grepl("15 - 25 percent", desc_, ignore.case = TRUE)) %>% st_geometry() %>% st_union()
  slope_25_40_mask <- slopes_df %>% filter(grepl("25 - 40 percent", desc_, ignore.case = TRUE)) %>% st_geometry() %>% st_union()
  
  # 2. Hard Environmental Bounds: High-resolution vector wetlands, core habitats, and water bodies
  wetland_mask  <- if(!is.null(wet_vec_df)) st_union(st_geometry(wet_vec_df)) else NULL
  habitat_mask  <- if(!is.null(habitat_df)) st_union(st_geometry(habitat_df)) else NULL
  hyd_poly_mask <- if(!is.null(hyd_poly_df)) st_union(st_geometry(hyd_poly_df)) else NULL
  
  # 3. Geological Hazards: Active landslide zones
  landslide_mask <- if(!is.null(landslid_df)) st_union(st_geometry(landslid_df)) else NULL
  landslp_mask   <- if(!is.null(landslp_df)) st_union(st_geometry(landslp_df)) else NULL
  severe_landslide_boundary <- if(!is.null(landslide_mask) || !is.null(landslp_mask)) {
    st_union(c(landslide_mask, landslp_mask))
  } else {
    NULL
  }
  
  # 4. Long-term Resource Operations
  mines_mask  <- if(!is.null(mines_df)) st_union(st_geometry(mines_df)) else NULL
  
  # 5. Sovereign Tribal Lands: Complete jurisdictional cutout outside local city/county codes
  tribal_mask <- if(!is.null(tribal_df)) st_union(st_geometry(tribal_df)) else NULL
  
  # 6. Unifying Hard Spatial Constraints (Land footprint drops completely to 0)
  hard_exclusion_list   <- list(hard_slope_mask, wetland_mask, habitat_mask, hyd_poly_mask, 
                                severe_landslide_boundary, mines_mask, tribal_mask)
  valid_exclusions      <- hard_exclusion_list[!sapply(hard_exclusion_list, is.null)]
  master_exclusion_mask <- do.call(st_union, valid_exclusions)
  
  # -------------------------------------------------------------------------
  # STEP C: RUN INDEPENDENT HAZARD ATTRIBUTION SPATIAL INTERSECTIONS
  # -------------------------------------------------------------------------
  cat("Calculating individual hazard footprint deductions for analysis reports...\n")
  
  calc_overlap_acres <- function(parcels, constraint_mask) {
    if (is.null(constraint_mask)) return(rep(0, nrow(parcels)))
    intersections <- st_intersection(st_geometry(parcels), constraint_mask)
    if (length(intersections) == 0) return(rep(0, nrow(parcels)))
    
    overlap_df <- st_intersection(parcels, constraint_mask)
    overlap_df$area_sqft <- as.numeric(st_area(overlap_df))
    summary_df <- overlap_df %>% st_drop_geometry() %>% 
      group_by(prop_id) %>% summarise(acres = sum(area_sqft, na.rm = TRUE) / 43560)
    
    return(summary_df)
  }
  
  lots_wetland_loss <- calc_overlap_acres(lots_with_rules, wetland_mask) %>% rename(Wetland_Acres = acres)
  lots_slope_loss   <- calc_overlap_acres(lots_with_rules, hard_slope_mask) %>% rename(Critical_Slope_Acres = acres)
  lots_total_loss   <- calc_overlap_acres(lots_with_rules, master_exclusion_mask) %>% rename(Hard_Excluded_Acres = acres)
  
  
  # -------------------------------------------------------------------------
  # 5. INTEGRATED CAPACITY VOLUMETRIC CALCULATION & ECONOMIC ENGINE
  # -------------------------------------------------------------------------
  cat("Calculating geometric intersections and tracking mitigation cost indicators...\n")
  
  ASSUMED_AVG_STORY_HEIGHT <- 11  
  ASSUMED_AVG_UNIT_SIZE    <- 1200 
  
  lots_capacity_model <- lots_with_rules %>%
    left_join(lots_wetland_loss, by = "prop_id") %>%
    left_join(lots_slope_loss, by = "prop_id") %>%
    left_join(lots_total_loss, by = "prop_id") %>%
    mutate(
      # Clean up missing data joins from empty overlap records
      Wetland_Acres        = ifelse(is.na(Wetland_Acres), 0, Wetland_Acres),
      Critical_Slope_Acres = ifelse(is.na(Critical_Slope_Acres), 0, Critical_Slope_Acres),
      Hard_Excluded_Acres  = ifelse(is.na(Hard_Excluded_Acres), 0, Hard_Excluded_Acres),
      
      Lot_Acres            = as.numeric(Shape_Area) / 43560,
      
      # 1. DEDUCT SPATIAL FOOTPRINT ACREAGE FROM PARENT SHAPES (Preserves 15-40% slopes)
      Net_Lot_Acres         = pmax(0, Lot_Acres - Hard_Excluded_Acres),
      
      # 2. MITIGABLE HAZARD INTERSECTIONS (Preserves acreage, injects cost multipliers)
      Has_Slope_15_25       = as.logical(st_intersects(geometry, slope_15_25_mask)),
      Has_Slope_25_40       = as.logical(st_intersects(geometry, slope_25_40_mask)),
      Has_Erosion_Hazard    = as.logical(st_intersects(geometry, st_union(st_geometry(erosion_df)))),
      Has_Liquefaction_Risk = as.logical(st_intersects(geometry, st_union(st_geometry(liq_df)))),
      Has_Aquifer_Protected = as.logical(st_intersects(geometry, st_union(st_geometry(aquifer_df)))),
      Has_WUI_Proposed      = as.logical(st_intersects(geometry, st_union(st_geometry(wui_proposed_df)))),
      
      Has_Slope_15_25       = ifelse(is.na(Has_Slope_15_25), FALSE, Has_Slope_15_25),
      Has_Slope_25_40       = ifelse(is.na(Has_Slope_25_40), FALSE, Has_Slope_25_40),
      Has_Erosion_Hazard    = ifelse(is.na(Has_Erosion_Hazard), FALSE, Has_Erosion_Hazard),
      Has_Liquefaction_Risk = ifelse(is.na(Has_Liquefaction_Risk), FALSE, Has_Liquefaction_Risk),
      Has_Aquifer_Protected = ifelse(is.na(Has_Aquifer_Protected), FALSE, Has_Aquifer_Protected),
      Has_WUI_Proposed      = ifelse(is.na(Has_WUI_Proposed), FALSE, Has_WUI_Proposed),
      
      # Calculate the cumulative cost premium matrix per square foot built
      Added_Cost_Per_SqFt   = 0 + 
        ifelse(Has_Slope_15_25, 15, 0) +        # Step foundations / grading cost
        ifelse(Has_Slope_25_40, 35, 0) +        # Structural deep pier / stilt anchors
        ifelse(Has_Erosion_Hazard, 12, 0) +     # Site stabilization controls
        ifelse(Has_Liquefaction_Risk, 25, 0) +  # Foundation pilings
        ifelse(Has_Aquifer_Protected, 8, 0) +   # Stormwater filtration vaults
        ifelse(Has_WUI_Proposed, 15, 0),        # Fire envelope hardening
      
      # 3. VOLUMETRIC AND REGULATORY GEOMETRY REDUCTIONS
      Setback_Reduction_Factor = case_when(
        Front_Setback_Ft >= 20 ~ 0.70,  
        Front_Setback_Ft == 15 ~ 0.75,  
        Front_Setback_Ft <= 10 ~ 0.85,  
        TRUE                    ~ 0.80
      ),
      Net_Footprint_SqFt       = (Net_Lot_Acres * 43560) * Setback_Reduction_Factor,
      Max_Stories              = floor(Max_Height_Ft / ASSUMED_AVG_STORY_HEIGHT),
      
      Max_Lot_Coverage_Pct     = case_when(
        grepl("R1-|R-6|R-7.5|R-10|RLD", desc_) ~ 0.40, 
        grepl("R-|MF|OR", desc_)                ~ 0.60, 
        TRUE                                   ~ 0.50
      ),
      
      Max_Potential_Floor_Area = (Net_Footprint_SqFt * Max_Lot_Coverage_Pct) * Max_Stories,
      Physical_Unit_Capacity   = floor(Max_Potential_Floor_Area / ASSUMED_AVG_UNIT_SIZE),
      Regulatory_Density_Cap   = floor(Net_Lot_Acres * UnitsPerAc),
      
      # 4. CHOKEPOINT CONSTRACTION INTERSECTION EVALUATION
      MaxPossibleConstruction  = pmin(Physical_Unit_Capacity, Regulatory_Density_Cap),
      Net_Realizable_Homes     = pmax(0, MaxPossibleConstruction - Units),
      Is_Useless_Upzone        = ifelse(Regulatory_Density_Cap > Units & MaxPossibleConstruction <= Units, TRUE, FALSE)
    )
  
  cat("Saving consolidated data cache to disk storage layout...\n")
  saveRDS(lots_capacity_model, file = cache_file)
}

# =========================================================================
# 6. CONSOLIDATE LOGICAL HOUSING TYPE ASSIGNMENT
# =========================================================================
res_keywords <- paste0("Residential|Resid|Single-family|Single Family|Multifamily|Multiple-family|",
                       "Mobile Home|MHP|MDR|LDR|HDR|RLD|Mixed Use|Mixed-Use|WMU|Office Residential|",
                       "Downtown|Town Center|Village|Commercial|Neighborhood Center|Community Center")

zoning_binary <- lots_capacity_model %>%
  mutate(
    Is_Residential = ifelse(
      grepl(res_keywords, desc_, ignore.case = TRUE) & 
        !grepl("Airport/Residential|Heavy Industrial|Light Industrial", desc_, ignore.case = TRUE), 
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
# EXTRA STEP: STRIP OUT CEMETERY FOOTPRINTS TO PREVENT ARTIFICIAL CAPACITY
# =========================================================================
cemeteries <- st_read("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/Cemetery.shp") %>%
  st_transform(target_crs) %>%
  st_make_valid()

lots_capacity_model <- lots_capacity_model %>%
  mutate(
    Intersects_Cemetery = as.logical(st_intersects(geometry, st_union(cemeteries))),
    Intersects_Cemetery = ifelse(is.na(Intersects_Cemetery), FALSE, Intersects_Cemetery),
    MaxPossibleConstruction = ifelse(Intersects_Cemetery, 0, MaxPossibleConstruction),
    Net_Realizable_Homes    = ifelse(Intersects_Cemetery, 0, Net_Realizable_Homes)
  )

# =========================================================================
# 8. VISUALIZATION AND GRAPHIC OUTPUT GENERATION
# =========================================================================

# --- GRAPHIC 1: Zone-Level Growth Potential Summary ---
zone_summary_layer <- lots_capacity_model %>%
  group_by(Zoning_ID) %>%
  summarise(geometry = st_union(geometry), Total_Net_Realizable = sum(Net_Realizable_Homes, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Zoning_Status = ifelse(Total_Net_Realizable > 0, "Under Zoned Limit (Has Room)", "At/Over Zoned Limit"))

map_categorical <- ggplot() +
  geom_sf(data = zone_summary_layer, aes(fill = Zoning_Status), color = NA) +
  geom_sf(data = city_outlines, fill = NA, color = "#4A4A4A", size = 0.5, linetype = "solid") +
  geom_sf_text(data = city_labels, aes(label = City), color = "black", fontface = "bold", size = 3, alpha = 0.6, check_overlap = TRUE) +
  scale_fill_manual(values = c("Under Zoned Limit (Has Room)" = "#21918c", "At/Over Zoned Limit" = "#CCCCFF"), name = "Zoning Limitations") +
  theme_minimal() + theme(panel.grid = element_blank(), axis.text = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "bottom")

# Print to RStudio Plot Viewer Pane
print(map_categorical)

# Save to Local Disk Storage Directory
ggsave(filename = file.path(output_dir, "Zoning_Limitations_Categorical.png"), plot = map_categorical, width = 10, height = 8, dpi = 300, bg = "white")


# --- GRAPHIC 2: Lot-Level ADJUSTED Realizable Headroom Gradient Map ---
lots_gradient <- lots_capacity_model %>%
  mutate(Headroom_Display = ifelse(Net_Realizable_Homes == 0, NA, Net_Realizable_Homes))

map_gradient <- ggplot() +
  geom_sf(data = lots_gradient, fill = "#D3D3D3", color = NA) +
  geom_sf(data = filter(lots_gradient, !is.na(Headroom_Display)), aes(fill = Headroom_Display), color = NA) +
  geom_sf(data = city_outlines, fill = NA, color = "#4A4A4A", size = 0.5) +
  geom_sf_text(data = city_labels, aes(label = City), color = "black", fontface = "bold", size = 3, alpha = 0.6, check_overlap = TRUE) +
  scale_fill_gradient(
    low = "#FFFFE0", 
    high = "#FF0000", 
    
    # Using \n breaks the long sentence into a neat, left-aligned vertical stack
    name = paste0("Remaining Potential\n",
                  "Housing Within\n",
                  "Existing Zoning\n",
                  "Limitations\n",
                  "(Net New Units)"), 
    
    na.value = "#D3D3D3"
  ) +
  labs(
    title = "Net Realizable Housing Headroom Gradient Map", 
    subtitle = "Lot-level units accounting for zoning parameters, existing homes, environmental traits, and sovereign lands"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.text = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position = "right")

print(map_gradient)
ggsave(filename = file.path(output_dir, "Zoning_Headroom_Gradient.png"),
       plot = map_gradient, width = 10, height = 8, dpi = 300, bg = "white")

# --- GRAPHIC 3: Residential Footprint Matrix ---
zoning_matrix_data <- zoning_cleaned %>%
  left_join(st_drop_geometry(lots_capacity_model) %>% group_by(Zoning_ID) %>% summarise(Units = sum(Units, na.rm=TRUE)), by="Zoning_ID") %>%
  mutate(Is_Residential = ifelse(grepl(res_keywords, desc_, ignore.case = TRUE) & !grepl("Airport/Residential|Heavy Industrial|Light Industrial", desc_, ignore.case = TRUE), 
                                 "Residential / Mixed-Use Zoning", "Pure Commercial / Industrial / Parks"))

map_residential <- ggplot() +
  geom_sf(data = zoning_matrix_data, aes(fill = Is_Residential), color = NA) +
  geom_sf(data = city_outlines, fill = NA, color = "#4A4A4A", size = 0.5) +
  geom_sf_text(data = city_labels, aes(label = City), color = "black", fontface = "bold", size = 3, alpha = 0.6, check_overlap = TRUE) +
  scale_fill_manual(values = c("Residential / Mixed-Use Zoning" = "#B19FF1", "Pure Commercial / Industrial / Parks" = "#CCCCFF"), name = "Regulatory Framework") +
  theme_minimal() + theme(panel.grid = element_blank(), axis.text = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "bottom")

# Print to RStudio Plot Viewer Pane
print(map_residential)

ggsave(filename = file.path(output_dir, "Zoning_Residential_Footprint_Matrix.png"), plot = map_residential, width = 10, height = 8, dpi = 300, bg = "white")

# --- GRAPHIC 4: Vancouver Urban Core Mixed-Use Expansion Zoom Map ---
cat("Generating Vancouver urban core mixed-use capacity addition zoom map...\n")

# 1. Define the baseline restrictive single-family/pure multi-family keyword array
restrictive_res_keywords <- "Residential|Resid|Single-family|Single Family|Multifamily|Multiple-family|Mobile Home|MHP|MDR|LDR|HDR|RLD"

# 2. Flag parcels that are ONLY caught by the expanded mixed-use/commercial keyword array
lots_mixed_use_zoom <- lots_capacity_model %>%
  mutate(
    Is_Baseline_Residential = grepl(restrictive_res_keywords, desc_, ignore.case = TRUE),
    Is_Expanded_Residential = grepl(res_keywords, desc_, ignore.case = TRUE) & 
      !grepl("Airport/Residential|Heavy Industrial|Light Industrial", desc_, ignore.case = TRUE),
    
    # Isolate the exact parcels added by the zoning classification expansion
    Addition_Status = case_when(
      Is_Baseline_Residential & Is_Expanded_Residential ~ "Baseline Residential Stock",
      !Is_Baseline_Residential & Is_Expanded_Residential ~ "Added via Commercial/Mixed-Use Expansion",
      TRUE                                               ~ "Non-Residential / Pure Industrial / Parks"
    )
  )

# 3. Render the focused urban core zoom-in map configuration
map_vancouver_zoom <- ggplot() +
  # Draw all parcels categorized by their addition status (color = NA avoids line grid noise)
  geom_sf(data = lots_mixed_use_zoom, aes(fill = Addition_Status), color = NA) +
  
  # Overlay city limits to provide structural geographic anchors
  geom_sf(data = city_outlines, fill = NA, color = "#4A4A4A", size = 0.5) +
  
  # Add semi-transparent city headers pushed 4,000 feet above boundaries
  geom_sf_text(data = city_labels, aes(label = City), 
               color = "black", fontface = "bold", size = 3, alpha = 0.6, check_overlap = TRUE) +
  
  # Custom manual color fills to highlight additions clearly
  scale_fill_manual(
    values = c(
      "Added via Commercial/Mixed-Use Expansion"  = "#FF0000",  # Bright Red
      "Baseline Residential Stock"                = "#B19FF1",  # Soft Violet
      "Non-Residential / Pure Industrial / Parks" = "#CCCCFF"  # Pale Periwinkle
    ),
    name = "Inventory Status"
  ) +
  
  # CRITICAL EXTENT FIX: Defines bounding box coordinates inside EPSG 2927 (ftUS) space for Vancouver's grid
  # Bypasses the narrow crop crash by targeting the southwest core of the county layout
  coord_sf(xlim = c(1060000, 1115000), ylim = c(70000, 145000), expand = FALSE) +
  
  labs(
    title = "Vancouver Urban Core Housing Inventory Expansion Focus",
    subtitle = "Zoomed perspective isolating parcel adjustments unlocked by capturing vertical multi-family allowances in commercial hubs",
    caption = "Bright red clusters highlight commercial, downtown, and town center village zones that legally permit vertical multi-family housing."
  ) +
  
  theme_minimal() + 
  theme(
    panel.grid = element_blank(), 
    axis.text = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    legend.position = "bottom",
    
    # Align text relative to the plot panel bounds to match centered legend tracking
    plot.title.position = "panel",
    plot.caption.position = "panel",
    
    # Precise margin alignment parameters to anchor headers squarely above the legend container text
    plot.title = element_text(face = "bold", size = 14, hjust = 0, margin = margin(t = 10, r = 0, b = 2, l = 85)),
    plot.subtitle = element_text(color = "#4A4A4A", size = 10, hjust = 0, margin = margin(t = 0, r = 0, b = 10, l = 85)),
    plot.caption = element_text(color = "#4A4A4A", size = 8, hjust = 0, margin = margin(t = 10, r = 0, b = 10, l = 85))
  )

# Print cleanly to your active RStudio Plot Viewer Pane for immediate visual verification
print(map_vancouver_zoom)

# Save the graphic file to your local documents directory
ggsave(
  filename = file.path(output_dir, "Zoning_MixedUse_Vancouver_Zoom.png"), 
  plot = map_vancouver_zoom, 
  width = 10, 
  height = 8, 
  dpi = 300, 
  bg = "white"
)

# --- GRAPHIC 5: County-Wide Commercial & Mixed-Use Housing Stock Expansion ---
cat("Generating county-wide commercial and mixed-use capacity addition map...\n")

# 1. Define the baseline restrictive single-family/pure multi-family keyword array
restrictive_res_keywords <- "Residential|Resid|Single-family|Single Family|Multifamily|Multiple-family|Mobile Home|MHP|MDR|LDR|HDR|RLD"

# 2. Flag parcels that are ONLY caught by the expanded mixed-use/commercial keyword array
lots_mixed_use_additions <- lots_capacity_model %>%
  mutate(
    Is_Baseline_Residential = grepl(restrictive_res_keywords, desc_, ignore.case = TRUE),
    Is_Expanded_Residential = grepl(res_keywords, desc_, ignore.case = TRUE) & 
      !grepl("Airport/Residential|Heavy Industrial|Light Industrial", desc_, ignore.case = TRUE),
    
    # Isolate the exact parcels added by the zoning classification expansion across the county
    Addition_Status = case_when(
      Is_Baseline_Residential & Is_Expanded_Residential ~ "Baseline Residential Stock",
      !Is_Baseline_Residential & Is_Expanded_Residential ~ "Added via Commercial/Mixed-Use Expansion",
      TRUE                                               ~ "Non-Residential / Pure Industrial / Parks"
    )
  )

# 3. Render the comprehensive county-wide addition map
map_mixed_use_additions <- ggplot() +
  geom_sf(data = lots_mixed_use_additions, aes(fill = Addition_Status), color = NA) +
  geom_sf(data = city_outlines, fill = NA, color = "#4A4A4A", size = 0.5) +
  geom_sf_text(data = city_labels, aes(label = City), 
               color = "black", fontface = "bold", size = 3, alpha = 0.6, check_overlap = TRUE) +
  
  scale_fill_manual(
    values = c(
      "Added via Commercial/Mixed-Use Expansion"  = "#FF0000",  # Bright Red
      "Baseline Residential Stock"                = "#B19FF1",  # Soft Violet
      "Non-Residential / Pure Industrial / Parks" = "#CCCCFF"  # Pale Periwinkle
    ),
    name = "Inventory Status"
  ) +
  
  labs(
    title = "County-Wide Housing Inventory Expansion: Commercial & Mixed-Use Frameworks",
    subtitle = "Isolating parcels unlocked by expanding regional housing filters to capture multi-family allowances in commercial hubs",
    caption = "Bright red clusters highlight commercial, downtown, and town center village zones county-wide that legally permit vertical multi-family housing."
  ) +
  
  theme_minimal() + 
  theme(
    panel.grid = element_blank(), 
    axis.text = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    legend.position = "bottom",
    
    # Align text relative to the plot panel bounds to match centered legend tracking
    plot.title.position = "panel",
    plot.caption.position = "panel",
    
    # Precise margin alignment parameters to anchor headers squarely above the legend container text
    plot.title = element_text(face = "bold", size = 14, hjust = 0, margin = margin(t = 10, r = 0, b = 2, l = 85)),
    plot.subtitle = element_text(color = "#4A4A4A", size = 10, hjust = 0, margin = margin(t = 0, r = 0, b = 10, l = 85)),
    plot.caption = element_text(color = "#4A4A4A", size = 8, hjust = 0, margin = margin(t = 10, r = 0, b = 10, l = 85))
  )

# Print to your active RStudio Plot Viewer Pane for immediate proofing
print(map_mixed_use_additions)

# Save the graphic file to your local documents directory
ggsave(
  filename = file.path(output_dir, "Zoning_MixedUse_Housing_Additions.png"), 
  plot = map_mixed_use_additions, 
  width = 10, 
  height = 8, 
  dpi = 300, 
  bg = "white"
)

# =========================================================================
# 9. INTEGRATED ANALYSIS REPORT MATRIX (Capacity vs Constraint Reductions)
# =========================================================================
city_intersections <- st_intersection(lots_capacity_model, city_outlines)
city_intersections$city_intersect_area <- st_area(city_intersections)

lots_with_city <- city_intersections %>%
  group_by(prop_id) %>% arrange(desc(city_intersect_area), .by_group = TRUE) %>% slice(1) %>% ungroup()

hazard_lookup <- lots_capacity_model %>%
  st_drop_geometry() %>%
  select(prop_id, Wetland_Acres, Critical_Slope_Acres, Intersects_Cemetery)

lots_with_city_fixed <- lots_with_city %>%
  left_join(hazard_lookup, by = "prop_id")

city_housing_report_matrix <- lots_with_city_fixed %>%
  st_drop_geometry() %>%
  mutate(
    Gross_Zoned_Capacity       = floor(Lot_Acres * UnitsPerAc),
    Units_Lost_To_Constraints  = pmax(0, Gross_Zoned_Capacity - MaxPossibleConstruction)
  ) %>%
  group_by(City) %>%
  summarise(
    Total_Viable_New_Housing_Units              = sum(Net_Realizable_Homes, na.rm = TRUE),
    Total_Housing_Potential_Lost_To_Constraints = sum(Units_Lost_To_Constraints, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Viable_New_Housing_Units))

print(city_housing_report_matrix)
write.csv(city_housing_report_matrix, file = file.path(output_dir, "City_UGB_Unified_Housing_Capacity_Report.csv"), row.names = FALSE)

# =========================================================================
# 9b. CONSTRAINT ATTRIBUTION BREAKDOWN: DISAGGREGATING LOSSES BY POLICY TYPE
# =========================================================================
cat("Deconstructing lost housing capacity metrics by explicit vector limitation type...\n")

city_hazard_breakdown_matrix <- lots_with_city_fixed %>%
  st_drop_geometry() %>%
  mutate(
    Gross_Zoned_Capacity       = floor(Lot_Acres * UnitsPerAc),
    Units_Lost_To_Wetlands     = ifelse(Wetland_Acres > 0, floor(Wetland_Acres * UnitsPerAc), 0),
    Units_Lost_To_Severe_Slopes = ifelse(Critical_Slope_Acres > 0, floor(Critical_Slope_Acres * UnitsPerAc), 0),
    Units_Lost_To_Cemetaries   = ifelse(Intersects_Cemetery, Gross_Zoned_Capacity, 0)
  ) %>%
  group_by(City) %>%
  summarise(
    Lost_To_Vector_Wetlands     = sum(Units_Lost_To_Wetlands, na.rm = TRUE),
    Lost_To_Severe_Slopes_40Pct = sum(Units_Lost_To_Severe_Slopes, na.rm = TRUE),
    Lost_To_Cemetery_Dedication = sum(Units_Lost_To_Cemetaries, na.rm = TRUE)
  ) %>%
  arrange(desc(Lost_To_Vector_Wetlands + Lost_To_Severe_Slopes_40Pct))

print(city_hazard_breakdown_matrix)
write.csv(city_hazard_breakdown_matrix, file = file.path(output_dir, "City_UGB_Housing_Loss_Constraint_Attribution.csv"), row.names = FALSE)
