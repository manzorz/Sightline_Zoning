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
# 4 & 5. ENVIRONMENTAL INTEGRATION & MASTER PARCEL CAPACITY PIPELINE
# =========================================================================
output_dir <- "C:\\Users\\gmann\\Documents\\ClarkCountyZoning"
cache_file <- file.path(output_dir, "processed_lots_capacity.rds")

if (file.exists(cache_file)) {
  
  cat("Found fully consolidated capacity model file. Loading cache instantly...\n")
  lots_capacity_model <- readRDS(cache_file)
  
} else {
  
  cat("No cache found. Initiating multi-layer raster/vector constraint matrix calculation...\n")
  
  # -------------------------------------------------------------------------
  # STEP A: RASTER WETLAND INTERSECTION PROCESSING
  # -------------------------------------------------------------------------
  gdb_path <- "C:/Users/gmann/Downloads/SEA_BIO_WetlandsInventory/Wetlands_Inventory.gdb"
  wetlands_raster <- rast(gdb_path, lyrs = "wetlands_inventory_2016")
  
  if (crs(wetlands_raster, proj = TRUE) != crs(lots_with_rules, proj = TRUE)) {
    wetlands_raster <- project(wetlands_raster, crs(lots_with_rules), method = "near")
  }
  
  cat("Extracting wetland raster fractions across parcel boundaries...\n")
  extracted_wetlands <- exact_extract(wetlands_raster, lots_with_rules, include_cols = "prop_id")
  
  lot_wetland_summary <- bind_rows(extracted_wetlands) %>%
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
  
  # -------------------------------------------------------------------------
  # STEP B: RAW VECTOR DISK IMPORTATION
  # -------------------------------------------------------------------------
  load_raw_shp <- function(file_path) {
    if (file.exists(file_path)) {
      st_read(file_path, quiet = TRUE) %>% st_transform(target_crs) %>% st_make_valid()
    } else {
      NULL
    }
  }
  
  cat("Importing environmental, geological, and jurisdictional vector frames...\n")
  slopes_df      <- load_raw_shp("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/Slopes.shp")
  wet_vec_df     <- load_raw_shp("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/WetInv.shp")
  erosion_df     <- load_raw_shp("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/ErosionHazard.shp")
  habitat_df     <- load_raw_shp("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/Habitat.shp")
  hyd_poly_df    <- load_raw_shp("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/HydPoly.shp")
  liq_df         <- load_raw_shp("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/Liquefaction.shp")
  landslid_df    <- load_raw_shp("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/Lndslid.shp")
  landslp_df     <- load_raw_shp("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/Lndslp.shp")
  mines_df       <- load_raw_shp("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/Mines.shp")
  tribal_df      <- load_raw_shp("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/TribalLands.shp")
  aquifer_df     <- load_raw_shp("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/Aquifer.shp")
  wui_proposed_df<- load_raw_shp("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/WildlandUrbanInterfaceProposed.shp")
  
  # -------------------------------------------------------------------------
  # STEP C: SEGREGATING GEOMETRIC RULES BY LEGAL SEVERITY
  # -------------------------------------------------------------------------
  cat("Executing layer-specific architectural logic evaluations...\n")
  
  # 1. Topography: Isolate severe steep slopes (>15% gradients or Steep text classifications)
  steep_slope_mask <- slopes_df %>%
    filter(grepl("15 - 25|25 - 40|40 - 100|greater than 100|Steep", desc_, ignore.case = TRUE) | 
             grepl("Steep", GENDESC, ignore.case = TRUE)) %>%
    st_geometry() %>% st_union()
  
  # 2. Habitat: Areas completely off-limits to core footprint construction
  habitat_mask <- if(!is.null(habitat_df)) st_union(st_geometry(habitat_df)) else NULL
  
  # 3. Hydrology Polygons: Open water bodies precluding substantial construction
  hyd_poly_mask <- if(!is.null(hyd_poly_df)) st_union(st_geometry(hyd_poly_df)) else NULL
  
  # 4. Landslides: High-hazard active slope movements preventing foundation permits
  landslide_mask <- if(!is.null(landslid_df)) st_union(st_geometry(landslid_df)) else NULL
  landslp_mask   <- if(!is.null(landslp_df)) st_union(st_geometry(landslp_df)) else NULL
  severe_landslide_boundary <- if(!is.null(landslide_mask) || !is.null(landslp_mask)) {
    st_union(c(landslide_mask, landslp_mask))
  } else {
    NULL
  }
  
  # 5. Long-term Resource Operations: Active surface mining permits
  mines_mask <- if(!is.null(mines_df)) st_union(st_geometry(mines_df)) else NULL
  
  # 6. Sovereign Exemptions: Native American tribal land cuts completely clear of local codes
  tribal_mask <- if(!is.null(tribal_df)) st_union(st_geometry(tribal_df)) else NULL
  
  # 7. Unifying Hard Spatial Exclusion Vectors (Land footprint drops completely to 0)
  hard_exclusion_list   <- list(steep_slope_mask, habitat_mask, hyd_poly_mask, severe_landslide_boundary, mines_mask, tribal_mask)
  valid_exclusions      <- hard_exclusion_list[!sapply(hard_exclusion_list, is.null)]
  master_exclusion_mask <- do.call(st_union, valid_exclusions)
  
  # -------------------------------------------------------------------------
  # STEP D: COMPUTING LOT GEOMETRIES & FINANCIAL VIABILITY RISK TAGGING
  # -------------------------------------------------------------------------
  cat("Calculating geometric intersections and tracking mitigation cost indicators...\n")
  
  # Compute hard vector overlay drops lot-by-lot
  exclusion_intersections <- st_intersection(lots_with_rules, master_exclusion_mask)
  exclusion_intersections$dropped_sqft <- st_area(exclusion_intersections)
  
  lot_exclusion_summary <- exclusion_intersections %>%
    st_drop_geometry() %>%
    group_by(prop_id) %>%
    summarise(Hard_Excluded_Acres = as.numeric(sum(dropped_sqft, na.rm = TRUE)) / 43560)
  
  # -------------------------------------------------------------------------
  # STEP E: INTEGRATED CAPACITY VOLUMETRIC CALCULATION ENGINE
  # -------------------------------------------------------------------------
  ASSUMED_AVG_STORY_HEIGHT <- 11  
  ASSUMED_AVG_UNIT_SIZE    <- 1200 
  
  lots_capacity_model <- lots_with_rules %>%
    left_join(lot_wetland_summary, by = "prop_id") %>%
    left_join(lot_exclusion_summary, by = "prop_id") %>%
    mutate(
      Pct_Wetland           = ifelse(is.na(Pct_Wetland), 0, Pct_Wetland),
      Hard_Excluded_Acres   = ifelse(is.na(Hard_Excluded_Acres), 0, Hard_Excluded_Acres),
      
      Lot_Acres             = as.numeric(Shape_Area) / 43560,
      Raster_Wetland_Acres  = Lot_Acres * (Pct_Wetland / 100),
      
      # 1. DEDUCT SPATIAL CONSTRICTIONS TO EMERGE WITH NET DEVELOPABLE GROUND
      Net_Lot_Acres         = pmax(0, Lot_Acres - Raster_Wetland_Acres - Hard_Excluded_Acres),
      
      # 2. INTERSECTION SPATIAL CHECKS FOR OVERLAY MITIGATION COST BALANCES
      # Erosion, Liquefaction, Aquifers, and Proposed WUI layers DO NOT reduce acreage. 
      # They flag the property matrix and inject baseline placeholder engineering multipliers.
      Has_Erosion_Hazard    = as.logical(st_intersects(geometry, st_union(st_geometry(erosion_df)))),
      Has_Liquefaction_Risk = as.logical(st_intersects(geometry, st_union(st_geometry(liq_df)))),
      Has_Aquifer_Protected = as.logical(st_intersects(geometry, st_union(st_geometry(aquifer_df)))),
      Has_WUI_Proposed      = as.logical(st_intersects(geometry, st_union(st_geometry(wui_proposed_df)))),
      
      Has_Erosion_Hazard    = ifelse(is.na(Has_Erosion_Hazard), FALSE, Has_Erosion_Hazard),
      Has_Liquefaction_Risk = ifelse(is.na(Has_Liquefaction_Risk), FALSE, Has_Liquefaction_Risk),
      Has_Aquifer_Protected = ifelse(is.na(Has_Aquifer_Protected), FALSE, Has_Aquifer_Protected),
      Has_WUI_Proposed      = ifelse(is.na(Has_WUI_Proposed), FALSE, Has_WUI_Proposed),
      
      # ECONOMIC ANALYSIS: Build Financial Placeholder Matrix ($ increase per SqFt built)
      Added_Cost_Per_SqFt   = 0 +
        ifelse(Has_Erosion_Hazard, 12, 0) +     # Shoring, retaining stabilization engineering
        ifelse(Has_Liquefaction_Risk, 25, 0) +  # Deep pilings and foundational structural tie-beams
        ifelse(Has_Aquifer_Protected, 8, 0) +   # Advanced stormwater filter vaults / separators
        ifelse(Has_WUI_Proposed, 15, 0),        # Class A ignition-resistant roof & eave components
      
      # 3. VOLUMETRIC DIMENSIONAL REDUCTIONS
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
      Is_Useless_Upzone        = ifelse(Regulatory_Density_Cap > Units & MaxPossibleConstruction <= Units, TRUE, FALSE))
  
  cat("Saving consolidated data cache to disk storage layout...\n")
  saveRDS(lots_capacity_model, file = cache_file)
}

# =========================================================================
# 6. CONSOLIDATE LOGICAL HOUSING TYPE ASSIGNMENT (Expanded)
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
  mutate(
    X = label_x_coords, 
    Y = label_y_coords + 4000
  ) %>%
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
    Net_Realizable_Homes = ifelse(Intersects_Cemetery == TRUE, 0, Net_Realizable_Homes)
  )

# =========================================================================
# 8. VISUALIZATION AND GRAPHIC OUTPUT GENERATION
# =========================================================================
# --- GRAPHIC 1: Zone-Level Growth Potential Summary ---
zone_summary_layer <- lots_capacity_model %>%
  group_by(Zoning_ID) %>%
  summarise(
    geometry = st_union(geometry), 
    Total_Net_Realizable = sum(Net_Realizable_Homes, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Zoning_Status = ifelse(Total_Net_Realizable > 0, "Under Zoned Limit (Has Room)", "At/Over Zoned Limit")
  )

map_categorical <- ggplot() +
  geom_sf(data = zone_summary_layer, aes(fill = Zoning_Status), color = NA) +
  geom_sf(data = city_outlines, fill = NA, color = "#4A4A4A", size = 0.5, linetype = "solid") +
  geom_sf_text(data = city_labels, aes(label = City), color = "black", fontface = "bold", size = 3, alpha = 0.6, check_overlap = TRUE) +
  scale_fill_manual(
    values = c("Under Zoned Limit (Has Room)" = "#21918c", "At/Over Zoned Limit" = "#CCCCFF"), 
    name = "Zoning Limitations"
  ) +
  labs(
    title = "Zoning Limitations & Growth Potential", 
    subtitle = "Zone-level status aggregated from individual parcel headroom analysis"
  ) +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(), 
    axis.text = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    legend.position = "bottom"
  )

ggsave(filename = file.path(output_dir, "Zoning_Limitations_Categorical.png"), plot = map_categorical, width = 10, height = 8, dpi = 300, bg = "white")

# --- GRAPHIC 2: Lot-Level Realizable Headroom Gradient Map ---
lots_gradient <- lots_capacity_model %>%
  mutate(
    Headroom_Display = ifelse(Net_Realizable_Homes == 0, NA, Net_Realizable_Homes)
  )

map_gradient <- ggplot() +
  geom_sf(data = lots_gradient, fill = "#D3D3D3", color = NA) +
  geom_sf(data = filter(lots_gradient, !is.na(Headroom_Display)), aes(fill = Headroom_Display), color = NA) +
  geom_sf(data = city_outlines, fill = NA, color = "#4A4A4A", size = 0.5) +
  geom_sf_text(data = city_labels, aes(label = City), color = "black", fontface = "bold", size = 3, alpha = 0.6, check_overlap = TRUE) +
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

ggsave(filename = file.path(output_dir, "Zoning_Headroom_Gradient.png"), plot = map_gradient, width = 10, height = 8, dpi = 300, bg = "white")

# --- GRAPHIC 3: Residential Footprint Matrix ---
zoning_matrix_data <- zoning_cleaned %>%
  left_join(
    st_drop_geometry(lots_capacity_model) %>% 
      group_by(Zoning_ID) %>% 
      summarise(Units = sum(Units, na.rm = TRUE)), 
    by = "Zoning_ID"
  ) %>%
  mutate(
    Is_Residential = ifelse(
      grepl(res_keywords, desc_, ignore.case = TRUE) & 
        !grepl("Airport/Residential|Heavy Industrial|Light Industrial", desc_, ignore.case = TRUE),
      "Residential / Mixed-Use Zoning", 
      "Pure Commercial / Industrial / Parks"
    )
  )

map_residential <- ggplot() +
  geom_sf(data = zoning_matrix_data, aes(fill = Is_Residential), color = NA) +
  geom_sf(data = city_outlines, fill = NA, color = "#4A4A4A", size = 0.5) +
  geom_sf_text(data = city_labels, aes(label = City), color = "black", fontface = "bold", size = 3, alpha = 0.6, check_overlap = TRUE) +
  scale_fill_manual(
    values = c("Residential / Mixed-Use Zoning" = "#B19FF1", "Pure Commercial / Industrial / Parks" = "#CCCCFF"), 
    name = "Regulatory Framework"
  ) +
  labs(
    title = "Residential vs Non-Residential Zoning Footprint Matrix", 
    subtitle = "Consolidated view of regulatory allowances for housing development across Clark County"
  ) +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(), 
    axis.text = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    legend.position = "bottom"
  )

ggsave(filename = file.path(output_dir, "Zoning_Residential_Footprint_Matrix.png"),
       plot = map_residential, width = 10, height = 8, dpi = 300, bg = "white")

# =========================================================================
# 9. ECONOMIC MATRIX: HOUSING POTENTIAL AND INFRASTRUCTURE MITIGATION COSTS
# =========================================================================
cat("Compiling economic capacity and mitigation risk matrices...\n")

# Intersect the lot capacity model with clean city limits to map geographic accountability
city_intersections <- st_intersection(lots_capacity_model, city_outlines)
city_intersections$city_intersect_area <- st_area(city_intersections)

# Allocate each lot strictly to its primary highest percentage city match
lots_with_city_economic <- city_intersections %>%
  group_by(prop_id) %>%
  arrange(desc(city_intersect_area), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

# Execute economic aggregation across urban growth boundaries
city_economic_matrix <- lots_with_city_economic %>%
  st_drop_geometry() %>%
  group_by(City) %>%
  summarise(
    # 1. Baseline Realizable Housing Yield Tracking
    Potential_New_Homes        = sum(Net_Realizable_Homes, na.rm = TRUE),
    
    # 2. Expose the structural count of "Useless Upzones" caused by physical boundaries
    Total_Useless_Upzones     = sum(Is_Useless_Upzone, na.rm = TRUE),
    
    # 3. Sum up total square footage of new construction that hits each hazard category
    # (Calculated by multiplying potential new home counts by your standard 1,200 sqft footprint)
    Erosion_Mitigation_SqFt   = sum(ifelse(Has_Erosion_Hazard, Net_Realizable_Homes * 1200, 0), na.rm = TRUE),
    Liquefaction_Eng_SqFt     = sum(ifelse(Has_Liquefaction_Risk, Net_Realizable_Homes * 1200, 0), na.rm = TRUE),
    Aquifer_Vault_SqFt        = sum(ifelse(Has_Aquifer_Protected, Net_Realizable_Homes * 1200, 0), na.rm = TRUE),
    WUI_Hardening_SqFt        = sum(ifelse(Has_WUI_Proposed, Net_Realizable_Homes * 1200, 0), na.rm = TRUE),
    
    # 4. Total Financial Mitigation Capital Overhead required to unlock the city's housing
    # Evaluates the placeholder square-footage cost premiums ($25, $12, $8, $15) assigned in Sec 5
    Total_Projected_Premium_USD = sum(Net_Realizable_Homes * 1200 * Added_Cost_Per_SqFt, na.rm = TRUE)
  ) %>%
  mutate(
    # Calculate the average mitigation tax overhead penalty per potential new housing unit
    Avg_Mitigation_Premium_Per_Unit = ifelse(Potential_New_Homes > 0, Total_Projected_Premium_USD / Potential_New_Homes, 0)
  ) %>%
  arrange(desc(Potential_New_Homes))

# Print the completed economic viability matrix to your R console
print(city_economic_matrix)

# Export the matrix to your specified documents folder layout as a production CSV file
write.csv(city_economic_matrix, 
          file = file.path(output_dir, "City_UGB_Housing_Economic_Viability_Matrix.csv"), 
          row.names = FALSE)


