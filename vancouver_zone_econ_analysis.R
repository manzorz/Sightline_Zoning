library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(terra)
library(exactextractr)

# Define project-wide directories and coordinate parameters
output_dir <- "C:\\Users\\gmann\\Documents\\ClarkCountyZoning"
target_crs <- 2927 # NAD83 / Washington South (ftUS)

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# =========================================================================
# 1, 2, & 3. DATA INGESTION & COHESIVE BASE SNAPSHOT CACHE CONTROL
# =========================================================================
pnw_cache_file   <- file.path(output_dir, "pnw_tracts_2024.rds")
clark_cache_file <- file.path(output_dir, "clark_county_tracts_2024.rds")

# --- STAGE 1: CHECK AND EXTRACT PACIFIC NORTHWEST / CLARK COUNTY CACHES ---
if (file.exists(clark_cache_file)) {
  
  cat("Found local Clark County spatial reference cache. Loading instantly...\n")
  clark_tracts <- readRDS(clark_cache_file)
  
} else {
  
  if (file.exists(pnw_cache_file)) {
    cat("Found PNW regional cache. Loading to extract Clark County...\n")
    pnw_tracts <- readRDS(pnw_cache_file)
  } else {
    cat("No caches found. Ingesting massive nationwide tract shapefile (this will take a moment)...\n")
    raw_us_path <- paste0("C:/Users/gmann/Downloads/nhgis0015_shape/nhgis0015_shape/nhgis0015_shapefile_tl2024_us_tract_2024/",
                          "us_tract_2024/US_tract_2024.shp")
    
    # Read the full US framework including the tabular .dbf join parameters
    us_tracts <- st_read(raw_us_path, quiet = TRUE)
    
    cat("Subsetting shapefile to Oregon (STATEFP '41') and Washington (STATEFP '53')...\n")
    # NHGIS standard FIPS state track boundaries: 41 = OR, 53 = WA
    pnw_tracts <- us_tracts %>%
      filter(STATEFP %in% c("41", "53")) %>%
      st_transform(target_crs) %>%
      st_make_valid()
    
    cat("Saving Pacific Northwest regional tract cache to disk...\n")
    saveRDS(pnw_tracts, file = pnw_cache_file)
    rm(us_tracts) # Clear massive US layer out of system RAM instantly
  }
  
  cat("Isolating Clark County, WA (COUNTYFP '011') from the Pacific Northwest dataset...\n")
  # Washington State FIPS = 53, Clark County FIPS = 011
  clark_tracts <- pnw_tracts %>%
    filter(STATEFP == "53" & COUNTYFP == "011")
  
  cat("Saving isolated Clark County tract snapshot layer to disk...\n")
  saveRDS(clark_tracts, file = clark_cache_file)
}

base_cache_file <- file.path(output_dir, "base_spatial_inputs.rds")

if (file.exists(base_cache_file)) {
  
  cat("Loading projected base spatial datasets from local disk snapshot...\n")
  base_inputs <- readRDS(base_cache_file)
  
  lots           <- base_inputs$lots
  zoning_cleaned <- base_inputs$zoning_cleaned
  ugabnd         <- base_inputs$ugabnd
  lots_with_rules<- base_inputs$lots_with_rules
  
} else {
  
  cat("No base spatial cache found. Initiating initial heavy disk data read layout...\n")
  
  lots   <- st_read("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/TaxlotsPublic.shp") %>%
    st_transform(target_crs) %>% st_make_valid()
  
  zoning <- st_read("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/Zoning.shp") %>%
    st_transform(target_crs) %>% st_make_valid()
  
  ugabnd <- st_read("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/Ugabnd.shp") %>%
    st_transform(target_crs) %>% st_make_valid()
  
  # Remove duplicate stacked geometries to keep calculations clean
  zoning_cleaned <- zoning %>% 
    filter(!duplicated(st_geometry(.))) %>%
    mutate(
      Zoning_ID = 1:n(),
      Zone_Acres = as.numeric(st_area(geometry)) / 43560,
      MaxZoneUnits = round(Zone_Acres * UnitsPerAc)
    )
  
  # Execute largest overlap spatial intersection (Lots inherit zone-level regulations)
  cat("Computing largest-overlap spatial matrix allocations...\n")
  intersections <- st_intersection(lots, zoning_cleaned)
  intersections$intersect_area <- st_area(intersections)
  
  lots_joined <- intersections %>%
    group_by(prop_id) %>%                       
    arrange(desc(intersect_area), .by_group = TRUE) %>%
    slice(1) %>%
    ungroup()
  
  # Map standard Clark County Title 40 height and yard setback parameters
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
  
  # Package base spatial datasets together into a single RDS bundle file
  cat("Exporting base spatial snapshot to disk to speed up next code runs...\n")
  saveRDS(list(lots=lots, zoning_cleaned=zoning_cleaned, ugabnd=ugabnd, lots_with_rules=lots_with_rules), file = base_cache_file)
}

# =========================================================================
# 3d. ECONOMIC PIPELINE: ACS DEMOGRAPHIC MATCH & DEMAND INDICES
# =========================================================================
cat("Parsing IPUMS NHGIS data columns to compile neighborhood viability matrices...\n")

# Ingest your downloaded IPUMS tabular file using your exact dictionary mappings
nhgis_raw <- read.csv("C:/Users/gmann/Downloads/nhgis_csv/nhgis0015_ds273_20245_tract.csv", 
                      stringsAsFactors = FALSE)

# Filter, rename, and engineer structural proxies from the codebook columns
nhgis_indicators <- nhgis_raw %>%
  select(
    GISJOIN,
    # 1. Income baselines
    Tract_Med_Inc_Total = AVF7E001,  # Table 10: Median household income - Total
    Tract_Med_Inc_Owner = AVF7E002,  # Table 10: Median household income - Owners
    Tract_Med_Inc_Rent  = AVF7E003,  # Table 10: Median household income - Renters
    
    # 2. Components for Family Formation Proxy (Table 8: Presence of own children under 18)
    Owner_Married_Kids  = AVF3E005,  
    Rent_Married_Kids   = AVF3E018,  
    Owner_Single_Parents= AVF3E009,  
    Rent_Single_Parents = AVF3E022,  
    Tract_Total_Units   = AVF3E001,  # Total households baseline
    
    # 3. Components for Infill Product Match Proxy (Table 3: Household type by structure)
    Family_Multi_Unit   = AU5XE005,  # Married couple family in 2+ unit multi-family structures
    Single_Multi_Unit   = AU5XE010,  # Male parent in 2+ unit structures
    Female_Multi_Unit   = AU5XE014,  # Female parent in 2+ unit structures
    NonFam_Multi_Unit   = AU5XE018,  # Non-family single/roommate householders in 2+ unit structures
    
    # 4. Components for Generational Home Equity Wealth Proxy (Table 7: Median home value)
    Tract_Med_Home_Value= AVFVE001   
  ) %>%
  mutate(
    # Clean up financial records to handle suppressed zero/null markers cleanly
    Tract_Med_Inc_Total = as.numeric(Tract_Med_Inc_Total),
    Tract_Med_Inc_Rent  = as.numeric(Tract_Med_Inc_Rent),
    Tract_Med_Home_Value= as.numeric(Tract_Med_Home_Value),
    
    # INDICES ENGINEERING:
    # A. Family Formation Demand Ratio: Percent of neighborhood households raising children
    Family_Formation_Rate = ((Owner_Married_Kids + Rent_Married_Kids + Owner_Single_Parents + Rent_Single_Parents) / 
                               pmax(1, Tract_Total_Units)) * 100,
    
    # B. Infill Multi-Family Absorption Score: Counts concentration of residents currently in apartments/multiplexes
    Apartment_Absorption_Score = Family_Multi_Unit + Single_Multi_Unit + Female_Multi_Unit + NonFam_Multi_Unit
  ) %>%
  select(GISJOIN, Tract_Med_Inc_Total, Tract_Med_Inc_Rent, Tract_Med_Home_Value, Family_Formation_Rate, Apartment_Absorption_Score)

# -------------------------------------------------------------------------
# DATABASE INTEGRATION STEP (Executed instantly inside Section 5 Pipeline)
# -------------------------------------------------------------------------
# Merging variables lot-by-lot using your tax lot's built-in CensusTrac code
lots_capacity_model <- lots_capacity_model %>%
  mutate(
    Census_Int   = as.numeric(CensusTrac),
    Census_Int   = ifelse(Census_Int == 0, NA, Census_Int),
    Tract_String = sprintf("%06d", Census_Int * 100),
    GISJOIN_Key  = paste0("G5300110", Tract_String)
  ) %>%
  # Instantaneous relational attribute database link step (Vector left join)
  left_join(nhgis_indicators, by = c("GISJOIN_Key" = "GISJOIN")) %>%
  mutate(
    # Fallback safety variables to handle edge-case null tract errors uniformly
    Tract_Med_Inc_Total = ifelse(is.na(Tract_Med_Inc_Total), 85000, Tract_Med_Inc_Total),
    Tract_Med_Inc_Rent  = ifelse(is.na(Tract_Med_Inc_Rent), 55000, Tract_Med_Inc_Rent),
    Tract_Med_Home_Value= ifelse(is.na(Tract_Med_Home_Value), 480000, Tract_Med_Home_Value),
    Family_Formation_Rate = ifelse(is.na(Family_Formation_Rate), 25, Family_Formation_Rate)
  )

# =========================================================================
# 4. ENVIRONMENT PROCESSING & CONSTRAINT MASK GENERATION
# =========================================================================
final_cache_file <- file.path(output_dir, "processed_lots_capacity.rds")

if (file.exists(final_cache_file)) {
  
  cat("Found fully consolidated capacity model file. Loading cache instantly...\n")
  lots_capacity_model <- readRDS(final_cache_file)
  
} else {
  
  cat("No final capacity cache found. Processing constraint intersections and economic variables...\n")
  
  load_raw_shp <- function(file_path) {
    if (file.exists(file_path)) {
      st_read(file_path, quiet = TRUE) %>% st_transform(target_crs) %>% st_make_valid()
    } else {
      NULL
    }
  }
  
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
  
  # Isolate extreme steep slopes (>= 40% based on Clark County Title 40 rules)
  hard_slope_mask <- slopes_df %>% filter(grepl("40 - 100|greater than 100", desc_, ignore.case = TRUE)) %>% st_geometry() %>% st_union()
  slope_15_25_mask <- slopes_df %>% filter(grepl("15 - 25 percent", desc_, ignore.case = TRUE)) %>% st_geometry() %>% st_union()
  slope_25_40_mask <- slopes_df %>% filter(grepl("25 - 40 percent", desc_, ignore.case = TRUE)) %>% st_geometry() %>% st_union()
  
  wetland_mask  <- if(!is.null(wet_vec_df)) st_union(st_geometry(wet_vec_df)) else NULL
  habitat_mask  <- if(!is.null(habitat_df)) st_union(st_geometry(habitat_df)) else NULL
  hyd_poly_mask <- if(!is.null(hyd_poly_df)) st_union(st_geometry(hyd_poly_df)) else NULL
  
  landslide_mask <- if(!is.null(landslid_df)) st_union(st_geometry(landslid_df)) else NULL
  landslp_mask   <- if(!is.null(landslp_df)) st_union(st_geometry(landslp_df)) else NULL
  severe_landslide_boundary <- if(!is.null(landslide_mask) || !is.null(landslp_mask)) st_union(c(landslide_mask, landslp_mask)) else NULL
  
  mines_mask  <- if(!is.null(mines_df)) st_union(st_geometry(mines_df)) else NULL
  tribal_mask <- if(!is.null(tribal_df)) st_union(st_geometry(tribal_df)) else NULL
  
  hard_exclusion_list   <- list(hard_slope_mask, wetland_mask, habitat_mask, hyd_poly_mask, severe_landslide_boundary, mines_mask, tribal_mask)
  valid_exclusions      <- hard_exclusion_list[!sapply(hard_exclusion_list, is.null)]
  master_exclusion_mask <- do.call(st_union, valid_exclusions)
  
  calc_overlap_acres <- function(parcels, constraint_mask) {
    if (is.null(constraint_mask)) return(rep(0, nrow(parcels)))
    intersections <- st_intersection(st_geometry(parcels), constraint_mask)
    if (length(intersections) == 0) return(rep(0, nrow(parcels)))
    
    overlap_df <- st_intersection(parcels, constraint_mask)
    overlap_df$area_sqft <- as.numeric(st_area(overlap_df))
    summary_df <- overlap_df %>% st_drop_geometry() %>% group_by(prop_id) %>% summarise(acres = sum(area_sqft, na.rm = TRUE) / 43560)
    return(summary_df)
  }
  
  lots_wetland_loss <- calc_overlap_acres(lots_with_rules, wetland_mask) %>% rename(Wetland_Acres = acres)
  lots_slope_loss   <- calc_overlap_acres(lots_with_rules, hard_slope_mask) %>% rename(Critical_Slope_Acres = acres)
  lots_total_loss   <- calc_overlap_acres(lots_with_rules, master_exclusion_mask) %>% rename(Hard_Excluded_Acres = acres)
  
  # =========================================================================
  # 5. PARCEL CAPACITY VOLUMETRIC CALCULATION & ECONOMIC ENGINE
  # =========================================================================
  cat("Running final economic indices and parcel capacity calculations...\n")
  
  ASSUMED_AVG_STORY_HEIGHT <- 11  
  ASSUMED_AVG_UNIT_SIZE    <- 2000 # Updated size based on your new home median counts
  
  # Ingest your downloaded IPUMS NHGIS ACS Data Table
  # Replace this file pathway with your exact localized text/csv download destination
  nhgis_data <- read.csv("C:/Users/gmann/Downloads/nhgis_csv/nhgis0001_csv.csv", stringsAsFactors = FALSE) %>%
    select(GISJOIN, Tract_Med_Inc = B19013e1)
  
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
      Added_Cost_Per_SqFt   = 0 + ifelse(Has_Slope_15_25, 15, 0) + ifelse(Has_Slope_25_40, 35, 0) + 
        ifelse(Has_Erosion_Hazard, 12, 0) + ifelse(Has_Liquefaction_Risk, 25, 0) + 
        ifelse(Has_Aquifer_Protected, 8, 0) + ifelse(Has_WUI_Proposed, 15, 0),
      
      # 3. VOLUMETRIC AND REGULATORY GEOMETRY REDUCTIONS
      Setback_Reduction_Factor = case_when(Front_Setback_Ft >= 20 ~ 0.70, Front_Setback_Ft == 15 ~ 0.75, Front_Setback_Ft <= 10 ~ 0.85, TRUE ~ 0.80),
      Net_Footprint_SqFt       = (Net_Lot_Acres * 43560) * Setback_Reduction_Factor,
      
      # Enforce realistic multi-family cap matching regional 7-story limits
      Max_Stories              = pmin(floor(Max_Height_Ft / ASSUMED_AVG_STORY_HEIGHT), 7),
      
      Max_Lot_Coverage_Pct     = case_when(grepl("R1-|R-6|R-7.5|R-10|RLD", desc_) ~ 0.40, grepl("R-|MF|OR", desc_) ~ 0.60, TRUE ~ 0.50),
      
      Max_Potential_Floor_Area = (Net_Footprint_SqFt * Max_Lot_Coverage_Pct) * Max_Stories,
      Physical_Unit_Capacity   = floor(Max_Potential_Floor_Area / ASSUMED_AVG_UNIT_SIZE),
      Regulatory_Density_Cap   = floor(Net_Lot_Acres * UnitsPerAc),
      
      # 4. CHOKEPOINT CONSTRACTION INTERSECTION EVALUATION
      MaxPossibleConstruction  = pmin(Physical_Unit_Capacity, Regulatory_Density_Cap),
      Net_Realizable_Homes     = pmax(0, MaxPossibleConstruction - Units),
      Is_Useless_Upzone        = ifelse(Regulatory_Density_Cap > Units & MaxPossibleConstruction <= Units, TRUE, FALSE),
      
      # Format NHGIS text codes vector string parameters
      Census_Int               = as.numeric(CensusTrac),
      Census_Int               = ifelse(Census_Int == 0, NA, Census_Int),
      Tract_String             = sprintf("%06d", Census_Int * 100),
      GISJOIN_Key              = paste0("G5300110", Tract_String)
    ) %>%
    # Connect demographics directly to the active dataset records without spatial cost
    left_join(nhgis_data, by = c("GISJOIN_Key" = "GISJOIN")) %>%
    mutate(Tract_Med_Inc = ifelse(is.na(Tract_Med_Inc), 85000, Tract_Med_Inc))
  
  cat("Saving consolidated analysis data to final rds cache file...\n")
  saveRDS(lots_capacity_model, file = final_cache_file)
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
# 9. INTEGRATED ANALYSIS REPORT MATRIX (Capacity vs Lost Potential - Fixed)
# =========================================================================
city_intersections <- st_intersection(lots_capacity_model, city_outlines)
city_intersections$city_intersect_area <- st_area(city_intersections)

lots_with_city <- city_intersections %>%
  group_by(prop_id) %>% 
  arrange(desc(city_intersect_area), .by_group = TRUE) %>% 
  slice(1) %>% 
  ungroup()

# MATCHED CRITICAL FIX: Direct variable mapping alignment to resolve the select() crash
hazard_lookup <- lots_capacity_model %>%
  st_drop_geometry() %>%
  select(prop_id, Wetland_Acres, Critical_Slope_Acres, Intersects_Cemetery)

lots_with_city_fixed <- lots_with_city %>%
  left_join(hazard_lookup, by = "prop_id")

city_housing_report_matrix <- lots_with_city_fixed %>%
  st_drop_geometry() %>%
  mutate(
    Gross_Zoned_Capacity  = floor(Lot_Acres * UnitsPerAc),
    Units_Lost_To_Hazards = pmax(0, Gross_Zoned_Capacity - MaxPossibleConstruction)
  ) %>%
  group_by(City) %>%
  summarise(
    Total_Viable_New_Housing_Units          = sum(Net_Realizable_Homes, na.rm = TRUE),
    Total_Housing_Potential_Lost_To_Hazards = sum(Units_Lost_To_Hazards, na.rm = TRUE)
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

