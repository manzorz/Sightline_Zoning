# =========================================================================
# SCRIPT 04: VECTOR MASK OVERLAPS & PROPERTY EXTENT LOSS MATH
# =========================================================================
cat("Executing Stage 4: Processing vector masks and extracting physical acreage constraints...\n")

# Assert variable directory pathways passed down from the master controller
if (!exists("DATA_DIR")) DATA_DIR <- "C:/Users/gmann/Downloads/Clark_County_GIS_Atlas"
if (!exists("OUTPUT_DIR")) OUTPUT_DIR <- "C:/Users/gmann/Documents/ClarkCountyZoning/output_products"
if (!exists("TARGET_CRS")) TARGET_CRS <- 2927

rules_cache_file  <- file.path(OUTPUT_DIR, "rules_spatial_inputs.rds")
final_cache_file  <- file.path(OUTPUT_DIR, "processed_lots_capacity.rds")

# Check if the final capacity data file is already active before running slow intersection loops
if (file.exists(final_cache_file)) {
  cat("Consolidated final data file discovered. Skipping Section 4 geometry calculations...\n")
} else {
  
  # Ensure the regulatory parcel layers generated in Stage 3 are active in system memory
  if (!exists("lots_with_rules")) {
    lots_with_rules <- readRDS(rules_cache_file)
  }
  
  # Helper tool function to safely ingest, validate, and project raw local disk data layers
  load_raw_shp <- function(file_path) {
    if (file.exists(file_path)) {
      st_read(
        file_path, 
        quiet = TRUE
      ) %>% 
        st_transform(TARGET_CRS) %>% 
        st_make_valid()
    } else {
      NULL
    }
  }
  
  cat("Reading individual constraint shapefiles into memory vectors...\n")
  slopes_df       <- load_raw_shp(file_path = file.path(DATA_DIR, "Slopes.shp"))
  wet_vec_df      <- load_raw_shp(file_path = file.path(DATA_DIR, "WetInv.shp"))
  erosion_df      <- load_raw_shp(file_path = file.path(DATA_DIR, "ErosionHazard.shp"))
  habitat_df      <- load_raw_shp(file_path = file.path(DATA_DIR, "Habitat.shp"))
  hyd_poly_df     <- load_raw_shp(file_path = file.path(DATA_DIR, "HydPoly.shp"))
  liq_df          <- load_raw_shp(file_path = file.path(DATA_DIR, "Liquefaction.shp"))
  landslid_df     <- load_raw_shp(file_path = file.path(DATA_DIR, "Lndslid.shp"))
  landslp_df      <- load_raw_shp(file_path = file.path(DATA_DIR, "Lndslp.shp"))
  mines_df        <- load_raw_shp(file_path = file.path(DATA_DIR, "Mines.shp"))
  tribal_df       <- load_raw_shp(file_path = file.path(DATA_DIR, "TribalLands.shp"))
  aquifer_df      <- load_raw_shp(file_path = file.path(DATA_DIR, "Aquifer.shp"))
  wui_proposed_df <- load_raw_shp(file_path = file.path(DATA_DIR, "WildlandUrbanInterfaceProposed.shp"))
  
  # -------------------------------------------------------------------------
  # STEP A: SEGREGATE SEVERE REDUCTIONS INTO HARD EXCLUSION GEOMETRIES
  # -------------------------------------------------------------------------
  cat("Aggregating structural unbuildable boundaries into a unified layout mask...\n")
  
  # Topography: Isolate ONLY extreme steep slopes (>= 40% based on Clark County Title 40 rules)
  hard_slope_mask <- slopes_df %>% 
    filter(grepl("40 - 100|greater than 100", desc_, ignore.case = TRUE)) %>% 
    st_geometry() %>% 
    st_union()
  
  # Separate milder rolling topography layers for downstream pricing multipliers
  slope_15_25_mask <- slopes_df %>% filter(grepl("15 - 25 percent", desc_, ignore.case = TRUE)) %>% st_geometry() %>% st_union()
  slope_25_40_mask <- slopes_df %>% filter(grepl("25 - 40 percent", desc_, ignore.case = TRUE)) %>% st_geometry() %>% st_union()
  
  # Combine other unbuildable environmental and administrative boundaries
  wetland_mask  <- if(!is.null(wet_vec_df)) st_union(st_geometry(wet_vec_df)) else NULL
  habitat_mask  <- if(!is.null(habitat_df)) st_union(st_geometry(habitat_df)) else NULL
  hyd_poly_mask <- if(!is.null(hyd_poly_df)) st_union(st_geometry(hyd_poly_df)) else NULL
  
  landslide_mask <- if(!is.null(landslid_df)) st_union(st_geometry(landslid_df)) else NULL
  landslp_mask   <- if(!is.null(landslp_df)) st_union(st_geometry(landslp_df)) else NULL
  severe_landslide_boundary <- if(!is.null(landslide_mask) || !is.null(landslp_mask)) st_union(c(landslide_mask, landslp_mask)) else NULL
  
  mines_mask  <- if(!is.null(mines_df)) st_union(st_geometry(mines_df)) else NULL
  tribal_mask <- if(!is.null(tribal_df)) st_union(st_geometry(tribal_df)) else NULL
  
  # Compile valid non-null elements into a master reduction footprint envelope
  hard_exclusion_list   <- list(hard_slope_mask, wetland_mask, habitat_mask, hyd_poly_mask, severe_landslide_boundary, mines_mask, tribal_mask)
  valid_exclusions      <- hard_exclusion_list[!sapply(hard_exclusion_list, is.null)]
  master_exclusion_mask <- do.call(st_union, valid_exclusions)
  
  # -------------------------------------------------------------------------
  # STEP B: EXECUTE PARCEL OVERLAP REACTION MATH
  # -------------------------------------------------------------------------
  cat("Measuring individual footprint acreage impacts across property records...\n")
  
  calc_overlap_acres <- function(parcels, constraint_mask) {
    if (is.null(constraint_mask)) return(rep(0, nrow(parcels)))
    intersections <- st_intersection(st_geometry(parcels), constraint_mask)
    if (length(intersections) == 0) return(rep(0, nrow(parcels)))
    
    overlap_df <- st_intersection(parcels, constraint_mask)
    overlap_df$area_sqft <- as.numeric(st_area(overlap_df))
    summary_df <- overlap_df %>% 
      st_drop_geometry() %>% 
      group_by(prop_id) %>% 
      summarise(acres = sum(area_sqft, na.rm = TRUE) / 43560)
    
    return(summary_df)
  }
  
  lots_wetland_loss <- calc_overlap_acres(lots_with_rules, wetland_mask) %>% rename(Wetland_Acres = acres)
  lots_slope_loss   <- calc_overlap_acres(lots_with_rules, hard_slope_mask) %>% rename(Critical_Slope_Acres = acres)
  lots_total_loss   <- calc_overlap_acres(lots_with_rules, master_exclusion_mask) %>% rename(Hard_Excluded_Acres = acres)
  
  # -------------------------------------------------------------------------
  # STEP C: INGEST REGIONAL IPUMS NHGIS ACS DATA CODES
  # -------------------------------------------------------------------------
  cat("Reading tabular tract indicators directly from downloaded NHGIS source files...\n")
  nhgis_raw <- read.csv(
    "C:/Users/gmann/Downloads/nhgis_csv/nhgis0015_ds273_20245_tract.csv", 
    stringsAsFactors = FALSE
  )
  
  nhgis_indicators <- nhgis_raw %>%
    select(
      GISJOIN,
      Tract_Med_Inc_Total  = AVF7E001,  
      Tract_Med_Inc_Rent   = AVF7E003,  
      Tract_Med_Home_Value = AVFVE001,  
      Owner_Married_Kids   = AVF3E005,  
      Rent_Married_Kids    = AVF3E018,  
      Owner_Single_Parents = AVF3E009,  
      Rent_Single_Parents  = AVF3E022,  
      Tract_Total_Units    = AVF3E001,
      Family_Multi_Unit    = AU5XE005,  
      Single_Multi_Unit    = AU5XE010,  
      Female_Multi_Unit    = AU5XE014,  
      NonFam_Multi_Unit    = AU5XE018   
    ) %>%
    mutate(
      Tract_Med_Inc_Total   = as.numeric(Tract_Med_Inc_Total),
      Tract_Med_Inc_Rent    = as.numeric(Tract_Med_Inc_Rent),
      Tract_Med_Home_Value  = as.numeric(Tract_Med_Home_Value),
      Family_Formation_Rate = ((Owner_Married_Kids + Rent_Married_Kids + Owner_Single_Parents + Rent_Single_Parents) / pmax(1, Tract_Total_Units)) * 100,
      Apartment_Absorption  = Family_Multi_Unit + Single_Multi_Unit + Female_Multi_Unit + NonFam_Multi_Unit
    ) %>%
    select(GISJOIN, Tract_Med_Inc_Total, Tract_Med_Inc_Rent, Tract_Med_Home_Value, Family_Formation_Rate, Apartment_Absorption)
}

cat("Stage 4 complete. Environmental metrics and demographic indices successfully prepared.\n")
