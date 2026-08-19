# =========================================================================
# SCRIPT 04: CONSTRAINT GEOMETRY MASKS & RASTER-FIRST WETLANDS ENGINE
# =========================================================================
cat("Executing Stage 4: Processing vector masks and extracting constraints...\n")

if (!exists("DATA_DIR")) DATA_DIR <- file.path(chartr("\\", "/", Sys.getenv("USERPROFILE")), "Downloads", "Clark_County_GIS_Atlas")
if (!exists("OUTPUT_DIR")) OUTPUT_DIR <- file.path(chartr("\\", "/", Sys.getenv("USERPROFILE")), "Documents", "ClarkCountyZoning", "output_products")
if (!exists("TARGET_CRS")) TARGET_CRS <- 2927

rules_cache_file  <- file.path(OUTPUT_DIR, "rules_spatial_inputs.rds")
final_cache_file  <- file.path(OUTPUT_DIR, "processed_lots_capacity.rds")

# File paths for wetlands layers, pre-computed matrices, and checkpoints
precalculated_vector_matrix   <- file.path(OUTPUT_DIR, "precalculated_vector_wetlands_matrix.rds") # Target output for Script 04b
precalculated_raster_wetlands <- file.path(OUTPUT_DIR, "precalculated_raster_wetlands_mask.rds")

chk_wetland <- file.path(OUTPUT_DIR, "checkpoint_stage4_wetlands.rds")
chk_slope   <- file.path(OUTPUT_DIR, "checkpoint_stage4_slopes.rds")
chk_total   <- file.path(OUTPUT_DIR, "checkpoint_stage4_total_exclusions.rds")

if (file.exists(final_cache_file)) {
  cat("Consolidated final data file discovered. Skipping Section 4 geometry calculations...\n")
} else {
  
  if (!exists("lots_with_rules")) lots_with_rules <- readRDS(rules_cache_file)
  
  load_raw_shp <- function(full_path, data_directory, layer_name) {
    if (file.exists(full_path)) {
      result <- tryCatch({
        st_read(dsn = data_directory, layer = layer_name, quiet = TRUE)
      }, error = function(e) {
        st_read(full_path, quiet = TRUE)
      })
      if (!is.null(result)) {
        result <- result %>% st_transform(TARGET_CRS) %>% st_make_valid()
      }
      return(result)
    } else {
      return(NULL)
    }
  }
  
  # --- PROGRESS BAR TRACKED CALCULATOR ENGINE ---
  calc_overlap_acres_tracked <- function(parcels, constraint_mask, label_name = "Layer") {
    if (is.null(constraint_mask)) return(data.frame(prop_id = parcels$prop_id, acres = 0))
    
    total_parcels <- nrow(parcels)
    chunk_size    <- 2000  
    num_chunks    <- ceiling(total_parcels / chunk_size)
    
    output_list <- list()
    cat(sprintf("\nEvaluating %s boundaries:\n", label_name))
    
    for (i in 1:num_chunks) {
      start_idx <- ((i - 1) * chunk_size) + 1
      end_idx   <- min(i * chunk_size, total_parcels)
      
      parcel_sub <- parcels[start_idx:end_idx, ]
      intersects_logical <- st_intersects(parcel_sub, constraint_mask, sparse = FALSE)[, 1]
      
      if (any(intersects_logical)) {
        hit_parcels <- parcel_sub[intersects_logical, ]
        overlap_df  <- st_intersection(hit_parcels, constraint_mask)
        overlap_df$area_sqft <- as.numeric(st_area(overlap_df))
        
        summary_df <- overlap_df %>% 
          st_drop_geometry() %>% 
          group_by(prop_id) %>% 
          summarise(acres = sum(area_sqft, na.rm = TRUE) / 43560, .groups = "drop")
        
        output_list[[i]] <- summary_df
      }
      
      pct_complete <- (end_idx / total_parcels) * 100
      bar_width    <- 20
      filled_width <- round((pct_complete / 100) * bar_width)
      progress_bar <- paste0("[", paste(rep("=", filled_width), collapse = ""), 
                             paste(rep(" ", bar_width - filled_width), collapse = ""), "]")
      
      cat(sprintf("\r  %s %6.1f%% | Block %d of %d complete", progress_bar, pct_complete, i, num_chunks))
      flush.console()
    }
    cat("\n") 
    
    if (length(output_list) == 0) {
      return(data.frame(prop_id = parcels$prop_id, acres = 0))
    } else {
      compiled_df <- bind_rows(output_list)
      final_df    <- data.frame(prop_id = parcels$prop_id) %>% 
        left_join(compiled_df, by = "prop_id") %>% 
        mutate(acres = ifelse(is.na(acres), 0, acres))
      return(final_df)
    }
  }
  
  # -------------------------------------------------------------------------
  # WETLANDS SELECTION LOGIC (PRE-COMPUTED VECTOR MATRIX -> RASTER DEFAULT)
  # -------------------------------------------------------------------------
  if (file.exists(chk_wetland)) {
    cat("  -> Wetland stage 4 checkpoint found. Restoring pre-computed calculations instantly...\n")
    lots_wetland_loss <- readRDS(chk_wetland)
    
  } else if (file.exists(precalculated_vector_matrix)) {
    cat("  -> Discovered pre-calculated parcel-to-wetland vector matrix (produced via Script 04b).\n")
    cat("  -> Restoring vector intersect matrix...\n")
    lots_wetland_loss <- readRDS(precalculated_vector_matrix)
    saveRDS(lots_wetland_loss, file = chk_wetland)
    
  } else {
    cat("  -> Pre-calculated vector matrix not found. Defaulting to fast RASTER output processing...\n")
    
    if (file.exists(precalculated_raster_wetlands)) {
      active_wetland_mask <- readRDS(precalculated_raster_wetlands)
      lots_wetland_loss   <- calc_overlap_acres_tracked(
        lots_with_rules, 
        active_wetland_mask, 
        "Raster Wetlands Overlay Framework"
      ) %>% rename(Wetland_Acres = acres)
      
      rm(active_wetland_mask)
      invisible(gc())
    } else {
      cat("  -> WARNING: Neither vector matrix nor raster wetlands file were found. Setting wetland acres to 0.\n")
      lots_wetland_loss <- data.frame(prop_id = lots_with_rules$prop_id, Wetland_Acres = 0)
    }
    
    saveRDS(lots_wetland_loss, file = chk_wetland)
  }
  
  # -------------------------------------------------------------------------
  # STEP 2: Critical Topography Slopes Layer Processing
  # -------------------------------------------------------------------------
  if (file.exists(chk_slope)) {
    cat("  -> Slope checkpoint found. Restoring pre-computed calculations instantly...\n")
    lots_slope_loss <- readRDS(chk_slope)
  } else {
    slopes_df <- load_raw_shp(file.path(DATA_DIR, "Slopes.shp"), DATA_DIR, "Slopes")
    hard_slope_mask <- slopes_df %>% 
      filter(grepl("40 - 100|greater than 100", desc_, ignore.case = TRUE)) %>% 
      st_geometry() %>% 
      st_union()
    
    lots_slope_loss <- calc_overlap_acres_tracked(lots_with_rules, hard_slope_mask, "Slopes >= 40%") %>% 
      rename(Critical_Slope_Acres = acres)
    saveRDS(lots_slope_loss, file = chk_slope)
    rm(slopes_df, hard_slope_mask); invisible(gc())
  }
  
  # -------------------------------------------------------------------------
  # STEP 3: Combined Mask Total Exclusions Processing
  # -------------------------------------------------------------------------
  if (file.exists(chk_total)) {
    cat("  -> Master exclusion checkpoint found. Restoring pre-computed calculations instantly...\n")
    lots_total_loss <- readRDS(chk_total)
  } else {
    cat("Compiling remaining mask components for spatial reduction (Wetlands managed separately)...\n")
    slopes_df       <- load_raw_shp(file.path(DATA_DIR, "Slopes.shp"), DATA_DIR, "Slopes")
    habitat_df      <- load_raw_shp(file.path(DATA_DIR, "Habitat.shp"), DATA_DIR, "Habitat")
    hyd_poly_df     <- load_raw_shp(file.path(DATA_DIR, "HydPoly.shp"), DATA_DIR, "HydPoly")
    landslid_df     <- load_raw_shp(file.path(DATA_DIR, "Lndslid.shp"), DATA_DIR, "Lndslid")
    landslp_df      <- load_raw_shp(file.path(DATA_DIR, "Lndslp.shp"), DATA_DIR, "Lndslp")
    mines_df        <- load_raw_shp(file.path(DATA_DIR, "Mines.shp"), DATA_DIR, "Mines")
    tribal_df       <- load_raw_shp(file.path(DATA_DIR, "TribalLands.shp"), DATA_DIR, "TribalLands")
    
    hard_slope_mask  <- if(!is.null(slopes_df)) slopes_df %>% filter(grepl("40 - 100|greater than 100", desc_, ignore.case = TRUE)) %>% st_geometry() %>% st_union() else NULL
    habitat_mask     <- if(!is.null(habitat_df)) st_union(st_geometry(habitat_df)) else NULL
    hyd_poly_mask    <- if(!is.null(hyd_poly_df)) st_union(st_geometry(hyd_poly_df)) else NULL
    landslide_mask   <- if(!is.null(landslid_df)) st_union(st_geometry(landslid_df)) else NULL
    landslp_mask     <- if(!is.null(landslp_df)) st_union(st_geometry(landslp_df)) else NULL
    severe_landslide <- if(!is.null(landslide_mask) || !is.null(landslp_mask)) st_union(c(landslide_mask, landslp_mask)) else NULL
    mines_mask       <- if(!is.null(mines_df)) st_union(st_geometry(mines_df)) else NULL
    tribal_mask      <- if(!is.null(tribal_df)) st_union(st_geometry(tribal_df)) else NULL
    
    hard_exclusion_list   <- list(hard_slope_mask, habitat_mask, hyd_poly_mask, severe_landslide, mines_mask, tribal_mask)
    valid_exclusions      <- hard_exclusion_list[!sapply(hard_exclusion_list, is.null)]
    master_exclusion_mask <- do.call(st_union, valid_exclusions)
    
    lots_total_loss <- calc_overlap_acres_tracked(lots_with_rules, master_exclusion_mask, "Master Combined Exclusions") %>% 
      rename(Hard_Excluded_Acres = acres)
    saveRDS(lots_total_loss, file = chk_total)
    
    rm(slopes_df, habitat_df, hyd_poly_df, landslid_df, landslp_df, mines_df, tribal_df)
    rm(hard_slope_mask, habitat_mask, hyd_poly_mask, severe_landslide, mines_mask, tribal_mask, master_exclusion_mask)
    invisible(gc())
  }
  
  # -------------------------------------------------------------------------
  # STEP C: INGEST SPECIFIED LOCAL DOWNLOAD PATHWAY NHGIS TABLES
  # -------------------------------------------------------------------------
  cat("Reading tabular tract indicators directly from downloaded NHGIS source folder...\n")
  nhgis_folder <- file.path(chartr("\\", "/", Sys.getenv("USERPROFILE")), 
                            "Downloads/nhgis0015_shape/nhgis0015_shape/nhgis0015_shapefile_tl2024_us_tract_2024")
  
  nhgis_raw <- read.csv(file.path(nhgis_folder, "nhgis0015_ds273_20245_tract.csv"), stringsAsFactors = FALSE)
  
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