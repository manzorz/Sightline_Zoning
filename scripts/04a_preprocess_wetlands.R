# =========================================================================
# SCRIPT 04a: WETLANDS SPECTRAL PARTITIONING & DISK CHECKPOINTING
# =========================================================================
cat("Executing Stage 4a: Partitioning wetlands dataset into cost progression spectrum masks...\n")

if (!exists("DATA_DIR")) DATA_DIR <- file.path(chartr("\\", "/", Sys.getenv("USERPROFILE")), "Downloads", "Clark_County_GIS_Atlas")
if (!exists("OUTPUT_DIR")) OUTPUT_DIR <- file.path(chartr("\\", "/", Sys.getenv("USERPROFILE")), "Documents", "ClarkCountyZoning", "output_products")
if (!exists("TARGET_CRS")) TARGET_CRS <- 2927

precalculated_vector_wetlands <- file.path(OUTPUT_DIR, "precalculated_vector_wetlands_mask.rds")
precalculated_shoreline_mask  <- file.path(OUTPUT_DIR, "precalculated_shoreline_wetlands_mask.rds")
precalculated_trans_mask      <- file.path(OUTPUT_DIR, "precalculated_transitional_wetlands_mask.rds")

gdb_path   <- file.path(chartr("\\", "/", Sys.getenv("USERPROFILE")), "Downloads/SEA_BIO_WetlandsInventory/Wetlands_Inventory.gdb")
layer_name <- "wetlands_inventory_2016"

# --- STRATEGY 2: CHECK AND PARSE THE GEODATABASE RASTER SEPARATION ---
if (!file.exists(precalculated_vector_wetlands) || !file.exists(precalculated_shoreline_mask) || !file.exists(precalculated_trans_mask)) {
  gdb_path   <- file.path(chartr("\\", "/", Sys.getenv("USERPROFILE")), "Downloads/SEA_BIO_WetlandsInventory/Wetlands_Inventory.gdb")
  layer_name <- "wetlands_inventory_2016"
  
  if (dir.exists(gdb_path)) {
    cat("Accessing local File Geodatabase repository to extract discrete raster spectrum...\n")
    
    # Securely load the specified layer matrix as a native SpatRaster grid map
    gdb_wetlands <- rast(gdb_path, subds = layer_name)
    
    # 1. ABSOLUTE PROHIBITIONS: Isolate values 21+ (Open Water / Core Saturated Wetlands)
    cat("  -> Extracting hard spatial exclusions (Values 21+)...\n")
    mask_21 <- gdb_wetlands >= 21
    poly_21 <- as.polygons(mask_21, values = TRUE)
    poly_21 <- poly_21[poly_21[[1]] == 1, ] # Keep only cells where condition is True
    
    vector_wetland_mask <- st_as_sf(poly_21) %>% st_union() %>% st_transform(TARGET_CRS)
    saveRDS(vector_wetland_mask, file = precalculated_vector_wetlands)
    
    # 2. HIGH SEVERITY MITIGATION: Isolate values 13 to 20 (Beaches, Mudflats, and Sandbars)
    cat("  -> Extracting high-risk shoreline engineering zones (Values 13-20)...\n")
    mask_13_20 <- gdb_wetlands >= 13 & gdb_wetlands <= 20
    poly_13_20 <- as.polygons(mask_13_20, values = TRUE)
    poly_13_20 <- poly_13_20[poly_13_20[[1]] == 1, ]
    
    shoreline_wetland_mask <- st_as_sf(poly_13_20) %>% st_union() %>% st_transform(TARGET_CRS)
    saveRDS(shoreline_wetland_mask, file = precalculated_shoreline_mask)
    
    # 3. LOW/MODERATE MITIGATION: Isolate values 1 to 12 (Seasonal Meadows and Saturated Buffers)
    cat("  -> Extracting transitional moderate wet soil zones (Values 1-12)...\n")
    mask_1_12 <- gdb_wetlands >= 1 & gdb_wetlands <= 12
    poly_1_12 <- as.polygons(mask_1_12, values = TRUE)
    poly_1_12 <- poly_1_12[poly_1_12[[1]] == 1, ]
    
    transitional_wetland_mask <- st_as_sf(poly_1_12) %>% st_union() %>% st_transform(TARGET_CRS)
    saveRDS(transitional_wetland_mask, file = precalculated_trans_mask)
    
    # Reclaim background memory resources instantly
    rm(gdb_wetlands, mask_21, poly_21, mask_13_20, poly_13_20, mask_1_12, poly_1_12)
    rm(vector_wetland_mask, shoreline_wetland_mask, transitional_wetland_mask)
    invisible(gc())
  } else {
    cat("  -> WARNING: Specified Wetlands Geodatabase directory path was not found on this machine.\n")
  }
}

cat("Stage 4a spectral separation complete.\n")
