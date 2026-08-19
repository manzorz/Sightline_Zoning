# =========================================================================
# SCRIPT 02: CORE GEOMETRY & JURISDICTIONAL DATA INGESTION
# =========================================================================
cat("Executing Stage 2: Processing nationwide tracts and base property parcel layers...\n")

# Assert global variable directories passed down from your main controller
if (!exists("DATA_DIR")) {
  user_root <- ifelse(Sys.info()[["sysname"]] == "Windows", chartr("\\", "/", Sys.getenv("USERPROFILE")), Sys.getenv("HOME"))
  if (Sys.info()[["sysname"]] == "Windows") {
    DATA_DIR <- file.path(user_root, "Downloads", "Clark_County_GIS_Atlas")
  } else {
    DATA_DIR <- file.path(user_root, "Downloads", "Clark_County_GIS_Atlas")
  }
}
if (!exists("OUTPUT_DIR")) {
  user_root <- ifelse(Sys.info()[["sysname"]] == "Windows", chartr("\\", "/", Sys.getenv("USERPROFILE")), Sys.getenv("HOME"))
  if (Sys.info()[["sysname"]] == "Windows") {
    OUTPUT_DIR <- file.path(user_root, "Documents", "ClarkCountyZoning", "output_products")
  } else {
    OUTPUT_DIR <- file.path(user_root, "Documents", "ClarkCountyZoning", "output_products")
  }
}
if (!exists("TARGET_CRS")) TARGET_CRS <- 2927

# Define local workspace cache storage targets
pnw_cache_file   <- file.path(OUTPUT_DIR, "pnw_tracts_2024.rds")
clark_cache_file <- file.path(OUTPUT_DIR, "clark_county_tracts_2024.rds")
base_cache_file  <- file.path(OUTPUT_DIR, "base_spatial_inputs.rds")

# -------------------------------------------------------------------------
# STEP A: NATIONWIDE CENSUS TRACT CACHE & PACIFIC NORTHWEST ISOLATION
# -------------------------------------------------------------------------
if (file.exists(clark_cache_file)) {
  cat("Found local Clark County spatial reference tract cache. Loading instantly...\n")
  clark_tracts <- readRDS(clark_cache_file)
} else {
  if (file.exists(pnw_cache_file)) {
    cat("Found PNW regional tract cache. Loading to extract county footprint...\n")
    pnw_tracts <- readRDS(pnw_cache_file)
  } else {
    cat("No caches found. Ingesting massive nationwide tract shapefile (this will take a moment)...\n")
    raw_us_path <- paste0("~/Downloads/nhgis0015_shape/nhgis0015_shape/nhgis0015_shapefile_tl2024_us_tract_2024/",
                          "us_tract_2024/US_tract_2024.shp")
    
    us_tracts <- st_read(raw_us_path, quiet = TRUE)
    
    cat("Subsetting shapefile to Oregon (STATEFP '41') and Washington (STATEFP '53')...\n")
    pnw_tracts <- us_tracts %>%
      filter(STATEFP %in% c("41", "53")) %>%
      st_transform(TARGET_CRS) %>%
      st_make_valid()
    
    cat("Saving Pacific Northwest regional tract cache to disk...\n")
    saveRDS(pnw_tracts, file = pnw_cache_file)
    rm(us_tracts) # Instantly clear massive US layer out of active system RAM
  }
  
  cat("Isolating Clark County, WA (COUNTYFP '011') from the Pacific Northwest dataset...\n")
  clark_tracts <- pnw_tracts %>% filter(STATEFP == "53" & COUNTYFP == "011")
  
  cat("Saving isolated Clark County tract snapshot layer to disk...\n")
  saveRDS(clark_tracts, file = clark_cache_file)
}

# -------------------------------------------------------------------------
# STEP B: BASE PROPERTY TAXLOTS AND ZONING RECEPTACLE INGESTION (Self-Healing)
# -------------------------------------------------------------------------
# Define a placeholder flag variable to track cache validity
base_cache_loaded <- FALSE

if (file.exists(base_cache_file)) {
  cat("Discovered base property spatial cache. Attempting to parse snapshot connection...\n")
  
  # Protect the pipeline from connection crashes or file corruption errors
  tryCatch({
    base_inputs <- readRDS(base_cache_file)
    
    lots           <- base_inputs$lots
    zoning_cleaned <- base_inputs$zoning_cleaned
    ugabnd         <- base_inputs$ugabnd
    
    # Assert successful retrieval if no structural connection errors occur
    base_cache_loaded <- TRUE
    cat("Base spatial datasets loaded successfully from local snapshot.\n")
  }, error = function(e) {
    # Execute automatic recovery cleanup protocols if the file is incomplete or broken
    cat("WARNING: Local cache file is corrupted or incomplete. Initiating self-healing reset...\n")
    cat("System Error Details:", message(e), "\n")
    
    file.remove(base_cache_file) # Automatically purges the broken RDS file from your drive
  })
}

# Fallback sequence: If no cache exists, or if tryCatch deleted a corrupted one
if (!base_cache_loaded) {
  cat("Rebuilding property layers from raw GIS shapefiles (this will take a few moments)...\n")
  
  lots   <- st_read(file.path(DATA_DIR, "TaxlotsPublic.shp"), quiet = TRUE) %>% st_transform(TARGET_CRS) %>% st_make_valid()
  zoning <- st_read(file.path(DATA_DIR, "Zoning.shp"), quiet = TRUE) %>% st_transform(TARGET_CRS) %>% st_make_valid()
  ugabnd <- st_read(file.path(DATA_DIR, "Ugabnd.shp"), quiet = TRUE) %>% st_transform(TARGET_CRS) %>% st_make_valid()
  
  # Remove duplicate stacked geometries to keep downstream analysis clean and error-free
  zoning_cleaned <- zoning %>% 
    filter(!duplicated(st_geometry(.))) %>%
    mutate(
      Zoning_ID = 1:n(),
      Zone_Acres = as.numeric(st_area(geometry)) / 43560,
      MaxZoneUnits = round(Zone_Acres * UnitsPerAc)
    )
  
  # Export freshly compiled data geometries to disk to preserve memory spaces across execution cycles
  cat("Exporting fresh, uncorrupted base property snapshot to disk folder layout...\n")
  saveRDS(
    list(lots = lots, zoning_cleaned = zoning_cleaned, ugabnd = ugabnd), 
    file = base_cache_file
  )
}

cat("Stage 2 processing completed. Active geometries securely staged in memory.\n")







