# =========================================================================
# SCRIPT 02: CORE GEOMETRY & JURISDICTIONAL DATA INGESTION
# =========================================================================
cat("Executing Stage 2: Processing nationwide tracts and base property parcel layers...\n")

# Assert global variable directories passed down from your main controller
if (!exists("DATA_DIR")) DATA_DIR <- "C:/Users/gmann/Downloads/Clark_County_GIS_Atlas"
if (!exists("OUTPUT_DIR")) OUTPUT_DIR <- "C:/Users/gmann/Documents/ClarkCountyZoning/output_products"
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
    raw_us_path <- paste0("C:/Users/gmann/Downloads/nhgis0015_shape/nhgis0015_shape/nhgis0015_shapefile_tl2024_us_tract_2024/",
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
# STEP B: BASE PROPERTY TAXLOTS AND ZONING RECEPTACLE INGESTION
# -------------------------------------------------------------------------
if (file.exists(base_cache_file)) {
  cat("Loading projected base spatial datasets from local disk snapshot...\n")
  base_inputs <- readRDS(base_cache_file)
  
  lots           <- base_inputs$lots
  zoning_cleaned <- base_inputs$zoning_cleaned
  ugabnd         <- base_inputs$ugabnd
} else {
  cat("No base property spatial cache found. Reading raw county GIS files...\n")
  
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
  
  # Export basic data geometries to disk to preserve memory spaces across execution cycles
  cat("Exporting base property snapshot to disk to speed up next code runs...\n")
  saveRDS(list(lots = lots, zoning_cleaned = zoning_cleaned, ugabnd = ugabnd), file = base_cache_file)
}

cat("Stage 2 processing completed. Active geometries securely staged in memory.\n")
