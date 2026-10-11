# ==============================================================================
# Robust Exclusion Mask Generator (ArcGIS API + Local Stash Fallback)
# ==============================================================================
rm(list = ls())

library(sf)
library(data.table)

# Ensure official R Esri package is installed for robust REST querying
if (!requireNamespace("arcgislayers", quietly = TRUE)) {
  install.packages("arcgislayers", repos = "https://r-arcgis.r-universe.dev")
}
library(arcgislayers)

# ------------------------------------------------------------------------------
# 1. Configuration & Local Fallback Directory
# ------------------------------------------------------------------------------
target_crs  <- 2927  # WA State Plane South (US Feet)
output_gpkg <- "C:/Users/gmann/Downloads/WA_GIS/precludes_construction_mask.gpkg"
data_dir    <- "C:/Users/gmann/Downloads/WA_GIS/exclusion_inputs/"

if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

# Safe fetching function: Tries Web API via arcgislayers -> Fallback to Local GPKG/RDS
fetch_exclusion_layer <- function(service_url, layer_name, local_file_path, where_sql = "1=1") {
  message(sprintf("--> Fetching layer [%s]...", layer_name))
  sf_out <- NULL
  
  # Attempt 1: Fetch live from REST API using arcgislayers
  tryCatch({
    fl <- arc_open(service_url)
    sf_out <- arc_select(fl, where = where_sql, crs = target_crs)
    
    if (!is.null(sf_out) && nrow(sf_out) > 0) {
      message(sprintf("    Successfully retrieved %d features via API.", nrow(sf_out)))
      # Stash locally for future offline/fast runs
      saveRDS(sf_out, file = local_file_path)
      return(sf_out)
    }
  }, error = function(e) {
    warning(sprintf("    API fetch failed for %s (%s).", layer_name, e$message))
  })
  
  # Attempt 2: Fallback to local disk stash if API fails or network drops
  if (file.exists(local_file_path)) {
    message(sprintf("    [FALLBACK] Loading local cached copy for [%s]...", layer_name))
    sf_out <- readRDS(local_file_path)
    return(sf_out)
  }
  
  warning(sprintf("    [SKIP] Layer [%s] could not be retrieved from API or local disk.", layer_name))
  return(NULL)
}

# ------------------------------------------------------------------------------
# 2. Extract Exclusion Layers with Verified REST Services
# ------------------------------------------------------------------------------

# A. RCO Public Lands & Municipal Parks
rco_url <- "https://services.arcgis.com/js83L97O0229eb21/arcgis/rest/services/Public_Lands_Inventory/FeatureServer/0"
rco_sf  <- fetch_exclusion_layer(rco_url, "RCO_Public_Lands", file.path(data_dir, "rco_lands.rds"))

# B. Ecology Contaminated Cleanup Sites (CSCS)
cleanup_url <- "https://gis.ecy.wa.gov/arcgis/rest/services/TCP/CSCS/MapServer/0"
cleanup_sf  <- fetch_exclusion_layer(cleanup_url, "Ecology_Cleanup", file.path(data_dir, "cleanup_sites.rds"))

# C. WSDOT Highway On-Ramps & Interchange Corridors
ramps_url <- "https://gis.wsdot.wa.gov/arcgis/rest/services/Shared/Ramps/MapServer/0"
ramps_sf  <- fetch_exclusion_layer(ramps_url, "WSDOT_Ramps", file.path(data_dir, "wsdot_ramps.rds"))

# D. National Wetlands Inventory (WA Ecology)
wetlands_url <- "https://gis.ecy.wa.gov/arcgis/rest/services/SEA/NWI_Wetlands/MapServer/0"
wetlands_sf  <- fetch_exclusion_layer(wetlands_url, "NWI_Wetlands", file.path(data_dir, "nwi_wetlands.rds"))

# ------------------------------------------------------------------------------
# 3. Geometric Standardization & Null-Guarded Spatial Union
# ------------------------------------------------------------------------------
message("\nStandardizing layers and repairing geometry...")

raw_layers <- list(
  rco = rco_sf,
  cleanup = cleanup_sf,
  ramps = ramps_sf,
  wetlands = wetlands_sf
)

valid_geoms <- list()

for (name in names(raw_layers)) {
  layer <- raw_layers[[name]]
  
  if (!is.null(layer) && nrow(layer) > 0) {
    message(sprintf("Processing geometry for [%s] (%d features)...", name, nrow(layer)))
    
    # 1. Transform to Washington State Plane South (US Feet)
    layer_proj <- st_transform(layer, crs = target_crs)
    
    # 2. Fix self-intersecting geometries
    layer_clean <- st_make_valid(layer_proj)
    
    # 3. Extract polygon geometries only
    layer_poly <- st_collection_extract(layer_clean, "POLYGON")
    
    if (nrow(layer_poly) > 0) {
      valid_geoms[[name]] <- st_geometry(layer_poly)
    }
  }
}

# CRITICAL GUARD: Ensure at least one valid spatial layer exists before unioning
if (length(valid_geoms) == 0) {
  stop("FATAL: No exclusion layers could be fetched or loaded from disk. Check local input paths or network VPN.")
}

message("\nPerforming spatial unary union across valid layers...")

# Combine list of geometries cleanly into a single sfc
combined_sfc <- do.call(c, valid_geoms)

# Dissolve overlapping exclusion boundaries
dissolved_exclusion <- st_union(combined_sfc)

# Package into structured sf object
precludes_construction_sf <- st_sf(
  precludes_construction = TRUE,
  exclusion_reason = "Environmental Hazard / Public Land / Transportation ROW",
  geometry = dissolved_exclusion
)

# ------------------------------------------------------------------------------
# 4. Write Output GeoPackage
# ------------------------------------------------------------------------------
st_write(
  obj = precludes_construction_sf,
  dsn = output_gpkg,
  layer = "precludes_construction",
  delete_layer = TRUE,
  quiet = FALSE
)

message(sprintf("\nSuccessfully created exclusion mask: %s", output_gpkg))