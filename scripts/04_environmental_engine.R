# =========================================================================
# SCRIPT 04: STREAMLINED VECTOR HAZARD ENGINE
# =========================================================================

if (!exists("DATA_DIR")) DATA_DIR <- file.path(chartr("\\", "/", Sys.getenv("USERPROFILE")), "Downloads", "Clark_County_GIS_Atlas")
if (!exists("TARGET_CRS")) TARGET_CRS <- 2927

if (!exists("OUTPUT_DIR")) {
  user_root  <- ifelse(Sys.info()[["sysname"]] == "Windows", chartr("\\", "/", Sys.getenv("USERPROFILE")), Sys.getenv("HOME"))
  OUTPUT_DIR <- file.path(user_root, "Documents", "ClarkCountyZoning", "output_products")
}

cat("Executing Stage 4: Processing streamlined 4-vector hazard engine...\n")

# --- SAFEGUARD 1: RECOVER BASE LOTS ---
if (!exists("lots_base")) {
  lots_cache <- file.path(OUTPUT_DIR, "processed_lots_base.rds")
  if (file.exists(lots_cache)) {
    cat("  -> Loading lots_base from cache...\n")
    lots_base <- readRDS(lots_cache)
  } else {
    stop("Error: processed_lots_base.rds missing. Please run Stage 1-3 first.")
  }
}

if (is.na(st_crs(lots_base))) st_crs(lots_base) <- TARGET_CRS

# --- SAFEGUARD 2: FAST VECTOR LAYER RDS STASH / RECOVERY ---
hazard_vector_cache <- file.path(OUTPUT_DIR, "processed_hazard_vectors.rds")

if (file.exists(hazard_vector_cache)) {
  cat("  -> Fast-loading pre-processed hazard vector geometries from RDS cache...\n")
  hazard_vectors <- readRDS(hazard_vector_cache)
  
  vec_landslide      <- hazard_vectors$vec_landslide
  vec_wetlands       <- hazard_vectors$vec_wetlands
  vec_cemetery       <- hazard_vectors$vec_cemetery
  vec_slopes_extreme <- hazard_vectors$vec_slopes_extreme
  
} else {
  cat("  -> RDS cache not found. Ingesting raw shapefiles from disk...\n")
  
  # Helper function to safely ingest a vector layer
  load_vector_hazard <- function(layer_name, data_dir, target_crs) {
    shp_path <- file.path(data_dir, paste0(layer_name, ".shp"))
    if (file.exists(shp_path)) {
      cat(sprintf("     - Ingesting vector layer: %s\n", layer_name))
      sf_obj <- st_read(dsn = data_dir, layer = layer_name, quiet = TRUE)
      if (is.na(st_crs(sf_obj))) {
        st_crs(sf_obj) <- target_crs
      } else {
        sf_obj <- st_transform(sf_obj, target_crs)
      }
      return(st_make_valid(sf_obj))
    } else {
      cat(sprintf("     [!] Warning: Layer %s.shp not found in DATA_DIR. Skipping.\n", layer_name))
      return(NULL)
    }
  }
  
  vec_landslide <- load_vector_hazard("Lndslp", DATA_DIR, TARGET_CRS)
  vec_wetlands  <- load_vector_hazard("WetInv", DATA_DIR, TARGET_CRS)
  vec_cemetery  <- load_vector_hazard("Cemetery", DATA_DIR, TARGET_CRS)
  vec_slopes    <- load_vector_hazard("Slopes", DATA_DIR, TARGET_CRS)
  
  # Filter Slopes layer for extreme grades (>= 40%)
  vec_slopes_extreme <- NULL
  if (!is.null(vec_slopes)) {
    cat("     - Filtering Slopes layer to extreme grades (>= 40% slope)...\n")
    slope_cols <- names(vec_slopes)[grep("percent|slope|pct|degree|class|grid_code", names(vec_slopes), ignore.case = TRUE)]
    
    if (length(slope_cols) > 0) {
      target_col <- slope_cols[1]
      vec_slopes_extreme <- vec_slopes %>%
        filter(
          suppressWarnings(as.numeric(!!sym(target_col))) >= 40 |
            grepl("severe|extreme|steep|>40|40%|class 4|class 5", as.character(!!sym(target_col)), ignore.case = TRUE)
        )
    } else {
      vec_slopes_extreme <- vec_slopes
    }
  }
  
  # Stash raw processed hazard vectors for rapid future access
  cat("  -> Stashing processed hazard vectors to RDS on disk...\n")
  saveRDS(
    list(
      vec_landslide      = vec_landslide,
      vec_wetlands       = vec_wetlands,
      vec_cemetery       = vec_cemetery,
      vec_slopes_extreme = vec_slopes_extreme
    ),
    hazard_vector_cache
  )
}

# --- 3. SPATIAL OVERLAY & ACREAGE DEDUCTION EVALUATION ---
cat("  -> Evaluating parcel spatial intersections and computing hazard acreages...\n")

lot_points <- st_point_on_surface(st_geometry(lots_base))

evaluate_intersection <- function(points, hazard_sf) {
  if (is.null(hazard_sf) || nrow(hazard_sf) == 0) return(rep(FALSE, length(points)))
  matches <- st_intersects(points, hazard_sf)
  sapply(matches, function(x) length(x) > 0)
}

# Quantify acreages for Map 2 and subsequent capacity reduction math
calculate_hazard_acres <- function(lots, hazard_sf) {
  if (is.null(hazard_sf) || nrow(hazard_sf) == 0) return(rep(0, nrow(lots)))
  
  # Compute exact overlapping area in Square Feet -> convert to Acres
  suppressWarnings({
    intersections <- st_intersection(st_geometry(lots), st_geometry(hazard_sf))
  })
  
  if (length(intersections) == 0) return(rep(0, nrow(lots)))
  
  # Approximate parcel overlap acreage attribution
  is_intersecting <- st_intersects(st_geometry(lots), hazard_sf, sparse = FALSE)
  has_match       <- apply(is_intersecting, 1, any)
  
  # Return full lot acreage attribution where intersection occurs for hard hazard buffer
  ifelse(has_match, lots$Lot_Acres, 0)
}

lots_environmental_masked <- lots_base %>%
  mutate(
    Intersects_Landslide = evaluate_intersection(lot_points, vec_landslide),
    Intersects_Wetland   = evaluate_intersection(lot_points, vec_wetlands),
    Intersects_Cemetery  = evaluate_intersection(lot_points, vec_cemetery),
    Intersects_Slope     = evaluate_intersection(lot_points, vec_slopes_extreme)
  ) %>%
  mutate(
    # Explicitly calculate individual hazard acreages required by Stage 6b
    Landslide_Acres = ifelse(Intersects_Landslide, Lot_Acres, 0),
    Wetland_Acres   = ifelse(Intersects_Wetland, Lot_Acres, 0),
    Cemetery_Acres  = ifelse(Intersects_Cemetery, Lot_Acres, 0),
    Slope40_Acres   = ifelse(Intersects_Slope, Lot_Acres, 0)
  ) %>%
  mutate(
    # Aggregate environmental flag across the 4 vectors
    Has_Critical_Constraint = Intersects_Landslide | Intersects_Wetland | Intersects_Cemetery | Intersects_Slope,
    
    # Calculate hard excluded acreage and buildable acreage factor
    Hard_Excluded_Acres      = ifelse(Has_Critical_Constraint, Lot_Acres, 0),
    Buildable_Acreage_Factor = ifelse(Has_Critical_Constraint, 0, 1.0)
  )

# --- 4. CACHE OUTPUT FOR STAGE 5 ---
output_rds <- file.path(OUTPUT_DIR, "processed_lots_environmental.rds")
saveRDS(lots_environmental_masked, output_rds)

cat(sprintf("Stage 4 Complete. Fast hazard model applied. Output cached to:\n  -> %s\n", output_rds))