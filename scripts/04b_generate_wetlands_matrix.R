# =========================================================================
# SCRIPT 04b: FAST PARCEL-TO-WETLANDS MATRIX PRE-COMPUTATION
# =========================================================================
# Description:
#   Pre-calculates the vector wetland intersection matrix using BBOX-indexed
#   spatial pruning.
#   
#   Output: precalculated_vector_wetlands_matrix.rds
# =========================================================================

cat("Executing Stage 4b: Generating heavy parcel-to-wetlands vector matrix...\n")

if (!exists("DATA_DIR")) DATA_DIR <- file.path(chartr("\\", "/", Sys.getenv("USERPROFILE")), "Downloads", "Clark_County_GIS_Atlas")
if (!exists("OUTPUT_DIR")) OUTPUT_DIR <- file.path(chartr("\\", "/", Sys.getenv("USERPROFILE")), "Documents", "ClarkCountyZoning", "output_products")
if (!exists("TARGET_CRS")) TARGET_CRS <- 2927

rules_cache_file              <- file.path(OUTPUT_DIR, "rules_spatial_inputs.rds")
precalculated_vector_wetlands <- file.path(OUTPUT_DIR, "precalculated_vector_wetlands_mask.rds")
precalculated_vector_matrix   <- file.path(OUTPUT_DIR, "precalculated_vector_wetlands_matrix.rds")

# Ensure required input files exist
if (!file.exists(rules_cache_file)) {
  stop("Error: rules_spatial_inputs.rds not found in output directory. Please run 01 through 03 first.")
}

if (!file.exists(precalculated_vector_wetlands)) {
  stop("Error: precalculated_vector_wetlands_mask.rds not found. Please ensure vector wetlands mask is placed in output directory.")
}

cat("Loading spatial rules and active vector wetland mask...\n")
lots_with_rules     <- readRDS(rules_cache_file)
active_wetland_mask <- readRDS(precalculated_vector_wetlands)

# Ensure geometry collection is individual features for STRtree indexing
if (inherits(active_wetland_mask, "sf")) {
  active_wetland_mask <- active_wetland_mask %>% select(geometry)
} else if (inherits(active_wetland_mask, "sfc")) {
  active_wetland_mask <- st_sf(geometry = active_wetland_mask)
}

total_parcels <- nrow(lots_with_rules)
chunk_size    <- 5000  
num_chunks    <- ceiling(total_parcels / chunk_size)

output_list <- list()
cat(sprintf("Processing %d total tax parcels across %d blocks:\n", total_parcels, num_chunks))

start_time <- Sys.time()

for (i in 1:num_chunks) {
  start_idx  <- ((i - 1) * chunk_size) + 1
  end_idx    <- min(i * chunk_size, total_parcels)
  parcel_sub <- lots_with_rules[start_idx:end_idx, ]
  
  # Step 1: GEOS BBOX filter to prune distant wetlands
  chunk_bbox      <- st_as_sfc(st_bbox(parcel_sub))
  sub_constraints <- st_filter(active_wetland_mask, chunk_bbox)
  
  acres_vector <- rep(0, nrow(parcel_sub))
  
  if (nrow(sub_constraints) > 0) {
    # Step 2: Dissolve local wetlands to prevent self-overlapping double counts
    local_union <- st_union(sub_constraints)
    
    hits <- st_intersects(parcel_sub, local_union, sparse = FALSE)[, 1]
    
    if (any(hits)) {
      hit_parcels <- parcel_sub[hits, ]
      overlap_df  <- st_intersection(hit_parcels, local_union)
      overlap_df$area_sqft <- as.numeric(st_area(overlap_df))
      
      summary_df <- overlap_df %>% 
        st_drop_geometry() %>% 
        group_by(prop_id) %>% 
        summarise(acres = sum(area_sqft, na.rm = TRUE) / 43560, .groups = "drop")
      
      match_idx <- match(parcel_sub$prop_id[hits], summary_df$prop_id)
      acres_vector[hits] <- ifelse(is.na(match_idx), 0, summary_df$acres[match_idx])
    }
  }
  
  output_list[[i]] <- data.frame(prop_id = parcel_sub$prop_id, Wetland_Acres = acres_vector)
  
  pct_complete <- (end_idx / total_parcels) * 100
  cat(sprintf("\r  -> Progress: %6.1f%% complete | Block %d of %d mapped", pct_complete, i, num_chunks))
  flush.console()
}
cat("\n")

end_time <- Sys.time()
elapsed  <- round(difftime(end_time, start_time, units = "mins"), 2)

cat(sprintf("Computation finished in %s minutes. Compiling results...\n", elapsed))

lots_wetland_loss <- bind_rows(output_list)

cat(sprintf("Saving completed vector matrix to: %s\n", precalculated_vector_matrix))
saveRDS(lots_wetland_loss, file = precalculated_vector_matrix)

cat("Stage 4b complete! The precalculated vector matrix is ready for Script 04.\n")