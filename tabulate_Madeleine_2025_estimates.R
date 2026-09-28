# ==============================================================================
# Compare Pipeline Outputs against TOD_cleaned.csv (HB 1491 Capacity Formula)
# ==============================================================================
rm(list = ls())

# Explicitly load data.table to enable the .() syntax
library(data.table)

csv_path <- "C:/Users/gmann/Downloads/TOD_cleaned.csv"

# Assumptions for unit yield conversion from floor area
avg_sqft_per_unit <- 1000  # Adjust as needed (e.g., 800-1000 sq ft per unit)
sqm_to_sqft       <- 10.7639

if (file.exists(csv_path)) {
  tod_ref <- fread(csv_path)
  
  # Standardize city names for join/comparison
  tod_ref[, city_clean := toupper(trimws(SITUS_CITY))]
  
  # ----------------------------------------------------------------------------
  # Calculate Net Added Floor Area & Estimated Unit Capacity (Madeleine's Formula)
  # Net Floor Area (sqm) = (new_far_max - devel_approx) * tot_area_m
  # ----------------------------------------------------------------------------
  tod_ref[, far_delta := pmax(0, as.numeric(new_far_max) - as.numeric(devel_approx), na.rm = TRUE)]
  tod_ref[, ref_add_sqm := far_delta * as.numeric(tot_area_m)]
  tod_ref[, ref_add_sqft := ref_add_sqm * sqm_to_sqft]
  tod_ref[, ref_est_tod_units := ref_add_sqft / avg_sqft_per_unit]
  
  # Group reference file by city
  ref_by_city <- tod_ref[, list(
    ref_parcels       = .N,
    ref_add_sqm       = sum(ref_add_sqm, na.rm = TRUE),
    ref_add_sqft      = sum(ref_add_sqft, na.rm = TRUE),
    ref_est_tod_units = sum(ref_est_tod_units, na.rm = TRUE)
  ), by = .(city = city_clean)][order(-ref_est_tod_units)]
  
  # Group current pipeline dt by city
  # Note: Ensure 'dt' exists in your global environment prior to running
  if (exists("dt") && is.data.table(dt)) {
    city_col <- head(intersect(c("SITUS_CITY_NM", "SITUS_CITY", "CITY_NM", "UGA_NM"), names(dt)), 1)
    
    pipeline_by_city <- dt[, list(
      pipe_parcels   = .N,
      pipe_tod_units = sum(tod_net_add, na.rm = TRUE)
    ), by = .(city = toupper(trimws(get(city_col))))]
    
    # Merge summaries side-by-side
    comparison <- merge(ref_by_city, pipeline_by_city, by = "city", all = TRUE)
    comparison[, diff_tod_units := pipe_tod_units - ref_est_tod_units]
    
    message("\n--- STATEWIDE OVERALL COMPARISON ---")
    print(data.table(
      Source = c("TOD_cleaned.csv (HB 1491)", "Current Pipeline (dt)"),
      Total_Parcels = c(sum(ref_by_city$ref_parcels, na.rm = TRUE), sum(pipeline_by_city$pipe_parcels, na.rm = TRUE)),
      Total_Floor_Area_SqFt = c(sum(ref_by_city$ref_add_sqft, na.rm = TRUE), NA),
      Total_Est_TOD_Units = c(sum(ref_by_city$ref_est_tod_units, na.rm = TRUE), sum(pipeline_by_city$pipe_tod_units, na.rm = TRUE))
    ))
    
    message("\n--- TOP CITIES SIDE-BY-SIDE COMPARISON ---")
    print(head(comparison[order(-ref_est_tod_units)], 15))
  } else {
    warning("Target object 'dt' does not exist in environment. Displaying TOD reference summary only.")
    print(head(ref_by_city, 15))
  }
  
} else {
  warning("File not found at: ", csv_path)
}