library(sf)
library(data.table)
library(fixest)

# ==============================================================================
# CONFIGURATION & INPUT VARIABLES
# ==============================================================================
CONFIG <- list(
  # --- File Paths ---
  paths = list(
    bg_boundaries = "path/to/census_block_groups.shp",   # Census Block Group polygons
    zoning_layer  = "path/to/zoning_capacity.shp",       # Zoning layer with max unit capacity
    permit_data   = "path/to/historical_permits.csv",     # Historical panel data (2019-2023)
    output_dir    = "path/to/output/"                    # Output directory for results
  ),
  
  # --- Spatial Processing Params ---
  target_crs = 32610,  # UTM Zone 10N (projected CRS in meters for area/distance calculations)
  
  # --- Forecast Horizon & Growth Assumptions ---
  historical_years = 2019:2023,
  future_years     = 2024:2030,
  annual_inc_growth = 1.02,   # 2% annual income growth assumption
  annual_rent_growth = 1.03,  # 3% annual rent growth assumption
  
  # --- Model Specification Settings ---
  seed = 42
)

set.seed(CONFIG$seed)

# ==============================================================================
# STEP 1: LOAD & PROCESS INPUT DATA (SPATIAL & TABULAR)
# ==============================================================================

# Option A: Read Spatial Data & Intersect Zoning Capacity (If starting from shapefiles)
# ------------------------------------------------------------------------------
if (file.exists(CONFIG$paths$bg_boundaries) && file.exists(CONFIG$paths$zoning_layer)) {
  
  # Read spatial layers
  bg_sf     <- st_read(CONFIG$paths$bg_boundaries) |> st_transform(CONFIG$target_crs)
  zoning_sf <- st_read(CONFIG$paths$zoning_layer)  |> st_transform(CONFIG$target_crs)
  
  # Spatial intersection to calculate total zoned unit capacity per block group
  bg_capacity_sf <- st_intersection(bg_sf, zoning_sf) |>
    mutate(poly_area = st_area(geometry)) |>
    # Assuming zoning_layer has 'max_density_per_sqm' attribute
    mutate(zoned_capacity = as.numeric(poly_area) * max_density_per_sqm)
  
  # Summarize capacity to block group ID and convert to data.table
  bg_capacity_dt <- as.data.table(bg_capacity_sf)[, .(
    max_zoned_units = sum(zoned_capacity, na.rm = TRUE)
  ), by = bg_id]
  
} else {
  # Mock capacity table if shapefile paths do not exist
  n_bg <- 100
  bg_capacity_dt <- data.table(
    bg_id = factor(1:n_bg),
    max_zoned_units = sample(300:1500, n_bg, replace = TRUE),
    slope_pct = runif(n_bg, 0, 15),
    dist_cbd_km = runif(n_bg, 1, 25)
  )
}

# Option B: Read Historical Panel Data via fread
# ------------------------------------------------------------------------------
if (file.exists(CONFIG$paths$permit_data)) {
  panel_dt <- fread(CONFIG$paths$permit_data)
} else {
  # Mock panel dataset matching configuration horizon
  panel_dt <- bg_capacity_dt[
    CJ(bg_id = bg_id, year = CONFIG$historical_years), 
    on = "bg_id"
  ]
  setorder(panel_dt, bg_id, year)
  
  # Generate synthetic metrics
  panel_dt[, `:=`(
    hh_inc = 50000 + 2000 * (year - min(CONFIG$historical_years)) + rnorm(.N, 0, 5000),
    median_rent = 1200 + 50 * (year - min(CONFIG$historical_years)) + (100000 / dist_cbd_km) + rnorm(.N, 0, 100)
  )]
  
  panel_dt[, existing_units := shift(
    cumprod(1 + runif(.N, 0.005, 0.02)),
    fill = sample(200:800, 1)
  ) * sample(200:800, 1), by = bg_id]
  
  panel_dt[, remaining_capacity := pmax(0, max_zoned_units - existing_units)]
  panel_dt[, permit_prob := plogis(-3 + 0.001 * median_rent - 0.00002 * hh_inc + 
                                     0.002 * remaining_capacity - 0.1 * slope_pct)]
  panel_dt[, permits_issued := rpois(.N, lambda = permit_prob * 30)]
}

# ==============================================================================
# STEP 2: FIT STAGE 2 POISSON MODEL
# ==============================================================================
permit_model <- fepois(
  permits_issued ~ log(median_rent) + hh_inc + I(hh_inc^2) + 
    remaining_capacity + slope_pct + dist_cbd_km | year,
  data = panel_dt
)

# ==============================================================================
# STEP 3: FORWARD SIMULATION ENGINE
# ==============================================================================
last_known <- panel_dt[year == max(CONFIG$historical_years)]
projection_list <- list()

for (y in CONFIG$future_years) {
  next_period <- copy(last_known)
  next_period[, year := y]
  
  # Apply growth assumptions from CONFIG
  next_period[, hh_inc := hh_inc * CONFIG$annual_inc_growth]
  next_period[, median_rent := median_rent * CONFIG$annual_rent_growth]
  
  # Predict & enforce hard zoning bounds
  next_period[, predicted_permits := predict(permit_model, newdata = next_period, type = "response")]
  next_period[, units_built := pmin(predicted_permits, remaining_capacity)]
  next_period[, existing_units := existing_units + units_built]
  next_period[, remaining_capacity := pmax(0, max_zoned_units - existing_units)]
  
  projection_list[[as.character(y)]] <- copy(next_period)
  last_known <- next_period
}

future_projections <- rbindlist(projection_list)

# ==============================================================================
# STEP 4: AGGREGATE SUMMARY & EXPORT
# ==============================================================================
housing_forecast_summary <- future_projections[, .(
  baseline_units      = first(existing_units) - first(units_built),
  forecast_units_end  = last(existing_units),
  total_new_units     = sum(units_built),
  zoned_capacity      = first(max_zoned_units),
  capacity_built_pct  = (last(existing_units) / first(max_zoned_units)) * 100
), by = bg_id]

# Export output if directory exists
if (dir.exists(CONFIG$paths$output_dir)) {
  fwrite(housing_forecast_summary, file.path(CONFIG$paths$output_dir, "housing_forecast_summary.csv"))
  fwrite(future_projections, file.path(CONFIG$paths$output_dir, "annual_projections_2024_2030.csv"))
}

print(head(housing_forecast_summary))