# ==============================================================================
# Comprehensive Housing Capacity & Imputation Script (HB 1110, HB 1337, E2SSB 5466)
# ==============================================================================
rm(list = ls())

library(sf)
library(data.table)

# ------------------------------------------------------------------------------
# 1. Configuration & Input/Output Paths
# ------------------------------------------------------------------------------
input_path  <- "C:/Users/gmann/Downloads/WA_GIS/Parcels_UGA_Transit_Joined.rds"
output_path <- "C:/Users/gmann/Downloads/WA_GIS/Parcels_With_Unit_Counts.rds"
csv_path    <- "C:/Users/gmann/Downloads/WA_GIS/City_Capacity_Summary.csv"

avg_unit_sqft <- 850 # Gross square feet per unit assumption for 5466 capacity

message("Loading joined spatial transit-parcel dataset...")
final_transit_sf <- readRDS(input_path)

# Extract data.table for fast in-place mutation
dt <- as.data.table(st_drop_geometry(final_transit_sf))

# Normalize DOR Land Use Code
dt[, lu_code := as.integer(as.character(LANDUSE_CD))]

# ------------------------------------------------------------------------------
# 2. Impute Baseline Existing Housing Units
# ------------------------------------------------------------------------------
# DOR Classification Mapping:
# 11: Single-Family (1) | 12: 2-4 Units (3 avg) | 13: 5+ Multi-Family (20 avg)
# 14: Condominiums (1)  | 15: Mobile Home Parks (10 avg) | 18/19: Leased/Seasonal (1)
# 60: Mixed-Use (2 avg) | 50-59, 61-69, 91: Commercial/Office/Industrial (0)

dt[, imputed_units := fifelse(lu_code %in% c(11, 14, 18, 19), 1L,
                              fifelse(lu_code == 12, 3L,
                                      fifelse(lu_code == 13, 20L,
                                              fifelse(lu_code == 15, 10L,
                                                      fifelse(lu_code == 60, 2L, 0L)))))]

# ==============================================================================
# 3. Model Capacity Levers (Realistic Redevelopment & Envelope Filtering)
# ==============================================================================
# Modeling Parameters
net_efficiency_factor <- 0.75  # 25% lost to corridors, elevators, mechanical, walls
max_lot_coverage      <- 0.55  # 45% lost to setbacks, step-backs, easements, open space
avg_unit_sqft         <- 850   # Gross sqft per residential unit

# Define Land Use Groups
res_sf_codes   <- c(11, 14, 18, 19)
comm_tod_codes <- c(50, 51:59, 60, 61:69, 91)

# Ensure parcel_sqft exists
if (!"parcel_sqft" %in% names(dt)) {
  dt[, parcel_sqft := as.numeric(st_area(final_transit_sf)) * 10.7639]
}
dt[is.na(parcel_sqft) | is.nan(parcel_sqft), parcel_sqft := 0]

# Distance tiers
if (!"station_dist_miles" %in% names(dt)) {
  if ("near_transit_025" %in% names(dt) && "near_transit_050" %in% names(dt)) {
    dt[, station_dist_miles := fifelse(near_transit_025 == TRUE, 0.25,
                                       fifelse(near_transit_050 == TRUE, 0.50, 99.0))]
  } else {
    dt[, station_dist_miles := 99.0]
  }
}
dt[is.na(station_dist_miles), station_dist_miles := 99.0]

# Assign E2SSB 5466 Target FAR
dt[, tod_target_far := fifelse(station_dist_miles <= 0.25, 4.0,
                               fifelse(station_dist_miles <= 0.50, 2.5, 0.0))]
dt[is.na(tod_target_far), tod_target_far := 0.0]

# Initialize output columns
dt[, `:=`(
  middle_add         = 0L,
  ADU_add_gross      = 0L,
  ADU_add_net        = 0L,
  tod_gross_units    = 0L,
  tod_net_add        = 0L,
  max_policy_net_add = 0L
)]

# ------------------------------------------------------------------------------
# 3A. Redevelopment Feasibility Filtering (Crucial step to remove built towers)
# ------------------------------------------------------------------------------
# Calculate Improvement-to-Land Value Ratio if values exist
if ("IMP_VAL" %in% names(dt) && "LND_VAL" %in% names(dt)) {
  dt[, imp_land_ratio := fifelse(LND_VAL > 0, IMP_VAL / LND_VAL, 99.0)]
} else {
  dt[, imp_land_ratio := 0.0] # Fallback if missing
}

# Impute baseline commercial density (1 unit eq per 1,000 sq ft commercial bldg)
if ("BLDG_SQFT" %in% names(dt)) {
  dt[, comm_baseline_units := fifelse(lu_code %in% comm_tod_codes & !is.na(BLDG_SQFT), 
                                      as.integer(BLDG_SQFT / 1000), 0L)]
} else {
  dt[, comm_baseline_units := 0L]
}

# Total baseline density existing on lot
dt[, total_baseline_units := pmax(fifelse(is.na(imputed_units), 0L, imputed_units), 
                                  comm_baseline_units, na.rm = TRUE)]

# ------------------------------------------------------------------------------
# 3B. Policy Modeling
# ------------------------------------------------------------------------------
# A. HB 1110 (Middle Housing) & HB 1337 (ADUs)
dt[!is.na(lu_code) & lu_code %in% res_sf_codes & !is.na(hb1110_max_units) & hb1110_max_units > 0, `:=`(
  middle_add    = pmax(0L, as.integer(hb1110_max_units - 1L), na.rm = TRUE),
  ADU_add_gross = 2L
)]
dt[!is.na(lu_code) & lu_code %in% res_sf_codes, 
   ADU_add_net := fifelse(middle_add >= 2L, 0L, ADU_add_gross)]

# B. E2SSB 5466 (TOD Density) with Envelope & Feasibility Constraints
# Conditions:
# 1. Parcel >= 3,000 sq ft (sub-3k sq ft lots can rarely build 2.5-4.0 FAR multi-family)
# 2. Improvement-to-Land Ratio < 1.0 (filters out non-redevelopable modern buildings)
dt[!is.na(lu_code) & lu_code %in% c(res_sf_codes, comm_tod_codes) & 
     tod_target_far > 0 & parcel_sqft >= 3000 & imp_land_ratio < 1.0, `:=`(
       tod_gross_units = as.integer((parcel_sqft * max_lot_coverage * tod_target_far * net_efficiency_factor) / avg_unit_sqft)
     )]
dt[is.na(tod_gross_units), tod_gross_units := 0L]

# Deduct baseline existing units (both residential and commercial equivalent)
dt[, tod_net_add := pmax(0L, tod_gross_units - total_baseline_units, na.rm = TRUE)]

# C. Binding Policy Maximum
dt[, max_policy_net_add := pmax(middle_add + ADU_add_net, tod_net_add, na.rm = TRUE)]
dt[is.na(max_policy_net_add), max_policy_net_add := 0L]

# ------------------------------------------------------------------------------
# 4. Attach Results Back to Spatial Object & Save RDS
# ------------------------------------------------------------------------------
final_transit_sf$imputed_units       <- dt$imputed_units
final_transit_sf$middle_add          <- dt$middle_add
final_transit_sf$ADU_add_gross       <- dt$ADU_add_gross
final_transit_sf$ADU_add_net         <- dt$ADU_add_net
final_transit_sf$tod_target_far      <- dt$tod_target_far
final_transit_sf$tod_gross_units     <- dt$tod_gross_units
final_transit_sf$tod_net_add         <- dt$tod_net_add
final_transit_sf$max_policy_net_add  <- dt$max_policy_net_add

saveRDS(final_transit_sf, output_path)
message(sprintf("Saved updated spatial dataset successfully to: %s", output_path))

# ------------------------------------------------------------------------------
# 5. Diagnostic Reporting
# ------------------------------------------------------------------------------
message("\n--- BASELINE IMPUTED HOUSING UNITS ---")
print(dt[imputed_units > 0, .(
  parcels         = .N,
  total_units     = sum(imputed_units),
  pct_res_parcels = round((.N / dt[imputed_units > 0, .N]) * 100, 2)
), by = .(DOR_Code = lu_code)][order(DOR_Code)])

message("\n--- E2SSB 5466 TOD COMMERCIAL & MIXED-USE CAPACITY ---")
print(dt[lu_code %in% comm_tod_codes & tod_target_far > 0, .(
  parcels             = .N,
  total_acres         = round(sum(parcel_sqft) / 43560, 1),
  tod_net_unit_yield  = sum(tod_net_add)
), by = .(DOR_Code = lu_code)][order(DOR_Code)])

message("\n--- TOTAL STATEWIDE LEGISLATIVE CAPACITY BREAKDOWN ---")
print(dt[, .(
  total_parcels      = .N,
  baseline_units     = sum(imputed_units, na.rm = TRUE),
  hb1337_net_adus    = sum(ADU_add_net, na.rm = TRUE),
  hb1110_middle_add  = sum(middle_add, na.rm = TRUE),
  e2ssb5466_tod_add  = sum(tod_net_add, na.rm = TRUE),
  binding_max_yield  = sum(max_policy_net_add, na.rm = TRUE)
)])

# City Breakdown Export
city_col <- head(intersect(c("SITUS_CITY_NM", "CITY_NM", "UGA_NM"), names(dt)), 1)

if (length(city_col) > 0) {
  city_summary <- dt[, .(
    total_parcels       = .N,
    baseline_units      = sum(imputed_units, na.rm = TRUE),
    hb1337_net_adus     = sum(ADU_add_net, na.rm = TRUE),
    hb1110_middle_add   = sum(middle_add, na.rm = TRUE),
    e2ssb5466_tod_add   = sum(tod_net_add, na.rm = TRUE),
    binding_max_yield   = sum(max_policy_net_add, na.rm = TRUE)
  ), by = .(city = get(city_col))][order(-total_parcels)]
  
  write.csv(city_summary, csv_path, row.names = FALSE)
  message(sprintf("\nCity summary exported to: %s", csv_path))
  print(head(city_summary, 10))
}
