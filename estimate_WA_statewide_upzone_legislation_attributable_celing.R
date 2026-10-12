# ==============================================================================
# Complete Transit & Housing Capacity Model (Vector PDF Pipeline Only)
# ==============================================================================
rm(list = ls())

library(sf)
library(data.table)
library(ggplot2)

# ------------------------------------------------------------------------------
# 1. Configuration & Input/Output Paths
# ------------------------------------------------------------------------------
input_path       <- "C:/Users/gmann/Downloads/WA_GIS/Parcels_UGA_Transit_Joined.rds"
output_path      <- "C:/Users/gmann/Downloads/WA_GIS/Parcels_Fully_Processed.rds"
vector_pdf_path  <- "C:/Users/gmann/Downloads/WA_GIS/Statewide_Capacity_Vector.pdf"

# Modeling Parameters
avg_unit_sqft                 <- 850
sqm_to_sqft                   <- 10.7639
net_efficiency_factor         <- 0.75
max_lot_coverage              <- 0.55
max_redev_parcel_sqft         <- 217800
min_buildable_footprint_sqft  <- 1000

message("Loading joined spatial transit-parcel dataset...")
final_transit_sf <- readRDS(input_path)

dt <- as.data.table(st_drop_geometry(final_transit_sf))

# ------------------------------------------------------------------------------
# 2. Parcel Deduplication & Schema Standardization
# ------------------------------------------------------------------------------
id_col <- head(intersect(c("prop_id", "PIN", "PARCEL_ID", "PARCELID", "PolyID"), names(dt)), 1)
if (length(id_col) > 0) {
  dt <- unique(dt, by = id_col)
}

if (!"lu_code" %in% names(dt) && "LANDUSE_CD" %in% names(dt)) {
  dt[, lu_code := as.integer(as.character(LANDUSE_CD))]
}

# Strict Buildable Residential & Mixed-Use Commercial Land Use Vectors
res_sf_codes          <- c(11, 12, 13, 14, 18, 19)
comm_tod_codes        <- c(53, 54, 59)
vacant_codes          <- c(91)
strict_buildable_set  <- c(res_sf_codes, comm_tod_codes, vacant_codes)

if (!"parcel_sqft" %in% names(dt)) {
  dt[, parcel_sqft := as.numeric(st_area(final_transit_sf)) * sqm_to_sqft]
}
dt[is.na(parcel_sqft) | is.nan(parcel_sqft), parcel_sqft := 0]
dt[, eff_parcel_sqft := pmin(parcel_sqft, max_redev_parcel_sqft)]
dt[, tot_area_m      := eff_parcel_sqft / sqm_to_sqft]

# ------------------------------------------------------------------------------
# 3. Baseline Attributes & Built-Up Flag (High-Hanging Fruit Exclusion)
# ------------------------------------------------------------------------------
dt[, imputed_units := fifelse(lu_code %in% c(11, 14, 18, 19), 1L,
                              fifelse(lu_code == 12, 3L,
                                      fifelse(lu_code == 13, 20L,
                                              fifelse(lu_code == 15, 10L,
                                                      fifelse(lu_code == 60, 2L, 0L)))))]
dt[, total_baseline_units := fifelse(is.na(imputed_units), 0L, as.integer(imputed_units))]

if ("devel_approx" %in% names(dt)) {
  dt[, current_far := as.numeric(devel_approx)]
} else if ("BLDG_SQFT" %in% names(dt)) {
  dt[, current_far := fifelse(eff_parcel_sqft > 0, as.numeric(BLDG_SQFT) / eff_parcel_sqft, 0.0)]
} else {
  dt[, current_far := 0.0]
}
dt[is.na(current_far), current_far := 0.0]

if ("IMP_VAL" %in% names(dt) && "LND_VAL" %in% names(dt)) {
  dt[, imp_land_ratio := fifelse(LND_VAL > 0, IMP_VAL / LND_VAL, 99.0)]
} else {
  dt[, imp_land_ratio := 0.0]
}

# High-Hanging Fruit Flag
dt[, is_built_up := fifelse(current_far > 0.75 | imp_land_ratio > 3.0 | total_baseline_units >= 4, TRUE, FALSE)]
dt[is.na(is_built_up), is_built_up := FALSE]

# ------------------------------------------------------------------------------
# 4. Capacity Calculations & Policy Waterfall (Using near_tod_buffer)
# ------------------------------------------------------------------------------
if (!"near_tod_buffer" %in% names(dt)) {
  dt[, near_tod_buffer := FALSE]
}
dt[is.na(near_tod_buffer), near_tod_buffer := FALSE]

# Set TOD Target FAR based exclusively on near_tod_buffer flag (4.0 FAR)
dt[, tod_target_far := fifelse(near_tod_buffer == TRUE, 4.0, 0.0)]

# Policy Caps (Restricted strictly to buildable land use codes)
dt[, cap_hb1337 := total_baseline_units]
dt[lu_code %in% res_sf_codes & is_built_up == FALSE, cap_hb1337 := pmax(total_baseline_units, 3L)]

dt[, cap_hb1110 := total_baseline_units]
if ("hb1110_max_units" %in% names(dt)) {
  dt[lu_code %in% res_sf_codes & is_built_up == FALSE & !is.na(hb1110_max_units) & hb1110_max_units > 0, 
     cap_hb1110 := pmax(total_baseline_units, as.integer(hb1110_max_units))]
}

# TOD FAR Calculations
dt[, far_delta := pmax(0.0, tod_target_far - current_far, na.rm = TRUE)]
dt[, tod_gross_sqft := 0.0]
dt[lu_code %in% strict_buildable_set & 
     is_built_up == FALSE & 
     tod_target_far > 0 & 
     eff_parcel_sqft >= 3000 & 
     (eff_parcel_sqft * max_lot_coverage) >= min_buildable_footprint_sqft & 
     imp_land_ratio < 1.0, 
   tod_gross_sqft := far_delta * tot_area_m * sqm_to_sqft * max_lot_coverage * net_efficiency_factor]

dt[, cap_tod := pmax(total_baseline_units, as.integer(tod_gross_sqft / avg_unit_sqft))]

# Non-Double-Counting Waterfall Net Additions
dt[, net_1337_add     := pmax(0L, cap_hb1337 - total_baseline_units)]
dt[, stack_after_1337 := pmax(total_baseline_units, cap_hb1337)]
dt[, net_tod_add      := pmax(0L, cap_tod - stack_after_1337)]
dt[, stack_after_tod  := pmax(stack_after_1337, cap_tod)]
dt[, net_1110_add     := pmax(0L, cap_hb1110 - stack_after_tod)]

dt[, max_policy_net_add := fifelse(is_built_up == TRUE, 0L, net_1337_add + net_tod_add + net_1110_add)]

# Zero out net additions for non-buildable/institutional land use codes explicitly
dt[!(lu_code %in% strict_buildable_set), `:=`(
  net_1337_add       = 0L,
  net_tod_add        = 0L,
  net_1110_add       = 0L,
  max_policy_net_add = 0L
)]

# ------------------------------------------------------------------------------
# 5. Story Bins Categorization
# ------------------------------------------------------------------------------
dt[, story_bin := fcase(
  max_policy_net_add == 0 & is_built_up == TRUE,  "Built-Up (High-Hanging)",
  max_policy_net_add == 0 & is_built_up == FALSE, "0 Stories (No Net Add)",
  max_policy_net_add <= 2,   "2 Stories",
  max_policy_net_add <= 6,   "3-4 Stories",
  max_policy_net_add <= 15,  "5-6 Stories",
  max_policy_net_add <= 40,  "6-9 Stories",
  max_policy_net_add <= 100, "10-15 Stories",
  max_policy_net_add >  100, "20+ Stories"
)]

story_levels <- c("Built-Up (High-Hanging)", "0 Stories (No Net Add)", "2 Stories", "3-4 Stories", "5-6 Stories", "6-9 Stories", "10-15 Stories", "20+ Stories")
dt[, story_bin := factor(story_bin, levels = story_levels)]

# Bind calculated fields back to spatial object
if (length(id_col) > 0) {
  final_transit_sf <- unique(as.data.table(final_transit_sf), by = id_col)
  final_transit_sf <- st_as_sf(final_transit_sf)
}

final_transit_sf$is_built_up        <- dt$is_built_up
final_transit_sf$max_policy_net_add <- dt$max_policy_net_add
final_transit_sf$story_bin          <- dt$story_bin
final_transit_sf$net_1337_add       <- dt$net_1337_add
final_transit_sf$net_tod_add        <- dt$net_tod_add
final_transit_sf$net_1110_add       <- dt$net_1110_add

saveRDS(final_transit_sf, output_path)
message(sprintf("Processed spatial object saved to: %s", output_path))

# ------------------------------------------------------------------------------
# 6. Export Infinite-Zoom Vector PDF
# ------------------------------------------------------------------------------
message("Rendering vector PDF map via cairo_pdf...")

stories_map <- ggplot(final_transit_sf) +
  geom_sf(aes(fill = story_bin), color = NA) +
  scale_fill_manual(
    values = c(
      "Built-Up (High-Hanging)" = "#cccccc",
      "0 Stories (No Net Add)"  = "#f2f0f7",
      "2 Stories"               = "#dadaeb",
      "3-4 Stories"             = "#bcbddc",
      "5-6 Stories"             = "#9e9ac8",
      "6-9 Stories"             = "#807dba",
      "10-15 Stories"           = "#6a51a3",
      "20+ Stories"             = "#3f007d"
    ),
    name   = "Parcel Scale & Status"
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title    = "Justifiable Building Scale (Excluding Built-Up High-Hanging Fruit)",
    subtitle = "Parcel-Level Building Height Tiers with Existing Dense Multi-Family Excluded",
    caption  = "Source: Parcel GIS & Legislative Policy Model"
  ) +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    axis.text       = element_blank(),
    axis.ticks      = element_blank(),
    panel.grid      = element_blank(),
    legend.position = "right"
  )

ggsave(vector_pdf_path, plot = stories_map, width = 11, height = 8.5, device = cairo_pdf)
message(sprintf("Vector PDF successfully written to: %s", vector_pdf_path))

# Convert sf to data.table temporarily for ultra-fast filtering
transit_dt <- as.data.table(final_transit_sf)
nonzero_dt <- transit_dt[net_tod_add > 0]
nonzero_transit_sf <- st_as_sf(nonzero_dt)
nonzero_transit_projected <- st_transform(nonzero_transit_sf, crs = 2927)

st_write(
  obj = nonzero_transit_projected,
  dsn = "transit_nonzero_impact.gpkg",
  layer = "tod_active_parcels",
  delete_layer = TRUE
)

# ------------------------------------------------------------------------------
# 7. Comprehensive Policy Tabulation
# ------------------------------------------------------------------------------
dt_summary <- as.data.table(st_drop_geometry(final_transit_sf))

policy_summary <- dt_summary[, .(
  Parcels_Analyzed   = .N,
  Built_Up_Parcels   = sum(is_built_up == TRUE, na.rm = TRUE),
  Net_Units_HB1337   = sum(net_1337_add, na.rm = TRUE),
  Net_Units_TOD_FAR  = sum(net_tod_add, na.rm = TRUE),
  Net_Units_HB1110   = sum(net_1110_add, na.rm = TRUE),
  Total_Net_Units    = sum(max_policy_net_add, na.rm = TRUE)
)]

message("\n==========================================================")
message("         STATEWIDE LEGISLATIVE CAPACITY TABULATION         ")
message("==========================================================")
print(policy_summary)
message("==========================================================")

if ("COUNTY_NM" %in% names(dt_summary)) {
  county_summary <- dt_summary[, .(
    Parcels_Analyzed  = .N,
    Built_Up_Parcels  = sum(is_built_up == TRUE, na.rm = TRUE),
    Net_Units_HB1337  = sum(net_1337_add, na.rm = TRUE),
    Net_Units_TOD     = sum(net_tod_add, na.rm = TRUE),
    Net_Units_HB1110  = sum(net_1110_add, na.rm = TRUE),
    Total_Net_Units   = sum(max_policy_net_add, na.rm = TRUE)
  ), by = COUNTY_NM][order(-Total_Net_Units)]
  
  message("\n--- Breakdown by County (COUNTY_NM) ---")
  print(county_summary)
}

if ("SITUS_CITY_NM" %in% names(dt_summary)) {
  city_summary <- dt_summary[, .(
    Parcels_Analyzed  = .N,
    Built_Up_Parcels  = sum(is_built_up == TRUE, na.rm = TRUE),
    Net_Units_HB1337  = sum(net_1337_add, na.rm = TRUE),
    Net_Units_TOD     = sum(net_tod_add, na.rm = TRUE),
    Net_Units_HB1110  = sum(net_1110_add, na.rm = TRUE),
    Total_Net_Units   = sum(max_policy_net_add, na.rm = TRUE)
  ), by = SITUS_CITY_NM][order(-Total_Net_Units)]
  
  message("\n--- Breakdown by Municipality (SITUS_CITY_NM) ---")
  print(head(city_summary, 20))
}

# Isolate working data.table from final spatial object for TOD metrics
dt_tod <- as.data.table(st_drop_geometry(final_transit_sf))
tod_buffer_dt <- dt_tod[near_tod_buffer == TRUE]

est_units_col <- head(intersect(c("est_units", "total_baseline_units"), names(dt_tod)), 1)
tod_metrics <- tod_buffer_dt[, .(
  total_tod_parcels       = .N,
  avg_existing_units      = round(mean(get(est_units_col), na.rm = TRUE), 2),
  avg_net_unit_increase   = round(mean(max_policy_net_add, na.rm = TRUE), 2),
  total_net_units_yielded = sum(max_policy_net_add, na.rm = TRUE)
)]

message("\n==========================================================")
message("           TOD BUFFER PARCEL METRICS TABULATION           ")
message("==========================================================")
message(sprintf("- Total TOD Buffer Qualifying Parcels:     %s", 
                format(tod_metrics$total_tod_parcels, big.mark = ",")))
message(sprintf("- Average Existing Units per Lot:          %s units", 
                tod_metrics$avg_existing_units))
message(sprintf("- Average Net Unit Increase (to FAR Cap):  %s units", 
                tod_metrics$avg_net_unit_increase))
message(sprintf("- Total Cumulative Net Units Yielded:      %s units", 
                format(tod_metrics$total_net_units_yielded, big.mark = ",")))
message("==========================================================")

# ------------------------------------------------------------------------------
# 8. Consolidated Tabulations & CSV Export
# ------------------------------------------------------------------------------
csv_output_path <- "C:/Users/gmann/Downloads/WA_GIS/Statewide_Capacity_Tabulations_BRT_Rail_Buffers.csv"

statewide_tab <- dt_summary[, .(
  Aggregation_Level = "Statewide",
  Geographic_Entity = "Statewide Total",
  Parcels_Analyzed  = .N,
  Built_Up_Parcels  = sum(is_built_up == TRUE, na.rm = TRUE),
  Net_Units_HB1337  = sum(net_1337_add, na.rm = TRUE),
  Net_Units_TOD     = sum(net_tod_add, na.rm = TRUE),
  Net_Units_HB1110  = sum(net_1110_add, na.rm = TRUE),
  Total_Net_Units   = sum(max_policy_net_add, na.rm = TRUE),
  Avg_Existing_Units    = NA_real_,
  Avg_Net_Unit_Increase = NA_real_
)]

county_tab <- if ("COUNTY_NM" %in% names(dt_summary)) {
  dt_summary[, .(
    Aggregation_Level = "County",
    Geographic_Entity = as.character(COUNTY_NM),
    Parcels_Analyzed  = .N,
    Built_Up_Parcels  = sum(is_built_up == TRUE, na.rm = TRUE),
    Net_Units_HB1337  = sum(net_1337_add, na.rm = TRUE),
    Net_Units_TOD     = sum(net_tod_add, na.rm = TRUE),
    Net_Units_HB1110  = sum(net_1110_add, na.rm = TRUE),
    Total_Net_Units   = sum(max_policy_net_add, na.rm = TRUE),
    Avg_Existing_Units    = NA_real_,
    Avg_Net_Unit_Increase = NA_real_
  ), by = COUNTY_NM][, COUNTY_NM := NULL]
} else data.table()

city_tab <- if ("SITUS_CITY_NM" %in% names(dt_summary)) {
  dt_summary[, .(
    Aggregation_Level = "Municipality",
    Geographic_Entity = as.character(SITUS_CITY_NM),
    Parcels_Analyzed  = .N,
    Built_Up_Parcels  = sum(is_built_up == TRUE, na.rm = TRUE),
    Net_Units_HB1337  = sum(net_1337_add, na.rm = TRUE),
    Net_Units_TOD     = sum(net_tod_add, na.rm = TRUE),
    Net_Units_HB1110  = sum(net_1110_add, na.rm = TRUE),
    Total_Net_Units   = sum(max_policy_net_add, na.rm = TRUE),
    Avg_Existing_Units    = NA_real_,
    Avg_Net_Unit_Increase = NA_real_
  ), by = SITUS_CITY_NM][, SITUS_CITY_NM := NULL]
} else data.table()

master_tabulation <- rbindlist(list(statewide_tab, county_tab, city_tab), use.names = TRUE, fill = TRUE)
fwrite(master_tabulation, file = csv_output_path)
message(sprintf("\nConsolidated tabulations successfully written to CSV: %s", csv_output_path))