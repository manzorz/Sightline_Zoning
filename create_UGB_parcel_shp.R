# ==============================================================================
# WA State Parcel, UGA, & Transit Buffer Spatial Join Pipeline
# Integrates HB 1110 (Middle Housing) & E2SSB 5466 (TOD FAR Tiers)
# ==============================================================================
rm(list = ls())

library(sf)
library(data.table)
library(ggplot2)

# Enable S2 processing for spherical calculations
sf_use_s2(TRUE)

# ------------------------------------------------------------------------------
# 1. Global Configuration, Paths & Helpers
# ------------------------------------------------------------------------------
target_crs         <- 2285 # EPSG 2285: WA State Plane North (US Feet)
half_mile_ft       <- 2640 # 0.50 miles in US Survey Feet
quarter_mile_ft    <- 1320 # 0.25 miles in US Survey Feet

uga_path           <- "C:/Users/gmann/Documents/WA_GIS/Washington_State_City_Urban_Growth_Areas/Urban_Growth_Areas_2026.shp"
parcels_path       <- "C:/Users/gmann/Downloads/Current_Parcels_-618300331512082721.gpkg"
transit_path       <- "C:/Users/gmann/Downloads/WA_GIS/Transit_Stops/Transit_Stops.shp"
output_path        <- "C:/Users/gmann/Downloads/WA_GIS/Parcels_UGA_Transit_Joined.rds"

ensure_crs <- function(sf_obj, crs = target_crs) {
  obj_crs <- st_crs(sf_obj)
  if (is.na(obj_crs)) {
    sf_obj <- st_set_crs(sf_obj, crs)
  } else {
    curr_epsg <- obj_crs$epsg
    if (is.na(curr_epsg) || curr_epsg != crs) {
      sf_obj <- st_transform(sf_obj, crs = crs)
    }
  }
  return(sf_obj)
}

# ------------------------------------------------------------------------------
# 2. Comprehensive City Population Reference Table (OFM Estimates)
# ------------------------------------------------------------------------------
city_pop <- data.table(
  CITY_NM = c(
    # Tier 1: 75,000+
    "Seattle", "Spokane", "Tacoma", "Vancouver", "Bellevue", "Kent", "Everett", 
    "Renton", "Federal Way", "Bellingham", "Kennewick", "Auburn", "Pasco", "Redmond",
    
    # Tier 2: 25,000 to 74,999
    "Kirkland", "Richland", "Shoreline", "Olympia", "Lacey", "Edmonds", "Bremerton", 
    "Puyallup", "Lynnwood", "Longview", "Sammamish", "Burien", "Bothell", "Marysville", 
    "Lakewood", "University Place", "Des Moines", "SeaTac", "Mercer Island", 
    "Maple Valley", "Oak Harbor", "Moses Lake", "Camas", "Mount Vernon", "Walla Walla", 
    "Wenatchee", "Pullman", "Yakima", "Spanaway", "Tukwila",
    
    # Tier 3 / Lower Density Municipalities (< 25,000)
    "Ridgefield", "Washougal", "Battle Ground", "La Center", "Woodland", "Yelm", 
    "Arlington", "Monroe", "Snohomish", "Mukilteo", "Centralia", "Chehalis", 
    "Enumclaw", "Bonney Lake", "Fife", "Milton", "Pacific", "Gig Harbor", 
    "Poulsbo", "Port Angeles", "Port Townsend", "Grandview", "Sunnyside", "Toppenish"
  ),
  pop = c(
    755078, 229071, 221776, 192169, 153200, 137710, 111475, 
    107900, 101030, 93910, 84920, 87280, 78700, 76310,
    93260, 62500, 58810, 56000, 55820, 42800, 44100, 
    43100, 40700, 38100, 67490, 52000, 48000, 70000, 
    63000, 34800, 32000, 31500, 25700, 28000, 24600, 
    26000, 27000, 35000, 34000, 35500, 33000, 97000, 35000, 22500,
    14000, 17000, 21000, 3500, 6500, 10500, 
    21000, 20000, 10500, 21500, 18000, 7500, 
    12500, 22500, 10500, 8500, 7200, 12000, 
    11500, 20000, 10000, 11000, 16500, 8900
  )
)
city_pop[, match_city := tolower(trimws(CITY_NM))]
city_pop <- unique(city_pop, by = "match_city")

# ------------------------------------------------------------------------------
# 3. Fast Spatial Load & WKT Filter
# ------------------------------------------------------------------------------
message("Loading Urban Growth Area (UGA) boundaries...")
uga_sf <- st_read(uga_path, quiet = TRUE)

gpkg_info    <- st_layers(parcels_path)
target_layer <- gpkg_info$name[1]
parcel_crs   <- gpkg_info$crs[[1]]

# Align UGA CRS to parcel native CRS to generate bounding box WKT filter
uga_native   <- st_transform(uga_sf, parcel_crs)
uga_wkt_str  <- st_as_text(st_as_sfc(st_bbox(uga_native)))

message("Reading GPKG parcels filtered spatially by UGA boundary WKT...")
parcels_sf <- st_read(
  dsn        = parcels_path,
  layer      = target_layer,
  wkt_filter = uga_wkt_str,
  quiet      = TRUE
)

# Project to target coordinate system
parcels_sf <- ensure_crs(parcels_sf, target_crs)
uga_sf     <- ensure_crs(uga_sf, target_crs)

# Strict spatial subset against exact UGA geometry
parcels_ugb_sf <- st_filter(parcels_sf, uga_sf, .predicate = st_intersects)
message(sprintf("Parcels inside UGB: %s", format(nrow(parcels_ugb_sf), big.mark = ",")))

# ------------------------------------------------------------------------------
# 4. Attribute Processing, Population Join & HB 1110 Tiers
# ------------------------------------------------------------------------------
message("Processing parcel attributes and assigning upzoning tiers...")
dt <- as.data.table(st_drop_geometry(parcels_ugb_sf))

# Calculate parcel area in acres
dt[, calc_acres := as.numeric(st_area(parcels_ugb_sf)) / 43560]

# Determine City Column & Normalize
city_col <- head(intersect(c("SITUS_CITY_NM", "CITY_NM", "Situs City", "CITY_NM.x", 
                             "CITY_NM.y", "Situs_City", "CityName", "UGA_NM"), names(dt)), 1)

if (length(city_col) > 0) {
  dt[, match_city := tolower(trimws(get(city_col)))]
} else {
  dt[, match_city := NA_character_]
}

# Update join population
dt[city_pop, pop := i.pop, on = "match_city"]
dt[is.na(pop), pop := 0L]
dt[, match_city := NULL]

# Classify HB 1110 Base Upzoning Tiers
dt[, hb1110_tier := fifelse(pop >= 75000, "Tier 1 (75k+)",
                            fifelse(pop >= 25000, "Tier 2 (25k-75k)",
                                    fifelse(pop > 0, "Tier 3 (<25k)", "Exempt / Unincorporated")))]

# Estimate Baseline Housing Units based on DOR Land Use
use_col  <- head(intersect(c("LANDUSE_CD", "DOR_CODE", "LAND_USE", "StateLandUse", "PROP_CLASS"), names(dt)), 1)
unit_col <- head(intersect(c("UNIT_COUNT", "NUM_UNITS", "NO_UNITS", "UNITS"), names(dt)), 1)

if (length(unit_col) > 0) {
  dt[, est_units := fifelse(is.na(get(unit_col)) | get(unit_col) == 0, 1L, as.integer(get(unit_col)))]
} else if (length(use_col) > 0) {
  dt[, lu_code := as.integer(as.character(get(use_col)))]
  dt[, est_units := fifelse(lu_code %in% c(11, 18, 19, 14), 1L,
                            fifelse(lu_code == 12, 2L,
                                    fifelse(lu_code == 13, 5L, 0L)))]
} else {
  dt[, est_units := 1L]
}

# ------------------------------------------------------------------------------
# 5. Frequent Transit Buffering (0.25 mi & 0.50 mi Tiers)
# ------------------------------------------------------------------------------
message("Loading transit stops and generating 0.25-mi and 0.50-mi buffers...")
transit_sf <- st_read(transit_path, quiet = TRUE)
transit_sf <- ensure_crs(transit_sf, target_crs)

# Identify frequent transit stops (Level 1 & Level 2 Criteria)
transit_dt <- as.data.table(st_drop_geometry(transit_sf))
freq_indices <- transit_dt[level1 == "1" | level2 == "1", which = TRUE]
freq_transit_sf <- transit_sf[freq_indices, ]

# No longer use level 2 -- only level 1 (15-min daytime frequency) -- for qtr (TOD)
freq_indices_tod <- transit_dt[level1 == "1", which = TRUE]
freq_trans_tod_sf <- transit_sf[freq_indices_tod, ]

# Create unified 0.25-mile and 0.50-mile spatial buffer rings
buffer_qtr_sf  <- st_union(st_buffer(freq_trans_tod_sf,
                                     dist = quarter_mile_ft))
buffer_half_sf <- st_union(st_buffer(freq_transit_sf, dist = half_mile_ft))

# Intersect parcel centroids or geometries against buffers
message("Executing transit proximity spatial joins...")
parcels_in_qtr  <- st_intersects(parcels_ugb_sf, buffer_qtr_sf, sparse = FALSE)[, 1]
saveRDS(parcels_in_qtr, "C:/Users/gmann/Downloads/WA_GIS/Quarter_Mile_Transit_Parcels_UGA.rds")

parcels_in_half <- st_intersects(parcels_ugb_sf, buffer_half_sf, sparse = FALSE)[, 1]
saveRDS(parcels_in_half, "C:/Users/gmann/Downloads/WA_GIS/Half_Mile_Transit_Parcels_UGA.rds")

# Assign transit spatial attributes
dt[, near_transit_025 := parcels_in_qtr]
dt[, near_transit_050 := parcels_in_half]
dt[, near_transit     := near_transit_050] # Backward compatibility for HB 1110 0.50mi rule

# Assign station distance tier
dt[, station_dist_miles := fifelse(near_transit_025 == TRUE, 0.25,
                                   fifelse(near_transit_050 == TRUE, 0.50, 99.0))]

# Assign E2SSB 5466 Target FAR Tiers
dt[, tod_target_far := fifelse(station_dist_miles <= 0.25, 4.0,
                               fifelse(station_dist_miles <= 0.50, 2.5, 0.0))]

# Update HB 1110 Max Units Capacity based on 0.50 mi proximity rule
dt[, hb1110_max_units := fifelse(hb1110_tier == "Tier 1 (75k+)" & near_transit_050 == TRUE, 6L,
                                 fifelse(hb1110_tier == "Tier 1 (75k+)", 4L,
                                         fifelse(hb1110_tier == "Tier 2 (25k-75k)" & near_transit_050 == TRUE, 4L,
                                                 fifelse(hb1110_tier == "Tier 2 (25k-75k)", 2L,
                                                         fifelse(hb1110_tier == "Tier 3 (<25k)", 2L, 1L)))))]

# ------------------------------------------------------------------------------
# 6. Re-attach Processed Attributes & Export Final Spatial RDS
# ------------------------------------------------------------------------------
parcels_ugb_sf$calc_acres         <- dt$calc_acres
parcels_ugb_sf$pop                <- dt$pop
parcels_ugb_sf$hb1110_tier        <- dt$hb1110_tier
parcels_ugb_sf$est_units          <- dt$est_units
parcels_ugb_sf$near_transit_025   <- dt$near_transit_025
parcels_ugb_sf$near_transit_050   <- dt$near_transit_050
parcels_ugb_sf$near_transit       <- dt$near_transit
parcels_ugb_sf$station_dist_miles <- dt$station_dist_miles
parcels_ugb_sf$tod_target_far     <- dt$tod_target_far
parcels_ugb_sf$hb1110_max_units   <- dt$hb1110_max_units

message(sprintf("Saving final processed dataset (%s parcels) to %s...", 
                format(nrow(parcels_ugb_sf), big.mark = ","), output_path))
saveRDS(parcels_ugb_sf, output_path)

# ------------------------------------------------------------------------------
# 7. Diagnostic Plotting (HB 1110 Tiers)
# ------------------------------------------------------------------------------
tier_colors <- c(
  "Tier 1 (75k+)"           = "#2b5c8f",
  "Tier 2 (25k-75k)"         = "#4682b4",
  "Tier 3 (<25k)"            = "#87ceeb",
  "Exempt / Unincorporated" = "#d3d3d3"
)

p <- ggplot(data = parcels_ugb_sf) +
  geom_sf(aes(fill = hb1110_tier), color = NA, alpha = 0.85) +
  scale_fill_manual(
    values   = tier_colors,
    name     = "HB 1110 Upzoning Tier",
    na.value = "#d3d3d3"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(color = "#555555", size = 10),
    legend.position  = "right",
    panel.grid.major = element_line(color = "#e0e0e0", linewidth = 0.2),
    panel.background = element_rect(fill = "#f8f9fa", color = NA)
  ) +
  labs(
    title    = "Washington State HB 1110 Middle Housing Capacity",
    subtitle = sprintf("Joined Dataset: %s Urban Parcels", format(nrow(parcels_ugb_sf), big.mark = ",")),
    caption  = "Source: WA Geoservices Parcels & OFM Population Estimates"
  )

print(p)
message("Pipeline run complete.")