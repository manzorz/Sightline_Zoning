# ==============================================================================
# Ultra-Efficient Parcel-Wetland & King County Parks Exclusion Masking Pipeline
# ==============================================================================
rm(list = ls())

library(sf)
library(data.table)

# ------------------------------------------------------------------------------
# 1. Configuration & Input/Output Paths
# ------------------------------------------------------------------------------
wetlands_path <- "C:/Users/gmann/Documents/WA_GIS/Wetlands_(FP)/Wetlands_(FP).shp"
parks_path    <- "C:/Users/gmann/Downloads/WA_GIS/PARK_AREA_228_1627470712262558603/Parks_in_King_County.shp"
parcels_path  <- "C:/Users/gmann/Downloads/WA_GIS/Parcels_Fully_Processed.rds"
output_path   <- "C:/Users/gmann/Downloads/WA_GIS/Parcels_Fully_Masked.rds"

message("Loading fully processed parcel dataset...")
parcels_sf <- readRDS(parcels_path)

# Ensure CRS match (Target WA State Plane South - EPSG:2927 / feet)
target_crs <- st_crs(parcels_sf)

# ------------------------------------------------------------------------------
# 2. Optimized Exclusion Layers Loading & Bounding-Box Filtering
# ------------------------------------------------------------------------------
parcel_bbox <- st_bbox(parcels_sf)

# A. Wetlands Layer
message("Loading and spatial-filtering wetlands layer...")
wetlands_sfc <- st_geometry(st_read(wetlands_path, quiet = TRUE))
if (st_crs(wetlands_sfc) != target_crs) {
  wetlands_sfc <- st_transform(wetlands_sfc, target_crs)
}
wetlands_sfc <- st_make_valid(wetlands_sfc)
wetlands_sfc <- st_crop(wetlands_sfc, parcel_bbox)

# B. King County Parks Layer
message("Loading and spatial-filtering King County parks layer...")
parks_sfc <- st_geometry(st_read(parks_path, quiet = TRUE))
if (st_crs(parks_sfc) != target_crs) {
  parks_sfc <- st_transform(parks_sfc, target_crs)
}
parks_sfc <- st_make_valid(parks_sfc)
parks_sfc <- st_crop(parks_sfc, parcel_bbox)

# ------------------------------------------------------------------------------
# 3. Optimized Spatial Indexing & Intersection (Avoiding Monolithic Union)
# ------------------------------------------------------------------------------
message("Building spatial indices and checking intersections separately...")

# Ensure valid geometries
wetlands_sfc <- st_make_valid(wetlands_sfc)
parks_sfc    <- st_make_valid(parks_sfc)

# Fast predicate checks leveraging native R-tree spatial indexing
# Checking them individually avoids the monolithic multi-polygon bottleneck
message("Checking wetland intersections...")
touches_wetlands <- lengths(st_intersects(parcels_sf, wetlands_sfc)) > 0

message("Checking park intersections...")
touches_parks    <- lengths(st_intersects(parcels_sf, parks_sfc)) > 0

# Combine flags
touches_exclusion <- touches_wetlands | touches_parks

message(sprintf("Found %s parcels touching wetlands or King County parks.", 
                format(sum(touches_exclusion), big.mark = ",")))

# ------------------------------------------------------------------------------
# 4. Zero-Out Unit Additions in data.table
# ------------------------------------------------------------------------------
message("Updating policy capacity fields to zero for excluded parcels...")

dt <- as.data.table(parcels_sf)

dt[, is_excluded_hazard := touches_exclusion]

dt[is_excluded_hazard == TRUE, `:=`(
  net_1337_add       = 0L,
  net_tod_add        = 0L,
  net_1110_add       = 0L,
  max_policy_net_add = 0L,
  story_bin          = factor("0 Stories (No Net Add)", levels = levels(dt$story_bin))
)]

# Re-attach updated fields back to sf spatial object
parcels_sf$is_excluded_hazard <- dt$is_excluded_hazard
parcels_sf$net_1337_add       <- dt$net_1337_add
parcels_sf$net_tod_add        <- dt$net_tod_add
parcels_sf$net_1110_add       <- dt$net_1110_add
parcels_sf$max_policy_net_add <- dt$max_policy_net_add
parcels_sf$story_bin          <- dt$story_bin

# ------------------------------------------------------------------------------
# 5. Save Masked Parcel Dataset & Non-Zero GeoPackage for QGIS
# ------------------------------------------------------------------------------
saveRDS(parcels_sf, output_path)
message(sprintf("Updated masked parcels saved to: %s", output_path))

message("Writing updated non-zero impact layer to GeoPackage for QGIS...")
nonzero_dt <- dt[max_policy_net_add > 0]
nonzero_sf <- st_as_sf(nonzero_dt)

if (st_crs(nonzero_sf)$epsg != 2927) {
  nonzero_sf <- st_transform(nonzero_sf, crs = 2927)
}

st_write(
  obj = nonzero_sf,
  dsn = "C:/Users/gmann/Downloads/WA_GIS/parcels_fully_masked_nonzero.gpkg",
  layer = "active_buildable_parcels",
  delete_layer = TRUE,
  quiet = TRUE
)

# ------------------------------------------------------------------------------
# 6. Summary Report
# ------------------------------------------------------------------------------
summary_tab <- dt[, .(
  Total_Parcels        = .N,
  Hazard_Impacted      = sum(is_excluded_hazard == TRUE),
  Net_Units_HB1337     = sum(net_1337_add),
  Net_Units_TOD        = sum(net_tod_add),
  Net_Units_HB1110     = sum(net_1110_add),
  Total_Net_Buildout   = sum(max_policy_net_add)
)]

message("\n==========================================================")
message("  FULLY MASKED STATEWIDE HOUSING CAPACITY SUMMARY (WETLANDS + PARKS)")
message("==========================================================")
print(summary_tab)
message("==========================================================")

beepr::beep()
