library(sf)
library(data.table)

# Load your processed parcels
parcels_sf <- readRDS("C:/Users/gmann/Downloads/WA_GIS/Parcels_Fully_Processed.rds")

# Filter for growth potential
active_parcels <- parcels_sf[parcels_sf$max_policy_net_add > 0, ]

# Keep absolute minimum columns required for the Shiny app popup and filters
keep_cols <- c(
  "SITUS_ADDR", "SITUS_CITY_NM", "COUNTY_NM", 
  "lu_code", "current_far", "total_baseline_units", 
  "net_1337_add", "net_tod_add", "net_1110_add", 
  "max_policy_net_add", "story_bin", "geometry"
)
active_parcels <- active_parcels[, names(active_parcels) %in% keep_cols]

# Optional: Simplify geometries slightly to reduce vertex weight (tolerance = 1 foot)
# active_parcels <- st_simplify(active_parcels, dTolerance = 1)

# Save with maximum compression (compression_level = 9)
setwd("C:/Users/gmann/Repos/Sightline_Zoning/shiny_viewer")
# 4. Save as a lean RDS file for your Shiny app
saveRDS(active_parcels, "data/parcels_lightweight.rds", compress = "xz")
