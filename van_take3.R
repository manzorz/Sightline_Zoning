rm(list = ls())

library(data.table)
library(sf)
library(ggplot2)

load("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/cache/full_workspace.RData")

sf_use_s2(FALSE)

# ==============================================================================
# 1. LOAD PARCEL DATA & COMPUTE GEOMETRY PROPERTIES
# ==============================================================================
parcels <- st_read("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/TaxlotsPublic.shp")
parcels_dt <- as.data.table(parcels)

# Compute full parcel area in Square Feet
parcels_dt[, lot_sqft := as.numeric(st_area(geometry))]

# Building Sqft handling
if (!"BldgSqft" %in% names(parcels_dt)) {
  parcels_dt[, BldgSqft := 0]
} else {
  parcels_dt[is.na(BldgSqft), BldgSqft := 0]
}

# Standardize ZoneDesc
parcels_dt[, ZoneDesc := trimws(as.character(ZoneDesc))]
parcels_dt[is.na(ZoneDesc) | ZoneDesc == "" | ZoneDesc == "NA", ZoneDesc := "Unknown"]

# Standardize SitusCity
parcels_dt[, SitusCity := trimws(as.character(SitusCity))]

# ==============================================================================
# 2. POPULATION & WA HB 1110 CITY TIER CLASSIFICATION
# ==============================================================================
# Assign 2024/2025 OFM estimated populations for Clark County cities
city_pop_map <- data.table(
  SitusCity = c("Amboy", "Battle Ground", "Brush Prairie", "Camas", "La Center", 
                "Ridgefield", "Vancouver", "Washougal", "Woodland", "Yacolt"),
  population = c(2200, 22000, 3100, 27500, 3900, 16000, 195000, 17500, 6500, 1700)
)

parcels_dt <- merge(parcels_dt, city_pop_map, by = "SitusCity", all.x = TRUE)
parcels_dt[is.na(population), population := 0]

# Classify cities into HB 1110 Tiers
parcels_dt[, hb1110_tier := fcase(
  population >= 75000, "Tier 1 (75k+)",
  population >= 25000 & population < 75000, "Tier 2 (25k-75k)",
  population > 0 & population < 25000, "Tier 3 (<25k in Contiguous UGA)",
  default = "Exempt / Non-Incorporated"
)]

# ==============================================================================
# 3. TRANSIT STOPS & SPATIAL INTERSECTION
# ==============================================================================
transit_stops <- st_read("C:\\Users\\gmann\\Downloads\\WA_GIS\\Transit_Stops\\Transit_Stops.shp")
transit_stops <- st_transform(transit_stops, st_crs(parcels))

clark_boundary <- st_union(parcels)
transit_stops_clark <- st_intersection(transit_stops, clark_boundary)

transit_dt <- as.data.table(transit_stops_clark)

# Quarter-mile (1,320 ft) buffers around transit stops
transit_stops_sf <- st_as_sf(transit_dt)
transit_buffers_sf <- st_buffer(transit_stops_sf, dist = 1320)
transit_union <- st_union(transit_buffers_sf)

# Field: Check if parcel touches transit buffer
parcels_sf_temp <- st_as_sf(parcels_dt)
parcels_dt[, touches_transit := st_intersects(parcels_sf_temp, transit_union, sparse = FALSE)[, 1]]

# ==============================================================================
# 4. URBAN GROWTH BOUNDARY (UGA) INTERSECTION & % CONTAINED
# ==============================================================================
uga_sf <- st_read("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/ugabnd.shp", quiet = TRUE)
uga_sf <- uga_sf[uga_sf$desc_ != "County" & !is.na(uga_sf$desc_), ]

uga_sf <- st_transform(uga_sf, st_crs(parcels))
uga_union <- st_union(uga_sf)

# Spatial intersection using correct data.table column syntax [, "prop_id"]
parcels_sf_all <- st_as_sf(parcels_dt)
intersection_sf <- st_intersection(parcels_sf_all[, "prop_id"], uga_union)

# Calculate area of intersected geometries inside UGA
intersection_dt <- as.data.table(intersection_sf)

if (nrow(intersection_dt) > 0) {
  intersection_dt[, uga_sqft := as.numeric(st_area(geometry))]
  uga_area_summary <- intersection_dt[, .(uga_sqft = sum(uga_sqft)), by = prop_id]
  
  parcels_dt <- merge(parcels_dt, uga_area_summary, by = "prop_id", all.x = TRUE)
  parcels_dt[is.na(uga_sqft), uga_sqft := 0]
} else {
  parcels_dt[, uga_sqft := 0]
}

# Calculate exact percentage contained within UGB
parcels_dt[, pct_uga := pmin(1.0, uga_sqft / lot_sqft)]

# ==============================================================================
# 5. ENVIRONMENTAL HAZARD LAYERS INTEGRATION (SLOPES, LANDSLIDES, WETLANDS)
# ==============================================================================
atlas_dir <- "C:/Users/gmann/Downloads/Clark_County_GIS_Atlas"

wetlands_sf <- st_read(file.path(atlas_dir, "WetInv.shp"), quiet = TRUE)
landslides_sf <- st_read(file.path(atlas_dir, "Lndslp.shp"), quiet = TRUE)
slopes_raw_sf <- st_read(file.path(atlas_dir, "Slopes.shp"), quiet = TRUE)

# Subset slopes (>= 40%)
slopes_steep_sf <- slopes_raw_sf[slopes_raw_sf$desc_ %in% c("greater than 100 percent", "40 - 100 percent"), ]

# Combine into single geometry collection and match parcel CRS
hazard_geoms <- c(
  st_geometry(slopes_steep_sf),
  st_geometry(landslides_sf),
  st_geometry(wetlands_sf)
)

target_crs <- st_crs(parcels)
hazard_combined <- st_sfc(hazard_geoms, crs = st_crs(slopes_raw_sf))
hazard_combined <- st_transform(hazard_combined, target_crs)

# Dissolve overlapping hazard geometries
hazard_union <- st_union(st_make_valid(hazard_combined))

# Intersect hazards with parcels using LandProp_i identifier
parcels_valid_sf <- st_make_valid(parcels_sf_all[, "LandProp_i"])
hazard_intersection <- st_intersection(parcels_valid_sf, hazard_union)

hazard_dt_temp <- as.data.table(hazard_intersection)

if (nrow(hazard_dt_temp) > 0) {
  hazard_dt_temp[, hazard_sqft := as.numeric(st_area(geometry))]
  hazard_summary <- hazard_dt_temp[, .(hazard_sqft = sum(hazard_sqft)), by = LandProp_i]
  
  parcels_dt <- merge(parcels_dt, hazard_summary, by = "LandProp_i", all.x = TRUE)
  parcels_dt[is.na(hazard_sqft), hazard_sqft := 0]
} else {
  parcels_dt[, hazard_sqft := 0]
}

# Calculate hazard footprint percentage and net buildable area
parcels_dt[, pct_hazard := pmin(1.0, hazard_sqft / lot_sqft)]
parcels_dt[, net_buildable_sqft := pmax(0, lot_sqft - hazard_sqft)]

# ==============================================================================
# 6. REALISTIC ZONING & HB 1110 POTENTIAL UNIT CALCULATION (WITH CAPS & HAZARDS)
# ==============================================================================
sf_zones <- c(
  "R-2", "R-4", "R-5", "R-6", "R-7.5", "R-9", "R-10", "R-15", "R-20",
  "R1-5", "R1-6", "R1-7.5", "R1-10", "R1-12.5", "R1-15", "R1-20",
  "R5", "R7", "R10", "R20", "R3", "RLD-4", "RLD-6", "RLD-8", "LDR-6", 
  "LDR-7.5", "LD-NS", "UP", "NP", "SU", "GW", "HX", "RP", "CR-1", 
  "CR-2", "RC", "RC-1", "RC-2.5"
)

mf_zones <- c(
  "R-12", "R-18", "R-22", "R-30", "R-43", "MF-12", "MF-16", "MF-18", 
  "MF-22", "MF-30", "MFR-12", "MFR-18", "MFR-22", "MFR-30", "HDR", 
  "MDR", "UMR", "MUR"
)

mixed_use_zones <- c(
  "MX", "CX", "CC", "NC", "C-2", "C-3", "VVC", "MU-R", "MU-E"
)

parcels_dt[, land_use_type := fcase(
  ZoneDesc %in% sf_zones, "Single-Family",
  ZoneDesc %in% mf_zones, "Multi-Family",
  ZoneDesc %in% mixed_use_zones, "Commercial / Mixed-Use",
  default = "Other / Non-Residential"
)]

parcels_dt[, is_single_family := land_use_type == "Single-Family"]
parcels_dt[, current_units := as.integer(Units)]

# 1. Base Zoning Density Multiplier using net_buildable_sqft
parcels_dt[, uncapped_base_max := fcase(
  pct_uga < 0.50 | ZoneDesc %in% c("AG", "FR", "IL", "IH", "BP", "OPEN", "PF", "UR"), 0L,
  
  # Low-Density / Estate Single-Family Zones
  ZoneDesc %in% c("R-10", "R1-10"), as.integer(floor(net_buildable_sqft / 10000)),
  ZoneDesc %in% c("R-15", "R1-15"), as.integer(floor(net_buildable_sqft / 15000)),
  ZoneDesc %in% c("R-20", "R1-20"), as.integer(floor(net_buildable_sqft / 20000)),
  ZoneDesc %in% c("RC-1", "CR-1"),  as.integer(floor(net_buildable_sqft / 43560)),
  ZoneDesc %in% c("RC-2.5", "CR-2"), as.integer(floor(net_buildable_sqft / 108900)),
  ZoneDesc %in% c("R-5", "R5"),     as.integer(floor(net_buildable_sqft / 217800)),
  
  # Standard Urban Single-Family Zones
  ZoneDesc %in% c("R-9", "R1-9"),   pmax(1L, as.integer(floor((net_buildable_sqft / 43560) * 9))),
  ZoneDesc %in% c("R-7.5", "R1-7.5", "LDR-7.5"), pmax(1L, as.integer(floor((net_buildable_sqft / 43560) * 7.5))),
  ZoneDesc %in% c("R-6", "R1-6", "RLD-6", "LDR-6"), pmax(1L, as.integer(floor((net_buildable_sqft / 43560) * 6))),
  ZoneDesc %in% c("R-5", "R1-5"),   pmax(1L, as.integer(floor((net_buildable_sqft / 43560) * 5))),
  ZoneDesc %in% c("R-4", "RLD-4"),  pmax(1L, as.integer(floor((net_buildable_sqft / 43560) * 4))),
  ZoneDesc %in% c("R-2", "R-3"),    pmax(1L, as.integer(floor((net_buildable_sqft / 43560) * 3))),
  ZoneDesc %in% sf_zones,           pmax(1L, as.integer(floor((net_buildable_sqft / 43560) * 6))),
  
  # Multi-Family Zones
  ZoneDesc %in% c("R-12", "MF-12", "MFR-12"), as.integer(floor((net_buildable_sqft / 43560) * 12)),
  ZoneDesc %in% c("R-18", "MF-16", "MF-18", "MFR-18"), as.integer(floor((net_buildable_sqft / 43560) * 18)),
  ZoneDesc %in% c("R-22", "MF-22", "MFR-22"), as.integer(floor((net_buildable_sqft / 43560) * 22)),
  ZoneDesc %in% c("R-30", "MF-30", "MFR-30"), as.integer(floor((net_buildable_sqft / 43560) * 30)),
  ZoneDesc %in% c("R-43", "HDR"),              as.integer(floor((net_buildable_sqft / 43560) * 43)),
  ZoneDesc %in% mf_zones,                     as.integer(floor((net_buildable_sqft / 43560) * 20)),
  
  # Commercial / Mixed-Use Zones
  ZoneDesc %in% mixed_use_zones,              as.integer(floor((net_buildable_sqft * 1.5) / 800)),
  
  default = 0L
)]

# 2. Apply Real-World Baseline Caps
parcels_dt[, base_zoned_max := fcase(
  ZoneDesc %in% sf_zones, pmin(uncapped_base_max, 6L),
  default = uncapped_base_max
)]

# 3. HB 1110 Tier Allowance
parcels_dt[, hb1110_statutory_tier := fcase(
  pct_uga < 0.50 | !(ZoneDesc %in% sf_zones), 0L,
  hb1110_tier == "Tier 1 (75k+)" & touches_transit == TRUE, 6L,
  hb1110_tier == "Tier 1 (75k+)", 4L,
  hb1110_tier == "Tier 2 (25k-75k)" & touches_transit == TRUE, 4L,
  hb1110_tier == "Tier 2 (25k-75k)", 2L,
  hb1110_tier == "Tier 3 (<25k in Contiguous UGA)", 2L,
  default = 0L
)]

# Physical minimum lot area constraint based on net buildable area
parcels_dt[, hb1110_physical_cap := as.integer(floor(net_buildable_sqft / 1200))]

# 4. Final Maximum Allowed Units (Including 80%+ Hazard Cutoff & Resource Exclusions)
parcels_dt[, hb1110_max_allowed := fcase(
  pct_hazard >= 0.80, current_units,
  ZoneDesc %in% sf_zones, pmax(base_zoned_max, pmin(hb1110_statutory_tier, hb1110_physical_cap)),
  default = base_zoned_max
)]

# ==============================================================================
# 4. VOLUMETRIC ENVELOPE & FINAL MAXIMUM UNITS RECALCULATION
# ==============================================================================
# Parameter Adjustments
default_lot_coverage <- 0.35   # 35% max footprint (accounting for setbacks/parking)
avg_unit_sqft        <- 1250   # Realistic multi-story townhome/duplex unit size

# Assign max stories based on transit proximity & zoning
parcels_dt[, max_stories := fcase(
  touches_transit == TRUE & ZoneDesc %in% sf_zones, 3L,  # 3 stories near transit
  ZoneDesc %in% sf_zones, 2L,                            # 2 stories standard single-family
  ZoneDesc %in% mf_zones, 4L,                            # 4 stories multi-family
  ZoneDesc %in% mixed_use_zones, 5L,                     # 5 stories commercial/mixed
  default = 0L
)]

# Recalculate envelope
parcels_dt[, max_footprint_sqft := net_buildable_sqft * default_lot_coverage]
parcels_dt[, max_gross_building_sqft := max_footprint_sqft * max_stories]
parcels_dt[, height_volume_cap := as.integer(floor(max_gross_building_sqft / avg_unit_sqft))]

# Re-run final assignment
parcels_dt[, hb1110_max_allowed := fcase(
  pct_hazard >= 0.80, current_units,
  ZoneDesc %in% sf_zones, pmax(
    base_zoned_max, 
    pmin(hb1110_statutory_tier, hb1110_physical_cap, height_volume_cap)
  ),
  default = base_zoned_max
)]

parcels_dt[ZoneDesc %in% c("AG", "FR", "IL", "IH", "BP", "OPEN") | lot_sqft > 435600, 
           hb1110_max_allowed := current_units]

# 1. Assign Density Caps (Units per Acre) safely with an explicit default
parcels_dt[, max_dua := fcase(
  ZoneDesc %in% sf_zones,        20L,  # HB 1110 middle housing scale
  ZoneDesc %in% mf_zones,        43L,  # Multi-family (e.g., R-43)
  ZoneDesc %in% mixed_use_zones, 60L,  # Commercial / Mixed-Use
  default = 0L                         # Hard stop for rural, industrial, resource, & unclassified
)]

# 2. Calculate density cap by lot acreage
parcels_dt[, max_units_by_dua := as.integer(floor((net_buildable_sqft / 43560) * max_dua))]

# 3. Final Unit Assignment with Fallback Rules
parcels_dt[, hb1110_max_allowed := fcase(
  # Hard Exclusion: High hazard or Resource/Industrial zones retain current density
  pct_hazard >= 0.80 | ZoneDesc %in% c("AG", "FR", "IL", "IH", "BP", "OPEN") | lot_sqft > 435600, current_units,
  
  # Single-Family Zones (Strictly bound by HB 1110 statutory tiers & volumetric limits)
  ZoneDesc %in% sf_zones, pmax(
    base_zoned_max, 
    pmin(hb1110_statutory_tier, hb1110_physical_cap, height_volume_cap, 6L)
  ),
  
  # Multi-Family / Commercial Zones (Cap by max_dua or an absolute max threshold of 100 units per parcel)
  ZoneDesc %in% c(mf_zones, mixed_use_zones), pmin(
    pmax(base_zoned_max, current_units), 
    pmax(current_units, max_units_by_dua), 
    100L
  ),
  
  # Fallback for all other zones: keep base zoned max or current units
  default = pmax(base_zoned_max, current_units)
)]

# 4. Recalculate Net Potential
parcels_dt[, Num_Potential := pmax(0L, hb1110_max_allowed - current_units)]

# Check max and total
print(paste("New Max Potential on a single parcel:", max(parcels_dt$Num_Potential, na.rm = TRUE)))
print(paste("New Total Potential Units:", sum(parcels_dt$Num_Potential, na.rm = TRUE)))

print(sum(parcels_dt$Num_Potential, na.rm = TRUE))

# ==============================================================================
# 7. VISUALIZATION WITH CONTINUOUS PASTEL GRADIENT
# ==============================================================================
uga_labels <- st_point_on_surface(uga_sf)
max_units <- max(parcels_dt$Num_Potential, na.rm = TRUE)

ggplot() +
  geom_sf(
    data = st_as_sf(parcels_dt),
    aes(fill = Num_Potential),
    color = NA
  ) +
  scale_fill_gradientn(
    colors = c("#d9d9d9", "#fff2ae", "#bc80bd"),
    values = scales::rescale(c(0, 1, pmax(1, max_units))),
    limits = c(0, max_units),
    name = "HB 1110 Potential Units"
  ) +
  geom_sf(
    data = uga_sf,
    fill = NA,
    color = "#1a1a1a",
    linewidth = 0.5,
    alpha = 0.4
  ) +
  geom_sf_text(
    data = uga_labels,
    aes(label = City),
    color = "#1a1a1a",
    size = 3.2,
    fontface = "bold",
    alpha = 0.6,
    check_overlap = TRUE
  ) +
  labs(
    title = "WA HB 1110 Middle Housing Unit Capacity in Clark County",
    subtitle = "Adjusted for Population Tiers, Transit Proximity, UGA Boundaries, & Environmental Hazards",
    caption = "Source: Clark County GIS & WA HB 1110 Statutory Thresholds"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "#4e4e4e"),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )

# ==============================================================================
# 7. VISUALIZATION WITH CONTINUOUS PASTEL GRADIENT & HIGH-RES PNG EXPORT
# ==============================================================================
uga_labels <- st_point_on_surface(uga_sf)
max_units  <- max(parcels_dt$Num_Potential, na.rm = TRUE)

p_map <- ggplot() +
  geom_sf(
    data = st_as_sf(parcels_dt),
    aes(fill = Num_Potential),
    color = NA
  ) +
  scale_fill_gradientn(
    colors = c("#d9d9d9", "#fff2ae", "#bc80bd"),
    values = scales::rescale(c(0, 1, pmax(1, max_units))),
    limits = c(0, max_units),
    name = "HB 1110 Potential Units"
  ) +
  geom_sf(
    data = uga_sf,
    fill = NA,
    color = "#1a1a1a",
    linewidth = 0.5,
    alpha = 0.4
  ) +
  geom_sf_text(
    data = uga_labels,
    aes(label = City),
    color = "#1a1a1a",
    size = 3.2,
    fontface = "bold",
    alpha = 0.6,
    check_overlap = TRUE
  ) +
  labs(
    title = "WA HB 1110 Middle Housing Unit Capacity in Clark County",
    subtitle = "Adjusted for Population Tiers, Transit Proximity, UGA Boundaries, & Environmental Hazards",
    caption = "Source: Clark County GIS & WA HB 1110 Statutory Thresholds"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "#4e4e4e"),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )

# Save to high-res PNG
ggsave(
  filename = "C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/Clark_County_Capacity_Clean_UGA.png",
  plot     = p_map,
  width    = 16,        # Inches
  height   = 12,        # Inches
  dpi      = 400,       # 400 DPI gives ~6400x4800 pixels for zooming
  bg       = "white"
)

print(sum(parcels_dt$Num_Potential, na.rm = TRUE))

print(sum(parcels_dt[!is.na(JurisDesc) & JurisDesc != "Clark County"]$Num_Potential, na.rm = TRUE))

# 1. Classify development scale per parcel based on net potential units
parcels_dt[, dev_scale := fcase(
  Num_Potential == 0,                      "0_No_Change",
  Num_Potential >= 1 & Num_Potential <= 5, "Modest (1-5 Units)",
  Num_Potential >= 6,                      "Corporate/Institutional (6+ Units)"
)]

# 2. Crunch the summary table
capacity_split <- parcels_dt[
  Num_Potential > 0, 
  .(
    Parcel_Count    = .N,
    Total_Potential = sum(Num_Potential, na.rm = TRUE)
  ), 
  by = dev_scale
]

# 3. Add percentage calculations
capacity_split[, `:=`(
  Pct_of_Total_Units   = round((Total_Potential / sum(Total_Potential)) * 100, 2),
  Pct_of_Active_Lots   = round((Parcel_Count / sum(Parcel_Count)) * 100, 2)
)]

# 4. Format and print results
print("=================== MIDDLE HOUSING CAPACITY SPLIT ===================")
print(capacity_split[order(-Total_Potential)])

# Print high-level summary statements directly
modest_units <- capacity_split[dev_scale == "Modest (1-3 Units)", Total_Potential]
corp_units   <- capacity_split[dev_scale == "Corporate/Institutional (4+ Units)", Total_Potential]
total_units  <- sum(capacity_split$Total_Potential)

cat(sprintf("\nModest Infill (1-3 Units): %s units (%.1f%%)\n", 
            format(modest_units, big.mark=","), (modest_units / total_units) * 100))
cat(sprintf("Corporate/Institutional (4+ Units): %s units (%.1f%%)\n", 
            format(corp_units, big.mark=","), (corp_units / total_units) * 100))

print(table(parcels_dt$Num_Potential))
