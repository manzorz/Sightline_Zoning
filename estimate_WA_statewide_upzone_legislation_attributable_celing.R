# ==============================================================================
# Refined Building Stories Binning & Dual Export Script
# ==============================================================================
rm(list = ls())

library(sf)
library(data.table)
library(ggplot2)
library(leaflet)
library(htmlwidgets)

# ------------------------------------------------------------------------------
# 1. Configuration & Output Paths
# ------------------------------------------------------------------------------
input_path       <- "C:/Users/gmann/Downloads/WA_GIS/Parcels_UGA_Transit_Joined.rds"
output_path      <- "C:/Users/gmann/Downloads/WA_GIS/Parcels_With_Refined_Stories.rds"
vector_pdf_path  <- "C:/Users/gmann/Downloads/WA_GIS/Statewide_Refined_Stories_Vector.pdf"
interactive_html <- "C:/Users/gmann/Downloads/WA_GIS/City_Capacity_Refined_Interactive.html"

# Modeling Parameters
avg_unit_sqft                 <- 850
sqm_to_sqft                   <- 10.7639
net_efficiency_factor         <- 0.75
max_lot_coverage              <- 0.55
max_redev_parcel_sqft         <- 217800
min_buildable_footprint_sqft <- 1000

message("Loading joined spatial transit-parcel dataset...")
final_transit_sf <- readRDS(input_path)

dt <- as.data.table(st_drop_geometry(final_transit_sf))

# ------------------------------------------------------------------------------
# 2. Parcel Deduplication & Schema Standardization
# ------------------------------------------------------------------------------
id_col <- head(intersect(c("PIN", "PARCEL_ID", "PARCELID", "PolyID"), names(dt)), 1)
if (length(id_col) > 0) {
  dt <- unique(dt, by = id_col)
}

dt[, lu_code := as.integer(as.character(LANDUSE_CD))]
res_sf_codes   <- c(11, 14, 18, 19)
comm_tod_codes <- c(50:69)

if (!"parcel_sqft" %in% names(dt)) {
  dt[, parcel_sqft := as.numeric(st_area(final_transit_sf)) * sqm_to_sqft]
}
dt[is.na(parcel_sqft) | is.nan(parcel_sqft), parcel_sqft := 0]
dt[, eff_parcel_sqft := pmin(parcel_sqft, max_redev_parcel_sqft)]
dt[, tot_area_m      := eff_parcel_sqft / sqm_to_sqft]

# ------------------------------------------------------------------------------
# 3. Capacity Calculations & Unit Yield Waterfall
# ------------------------------------------------------------------------------
dt[, imputed_units := fifelse(lu_code %in% c(11, 14, 18, 19), 1L,
                              fifelse(lu_code == 12, 3L,
                                      fifelse(lu_code == 13, 20L,
                                              fifelse(lu_code == 15, 10L,
                                                      fifelse(lu_code == 60, 2L, 0L)))))]
dt[, total_baseline_units := fifelse(is.na(imputed_units), 0L, as.integer(imputed_units))]

if (!"station_dist_miles" %in% names(dt)) {
  if ("near_transit_025" %in% names(dt) && "near_transit_050" %in% names(dt)) {
    dt[, station_dist_miles := fifelse(near_transit_025 == TRUE, 0.25,
                                       fifelse(near_transit_050 == TRUE, 0.50, 99.0))]
  } else {
    dt[, station_dist_miles := 99.0]
  }
}
dt[is.na(station_dist_miles), station_dist_miles := 99.0]

dt[, tod_target_far := fifelse(station_dist_miles <= 0.25, 4.0,
                               fifelse(station_dist_miles <= 0.50, 2.5, 0.0))]
dt[is.na(tod_target_far), tod_target_far := 0.0]

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

dt[, cap_hb1337 := total_baseline_units]
dt[lu_code %in% res_sf_codes, cap_hb1337 := pmax(total_baseline_units, 3L)]

dt[, cap_hb1110 := total_baseline_units]
dt[lu_code %in% res_sf_codes & !is.na(hb1110_max_units) & hb1110_max_units > 0, 
   cap_hb1110 := pmax(total_baseline_units, as.integer(hb1110_max_units))]

dt[, far_delta := pmax(0.0, tod_target_far - current_far, na.rm = TRUE)]
dt[, tod_gross_sqft := 0.0]
dt[lu_code %in% c(res_sf_codes, comm_tod_codes) & 
     tod_target_far > 0 & 
     eff_parcel_sqft >= 3000 & 
     (eff_parcel_sqft * max_lot_coverage) >= min_buildable_footprint_sqft & 
     imp_land_ratio < 1.0, 
   tod_gross_sqft := far_delta * tot_area_m * sqm_to_sqft * max_lot_coverage * net_efficiency_factor]

dt[, cap_tod := pmax(total_baseline_units, as.integer(tod_gross_sqft / avg_unit_sqft))]

dt[, net_1337_add  := pmax(0L, cap_hb1337 - total_baseline_units)]
dt[, stack_after_1337 := pmax(total_baseline_units, cap_hb1337)]
dt[, net_tod_add   := pmax(0L, cap_tod - stack_after_1337)]
dt[, stack_after_tod := pmax(stack_after_1337, cap_tod)]
dt[, net_1110_add  := pmax(0L, cap_hb1110 - stack_after_tod)]

dt[, max_policy_net_add := net_1337_add + net_tod_add + net_1110_add]

# ------------------------------------------------------------------------------
# 4. Refined Story Bins Translation
# ------------------------------------------------------------------------------
# Implied raw stories based on footprint density & net unit yield
dt[, raw_stories := fifelse(max_policy_net_add == 0, 0,
                            pmax(2, ceiling(max_policy_net_add / (pmax(1, eff_parcel_sqft * max_lot_coverage / avg_unit_sqft)))))]

# Grouping into requested categorical bins:
# 0, 2, 3-4, 5-6, 6-9, 10-15, 20+
dt[, story_bin := fcase(
  max_policy_net_add == 0, "0 Stories",
  max_policy_net_add <= 2, "2 Stories",
  max_policy_net_add <= 6, "3-4 Stories",
  max_policy_net_add <= 15, "5-6 Stories",
  max_policy_net_add <= 40, "6-9 Stories",
  max_policy_net_add <= 100, "10-15 Stories",
  max_policy_net_add >  100, "20+ Stories"
)]

# Factor ordering for legend and plotting
story_levels <- c("0 Stories", "2 Stories", "3-4 Stories", "5-6 Stories", "6-9 Stories", "10-15 Stories", "20+ Stories")
dt[, story_bin := factor(story_bin, levels = story_levels)]

if (length(id_col) > 0) {
  final_transit_sf <- unique(as.data.table(final_transit_sf), by = id_col)
  final_transit_sf <- st_as_sf(final_transit_sf)
}

final_transit_sf$max_policy_net_add <- dt$max_policy_net_add
final_transit_sf$story_bin          <- dt$story_bin

saveRDS(final_transit_sf, output_path)

# ------------------------------------------------------------------------------
# 5. Export Vector PDF
# ------------------------------------------------------------------------------
message("Rendering refined vector PDF...")

stories_map <- ggplot(final_transit_sf) +
  geom_sf(aes(fill = story_bin), color = NA) +
  scale_fill_manual(
    values = c(
      "0 Stories"     = "#f2f0f7",
      "2 Stories"     = "#dadaeb",
      "3-4 Stories"   = "#bcbddc",
      "5-6 Stories"   = "#9e9ac8",
      "6-9 Stories"   = "#807dba",
      "10-15 Stories" = "#6a51a3",
      "20+ Stories"   = "#3f007d"
    ),
    name   = "Justifiable\nBuilding Scale"
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title    = "Refined Justifiable Building Scale (Stories)",
    subtitle = "Parcel-Level Maximum Building Height Based on Expanded Unit Yield Tiers",
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
message(sprintf("Vector PDF saved to: %s", vector_pdf_path))

# ------------------------------------------------------------------------------
# 6. Export Interactive Leaflet Web Map
# ------------------------------------------------------------------------------
message("Generating interactive Web Map...")

sf_wgs84 <- st_transform(final_transit_sf, 4326)

pal <- colorFactor(
  palette = c("#f2f0f7", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#3f007d"),
  levels  = story_levels
)

interactive_map <- leaflet(sf_wgs84) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~pal(story_bin),
    weight = 0.5,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.8,
    popup = ~sprintf(
      "<strong>Parcel ID:</strong> %s<br/><strong>Net New Units:</strong> %d<br/><strong>Building Scale:</strong> %s",
      if(length(id_col) > 0) get(id_col) else "N/A",
      max_policy_net_add,
      story_bin
    )
  ) %>%
  addLegend(
    pal = pal,
    values = ~story_bin,
    title = "Building Scale",
    opacity = 0.9,
    position = "bottomright"
  )

saveWidget(interactive_map, interactive_html, selfcontained = TRUE)
message(sprintf("Interactive HTML map saved to: %s", interactive_html))