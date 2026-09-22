# ==============================================================================
# Continuous Gradient Housing Capacity Mapping Pipeline
# ==============================================================================
rm(list = ls())

library(sf)
library(data.table)
library(ggplot2)

output_path    <- "C:/Users/gmann/Downloads/WA_GIS/Parcels_With_Unit_Counts.rds"
state_png_path <- "C:/Users/gmann/Downloads/WA_GIS/Statewide_Capacity_Map.png"
king_png_path  <- "C:/Users/gmann/Downloads/WA_GIS/King_County_Capacity_Map.png"

# Load dataset and cast to data.table
final_transit_sf <- readRDS(output_path)
setDT(final_transit_sf)

# Calculate net additional units under HB 1110 & HB 1337
final_transit_sf[, net_buildable_units := middle_add + ADU_add_net]

# Palette setup
low_color  <- "#E9D5FF" # Light lavender for 1-unit potential
high_color <- "#7E22CE" # Vibrant deep violet for max potential

# ------------------------------------------------------------------------------
# Shared ggplot plotting function
# ------------------------------------------------------------------------------
build_continuous_map <- function(dt_data, map_title, map_subtitle, map_caption) {
  
  # Split dataset to render zero/NA potential parcels in light grey behind buildable ones
  zero_sf  <- st_as_sf(dt_data[is.na(net_buildable_units) | net_buildable_units == 0])
  build_sf <- st_as_sf(dt_data[net_buildable_units >= 1])
  
  min_add <- min(build_sf$net_buildable_units, na.rm = TRUE)
  max_add <- max(build_sf$net_buildable_units, na.rm = TRUE)
  
  ggplot() +
    # Layer A: Parcels with 0 net build potential (Light Grey)
    geom_sf(
      data = zero_sf,
      fill = "#E5E7EB",
      color = NA,
      linewidth = 0
    ) +
    
    # Layer B: Parcels with >= 1 net build potential (Vibrant Violet Gradient)
    geom_sf(
      data = build_sf,
      aes(fill = net_buildable_units, color = after_scale(fill)),
      linewidth = 0.01
    ) +
    
    # Continuous Gradient Scale
    scale_fill_gradient(
      low      = low_color,
      high     = high_color,
      name     = "Net Housing Add",
      limits   = c(min_add, max_add),
      na.value = "#E5E7EB"
    ) +
    
    # Theme Setup: Clean White Backdrop
    theme_void(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 16, color = "#111827"),
      plot.subtitle    = element_text(color = "#4B5563", size = 11, margin = margin(b = 10)),
      plot.caption     = element_text(color = "#6B7280", size = 9, margin = margin(t = 10)),
      legend.position  = "right",
      legend.title     = element_text(size = 10, face = "bold", color = "#1F2937"),
      legend.text      = element_text(size = 9, color = "#374151"),
      panel.background = element_rect(fill = "#FFFFFF", color = NA),
      plot.background  = element_rect(fill = "#FFFFFF", color = NA),
      legend.background= element_rect(fill = "#FFFFFF", color = NA),
      plot.margin      = margin(15, 15, 15, 15)
    ) +
    labs(
      title    = map_title,
      subtitle = map_subtitle,
      caption  = map_caption
    )
}

# ------------------------------------------------------------------------------
# 1. Render & Export Statewide Map
# ------------------------------------------------------------------------------
message("Processing Statewide Map...")

p_state <- build_continuous_map(
  dt_data      = final_transit_sf,
  map_title    = "Washington State Net Housing Capacity Add per Parcel",
  map_subtitle = "Modeled Net Additional Units Under HB 1110 & HB 1337 (Light Grey = 0 Add)",
  map_caption  = "Source: WA Parcel Dataset, OFM Population & Transit Route Buffers"
)

ggsave(
  filename = state_png_path,
  plot     = p_state,
  width    = 14,
  height   = 10,
  dpi      = 300,
  units    = "in"
)
message("Successfully saved Statewide Map: ", state_png_path)

# ------------------------------------------------------------------------------
# 2. Render & Export King County Map
# ------------------------------------------------------------------------------
message("Processing King County Map...")

king_dt <- final_transit_sf[COUNTY_NM %in% c("King", "KING") | FIPS_NR %in% c("033", 33, "33")]

p_king <- build_continuous_map(
  dt_data      = king_dt,
  map_title    = "King County Net Housing Capacity Add per Parcel",
  map_subtitle = "Modeled Net Additional Units Under HB 1110 & HB 1337 (Light Grey = 0 Add)",
  map_caption  = "Source: WA Parcel Dataset & Transit Route Buffers | Filter: King County (FIPS 033)"
)

ggsave(
  filename = king_png_path,
  plot     = p_king,
  width    = 12,
  height   = 10,
  dpi      = 300,
  units    = "in"
)
message("Successfully saved King County Map: ", king_png_path)