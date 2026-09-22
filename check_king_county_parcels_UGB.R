library(sf)
library(data.table)
library(ggplot2)

# ------------------------------------------------------------------------------
# 1. Align CRS and Isolate King County Features
# ------------------------------------------------------------------------------
message("Aligning CRS between layers...")
if (st_crs(parcels_all) != st_crs(uga_sf)) {
  uga_sf <- st_transform(uga_sf, st_crs(parcels_all))
}

# Convert parcel attributes to data.table for fast filtering
dt_parcels <- as.data.table(parcels_all)

king_parcels_sf <- parcels_all[parcels_all$COUNTY_NM == 33, ]

# ------------------------------------------------------------------------------
# 2. Render ggplot Overlay
# ------------------------------------------------------------------------------
# Note: Using point centroids for parcels speeds up rendering over 500k+ features
king_parcel_points <- st_centroid(st_geometry(king_parcels_sf))

ggplot() +
  # Draw Urban Growth Area (UGA) boundaries as filled polygon base layer
  geom_sf(data = king_parcels_sf, fill = "#e0f2fe", color = "#0284c7", linewidth = 0.8, alpha = 0.5) +
  
  # Overlay Parcel Centroids
  geom_sf(data = king_parcel_points, color = "#dc2626", size = 0.05, alpha = 0.3) +
  
  # Styling & Labels
  labs(
    title = "King County Parcel Distribution vs. Urban Growth Areas (2026)",
    subtitle = sprintf("King County Parcels (n = %s) plotted against UGA Boundaries", 
                       format(nrow(king_parcels_sf), big.mark = ",")),
    caption = "Red points = Parcel Centroids | Blue outline = UGA Boundary Layer"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "#f1f5f9"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "#475569", size = 10)
  )
