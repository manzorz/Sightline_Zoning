# =========================================================================
# SCRIPT 06a: MAP VISUALIZATION ENGINE - BASELINE POLICY LAYERS
# =========================================================================

if (!exists("GENERATE_GRAPHICS")) GENERATE_GRAPHICS <- TRUE
if (!exists("OUTPUT_DIR")) OUTPUT_DIR <- "C:/Users/gmann/Documents/ClarkCountyZoning/output_products"

if (GENERATE_GRAPHICS) {
  cat("Executing Stage 6a: Rendering Sightline-style baseline policy maps...\n")
  
  if (!exists("SIGHTLINE_FONT")) SIGHTLINE_FONT <- "sans"
  if (!exists("COLOR_UNINCORPORATED")) COLOR_UNINCORPORATED <- "#F2F2F2"
  if (!exists("COLOR_ZERO_CAPACITY")) COLOR_ZERO_CAPACITY <- "#D3D3D3"
  
  # Shift labels half font size downward from old ymax + 4000
  city_labels_shifted <- city_labels %>%
    mutate(geometry = geometry - c(0, 2000))
  
  # --- CONSOLIDATE BASELINE CATEGORIES ---
  restrictive_res_keywords <- "Residential|Resid|Single-family|Single Family|Multifamily|Multiple-family|Mobile Home|MHP|MDR|LDR|HDR|RLD"
  res_keywords <- paste0(restrictive_res_keywords, "|Mixed Use|Mixed-Use|WMU|Office Residential|",
                         "Downtown|Town Center|Village|Commercial|Neighborhood Center|Community Center")
  
  zoning_matrix_data <- zoning_cleaned %>%
    left_join(
      st_drop_geometry(lots_capacity_model) %>% 
        group_by(Zoning_ID) %>% 
        summarise(Units = sum(Units, na.rm = TRUE)), 
      by = "Zoning_ID"
    ) %>%
    mutate(
      Is_Residential = ifelse(
        grepl(res_keywords, desc_, ignore.case = TRUE) & 
          !grepl("Airport/Residential|Heavy Industrial|Light Industrial", desc_, ignore.case = TRUE), 
        "Residential / Mixed-Use Zoning", 
        "Pure Commercial / Industrial / Parks"
      )
    )
  
  zone_summary_layer <- lots_capacity_model %>%
    group_by(Zoning_ID) %>%
    summarise(
      geometry = st_union(geometry), 
      Total_Net_Realizable = sum(Net_Realizable_Homes, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      Zoning_Status = ifelse(Total_Net_Realizable > 0, "Under Zoned Limit (Has Room)", "At/Over Zoned Limit")
    )
  
  # -------------------------------------------------------------------------
  # --- GRAPHIC 1: Zone-Level Growth Potential Summary ---
  # -------------------------------------------------------------------------
  cat("Compiling Graphic 1: Zone-Level Limitations Baseline...\n")
  
  map_categorical <- ggplot() +
    geom_sf(data = lots_capacity_model, fill = COLOR_UNINCORPORATED, color = NA) +
    geom_sf(data = zone_summary_layer, aes(fill = Zoning_Status), color = NA) +
    geom_sf(data = city_outlines, fill = NA, color = "#4A4A4A", size = 0.5, linetype = "solid") +
    geom_sf_text(
      data = city_labels_shifted, aes(label = City), color = "#222222", 
      family = SIGHTLINE_FONT, fontface = "bold", size = 3, alpha = 0.8, check_overlap = TRUE
    ) +
    scale_fill_manual(
      values = c("Under Zoned Limit (Has Room)" = "#21918c", "At/Over Zoned Limit" = "#CCCCFF"), 
      name = "Zoning Limitations"
    ) +
    labs(
      title = "Zoning Limitations & Growth Potential", 
      subtitle = "Zone-level capacity baseline aggregated from individual property parcel inventory constraints",
      caption = "Source: Clark County GIS Atlas & Regulatory Database Policy Model Analysis."
    ) +
    theme_minimal(base_family = SIGHTLINE_FONT) + 
    theme(
      panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank(), legend.position = "bottom",
      plot.title.position = "panel",
      plot.title = element_text(face = "bold", size = 14, color = "#111111", margin = margin(b = 4)),
      plot.subtitle = element_text(color = "#555555", size = 10, margin = margin(b = 12)),
      plot.caption = element_text(color = "#777777", size = 8, hjust = 0, margin = margin(t = 8))
    )
  
  print(map_categorical)
  ggsave(
    filename = file.path(OUTPUT_DIR, "Zoning_Limitations_Categorical.png"), 
    plot = map_categorical, width = 10, height = 8, dpi = 300, bg = "white"
  )
  
  # -------------------------------------------------------------------------
  # --- GRAPHIC 2: Lot-Level Zoning-Constrained Housing Potential Gradient ---
  # -------------------------------------------------------------------------
  cat("Compiling Graphic 2: Capacity Gradient Map...\n")
  
  lots_gradient <- lots_capacity_model %>%
    mutate(Headroom_Display = ifelse(Net_Realizable_Homes == 0, NA, Net_Realizable_Homes))
  
  map_gradient <- ggplot() +
    geom_sf(data = lots_gradient, fill = COLOR_UNINCORPORATED, color = NA) +
    geom_sf(data = filter(lots_gradient, is.na(Headroom_Display)), fill = COLOR_ZERO_CAPACITY, color = NA) +
    geom_sf(data = filter(lots_gradient, !is.na(Headroom_Display)), aes(fill = Headroom_Display), color = NA) +
    geom_sf(data = city_outlines, fill = NA, color = "#4A4A4A", size = 0.5) +
    geom_sf_text(
      data = city_labels_shifted, aes(label = City), color = "#222222", 
      family = SIGHTLINE_FONT, fontface = "bold", size = 3, alpha = 0.8, check_overlap = TRUE
    ) +
    scale_fill_gradient(
      low = "#FFFFE0", high = "#FF0000", 
      name = "Zoning-Constrained\nHousing Potential\n(Net New Homes\nAllowed by Law)", na.value = COLOR_ZERO_CAPACITY
    ) +
    labs(
      title = "Net Realizable Zoning-Constrained Housing Potential Map", 
      subtitle = "Lot-level net unit headroom accounting for local setbacks, variable story caps, and environmental restrictions",
      caption = "Note: Grey parcels represent land that has reached its effective cap on housing supply under current rules."
    ) +
    theme_minimal(base_family = SIGHTLINE_FONT) + 
    theme(
      panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank(), legend.position = "right",
      plot.title.position = "panel",
      plot.title = element_text(face = "bold", size = 14, color = "#111111", margin = margin(b = 4)),
      plot.subtitle = element_text(color = "#555555", size = 10, margin = margin(b = 12)),
      plot.caption = element_text(color = "#777777", size = 8, hjust = 0, margin = margin(t = 8))
    )
  
  print(map_gradient)
  ggsave(
    filename = file.path(OUTPUT_DIR, "Zoning_Headroom_Gradient.png"), 
    plot = map_gradient, width = 10, height = 8, dpi = 300, bg = "white"
  )
  
  # -------------------------------------------------------------------------
  # --- GRAPHIC 3: Residential Footprint Matrix ---
  # -------------------------------------------------------------------------
  cat("Compiling Graphic 3: Policy Footprint Matrix...\n")
  
  map_residential <- ggplot() +
    geom_sf(data = zoning_matrix_data, aes(fill = Is_Residential), color = NA) +
    geom_sf(data = city_outlines, fill = NA, color = "#4A4A4A", size = 0.5) +
    geom_sf_text(
      data = city_labels_shifted, aes(label = City), color = "#222222", 
      family = SIGHTLINE_FONT, fontface = "bold", size = 3, alpha = 0.8, check_overlap = TRUE
    ) +
    scale_fill_manual(
      values = c("Residential / Mixed-Use Zoning" = "#B19FF1", "Pure Commercial / Industrial / Parks" = "#CCCCFF"), 
      name = "Regulatory Framework"
    ) +
    labs(
      title = "Residential vs Non-Residential Zoning Footprint Matrix", 
      subtitle = "Consolidated county-wide view of regulatory land allowances and structural prohibitions for housing",
      caption = "Source: Clark County Spatial Planning Layer Framework Database."
    ) +
    theme_minimal(base_family = SIGHTLINE_FONT) + 
    theme(
      panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank(), legend.position = "bottom",
      plot.title.position = "panel",
      plot.title = element_text(face = "bold", size = 14, color = "#111111", margin = margin(b = 4)),
      plot.subtitle = element_text(color = "#555555", size = 10, margin = margin(b = 12)),
      plot.caption = element_text(color = "#777777", size = 8, hjust = 0, margin = margin(t = 8))
    )
  
  print(map_residential)
  ggsave(
    filename = file.path(OUTPUT_DIR, "Zoning_Residential_Footprint_Matrix.png"), 
    plot = map_residential, width = 10, height = 8, dpi = 300, bg = "white"
  )
}
