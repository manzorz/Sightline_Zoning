# =========================================================================
# SCRIPT 06b: MAP VISUALIZATION ENGINE - INVENTORY EXPANSION CORRECTIONS
# =========================================================================

if (!exists("GENERATE_GRAPHICS")) GENERATE_GRAPHICS <- TRUE
if (!exists("OUTPUT_DIR")) {
  user_root <- ifelse(Sys.info()[["sysname"]] == "Windows", chartr("\\", "/", Sys.getenv("USERPROFILE")), Sys.getenv("HOME"))
  if (Sys.info()[["sysname"]] == "Windows") {
    OUTPUT_DIR <- file.path(user_root, "Documents", "ClarkCountyZoning", "output_products")
  } else {
    OUTPUT_DIR <- file.path(user_root, "Documents", "ClarkCountyZoning", "output_products")
  }
}

if (GENERATE_GRAPHICS) {
  cat("Executing Stage 6b: Rendering Sightline-style expansion analysis maps...\n")
  
  if (!exists("SIGHTLINE_FONT")) SIGHTLINE_FONT <- "sans"
  
  city_labels_shifted <- city_labels %>%
    mutate(geometry = geometry - c(0, 2000))
  
  restrictive_res_keywords <- "Residential|Resid|Single-family|Single Family|Multifamily|Multiple-family|Mobile Home|MHP|MDR|LDR|HDR|RLD"
  res_keywords <- paste0(restrictive_res_keywords, "|Mixed Use|Mixed-Use|WMU|Office Residential|",
                         "Downtown|Town Center|Village|Commercial|Neighborhood Center|Community Center")
  
  # -------------------------------------------------------------------------
  # --- GRAPHIC 4: Vancouver Urban Core Mixed-Use Expansion Zoom Map ---
  # -------------------------------------------------------------------------
  cat("Compiling Graphic 4: Focused Urban Core Expansion Zoom...\n")
  
  lots_mixed_use_zoom <- lots_capacity_model %>%
    mutate(
      Is_Baseline_Residential = grepl(restrictive_res_keywords, desc_, ignore.case = TRUE),
      Is_Expanded_Residential = grepl(res_keywords, desc_, ignore.case = TRUE) & 
        !grepl("Airport/Residential|Heavy Industrial|Light Industrial", desc_, ignore.case = TRUE),
      Addition_Status = case_when(
        Is_Baseline_Residential & Is_Expanded_Residential ~ "Baseline Residential Stock",
        !Is_Baseline_Residential & Is_Expanded_Residential ~ "Added via Commercial/Mixed-Use Expansion",
        TRUE                                               ~ "Non-Residential / Pure Industrial / Parks"
      )
    )
  
  map_vancouver_zoom <- ggplot() +
    geom_sf(data = lots_mixed_use_zoom, aes(fill = Addition_Status), color = NA) +
    geom_sf(data = city_outlines, fill = NA, color = "#4A4A4A", size = 0.5) +
    geom_sf_text(
      data = city_labels_shifted, aes(label = City), color = "#222222", 
      family = SIGHTLINE_FONT, fontface = "bold", size = 3, alpha = 0.8, check_overlap = TRUE
    ) +
    scale_fill_manual(
      values = c("Added via Commercial/Mixed-Use Expansion" = "#FF0000", "Baseline Residential Stock" = "#B19FF1", "Non-Residential / Pure Industrial / Parks" = "#CCCCFF"), 
      name = "Inventory Status"
    ) +
    coord_sf(xlim = c(1060000, 1115000), ylim = c(70000, 145000), expand = FALSE) +
    labs(
      title = "Vancouver Urban Core Housing Inventory Expansion Focus", 
      subtitle = "Zoomed perspective isolating multi-family allowances recovered within commercial and downtown hubs",
      caption = "Bright red clusters highlight parcels that legally permit vertical multi-family housing options."
    ) +
    theme_minimal(base_family = SIGHTLINE_FONT) + 
    theme(
      panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank(), legend.position = "bottom",
      plot.title.position = "panel", plot.caption.position = "panel",
      plot.title = element_text(face = "bold", size = 14, hjust = 0, margin = margin(t = 10, r = 0, b = 2, l = 85)),
      plot.subtitle = element_text(color = "#555555", size = 10, hjust = 0, margin = margin(t = 0, r = 0, b = 10, l = 85)),
      plot.caption = element_text(color = "#777777", size = 8, hjust = 0, margin = margin(t = 10, r = 0, b = 10, l = 85))
    )
  
  print(map_vancouver_zoom)
  ggsave(
    filename = file.path(OUTPUT_DIR, "Zoning_MixedUse_Vancouver_Zoom.png"), 
    plot = map_vancouver_zoom, width = 10, height = 8, dpi = 300, bg = "white"
  )
  
  # -------------------------------------------------------------------------
  # --- GRAPHIC 5: County-Wide Commercial & Mixed-Use Housing Stock Expansion ---
  # -------------------------------------------------------------------------
  cat("Compiling Graphic 5: County-Wide Inventory Expansion Frameworks...\n")
  
  lots_mixed_use_additions <- lots_capacity_model %>%
    mutate(
      Is_Baseline_Residential = grepl(restrictive_res_keywords, desc_, ignore.case = TRUE),
      Is_Expanded_Residential = grepl(res_keywords, desc_, ignore.case = TRUE) & 
        !grepl("Airport/Residential|Heavy Industrial|Light Industrial", desc_, ignore.case = TRUE),
      Addition_Status = case_when(
        Is_Baseline_Residential & Is_Expanded_Residential ~ "Baseline Residential Stock",
        !Is_Baseline_Residential & Is_Expanded_Residential ~ "Added via Commercial/Mixed-Use Expansion",
        TRUE                                               ~ "Non-Residential / Pure Industrial / Parks"
      )
    )
  
  map_mixed_use_additions <- ggplot() +
    geom_sf(data = lots_mixed_use_additions, aes(fill = Addition_Status), color = NA) +
    geom_sf(data = city_outlines, fill = NA, color = "#4A4A4A", size = 0.5) +
    geom_sf_text(
      data = city_labels_shifted, aes(label = City), color = "#222222", 
      family = SIGHTLINE_FONT, fontface = "bold", size = 3, alpha = 0.6, check_overlap = TRUE
    ) +
    scale_fill_manual(
      values = c("Added via Commercial/Mixed-Use Expansion" = "#FF0000", "Baseline Residential Stock" = "#B19FF1", "Non-Residential / Pure Industrial / Parks" = "#CCCCFF"), 
      name = "Inventory Status"
    ) +
    labs(
      title = "County-Wide Housing Inventory Expansion: Commercial & Mixed-Use Frameworks", 
      subtitle = "Isolating parcels unlocked by expanding regional housing filters to capture multi-family allowances in commercial hubs",
      caption = "Bright red clusters highlight commercial, downtown, and town center village zones county-wide that legally permit vertical multi-family housing."
    ) +
    theme_minimal(base_family = SIGHTLINE_FONT) + 
    theme(
      panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank(), legend.position = "bottom",
      plot.title.position = "panel", plot.caption.position = "panel",
      plot.title = element_text(face = "bold", size = 14, hjust = 0, margin = margin(t = 10, r = 0, b = 2, l = 85)),
      plot.subtitle = element_text(color = "#555555", size = 10, hjust = 0, margin = margin(t = 0, r = 0, b = 10, l = 85)),
      plot.caption = element_text(color = "#777777", size = 8, hjust = 0, margin = margin(t = 10, r = 0, b = 10, l = 85))
    )
  
  print(map_mixed_use_additions)
  ggsave(
    filename = file.path(OUTPUT_DIR, "Zoning_MixedUse_Housing_Additions.png"), 
    plot = map_mixed_use_additions, width = 10, height = 8, dpi = 300, bg = "white"
  )
}







