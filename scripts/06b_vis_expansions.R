# =========================================================================
# SCRIPT 06b: MAP VISUALIZATION ENGINE - HAZARD OVERLAYS & EXPANSION ZOOMS
# =========================================================================

if (!exists("DATA_DIR")) DATA_DIR <- file.path(chartr("\\", "/", Sys.getenv("USERPROFILE")), "Downloads", "Clark_County_GIS_Atlas")
if (!exists("TARGET_CRS")) TARGET_CRS <- 2927

if (!exists("GENERATE_GRAPHICS")) GENERATE_GRAPHICS <- TRUE
if (!exists("OUTPUT_DIR")) {
  user_root  <- ifelse(Sys.info()[["sysname"]] == "Windows", chartr("\\", "/", Sys.getenv("USERPROFILE")), Sys.getenv("HOME"))
  OUTPUT_DIR <- file.path(user_root, "Documents", "ClarkCountyZoning", "output_products")
}

final_cache_file <- file.path(OUTPUT_DIR, "processed_lots_capacity.rds")

if (GENERATE_GRAPHICS) {
  cat("Executing Stage 6b: Rendering strict hazard overlay and expansion focus maps...\n")
  
  if (!exists("SIGHTLINE_FONT")) SIGHTLINE_FONT <- "sans"
  
  # --- SAFEGUARD 1: RECOVER CAPACITY MODEL ---
  if (!exists("lots_capacity_model")) {
    if (file.exists(final_cache_file)) {
      cat("  -> Loading lots_capacity_model from cache...\n")
      lots_capacity_model <- readRDS(final_cache_file)
    } else {
      stop("Error: processed_lots_capacity.rds missing. Please run Stage 5 first.")
    }
  }
  
  # --- SAFEGUARD 2: RECOVER REFERENCE LAYERS ---
  if (!exists("city_labels") || !exists("city_outlines")) {
    cat("  -> Map reference layers missing. Ingesting municipal outlines and labels...\n")
    city_shp     <- file.path(DATA_DIR, "City.shp")
    city_alt_shp <- file.path(DATA_DIR, "Cities.shp")
    
    if (file.exists(city_shp) || file.exists(city_alt_shp)) {
      target_path <- if (file.exists(city_shp)) city_shp else city_alt_shp
      layer_name  <- sub("\\.shp$", "", basename(target_path), ignore.case = TRUE)
      
      city_sf <- st_read(dsn = DATA_DIR, layer = layer_name, quiet = TRUE)
      
      if (is.na(st_crs(city_sf))) {
        st_crs(city_sf) <- TARGET_CRS
      } else {
        city_sf <- st_transform(city_sf, TARGET_CRS)
      }
      
      city_sf <- st_make_valid(city_sf)
      city_outlines <- city_sf %>% select(geometry)
      
      name_col <- names(city_sf)[grep("CITY|NAME|JURIS", names(city_sf), ignore.case = TRUE)][1]
      
      city_labels <- city_sf %>% 
        group_by(across(all_of(name_col))) %>% 
        summarise(geometry = st_union(geometry), .groups = "drop") %>% 
        st_centroid() %>% 
        rename(city_name = !!sym(name_col))
    } else {
      city_outlines <- st_sf(geometry = st_sfc(crs = TARGET_CRS))
      fallback_cities <- data.frame(
        city_name = c("Vancouver", "Camas", "Washougal", "Battle Ground", "Ridgefield", "La Center"),
        lon = c(-122.6762, -122.4014, -122.3526, -122.5334, -122.7445, -122.6718),
        lat = c(45.6387, 45.5872, 45.5826, 45.7809, 45.8154, 45.8648)
      )
      city_labels <- st_as_sf(fallback_cities, coords = c("lon", "lat"), crs = 4326) %>% 
        st_transform(TARGET_CRS)
    }
  }
  
  if (is.na(st_crs(lots_capacity_model))) st_crs(lots_capacity_model) <- TARGET_CRS
  if (is.na(st_crs(city_outlines)))       st_crs(city_outlines)        <- TARGET_CRS
  if (is.na(st_crs(city_labels)))         st_crs(city_labels)          <- TARGET_CRS
  
  if ("City" %in% names(city_labels)) city_labels <- city_labels %>% rename(city_name = City)
  
  city_labels_shifted <- city_labels
  st_geometry(city_labels_shifted) <- st_geometry(city_labels_shifted) - c(0, 2000)
  st_crs(city_labels_shifted) <- TARGET_CRS
  
  # -------------------------------------------------------------------------
  # --- MAP 2: STRICT HARD HAZARD OVERLAYS MAP ---
  # -------------------------------------------------------------------------
  cat("Compiling Map 2: 4 Strict Environmental & Use Exclusion Overlays...\n")
  
  lots_hazards <- lots_capacity_model %>%
    mutate(
      Has_Hard_Hazard  = (Wetland_Acres > 0 | Slope40_Acres > 0 | Landslide_Acres > 0 | Cemetery_Acres > 0),
      Excluded_Display = ifelse(Has_Hard_Hazard & Hard_Excluded_Acres > 0, Hard_Excluded_Acres, NA)
    )
  
  map_hazards_overlay <- ggplot() +
    geom_sf(data = lots_hazards, fill = "#EAEAEA", color = NA) +
    geom_sf(data = filter(lots_hazards, !is.na(Excluded_Display)), aes(fill = Excluded_Display), color = NA) +
    geom_sf(data = city_outlines, fill = NA, color = "#4A4A4A", size = 0.5) +
    geom_sf_text(
      data = city_labels_shifted, aes(label = city_name), color = "#222222", 
      family = SIGHTLINE_FONT, fontface = "bold", size = 3, alpha = 0.8, check_overlap = TRUE
    ) +
    scale_fill_distiller(
      palette = "Reds", direction = 1, na.value = "#EAEAEA",
      name = "Strict Precluded\nAcres per Parcel\n(4 Hard Hazards)"
    ) +
    labs(
      title = "Map 2: Active Hard Hazard & Exclusion Overlays", 
      subtitle = "Geographic footprint of strict hard preclusions: Slopes >= 40%, Landslide (Lndslp), Wetlands (WetInv), & Cemeteries",
      caption = "Parcels in dark red carry strict non-buildable acreages under updated 4-hazard preclusion criteria."
    ) +
    theme_minimal(base_family = SIGHTLINE_FONT) + 
    theme(
      panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank(), legend.position = "right",
      plot.title.position = "panel",
      plot.title = element_text(face = "bold", size = 14, color = "#111111", margin = margin(b = 4)),
      plot.subtitle = element_text(color = "#555555", size = 10, margin = margin(b = 12)),
      plot.caption = element_text(color = "#777777", size = 8, hjust = 0, margin = margin(t = 8))
    )
  
  print(map_hazards_overlay)
  ggsave(filename = file.path(OUTPUT_DIR, "Map2_Strict_Hazard_Overlays.png"), plot = map_hazards_overlay, width = 10, height = 8, dpi = 300, bg = "white")
  
  # -------------------------------------------------------------------------
  # --- GRAPHIC 4: Vancouver Urban Core Mixed-Use Expansion Zoom Map ---
  # -------------------------------------------------------------------------
  cat("Compiling Graphic 4: Focused Urban Core Expansion Zoom...\n")
  
  restrictive_res_keywords <- "Residential|Resid|Single-family|Single Family|Multifamily|Multiple-family|Mobile Home|MHP|MDR|LDR|HDR|RLD"
  res_keywords <- paste0(restrictive_res_keywords, "|Mixed Use|Mixed-Use|WMU|Office Residential|",
                         "Downtown|Town Center|Village|Commercial|Neighborhood Center|Community Center")
  
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
  
  st_crs(lots_mixed_use_zoom) <- TARGET_CRS
  
  map_vancouver_zoom <- ggplot() +
    geom_sf(data = lots_mixed_use_zoom, aes(fill = Addition_Status), color = NA) +
    geom_sf(data = city_outlines, fill = NA, color = "#4A4A4A", size = 0.5) +
    geom_sf_text(
      data = city_labels_shifted, aes(label = city_name), color = "#222222", 
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
  ggsave(filename = file.path(OUTPUT_DIR, "Zoning_MixedUse_Vancouver_Zoom.png"), plot = map_vancouver_zoom, width = 10, height = 8, dpi = 300, bg = "white")
}