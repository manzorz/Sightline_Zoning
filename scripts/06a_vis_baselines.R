# =========================================================================
# SCRIPT 06a: MAP VISUALIZATION ENGINE - BASELINE POLICY & UNCONSTRAINED POTENTIAL
# =========================================================================

if (!exists("DATA_DIR")) DATA_DIR <- file.path(chartr("\\", "/", Sys.getenv("USERPROFILE")), "Downloads", "Clark_County_GIS_Atlas")
if (!exists("TARGET_CRS")) TARGET_CRS <- 2927

if (!exists("GENERATE_GRAPHICS")) GENERATE_GRAPHICS <- TRUE
if (!exists("OUTPUT_DIR")) {
  user_root  <- ifelse(Sys.info()[["sysname"]] == "Windows", chartr("\\", "/", Sys.getenv("USERPROFILE")), Sys.getenv("HOME"))
  OUTPUT_DIR <- file.path(user_root, "Documents", "ClarkCountyZoning", "output_products")
}

final_cache_file <- file.path(OUTPUT_DIR, "processed_lots_capacity.rds")

# Safeguard: Ensure hazard acreage columns exist on lots_capacity_model
required_hazard_cols <- c("Slope40_Acres", "Landslide_Acres", "Wetland_Acres", "Cemetery_Acres", "Hard_Excluded_Acres")

for (col in required_hazard_cols) {
  if (!col %in% names(lots_capacity_model)) {
    cat(sprintf("  [!] Warning: %s missing from dataset. Deriving fallback column...\n", col))
    
    lots_capacity_model <- lots_capacity_model %>%
      mutate(
        Slope40_Acres   = if ("Intersects_Slope" %in% names(.)) ifelse(Intersects_Slope, Lot_Acres, 0) else 0,
        Landslide_Acres = if ("Intersects_Landslide" %in% names(.)) ifelse(Intersects_Landslide, Lot_Acres, 0) else 0,
        Wetland_Acres   = if ("Intersects_Wetland" %in% names(.)) ifelse(Intersects_Wetland, Lot_Acres, 0) else 0,
        Cemetery_Acres  = if ("Intersects_Cemetery" %in% names(.)) ifelse(Intersects_Cemetery, Lot_Acres, 0) else 0,
        Hard_Excluded_Acres = Slope40_Acres + Landslide_Acres + Wetland_Acres + Cemetery_Acres
      )
    break
  }
}

# --- SAFEGUARD 1: RECOVER CITY LABELS & OUTLINES ---
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
    cat("  -> Local city shapefile not found. Generating fallback spatial labels...\n")
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

if (GENERATE_GRAPHICS) {
  cat("Executing Stage 6a: Rendering unconstrained capacity and policy baseline maps...\n")
  
  # --- SAFEGUARD 2: RECOVER CAPACITY MODEL ---
  if (!exists("lots_capacity_model")) {
    if (file.exists(final_cache_file)) {
      cat("  -> Loading lots_capacity_model from cache...\n")
      lots_capacity_model <- readRDS(final_cache_file)
    } else {
      stop("Error: processed_lots_capacity.rds missing. Please run Stage 5 first.")
    }
  }
  
  if (is.na(st_crs(lots_capacity_model))) st_crs(lots_capacity_model) <- TARGET_CRS
  
  # --- SAFEGUARD 3: RECOVER CLEANED ZONING LAYER ---
  if (!exists("zoning_cleaned")) {
    zoning_cache <- file.path(OUTPUT_DIR, "zoning_cleaned.rds")
    if (file.exists(zoning_cache)) {
      cat("  -> Loading zoning_cleaned layer from disk...\n")
      zoning_cleaned <- readRDS(zoning_cache)
    } else {
      cat("  -> Deriving zoning boundary framework directly from capacity model...\n")
      zoning_cleaned <- lots_capacity_model %>% 
        group_by(Zoning_ID, desc_) %>% 
        summarise(geometry = st_union(geometry), .groups = "drop")
    }
  }
  
  if (is.na(st_crs(zoning_cleaned))) st_crs(zoning_cleaned) <- TARGET_CRS
  
  if (!exists("SIGHTLINE_FONT")) SIGHTLINE_FONT <- "sans"
  if (!exists("COLOR_UNINCORPORATED")) COLOR_UNINCORPORATED <- "#F2F2F2"
  if (!exists("COLOR_ZERO_CAPACITY")) COLOR_ZERO_CAPACITY <- "#D3D3D3"
  
  city_labels_shifted <- city_labels
  st_geometry(city_labels_shifted) <- st_geometry(city_labels_shifted) - c(0, 2000)
  
  restrictive_res_keywords <- "Residential|Resid|Single-family|Single Family|Multifamily|Multiple-family|Mobile Home|MHP|MDR|LDR|HDR|RLD"
  res_keywords <- paste0(restrictive_res_keywords, "|Mixed Use|Mixed-Use|WMU|Office Residential|",
                         "Downtown|Town Center|Village|Commercial|Neighborhood Center|Community Center")
  
  headroom_col <- names(lots_capacity_model)[grep("Net_Realizable_Homes|Net_Housing_Headroom|Net_Realizable_Units|Net_Headroom", names(lots_capacity_model), ignore.case = TRUE)][1]
  
  zone_summary_layer <- lots_capacity_model %>%
    group_by(Zoning_ID) %>%
    summarise(
      geometry = st_union(geometry), 
      Total_Net_Realizable = sum(suppressWarnings(as.numeric(!!sym(headroom_col))), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Zoning_Status = ifelse(Total_Net_Realizable > 0, "Under Zoned Limit (Has Room)", "At/Over Zoned Limit")
    )
  
  st_crs(lots_capacity_model) <- TARGET_CRS
  st_crs(zone_summary_layer)  <- TARGET_CRS
  st_crs(city_outlines)       <- TARGET_CRS
  st_crs(city_labels_shifted) <- TARGET_CRS
  
  # -------------------------------------------------------------------------
  # --- GRAPHIC 1: Zone-Level Growth Potential Summary ---
  # -------------------------------------------------------------------------
  cat("Compiling Graphic 1: Zone-Level Limitations Baseline...\n")
  
  map_categorical <- ggplot() +
    geom_sf(data = lots_capacity_model, fill = COLOR_UNINCORPORATED, color = NA) +
    geom_sf(data = zone_summary_layer, aes(fill = Zoning_Status), color = NA) +
    geom_sf(data = city_outlines, fill = NA, color = "#4A4A4A", size = 0.5, linetype = "solid") +
    geom_sf_text(
      data = city_labels_shifted, aes(label = city_name), color = "#222222", 
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
  ggsave(filename = file.path(OUTPUT_DIR, "Zoning_Limitations_Categorical.png"), plot = map_categorical, width = 10, height = 8, dpi = 300, bg = "white")
  
  # -------------------------------------------------------------------------
  # --- GRAPHIC 2: MAP 1 - UNCONSTRAINED ROOM TO GROW (RAW ZONING DENSITY) ---
  # -------------------------------------------------------------------------
  cat("Compiling Graphic 2: Map 1 - Unconstrained Room to Grow Gradient...\n")
  
  lots_unconstrained <- lots_capacity_model %>%
    mutate(
      Gross_Footprint_SqFt       = (Lot_Acres * 43560) * Setback_Reduction_Factor,
      Gross_Floor_Area           = (Gross_Footprint_SqFt * Max_Lot_Coverage_Pct) * Max_Stories,
      Gross_Unit_Capacity        = floor(Gross_Floor_Area / Assumed_Unit_Size),
      Gross_Reg_Cap              = ifelse(is.infinite(Regulatory_Density_Cap), Inf, floor(Lot_Acres * UnitsPerAc)),
      Unconstrained_Max_Units    = pmin(Gross_Unit_Capacity, Gross_Reg_Cap),
      Unconstrained_Room_To_Grow = pmax(0, Unconstrained_Max_Units - Units),
      Display_Unconstrained      = ifelse(Unconstrained_Room_To_Grow > 0, Unconstrained_Room_To_Grow, NA)
    )
  
  map_unconstrained <- ggplot() +
    geom_sf(data = lots_unconstrained, fill = COLOR_UNINCORPORATED, color = NA) +
    geom_sf(data = filter(lots_unconstrained, is.na(Display_Unconstrained)), fill = COLOR_ZERO_CAPACITY, color = NA) +
    geom_sf(data = filter(lots_unconstrained, !is.na(Display_Unconstrained)), aes(fill = Display_Unconstrained), color = NA) +
    geom_sf(data = city_outlines, fill = NA, color = "#4A4A4A", size = 0.5) +
    geom_sf_text(
      data = city_labels_shifted, aes(label = city_name), color = "#222222", 
      family = SIGHTLINE_FONT, fontface = "bold", size = 3, alpha = 0.8, check_overlap = TRUE
    ) +
    scale_fill_viridis_c(
      option = "magma", trans = "sqrt", na.value = COLOR_ZERO_CAPACITY,
      name = "Unconstrained\nNet Room to Grow\n(Max Homes Allowed\nIgnoring Hazards)"
    ) +
    labs(
      title = "Unconstrained Housing Capacity: Outstanding Room to Grow", 
      subtitle = "Theoretical maximum buildable housing unit headroom strictly derived from base zoning envelope (Hazards Ignored)",
      caption = "Note: Grey parcels represent built-out sites or properties with zero remaining zoning envelope headroom."
    ) +
    theme_minimal(base_family = SIGHTLINE_FONT) + 
    theme(
      panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank(), legend.position = "right",
      plot.title.position = "panel",
      plot.title = element_text(face = "bold", size = 14, color = "#111111", margin = margin(b = 4)),
      plot.subtitle = element_text(color = "#555555", size = 10, margin = margin(b = 12)),
      plot.caption = element_text(color = "#777777", size = 8, hjust = 0, margin = margin(t = 8))
    )
  
  print(map_unconstrained)
  ggsave(filename = file.path(OUTPUT_DIR, "Map1_Unconstrained_Room_To_Grow.png"), plot = map_unconstrained, width = 10, height = 8, dpi = 300, bg = "white")
}