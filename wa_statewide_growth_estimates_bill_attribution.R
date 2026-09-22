# ==============================================================================
# Washington State Housing Legislation Capacity Estimation Script
# Modular UGA-by-UGA Batch Processing & Multi-Scale Map Generation
# ==============================================================================
rm(list = ls())

library(arcgislayers)
library(sf)
library(data.table)
library(ggplot2)

# ==============================================================================
# CONFIGURATION & TOGGLES
# ==============================================================================
# Spatial Processing Toggles
RUN_WETLANDS_PROCESSING <- FALSE  # Default: FALSE (Skips wetlands spatial join)
RUN_TRANSIT_PROCESSING  <- TRUE   # Default: TRUE  (Runs transit buffer join)

CHUNK_SIZE              <- 10000 

# Directory Setup
cache_dir       <- "C:/Users/gmann/Downloads/WA_GIS/Cache"
uga_cache_dir   <- file.path(cache_dir, "UGA_Outputs")
if (!dir.exists(uga_cache_dir)) dir.create(uga_cache_dir, recursive = TRUE)

waza_cache_file <- file.path(cache_dir, "waza_parcels_sf.rds")
uga_path        <- "C:/Users/gmann/Downloads/WA_GIS/UGA/Urban_Growth_Areas.shp"
wetlands_path   <- "C:/Users/gmann/Documents/WA_GIS/Wetlands_(FP)/Wetlands_(FP).shp"
transit_path    <- "C:/Users/gmann/Downloads/WA_GIS/Transit_Stops/Transit_Stops.shp"

# Target EPSG: WA State Plane North (US Feet) - EPSG 2285
target_crs <- 2285

ensure_crs <- function(sf_obj, crs = target_crs) {
  if (is.na(st_crs(sf_obj))) {
    sf_obj <- st_set_crs(sf_obj, crs)
  } else if (st_crs(sf_obj)$epsg != crs) {
    sf_obj <- st_transform(sf_obj, crs = crs)
  }
  return(sf_obj)
}

# Selective Global Spatial Layer Loading
wetlands_sf          <- NULL
transit_buffers_geom <- NULL

if (RUN_WETLANDS_PROCESSING) {
  cat("Loading Wetlands layer...\n")
  wetlands_sf <- st_read(wetlands_path, quiet = TRUE)
  wetlands_sf <- ensure_crs(wetlands_sf, target_crs)
}

if (RUN_TRANSIT_PROCESSING) {
  cat("Loading Transit Stops layer & buffering (1/4 mile)...\n")
  transit_sf           <- st_read(transit_path, quiet = TRUE)
  transit_sf           <- ensure_crs(transit_sf, target_crs)
  transit_buffers_geom <- st_buffer(st_geometry(transit_sf), dist = 1320)
  rm(transit_sf)
  gc()
}

# ------------------------------------------------------------------------------
# 1. Load Base Zoning Dataset (WAZA)
# ------------------------------------------------------------------------------
if (file.exists(waza_cache_file)) {
  cat("Loading cached WAZA spatial dataset...\n")
  parcels_sf <- readRDS(waza_cache_file)
  parcels_sf <- ensure_crs(parcels_sf, target_crs)
} else {
  cat("Downloading zoning dataset from WAZA FeatureServer...\n")
  waza_url <- "https://services6.arcgis.com/tboeqGwETr5ppr5Q/arcgis/rest/services/WAZA_Prototype_Layers/FeatureServer/0"
  
  waza_conn  <- arc_open(waza_url)
  parcels_sf <- arc_select(waza_conn)
  parcels_sf <- ensure_crs(parcels_sf, target_crs)
  
  saveRDS(parcels_sf, waza_cache_file)
}

if ("LandProp_id" %in% names(parcels_sf) & !"LandProp_i" %in% names(parcels_sf)) {
  names(parcels_sf)[names(parcels_sf) == "LandProp_id"] <- "LandProp_i"
}

parcels_sf$calc_acres <- as.numeric(st_area(parcels_sf)) / 43560

# ------------------------------------------------------------------------------
# 2. Tag Urban Growth Areas & City Attributes
# ------------------------------------------------------------------------------
if (file.exists(uga_path)) {
  cat("Tagging UGA boundaries via spatial join...\n")
  uga_sf <- st_read(uga_path, quiet = TRUE)
  uga_sf <- ensure_crs(uga_sf, target_crs)
  
  uga_join <- st_join(parcels_sf["LandProp_i"], uga_sf[, c("UGA_NM", "CITY_NM")], join = st_intersects, left = TRUE)
  uga_join <- uga_join[!duplicated(uga_join$LandProp_i), ]
  
  if ("UGA_NM" %in% names(uga_join))  parcels_sf$UGA_NM  <- uga_join$UGA_NM
  if ("CITY_NM" %in% names(uga_join)) parcels_sf$CITY_NM <- uga_join$CITY_NM
  
  if (!"CITY_NM" %in% names(parcels_sf)) parcels_sf$CITY_NM <- NA_character_
  if (!"UGA_NM" %in% names(parcels_sf))  parcels_sf$UGA_NM  <- NA_character_
  
  parcels_sf$UGA_NM[is.na(parcels_sf$UGA_NM) & !is.na(parcels_sf$CITY_NM)] <- parcels_sf$CITY_NM[is.na(parcels_sf$UGA_NM) & !is.na(parcels_sf$CITY_NM)]
  parcels_sf$UGA_NM[is.na(parcels_sf$UGA_NM)] <- "Unincorporated"
  
  parcels_sf$in_ugb <- parcels_sf$UGA_NM != "Unincorporated"
  
  rm(uga_sf, uga_join)
  gc()
} else {
  if ("CITY_NM" %in% names(parcels_sf) && !is.null(parcels_sf$CITY_NM)) {
    parcels_sf$UGA_NM <- ifelse(!is.na(parcels_sf$CITY_NM), as.character(parcels_sf$CITY_NM), "Unincorporated")
  } else {
    parcels_sf$CITY_NM <- "Unincorporated"
    parcels_sf$UGA_NM  <- "Unincorporated"
  }
  
  parcels_sf$in_ugb <- parcels_sf$UGA_NM != "Unincorporated"
}

# Population lookup table for HB 1110 tiering logic
city_pop <- data.table(
  CITY_NM = c("Seattle", "Spokane", "Tacoma", "Vancouver", "Bellevue", "Kent", "Everett", 
              "Renton", "Federal Way", "Bellingham", "Kennewick", "Auburn", "Pasco", 
              "Redmond", "Richland", "Shoreline", "Kirkland", "Olympia", "Lacey", 
              "Edmonds", "Bremerton", "Puyallup", "Longview", "Lynnwood", "Camas", "Ridgefield"),
  pop = c(755078, 229071, 221776, 192169, 153200, 137710, 111475, 
          107900, 101030, 93910, 84920, 87280, 78700, 
          76310, 62500, 58810, 93260, 56000, 55820, 
          42800, 44100, 43100, 38100, 40700, 27000, 14000)
)

# Robust definition of uga_list
existing_ugas   <- if ("UGA_NM" %in% names(parcels_sf)) na.omit(unique(parcels_sf$UGA_NM)) else character(0)
existing_cities <- if ("CITY_NM" %in% names(parcels_sf)) na.omit(unique(parcels_sf$CITY_NM)) else character(0)

uga_list <- unique(c(city_pop$CITY_NM, existing_ugas, existing_cities))
uga_list <- uga_list[uga_list %in% existing_ugas | uga_list %in% existing_cities]

if ("Seattle" %in% uga_list) {
  uga_list <- c("Seattle", setdiff(uga_list, "Seattle"))
}

# ------------------------------------------------------------------------------
# 3. Batch Process Each UGA Separately & Write to Cache
# ------------------------------------------------------------------------------
cat("\n=== BEGINNING INDIVIDUAL UGA PROCESSING LOOP ===\n")

for (uga_name in uga_list) {
  
  clean_uga_name <- gsub("[^A-Za-z0-9_]", "_", uga_name)
  out_file       <- file.path(uga_cache_dir, paste0("uga_", clean_uga_name, ".rds"))
  
  if (file.exists(out_file)) {
    cat(sprintf("[SKIP] UGA '%s' already exists on disk.\n", uga_name))
    next
  }
  
  cat(sprintf("\n[PROCESSING] UGA/City: '%s'...\n", uga_name))
  
  sub_parcels <- parcels_sf[(parcels_sf$UGA_NM == uga_name | parcels_sf$CITY_NM == uga_name) & 
                              !is.na(parcels_sf$UGA_NM), ]
  n_sub       <- nrow(sub_parcels)
  
  if (n_sub == 0) next
  
  uga_bbox     <- st_as_sfc(st_bbox(sub_parcels))
  wetland_hits <- logical(n_sub)
  transit_hits <- logical(n_sub)
  
  # --- WETLANDS INTERSECTION ---
  if (RUN_WETLANDS_PROCESSING && !is.null(wetlands_sf)) {
    sub_wetlands <- st_crop(wetlands_sf, uga_bbox)
    
    if (length(sub_wetlands) > 0) {
      wetland_geom <- st_simplify(st_geometry(sub_wetlands), dTolerance = 5, preserveTopology = TRUE)
      for (i in seq(1, n_sub, by = CHUNK_SIZE)) {
        idx <- i:min(i + CHUNK_SIZE - 1, n_sub)
        sub_int <- st_intersects(st_geometry(sub_parcels[idx, ]), wetland_geom, sparse = TRUE)
        wetland_hits[idx] <- lengths(sub_int) > 0
        gc()
      }
      rm(wetland_geom)
    }
  }
  
  # --- TRANSIT INTERSECTION ---
  if (RUN_TRANSIT_PROCESSING && !is.null(transit_buffers_geom)) {
    sub_transit <- st_crop(transit_buffers_geom, uga_bbox)
    
    if (length(sub_transit) > 0) {
      for (i in seq(1, n_sub, by = CHUNK_SIZE)) {
        idx <- i:min(i + CHUNK_SIZE - 1, n_sub)
        sub_int <- st_intersects(st_geometry(sub_parcels[idx, ]), sub_transit, sparse = TRUE)
        transit_hits[idx] <- lengths(sub_int) > 0
        gc()
      }
    }
  }
  
  # Data Table Transformations
  sub_dt <- as.data.table(st_drop_geometry(sub_parcels))
  sub_dt[, retains_hazard  := wetland_hits]
  sub_dt[, touches_transit := transit_hits]
  
  # Density & Legislative Modeling
  sub_dt <- merge(sub_dt, city_pop, by = "CITY_NM", all.x = TRUE)
  sub_dt[is.na(pop), pop := 0]
  
  sub_dt[, hb1110_tier := fifelse(pop >= 75000, "Tier 1",
                                  fifelse(pop >= 25000, "Tier 2",
                                          fifelse(in_ugb == TRUE & pop < 25000, "Tier 3", "Exempt")))]
  
  # 1. Base Density per Acre Rules
  sub_dt[, density_per_acre := DenMaxDensity]
  sub_dt[is.na(density_per_acre) & !is.na(DenMinLotSizeSqFt) & DenMinLotSizeSqFt > 0, 
         density_per_acre := 43560 / DenMinLotSizeSqFt]
  
  # Limit unit-per-lot division fallback strictly to urban parcels under 5 acres
  sub_dt[is.na(density_per_acre) & !is.na(DenMaxPrimaryUnitsPerLot) & calc_acres < 5, 
         density_per_acre := DenMaxPrimaryUnitsPerLot / pmax(calc_acres, 0.1)]
  sub_dt[is.na(density_per_acre), density_per_acre := 0]
  
  # 2. Baseline Max Units Calculation
  sub_dt[, baseline_max_units := floor(calc_acres * density_per_acre)]
  
  # FIX: Cap baseline capacity for rural/unincorporated lands to prevent huge acre multiplication
  sub_dt[in_ugb == FALSE | WAZAZoneGeneral %in% c("RUR", "AG", "FOR"), 
         baseline_max_units := pmin(baseline_max_units, 2L)]
  
  # Residential Classification (Excludes purely rural/agricultural flags from HB 1110 urban floor)
  sub_dt[, is_residential := (UseResidential == "P" | grepl("RESIDENTIAL", ZoneName, ignore.case = TRUE)) & 
           !WAZAZoneGeneral %in% c("RUR", "AG", "FOR")]
  
  # 3. FIX: Middle Housing Floor Applies STRICTLY within Urban Growth Boundaries
  sub_dt[, hb1110_unit_floor := 0L]
  sub_dt[is_residential == TRUE & in_ugb == TRUE & hb1110_tier == "Tier 1", 
         hb1110_unit_floor := fifelse(touches_transit == TRUE, 6L, 4L)]
  sub_dt[is_residential == TRUE & in_ugb == TRUE & hb1110_tier == "Tier 2", 
         hb1110_unit_floor := fifelse(touches_transit == TRUE, 4L, 2L)]
  sub_dt[is_residential == TRUE & in_ugb == TRUE & hb1110_tier == "Tier 3", 
         hb1110_unit_floor := 2L]
  
  # 4. State Law Capacity Floor vs. Baseline Comparison
  sub_dt[, state_law_max_units := baseline_max_units]
  sub_dt[is_residential == TRUE & in_ugb == TRUE, state_law_max_units := pmax(
    baseline_max_units,
    hb1110_unit_floor,
    baseline_max_units + 2L
  )]
  
  # Hazard / Non-UGB Zeroing Rule
  sub_dt[retains_hazard == TRUE | in_ugb == FALSE, `:=`(
    baseline_max_units  = pmin(baseline_max_units, 1L), # Cap non-UGB/hazard to 1 or 0 units
    state_law_max_units = pmin(state_law_max_units, 1L)
  )]
  
  # Attach back to sf object (Clamp negative values to 0)
  sub_parcels$state_max_units        <- pmax(sub_dt$state_law_max_units, 0)
  sub_parcels$baseline_max_units     <- pmax(sub_dt$baseline_max_units, 0)
  sub_parcels$retains_hazard         <- sub_dt$retains_hazard
  sub_parcels$touches_transit        <- sub_dt$touches_transit
  sub_parcels$state_max_units_capped <- pmin(sub_parcels$state_max_units, 1000)
  
  cat(sprintf("Saving processed UGA '%s' to disk (%d parcels)...\n", uga_name, n_sub))
  saveRDS(sub_parcels, out_file)
  
  rm(sub_parcels, sub_dt, uga_bbox, wetland_hits, transit_hits)
  gc()
}

# ------------------------------------------------------------------------------
# 4. Aggregation & Multi-Scale Map Generation
# ------------------------------------------------------------------------------
cat("\n=== LOADING COMPLETED UGA CACHE FILES FOR MAP PRODUCTION ===\n")
processed_files <- list.files(uga_cache_dir, pattern = "^uga_.*\\.rds$", full.names = TRUE)

if (length(processed_files) == 0) {
  stop("No processed UGA files found in cache directory.")
}

processed_list <- lapply(processed_files, readRDS)
mapped_sf      <- do.call(rbind, processed_list)

custom_viridis_fill <- scale_fill_viridis_c(
  option = "magma",
  name   = "Max Buildable\nUnits",
  trans  = "sqrt",
  limits = c(0, 1000),
  breaks = c(0, 10, 50, 250, 500, 1000),
  labels = c("0", "10", "50", "250", "500", "1,000+")
)

# ------------------------------------------------------------------------------
# MAP 1: SEATTLE ONLY
# ------------------------------------------------------------------------------
seattle_sf <- mapped_sf[mapped_sf$UGA_NM == "Seattle" & !is.na(mapped_sf$UGA_NM), ]

if (nrow(seattle_sf) > 0) {
  p_seattle <- ggplot(data = seattle_sf) +
    geom_sf(aes(fill = state_max_units_capped), color = NA) +
    custom_viridis_fill +
    labs(
      title    = "City of Seattle: Housing Mandate Capacity Analysis",
      subtitle = "Modeled Capacity under HB 1110 & HB 1337",
      caption  = "Source: WA Dept of Commerce WAZA Atlas"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14))
  
  cat("Rendering Map 1: Seattle Only...\n")
  print(p_seattle)
}

# ------------------------------------------------------------------------------
# MAP 2: GREATER SEATTLE METRO
# ------------------------------------------------------------------------------
metro_ugas <- c("Seattle", "Bellevue", "Redmond", "Kirkland", "Renton", "Kent", 
                "Shoreline", "Federal Way", "Auburn", "Edmonds", "Lynnwood")

metro_sf <- mapped_sf[mapped_sf$UGA_NM %in% metro_ugas, ]

if (nrow(metro_sf) > 0) {
  p_metro <- ggplot(data = metro_sf) +
    geom_sf(aes(fill = state_max_units_capped), color = NA) +
    custom_viridis_fill +
    labs(
      title    = "Greater Seattle Metro: Housing Mandate Capacity Analysis",
      subtitle = "Modeled Capacity across Central Puget Sound Jurisdictions",
      caption  = "Source: WA Dept of Commerce WAZA Atlas"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14))
  
  cat("Rendering Map 2: Greater Seattle Metro...\n")
  print(p_metro)
}

# ------------------------------------------------------------------------------
# MAP 3: STATEWIDE / AVAILABLE PORTFOLIO
# ------------------------------------------------------------------------------
p_statewide <- ggplot(data = mapped_sf) +
  geom_sf(aes(fill = state_max_units_capped), color = NA) +
  custom_viridis_fill +
  labs(
    title    = "Washington State Housing Legislation Capacity Map",
    subtitle = sprintf("Aggregated Capacity Analysis across %d Loaded Area(s)", length(processed_files)),
    caption  = "Source: WA Dept of Commerce WAZA Atlas"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

cat("Rendering Map 3: Statewide Portfolio (Available Areas)...\n")
print(p_statewide)