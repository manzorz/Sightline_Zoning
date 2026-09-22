# =========================================================================
# SCRIPT 07: REPORTING MATRIX & POLICY LOSS AGGREGATION ENGINE
# =========================================================================

if (!exists("DATA_DIR")) DATA_DIR <- file.path(chartr("\\", "/", Sys.getenv("USERPROFILE")), "Downloads", "Clark_County_GIS_Atlas")
if (!exists("TARGET_CRS")) TARGET_CRS <- 2927

if (!exists("OUTPUT_DIR")) {
  user_root  <- ifelse(Sys.info()[["sysname"]] == "Windows", chartr("\\", "/", Sys.getenv("USERPROFILE")), Sys.getenv("HOME"))
  OUTPUT_DIR <- file.path(user_root, "Documents", "ClarkCountyZoning", "output_products")
}

cat("Executing Stage 7: Compiling final summary reports and policy loss matrices...\n")

# --- SAFEGUARD 1: RECOVER CAPACITY MODEL ---
final_cache_file <- file.path(OUTPUT_DIR, "processed_lots_capacity.rds")

if (!exists("lots_capacity_model")) {
  if (file.exists(final_cache_file)) {
    cat("  -> Loading lots_capacity_model from cache...\n")
    lots_capacity_model <- readRDS(final_cache_file)
  } else {
    stop("Error: processed_lots_capacity.rds missing. Please run Stage 5 first.")
  }
}

# Ensure spatial integrity
if (is.na(st_crs(lots_capacity_model))) st_crs(lots_capacity_model) <- TARGET_CRS

# --- DYNAMIC SAFEGUARD FOR ACRES / AREA COLUMN ---
if (!"Acres" %in% names(lots_capacity_model)) {
  acres_col_match <- names(lots_capacity_model)[grep("^acres$|^gis_acres$|^acreage$|^lot_acres$", names(lots_capacity_model), ignore.case = TRUE)][1]
  
  if (!is.na(acres_col_match)) {
    lots_capacity_model$Acres <- as.numeric(lots_capacity_model[[acres_col_match]])
  } else if ("SQUARE_FEET" %in% names(lots_capacity_model) || "sqft" %in% names(lots_capacity_model)) {
    sqft_col <- names(lots_capacity_model)[grep("square_feet|sqft", names(lots_capacity_model), ignore.case = TRUE)][1]
    lots_capacity_model$Acres <- as.numeric(lots_capacity_model[[sqft_col]]) / 43560
  } else {
    lots_capacity_model$Acres <- as.numeric(st_area(lots_capacity_model)) / 43560
  }
}

# --- SAFEGUARD 2: MUNICIPAL BOUNDARY SPATIAL OVERLAY ---
cat("Mapping lot coordinates to primary municipal boundaries...\n")

if (!"Jurisdiction" %in% names(lots_capacity_model)) {
  city_shp     <- file.path(DATA_DIR, "City.shp")
  city_alt_shp <- file.path(DATA_DIR, "Cities.shp")
  
  jurisdiction_vec <- rep("Unincorporated Clark County", nrow(lots_capacity_model))
  
  if (file.exists(city_shp) || file.exists(city_alt_shp)) {
    target_path <- if (file.exists(city_shp)) city_shp else city_alt_shp
    layer_name  <- sub("\\.shp$", "", basename(target_path), ignore.case = TRUE)
    
    city_sf <- st_read(dsn = DATA_DIR, layer = layer_name, quiet = TRUE)
    if (is.na(st_crs(city_sf))) {
      st_crs(city_sf) <- TARGET_CRS
    } else {
      city_sf <- st_transform(city_sf, TARGET_CRS)
    }
    
    name_col <- names(city_sf)[grep("CITY|NAME|JURIS", names(city_sf), ignore.case = TRUE)][1]
    
    if (!is.na(name_col) && length(name_col) > 0) {
      city_clean <- city_sf %>% 
        select(Jurisdiction = !!sym(name_col)) %>% 
        filter(!is.na(Jurisdiction))
      
      lots_pts <- st_point_on_surface(st_geometry(lots_capacity_model))
      spatial_matches <- st_intersects(lots_pts, city_clean)
      
      matched_indices <- sapply(spatial_matches, function(x) if (length(x) > 0) x[1] else NA)
      matched_names   <- city_clean$Jurisdiction[matched_indices]
      
      has_match <- !is.na(matched_names)
      jurisdiction_vec[has_match] <- as.character(matched_names[has_match])
    }
  }
  
  lots_capacity_model$Jurisdiction <- jurisdiction_vec
}

# --- DYNAMIC SAFEGUARD FOR CONSTRAINT COLUMNS ---
potential_flags <- c(
  "Intersects_Cemetery", "Intersects_Wetland", "Intersects_Slope", 
  "Intersects_Shoreline", "Intersects_Floodplain", "Intersects_Critical_Aquifer",
  "Has_Cemetery", "Has_Wetland", "Has_Slope", "Has_Shoreline"
)

existing_flags <- intersect(potential_flags, names(lots_capacity_model))

for (flag in setdiff(potential_flags, existing_flags)) {
  lots_capacity_model[[flag]] <- FALSE
}

# Ensure Units and Max_Allowed_Homes exist safely
if (!"Units" %in% names(lots_capacity_model)) lots_capacity_model$Units <- 0
if (!"Max_Allowed_Homes" %in% names(lots_capacity_model)) lots_capacity_model$Max_Allowed_Homes <- 0
if (!"Net_Realizable_Homes" %in% names(lots_capacity_model)) lots_capacity_model$Net_Realizable_Homes <- 0

# --- 1. JURISDICTION & ZONE CAPACITY SUMMARY MATRIX ---
cat("Compiling Summary Matrix 1: Capacity Headroom by Jurisdiction...\n")

summary_by_jurisdiction <- st_drop_geometry(lots_capacity_model) %>%
  group_by(Jurisdiction) %>%
  summarise(
    Total_Parcels       = n(),
    Gross_Acres         = round(sum(Acres, na.rm = TRUE), 2),
    Existing_Units      = sum(Units, na.rm = TRUE),
    Theoretical_Max     = sum(Max_Allowed_Homes, na.rm = TRUE),
    Net_Realizable_Headroom = sum(Net_Realizable_Homes, na.rm = TRUE),
    Parcels_With_Capacity   = sum(Net_Realizable_Homes > 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Net_Realizable_Headroom))

write.csv(
  summary_by_jurisdiction, 
  file.path(OUTPUT_DIR, "Summary_Capacity_By_Jurisdiction.csv"), 
  row.names = FALSE
)

# --- 2. POLICY LOSS & ENVIRONMENTAL MASKING MATRIX ---
cat("Compiling Summary Matrix 2: Environmental & Regulatory Capacity Loss Matrix...\n")

zoning_id_col <- names(lots_capacity_model)[grep("^zoning_id$|^zone_id$|^zoning$", names(lots_capacity_model), ignore.case = TRUE)][1]
if (is.na(zoning_id_col)) {
  lots_capacity_model$Zoning_ID <- "Unknown_Zone"
} else if (zoning_id_col != "Zoning_ID") {
  lots_capacity_model$Zoning_ID <- lots_capacity_model[[zoning_id_col]]
}

base_loss_columns <- c("Zoning_ID", "desc_", "Jurisdiction", "Acres", "Units", "Max_Allowed_Homes", "Net_Realizable_Homes")

policy_loss_matrix <- st_drop_geometry(lots_capacity_model) %>%
  select(any_of(c(base_loss_columns, potential_flags))) %>%
  group_by(Jurisdiction, Zoning_ID) %>%
  summarise(
    Total_Parcels         = n(),
    Total_Gross_Acres     = round(sum(Acres, na.rm = TRUE), 2),
    Raw_Theoretical_Units = sum(Max_Allowed_Homes, na.rm = TRUE),
    Net_Realizable_Units  = sum(Net_Realizable_Homes, na.rm = TRUE),
    Capacity_Lost_To_Policy_And_Env = sum(Max_Allowed_Homes - Net_Realizable_Homes, na.rm = TRUE),
    Parcels_Constrained_Cemetery  = sum(Intersects_Cemetery, na.rm = TRUE),
    Parcels_Constrained_Wetland   = sum(Intersects_Wetland, na.rm = TRUE),
    Parcels_Constrained_Slope     = sum(Intersects_Slope, na.rm = TRUE),
    Parcels_Constrained_Shoreline = sum(Intersects_Shoreline, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Percent_Capacity_Retained = ifelse(
      Raw_Theoretical_Units > 0, 
      round((Net_Realizable_Units / Raw_Theoretical_Units) * 100, 1), 
      0
    )
  ) %>%
  arrange(Jurisdiction, Zoning_ID)

write.csv(
  policy_loss_matrix, 
  file.path(OUTPUT_DIR, "Summary_Policy_Loss_Matrix.csv"), 
  row.names = FALSE
)

cat("Stage 7 Complete. Reports successfully saved to:\n  -> ", OUTPUT_DIR, "\n")