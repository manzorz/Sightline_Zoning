# =========================================================================
# SCRIPT 03: LARGEST-OVERLAP REGULATORY ALLOCATION & SETBACK DEFINITIONS
# =========================================================================
cat("Executing Stage 3: Computing parcel-to-zone mappings and structural setbacks...\n")

# Assert local workspace cache targets established in prior steps
if (!exists("OUTPUT_DIR")) {
  user_root <- ifelse(Sys.info()[["sysname"]] == "Windows", chartr("\\", "/", Sys.getenv("USERPROFILE")), Sys.getenv("HOME"))
  if (Sys.info()[["sysname"]] == "Windows") {
    OUTPUT_DIR <- file.path(user_root, "Documents", "ClarkCountyZoning", "output_products")
  } else {
    OUTPUT_DIR <- file.path(user_root, "Documents", "ClarkCountyZoning", "output_products")
  }
}
if (!exists("TARGET_CRS")) TARGET_CRS <- 2927

base_cache_file  <- file.path(OUTPUT_DIR, "base_spatial_inputs.rds")
rules_cache_file <- file.path(OUTPUT_DIR, "rules_spatial_inputs.rds")

# -------------------------------------------------------------------------
# STEP A: SPATIAL ALLOCATION MATRIX CALCULATION (LARGEST OVERLAP JOINS)
# -------------------------------------------------------------------------
if (file.exists(rules_cache_file)) {
  cat("Found local parcel regulatory rules cache. Loading dataset instantly...\n")
  lots_with_rules <- readRDS(rules_cache_file)
} else {
  cat("No rules cache discovered. Initiating spatial overlap calculations...\n")
  
  # Ensure baseline datasets loaded from Stage 2 are active in memory
  if (!exists("lots") || !exists("zoning_cleaned")) {
    base_inputs     <- readRDS(base_cache_file)
    lots            <- base_inputs$lots
    zoning_cleaned  <- base_inputs$zoning_cleaned
    ugabnd          <- base_inputs$ugabnd
  }
  
  cat("Running geometric intersections (this may take a few moments for dense counties)...\n")
  intersections <- st_intersection(
    lots, 
    zoning_cleaned
  )
  intersections$intersect_area <- st_area(intersections)
  
  # Allocate each unique property strictly to its single highest overlapping zoning tier
  lots_joined <- intersections %>%
    group_by(prop_id) %>%                       
    arrange(
      desc(intersect_area), 
      .by_group = TRUE
    ) %>%
    slice(1) %>%
    ungroup()
  
  # -------------------------------------------------------------------------
  # STEP B: ASSIGN BUILDING HEIGHTS AND DIMENSIONAL YARD SETBACKS
  # -------------------------------------------------------------------------
  cat("Parsing zoning text strings to attach local Title 40 design criteria...\n")
  lots_with_rules <- lots_joined %>%
    mutate(
      Zone_Code = sub(".*/((.*)/).*", "/1", desc_),
      
      # Determine absolute maximum height restrictions
      Max_Height_Ft = case_when(
        grepl("R1-|R-6|R-7.5|R-10|RLD", Zone_Code) ~ 35,
        grepl("R-12|R-18|R-22", Zone_Code)        ~ 35,
        grepl("R-30|R-43", Zone_Code)             ~ 45,
        grepl("OR-15|OR-18|OR-22", Zone_Code)     ~ 45,
        grepl("OR-30|OR-43", Zone_Code)           ~ 60,
        grepl("IH|IL|ML|IR", Zone_Code)           ~ 100, 
        TRUE                                      ~ 35   
      ),
      
      # Determine required structural front setbacks
      Front_Setback_Ft = case_when(
        grepl("R1-20|R1-10", Zone_Code)                ~ 20,
        grepl("R1-5|R1-6|R1-7.5|R-6|R-7.5", Zone_Code) ~ 10, 
        grepl("R-12|R-18|R-22|R-30|R-43", Zone_Code)    ~ 10,
        grepl("RLD", Zone_Code)                        ~ 15,
        TRUE                                           ~ 15   
      )
    )
  
  cat("Caching processed regulatory lots dataset to speed up future runs...\n")
  saveRDS(
    lots_with_rules, 
    file = rules_cache_file
  )
}

cat("Stage 3 completed successfully. Regulatory parameters locked to property frames.\n")







