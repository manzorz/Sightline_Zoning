# =========================================================================
# SCRIPT 05: DYNAMIC BUILDING ENVELOPE VOLUMETRIC CAPACITY ENGINE
# =========================================================================
cat("Executing Stage 5: Computing zoning-specific story limits and unit sizes...\n")

if (!exists("DATA_DIR")) DATA_DIR <- file.path(chartr("\\", "/", Sys.getenv("USERPROFILE")), "Downloads", "Clark_County_GIS_Atlas")
if (!exists("OUTPUT_DIR")) OUTPUT_DIR <- file.path(chartr("\\", "/", Sys.getenv("USERPROFILE")), "Documents", "ClarkCountyZoning", "output_products")
if (!exists("TARGET_CRS")) TARGET_CRS <- 2927

rules_cache_file  <- file.path(OUTPUT_DIR, "rules_spatial_inputs.rds")
final_cache_file  <- file.path(OUTPUT_DIR, "processed_lots_capacity.rds")

# Check if final cache is already compiled
if (file.exists(final_cache_file)) {
  cat("Found fully consolidated capacity model file. Loading cache instantly...\n")
  lots_capacity_model <- readRDS(final_cache_file)
} else {
  cat("Running final economic indices and parcel capacity calculations...\n")
  
  # Ensure base parcels exist in workspace
  if (!exists("lots_with_rules")) lots_with_rules <- readRDS(rules_cache_file)
  
  # Ensure stage 4 strict exclusion checkpoint tables exist
  chk_wetland  <- file.path(OUTPUT_DIR, "checkpoint_stage4_wetlands.rds")
  chk_slope40  <- file.path(OUTPUT_DIR, "checkpoint_stage4_slopes40.rds")
  chk_lndslp   <- file.path(OUTPUT_DIR, "checkpoint_stage4_landslide.rds")
  chk_cemetery <- file.path(OUTPUT_DIR, "checkpoint_stage4_cemetery.rds")
  chk_total    <- file.path(OUTPUT_DIR, "checkpoint_stage4_total_exclusions.rds")
  
  lots_wetland_loss  <- if(file.exists(chk_wetland))  readRDS(chk_wetland)  else data.frame(prop_id = lots_with_rules$prop_id, Wetland_Acres = 0)
  lots_slope40_loss  <- if(file.exists(chk_slope40))  readRDS(chk_slope40)  else data.frame(prop_id = lots_with_rules$prop_id, Slope40_Acres = 0)
  lots_lndslp_loss   <- if(file.exists(chk_lndslp))   readRDS(chk_lndslp)   else data.frame(prop_id = lots_with_rules$prop_id, Landslide_Acres = 0)
  lots_cemetery_loss <- if(file.exists(chk_cemetery)) readRDS(chk_cemetery) else data.frame(prop_id = lots_with_rules$prop_id, Cemetery_Acres = 0)
  lots_total_loss    <- if(file.exists(chk_total))    readRDS(chk_total)    else data.frame(prop_id = lots_with_rules$prop_id, Hard_Excluded_Acres = 0)
  
  # -------------------------------------------------------------------------
  # SAFEGUARD: RECOVER DEMOGRAPHIC NHGIS DATA IF MISSING
  # -------------------------------------------------------------------------
  if (!exists("nhgis_indicators")) {
    cat("  -> Re-ingesting NHGIS Census indicators...\n")
    nhgis_folder <- file.path(chartr("\\", "/", Sys.getenv("USERPROFILE")), 
                              "Downloads/nhgis0015_shape/nhgis0015_shape/nhgis0015_shapefile_tl2024_us_tract_2024")
    nhgis_csv <- file.path(nhgis_folder, "nhgis0015_ds273_20245_tract.csv")
    
    if (file.exists(nhgis_csv)) {
      nhgis_raw <- read.csv(nhgis_csv, stringsAsFactors = FALSE)
      nhgis_indicators <- nhgis_raw %>%
        select(
          GISJOIN,
          Tract_Med_Inc_Total  = AVF7E001,
          Tract_Med_Inc_Rent   = AVF7E003,  
          Tract_Med_Home_Value = AVFVE001,  
          Owner_Married_Kids   = AVF3E005,  
          Rent_Married_Kids    = AVF3E018,  
          Owner_Single_Parents = AVF3E009,  
          Rent_Single_Parents  = AVF3E022,  
          Tract_Total_Units    = AVF3E001,
          Family_Multi_Unit    = AU5XE005,  
          Single_Multi_Unit    = AU5XE010,  
          Female_Multi_Unit    = AU5XE014,  
          NonFam_Multi_Unit    = AU5XE018   
        ) %>%
        mutate(
          Tract_Med_Inc_Total   = as.numeric(Tract_Med_Inc_Total),
          Tract_Med_Inc_Rent    = as.numeric(Tract_Med_Inc_Rent),
          Tract_Med_Home_Value  = as.numeric(Tract_Med_Home_Value),
          Family_Formation_Rate = ((Owner_Married_Kids + Rent_Married_Kids + Owner_Single_Parents + Rent_Single_Parents) / pmax(1, Tract_Total_Units)) * 100,
          Apartment_Absorption  = Family_Multi_Unit + Single_Multi_Unit + Female_Multi_Unit + NonFam_Multi_Unit
        ) %>%
        select(GISJOIN, Tract_Med_Inc_Total, Tract_Med_Inc_Rent, Tract_Med_Home_Value, Family_Formation_Rate, Apartment_Absorption)
    } else {
      nhgis_indicators <- data.frame(
        GISJOIN = character(), Tract_Med_Inc_Total = numeric(), Tract_Med_Inc_Rent = numeric(),
        Tract_Med_Home_Value = numeric(), Family_Formation_Rate = numeric(), Apartment_Absorption = numeric()
      )
    }
  }
  
  ASSUMED_AVG_STORY_HEIGHT <- 11  
  
  # -------------------------------------------------------------------------
  # CONSOLIDATED ZONING REGEX VECTOR PATTERNS
  # -------------------------------------------------------------------------
  res_high_density      <- "R-35|R-43|OR-43|HD-NS|Core|Village|Mixed use"
  res_medium_density    <- "R-12|R-15|R-16|R-18|R-22|R-30|AR-|MF-|MDR|MDR-16"
  res_low_density       <- "R1-|R-4|R-6|R-7.5|R-10|RLD-|LDR|LD-NS"
  res_rural            <- "Rural|AG-|FR-|Gorge"
  res_unlimited_density <- "Core|Village|Mixed use|MX|WMU|DC"
  
  lots_capacity_model <- lots_with_rules %>%
    left_join(lots_wetland_loss,  by = "prop_id") %>%
    left_join(lots_slope40_loss,  by = "prop_id") %>%
    left_join(lots_lndslp_loss,   by = "prop_id") %>%
    left_join(lots_cemetery_loss, by = "prop_id") %>%
    left_join(lots_total_loss,    by = "prop_id") %>%
    mutate(
      # Clean missing data joins for strict hard exclusions
      Wetland_Acres       = ifelse(is.na(Wetland_Acres), 0, Wetland_Acres),
      Slope40_Acres       = ifelse(is.na(Slope40_Acres), 0, Slope40_Acres),
      Landslide_Acres     = ifelse(is.na(Landslide_Acres), 0, Landslide_Acres),
      Cemetery_Acres      = ifelse(is.na(Cemetery_Acres), 0, Cemetery_Acres),
      Hard_Excluded_Acres = ifelse(is.na(Hard_Excluded_Acres), 0, Hard_Excluded_Acres),
      
      Lot_Acres           = as.numeric(Shape_Area) / 43560,
      Net_Lot_Acres       = pmax(0, Lot_Acres - Hard_Excluded_Acres),
      
      Setback_Reduction_Factor = case_when(
        Front_Setback_Ft >= 20 ~ 0.70, 
        Front_Setback_Ft == 15 ~ 0.75, 
        Front_Setback_Ft <= 10 ~ 0.85, 
        TRUE                   ~ 0.80
      ),
      Net_Footprint_SqFt = (Net_Lot_Acres * 43560) * Setback_Reduction_Factor,
      
      # Unit size assignments via consolidated patterns
      Assumed_Unit_Size = case_when(
        grepl(res_high_density, desc_, ignore.case = TRUE)   ~ 900,  
        grepl(res_medium_density, desc_, ignore.case = TRUE) ~ 1200, 
        grepl(res_low_density, desc_, ignore.case = TRUE)    ~ 2100, 
        grepl(res_rural, desc_, ignore.case = TRUE)          ~ 2400, 
        TRUE                                                 ~ 2100
      ),
      
      # Max vertical stories via consolidated patterns
      Max_Stories = case_when(
        grepl(res_high_density, desc_, ignore.case = TRUE)   ~ pmin(floor(Max_Height_Ft / ASSUMED_AVG_STORY_HEIGHT), 7),
        grepl(res_medium_density, desc_, ignore.case = TRUE) ~ pmin(floor(Max_Height_Ft / ASSUMED_AVG_STORY_HEIGHT), 4),
        grepl(res_low_density, desc_, ignore.case = TRUE)    ~ pmin(floor(Max_Height_Ft / ASSUMED_AVG_STORY_HEIGHT), 3),
        grepl(res_rural, desc_, ignore.case = TRUE)          ~ pmin(floor(Max_Height_Ft / ASSUMED_AVG_STORY_HEIGHT), 2),
        TRUE                                                 ~ pmin(floor(Max_Height_Ft / ASSUMED_AVG_STORY_HEIGHT), 3)
      ),
      
      Max_Lot_Coverage_Pct = case_when(
        grepl("R1-|R-6|R-7.5|R-10|RLD", desc_, ignore.case = TRUE) ~ 0.40, 
        grepl("R-|MF|OR", desc_, ignore.case = TRUE)                ~ 0.60, 
        TRUE                                                        ~ 0.50
      ),
      
      Max_Potential_Floor_Area = (Net_Footprint_SqFt * Max_Lot_Coverage_Pct) * Max_Stories,
      Physical_Unit_Capacity   = floor(Max_Potential_Floor_Area / Assumed_Unit_Size),
      
      # Regulatory density ceilings via consolidated patterns
      Regulatory_Density_Cap = case_when(
        grepl(res_unlimited_density, desc_, ignore.case = TRUE) ~ Inf, 
        TRUE                                                    ~ floor(Net_Lot_Acres * UnitsPerAc)
      ),
      
      MaxPossibleConstruction  = pmin(Physical_Unit_Capacity, Regulatory_Density_Cap),
      Net_Realizable_Homes     = pmax(0, MaxPossibleConstruction - Units),
      Is_Useless_Upzone        = ifelse(Regulatory_Density_Cap > Units & MaxPossibleConstruction <= Units, TRUE, FALSE),
      
      # Format key for NHGIS
      Census_Int               = as.numeric(CensusTrac),
      Census_Int               = ifelse(Census_Int == 0, NA, Census_Int),
      Tract_String             = sprintf("%06d", Census_Int * 100),
      GISJOIN_Key              = paste0("G5300110", Tract_String)
    ) %>%
    left_join(nhgis_indicators, by = c("GISJOIN_Key" = "GISJOIN")) %>%
    mutate(
      Tract_Med_Inc_Total   = ifelse(is.na(Tract_Med_Inc_Total), 85000, Tract_Med_Inc_Total),
      Tract_Med_Inc_Rent    = ifelse(is.na(Tract_Med_Inc_Rent), 55000, Tract_Med_Inc_Rent),
      Tract_Med_Home_Value  = ifelse(is.na(Tract_Med_Home_Value), 480000, Tract_Med_Home_Value),
      Family_Formation_Rate = ifelse(is.na(Family_Formation_Rate), 25, Family_Formation_Rate),
      Apartment_Absorption  = ifelse(is.na(Apartment_Absorption), 0, Apartment_Absorption)
    )
  
  cat("Saving consolidated analysis data to final rds cache file...\n")
  saveRDS(lots_capacity_model, file = final_cache_file)
}

cat("Stage 5 processing complete. Dynamic capacity model strictly limited to 4 hard hazard exclusions.\n")