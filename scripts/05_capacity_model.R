# =========================================================================
# SCRIPT 05: DYNAMIC BUILDING ENVELOPE VOLUMETRIC CAPACITY ENGINE
# =========================================================================
cat("Executing Stage 5: Computing zoning-specific story limits, unit sizes, and net additions...\n")

# Assert variable directory pathways passed down from the master controller
if (!exists("OUTPUT_DIR")) OUTPUT_DIR <- "C:/Users/gmann/Documents/ClarkCountyZoning/output_products"

final_cache_file  <- file.path(OUTPUT_DIR, "processed_lots_capacity.rds")

# Check if the cache is already fully compiled on your disk drive
if (file.exists(final_cache_file)) {
  cat("Found fully consolidated capacity model file. Loading cache instantly...\n")
  lots_capacity_model <- readRDS(final_cache_file)
} else {
  cat("Running final economic indices and parcel capacity calculations...\n")
  
  ASSUMED_AVG_STORY_HEIGHT <- 11  
  
  lots_capacity_model <- lots_with_rules %>%
    left_join(lots_wetland_loss, by = "prop_id") %>%
    left_join(lots_slope_loss, by = "prop_id") %>%
    left_join(lots_total_loss, by = "prop_id") %>%
    mutate(
      # Clean up missing data joins from empty overlap records
      Wetland_Acres        = ifelse(is.na(Wetland_Acres), 0, Wetland_Acres),
      Critical_Slope_Acres = ifelse(is.na(Critical_Slope_Acres), 0, Critical_Slope_Acres),
      Hard_Excluded_Acres  = ifelse(is.na(Hard_Excluded_Acres), 0, Hard_Excluded_Acres),
      
      Lot_Acres            = as.numeric(Shape_Area) / 43560,
      
      # 1. Deduct spatial footprint acreage from parent shapes (Preserves 15-40% slopes)
      Net_Lot_Acres         = pmax(0, Lot_Acres - Hard_Excluded_Acres),
      
      # 2. Mitigable limit intersections (Preserves acreage, injects cost multipliers)
      Has_Slope_15_25       = as.logical(st_intersects(geometry, slope_15_25_mask)),
      Has_Slope_25_40       = as.logical(st_intersects(geometry, slope_25_40_mask)),
      Has_Erosion_Risk      = as.logical(st_intersects(geometry, st_union(st_geometry(erosion_df)))),
      Has_Liquefaction_Risk = as.logical(st_intersects(geometry, st_union(st_geometry(liq_df)))),
      Has_Aquifer_Protected = as.logical(st_intersects(geometry, st_union(st_geometry(aquifer_df)))),
      Has_WUI_Proposed      = as.logical(st_intersects(geometry, st_union(st_geometry(wui_proposed_df)))),
      
      Has_Slope_15_25       = ifelse(is.na(Has_Slope_15_25), FALSE, Has_Slope_15_25),
      Has_Slope_25_40       = ifelse(is.na(Has_Slope_25_40), FALSE, Has_Slope_25_40),
      Has_Erosion_Risk      = ifelse(is.na(Has_Erosion_Risk), FALSE, Has_Erosion_Risk),
      Has_Liquefaction_Risk = ifelse(is.na(Has_Liquefaction_Risk), FALSE, Has_Liquefaction_Risk),
      Has_Aquifer_Protected = ifelse(is.na(Has_Aquifer_Protected), FALSE, Has_Aquifer_Protected),
      Has_WUI_Proposed      = ifelse(is.na(Has_WUI_Proposed), FALSE, Has_WUI_Proposed),
      
      # Structural engineering premium cost placeholders ($ per square foot built)
      Added_Cost_Per_SqFt   = 0 + ifelse(Has_Slope_15_25, 15, 0) + ifelse(Has_Slope_25_40, 35, 0) + 
        ifelse(Has_Erosion_Risk, 12, 0) + ifelse(Has_Liquefaction_Risk, 25, 0) + 
        ifelse(Has_Aquifer_Protected, 8, 0) + ifelse(Has_WUI_Proposed, 15, 0),
      
      Setback_Reduction_Factor = case_when(Front_Setback_Ft >= 20 ~ 0.70, Front_Setback_Ft == 15 ~ 0.75, Front_Setback_Ft <= 10 ~ 0.85, TRUE ~ 0.80),
      Net_Footprint_SqFt       = (Net_Lot_Acres * 43560) * Setback_Reduction_Factor,
      
      # Refinement 1: Assign Average Unit Sizes based on local legal zoning bands from .dbf text
      Assumed_Unit_Size = case_when(
        grepl("R-35|R-43|OR-43|HD-NS|Core|Village|Mixed use", desc_, ignore.case = TRUE)    ~ 900,  # Urban core flats
        grepl("R-12|R-15|R-16|R-18|R-22|R-30|AR-|MF-|MDR|MDR-16", desc_, ignore.case = TRUE) ~ 1200, # Garden apartments/townhomes
        grepl("R1-|R-4|R-6|R-7.5|R-10|RLD-|LDR|LD-NS", desc_, ignore.case = TRUE)            ~ 2100, # Suburban single-family
        grepl("Rural|AG-|FR-|Gorge", desc_, ignore.case = TRUE)                               ~ 2400, # Rural residential tracts
        TRUE                                                                                 ~ 2100
      ),
      
      # Refinement 2: Calculate maximum allowed vertical stories varying with zoning districts
      Max_Stories = case_when(
        grepl("R-35|R-43|OR-43|HD-NS|Core|Village|Mixed use", desc_, ignore.case = TRUE)    ~ pmin(floor(Max_Height_Ft / ASSUMED_AVG_STORY_HEIGHT), 7),
        grepl("R-12|R-15|R-16|R-18|R-22|R-30|AR-|MF-|MDR|MDR-16", desc_, ignore.case = TRUE) ~ pmin(floor(Max_Height_Ft / ASSUMED_AVG_STORY_HEIGHT), 4),
        grepl("R1-|R-4|R-6|R-7.5|R-10|RLD-|LDR|LD-NS", desc_, ignore.case = TRUE)            ~ pmin(floor(Max_Height_Ft / ASSUMED_AVG_STORY_HEIGHT), 3),
        grepl("Rural|AG-|FR-|Gorge", desc_, ignore.case = TRUE)                               ~ pmin(floor(Max_Height_Ft / ASSUMED_AVG_STORY_HEIGHT), 2),
        TRUE                                                                                 ~ pmin(floor(Max_Height_Ft / ASSUMED_AVG_STORY_HEIGHT), 3)
      ),
      
      Max_Lot_Coverage_Pct = case_when(grepl("R1-|R-6|R-7.5|R-10|RLD", desc_) ~ 0.40, grepl("R-|MF|OR", desc_) ~ 0.60, TRUE ~ 0.50),
      
      Max_Potential_Floor_Area = (Net_Footprint_SqFt * Max_Lot_Coverage_Pct) * Max_Stories,
      Physical_Unit_Capacity   = floor(Max_Potential_Floor_Area / Assumed_Unit_Size),
      
      # Refinement 3: Waive regulatory density ceilings entirely for mixed-use commercial overlays per regional laws
      Regulatory_Density_Cap = case_when(
        grepl("Core|Village|Mixed use|MX|WMU|DC", desc_, ignore.case = TRUE) ~ Inf, 
        TRUE                                                                 ~ floor(Net_Lot_Acres * UnitsPerAc)
      ),
      
      # Chokepoint selector evaluation picking whichever constraint sets the baseline ceiling first
      MaxPossibleConstruction  = pmin(Physical_Unit_Capacity, Regulatory_Density_Cap),
      Net_Realizable_Homes     = pmax(0, MaxPossibleConstruction - Units),
      Is_Useless_Upzone        = ifelse(Regulatory_Density_Cap > Units & MaxPossibleConstruction <= Units, TRUE, FALSE),
      
      # Format vector string keys to link directly to NHGIS datasets
      Census_Int               = as.numeric(CensusTrac),
      Census_Int               = ifelse(Census_Int == 0, NA, Census_Int),
      Tract_String             = sprintf("%06d", Census_Int * 100),
      GISJOIN_Key              = paste0("G5300110", Tract_String)
    ) %>%
    # Connect demographics directly to the active dataset records without spatial cost
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

cat("Stage 5 processing complete. Capacity headroom and economic layers securely indexed.\n")
