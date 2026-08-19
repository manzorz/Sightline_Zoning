# =========================================================================
# SCRIPT 07: SPATIAL AGGREGATION & REPORT REPORT DATA CSV OUTPUTS
# =========================================================================
cat("Executing Stage 7: Compiling final summary reports and policy loss matrices...\n")

# Assert variable directory pathways passed down from the master controller
if (!exists("OUTPUT_DIR")) {
  OUTPUT_DIR <- "~/Documents/ClarkCountyZoning/output_products"
}

# -------------------------------------------------------------------------
# STEP A: GEOGRAPHIC ACCOUNTABILITY OVERLAY (CITY BOUNDARY ASSIGNMENT)
# -------------------------------------------------------------------------
cat("Mapping lot coordinates to primary municipal boundaries...\n")
city_intersections <- st_intersection(
  lots_capacity_model, 
  city_outlines
)
city_intersections$city_intersect_area <- st_area(city_intersections)

# Relate each lot to its single highest percentage municipal overlay layout
lots_with_city <- city_intersections %>%
  group_by(prop_id) %>% 
  arrange(
    desc(city_intersect_area), 
    .by_group = TRUE
  ) %>% 
  slice(1) %>% 
  ungroup()

# Extract only the explicit constraint metrics from the parent data frame cache
constraint_lookup <- lots_capacity_model %>%
  st_drop_geometry() %>%
  select(
    prop_id, 
    Wetland_Acres, 
    Critical_Slope_Acres, 
    Intersects_Cemetery
  )

lots_with_city_fixed <- lots_with_city %>%
  left_join(
    constraint_lookup, 
    by = "prop_id"
  )

# --------------------------------=========================================
# STEP B: REPORT 1 - HOUSING CAPACITY VS NET CONSTRAINT MATRIX
# --------------------------------=========================================
cat("Compiling Report 1: Municipal Capacity vs Constraint Reductions...\n")

city_housing_report_matrix <- lots_with_city_fixed %>%
  st_drop_geometry() %>%
  mutate(
    Gross_Zoned_Capacity      = floor(Lot_Acres * UnitsPerAc),
    Units_Lost_To_Constraints = pmax(0, Gross_Zoned_Capacity - MaxPossibleConstruction)
  ) %>%
  group_by(City) %>%
  summarise(
    Total_Viable_New_Housing_Units              = sum(Net_Realizable_Homes, na.rm = TRUE),
    Total_Housing_Potential_Lost_To_Constraints = sum(Units_Lost_To_Constraints, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Viable_New_Housing_Units))

# Output Report 1 to the live console view
cat("\n--- REPORT 1: UNUSED HOUSING CAPACITY BY MUNICIPAL UGB ---\n")
print(city_housing_report_matrix)

write.csv(
  city_housing_report_matrix, 
  file = file.path(OUTPUT_DIR, "City_UGB_Unified_Housing_Capacity_Report.csv"), 
  row.names = FALSE
)

# --------------------------------=========================================
# STEP C: REPORT 2 - DISAGGREGATED LOSS ATTRIBUTION MATRIX
# --------------------------------=========================================
cat("Compiling Report 2: Constraint Attribution Disaggregation Breakdown...\n")

city_constraint_breakdown_matrix <- lots_with_city_fixed %>%
  st_drop_geometry() %>%
  mutate(
    Gross_Zoned_Capacity        = floor(Lot_Acres * UnitsPerAc),
    Units_Lost_To_Wetlands      = ifelse(Wetland_Acres > 0, floor(Wetland_Acres * UnitsPerAc), 0),
    Units_Lost_To_Severe_Slopes = ifelse(Critical_Slope_Acres > 0, floor(Critical_Slope_Acres * UnitsPerAc), 0),
    Units_Lost_To_Cemetaries    = ifelse(Intersects_Cemetery, Gross_Zoned_Capacity, 0)
  ) %>%
  group_by(City) %>%
  summarise(
    Lost_To_Vector_Wetlands     = sum(Units_Lost_To_Wetlands, na.rm = TRUE),
    Lost_To_Severe_Slopes_40Pct = sum(Units_Lost_To_Severe_Slopes, na.rm = TRUE),
    Lost_To_Cemetery_Dedication = sum(Units_Lost_To_Cemetaries, na.rm = TRUE)
  ) %>%
  arrange(desc(Lost_To_Vector_Wetlands + Lost_To_Severe_Slopes_40Pct))

# Output Report 2 to the live console view
cat("\n--- REPORT 2: LOSS ATTRIBUTION DISAGGREGATION MATRIX ---\n")
print(city_constraint_breakdown_matrix)

write.csv(
  city_constraint_breakdown_matrix, 
  file = file.path(OUTPUT_DIR, "City_UGB_Housing_Loss_Constraint_Attribution.csv"), 
  row.names = FALSE
)

cat("Stage 7 processing complete. CSV analysis products successfully saved to disk.\n")







