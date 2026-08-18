# =========================================================================
# CLARK COUNTY HOUSING CAPACITY PIPELINE: MASTER CONTROLLER
# =========================================================================

# Clear the workspace slate to free up system memory
rm(list = ls())

# Set global runtime execution parameter toggles
GENERATE_GRAPHICS <- FALSE  # Sightline graphic module controller switch
TARGET_CRS        <- 2927  # NAD83 / Washington South (ftUS)

# Define file pathway routing locations
BASE_DIR   <- "C:/Users/gmann/Documents/ClarkCountyZoning"
SCRIPT_DIR <- file.path(BASE_DIR, "scripts")
DATA_DIR   <- "C:/Users/gmann/Downloads/Clark_County_GIS_Atlas"
OUTPUT_DIR <- file.path(BASE_DIR, "output_products")

# Ensure your local file paths exist physically on your disk drive
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# -------------------------------------------------------------------------
# RUN PROCESSING SUB-MODULE MODULES IN SEQUENTIAL SEQUENCE
# -------------------------------------------------------------------------
cat("Starting modular housing capacity execution pipeline...\n")

# Stage 1: Load active R packages and assert baseline runtime requirements
source(file.path(SCRIPT_DIR, "01_set_env.R"))

# Stage 2: Heavy disk read operations and initial local cache snapshot check
source(file.path(SCRIPT_DIR, "02_ingest_data.R"))

# Stage 3: Resolve property overlapping metrics and attach text rules
source(file.path(SCRIPT_DIR, "03_spatial_rules.R"))

# Stage 4: Run vector mask overlaps to isolate footprint reductions
source(file.path(SCRIPT_DIR, "04_environmental_engine.R"))

# Stage 5: Execute 3D volumetric math, dynamic pricing, and database joins
source(file.path(SCRIPT_DIR, "05_capacity_model.R"))

# Stage 6: Render all categorical, gradient, and expansion maps
source(file.path(SCRIPT_DIR, "06a_vis_baselines.R"))
source(file.path(SCRIPT_DIR, "06b_vis_expansions.R"))

# Stage 7: Compile and write report matrix tables directly to CSV outputs
source(file.path(SCRIPT_DIR, "07_reporting_matrix.R"))

cat("Pipeline run completed successfully. All outputs saved to disk.\n")
