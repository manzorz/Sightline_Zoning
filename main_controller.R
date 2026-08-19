# =========================================================================
# CLARK COUNTY HOUSING CAPACITY PIPELINE: MASTER CONTROLLER (Data Path Fixed)
# =========================================================================
rm(list = ls())

GENERATE_GRAPHICS <- TRUE  
TARGET_CRS        <- 2927  

# Universal Forward-Slash User Profile Generation
USER_PROFILE <- chartr("\\", "/", Sys.getenv("USERPROFILE"))

BASE_DIR   <- file.path(USER_PROFILE, "Repos", "Sightline_Zoning")
SCRIPT_DIR <- file.path(BASE_DIR, "scripts")

# DATA PATH CORRECTION: Points directly to your true Downloads folder layout
DATA_DIR   <- file.path(USER_PROFILE, "Downloads", "Clark_County_GIS_Atlas") 

# Keeps your heavy .rds caches and report spreadsheets writing safely to Documents
OUTPUT_DIR <- file.path(USER_PROFILE, "Documents", "ClarkCountyZoning", "output_products")

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# -------------------------------------------------------------------------
# RUN PROCESSING MODULES SEQUENTIALLY (Connections fully secured)
# -------------------------------------------------------------------------
cat("Starting modular housing capacity execution pipeline...\n")

source(file.path(SCRIPT_DIR, "01_set_env.R"))
source(file.path(SCRIPT_DIR, "02_ingest_data.R"))
source(file.path(SCRIPT_DIR, "03_spatial_rules.R"))
source(file.path(SCRIPT_DIR, "04_environmental_engine.R"))
source(file.path(SCRIPT_DIR, "05_capacity_model.R"))
source(file.path(SCRIPT_DIR, "06a_vis_baselines.R"))
source(file.path(SCRIPT_DIR, "06b_vis_expansions.R"))
source(file.path(SCRIPT_DIR, "07_reporting_matrix.R"))

cat("Pipeline run completed successfully. All products saved cleanly to disk.\n")







