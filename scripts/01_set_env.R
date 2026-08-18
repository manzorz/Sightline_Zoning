# =========================================================================
# SCRIPT 01: ENVIRONMENT SETUP & DEPENDENCY ASSERTION
# =========================================================================
cat("Executing Stage 1: Asserting core libraries and environment parameters...\n")

# 1. Define required core packages for the spatial and analytical pipeline
required_packages <- c("sf", "dplyr", "ggplot2", "viridis", "terra", "exactextractr")

# 2. Programmatically verify, install, and load dependencies seamlessly
missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]

if (length(missing_packages) > 0) {
  cat("Missing package dependencies detected. Installing required modules:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages, repos = "https://r-project.org")
}

# Silent load execution loops to prevent console pollution during initialization
invisible(lapply(required_packages, library, character.only = TRUE))

# 3. Configure environmental system and garbage collection boundaries
# Forces R to be aggressive with memory cleanup during heavy geometric processing steps
invisible(gc(verbose = FALSE))

# Suppress repetitive GDAL/GEOS geometry constant attribute warnings across spatial joins
options(sf_max_print = 10, warn = -1)

# Double-check global controller parameter routing variables passed down from main_controller.R
if (!exists("TARGET_CRS")) TARGET_CRS <- 2927
if (!exists("OUTPUT_DIR")) OUTPUT_DIR <- "C:/Users/gmann/Documents/ClarkCountyZoning/output_products"

# Define shared typography constant variables for unified Sightline styling outputs
SIGHTLINE_FONT       <- "sans"
COLOR_UNINCORPORATED <- "#F2F2F2" 
COLOR_ZERO_CAPACITY  <- "#D3D3D3" 

cat("Stage 1 environment successfully initialized. All spatial extensions loaded.\n")
