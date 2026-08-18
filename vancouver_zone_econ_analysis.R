library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(terra)
library(exactextractr)

# -------------------------------------------------------------------------
# 1. LOAD DATA & SET PROJECTION
# -------------------------------------------------------------------------
# Load your shapefiles or GeoPackage layers
lots   <- st_read("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/TaxlotsPublic.shp")
zoning <- st_read("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/Zoning.shp")

# Ensure both layers are explicitly using your feet-based projection (NAD83 / Washington South)
# EPSG 2927 is the standard code for NAD83(HARN) / Washington South (ftUS)
target_crs <- 2927 
lots   <- st_transform(lots, target_crs)
zoning <- st_transform(zoning, target_crs)

# Clean up any invalid geometries right away (equivalent to "Fix Geometries" in QGIS)
lots   <- st_make_valid(lots)
zoning <- st_make_valid(zoning)

# Create a unique ID and calculate MaxZoneUnits from physical acreage
zoning_cleaned <- zoning %>% 
  filter(!duplicated(st_geometry(.))) %>%
  mutate(
    Zoning_ID = 1:n(),
    # Calculate area in acres (st_area returns square feet for CRS 2927)
    Zone_Acres = as.numeric(st_area(geometry)) / 43560,
    # Calculate maximum possible units based on density and area
    MaxZoneUnits = round(Zone_Acres * UnitsPerAc)
  )


# -------------------------------------------------------------------------
# 3. SPATIAL JOIN BY LARGEST OVERLAP (One-to-One)
# -------------------------------------------------------------------------
# Intersect lots with zones to calculate spatial overlap pieces
intersections <- st_intersection(lots, zoning_cleaned)
intersections$intersect_area <- st_area(intersections)

# Allocate each lot strictly to its highest percentage overlap zone (or first if tied)
lots_joined <- intersections %>%
  group_by(prop_id) %>%                       # Replace with your unique Lot ID field name
  arrange(desc(intersect_area), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()


# -------------------------------------------------------------------------
# 3b. AGGREGATE LOTS & ASSESS CAPACITIES
# -------------------------------------------------------------------------
# Calculate the total sum of actual units per zone safely
lot_summary <- lots_joined %>%
  st_drop_geometry() %>%                     
  group_by(Zoning_ID) %>%                    
  summarise(Actual_Units = sum(Units, na.rm = TRUE)) # Replace with your units field name

# Merge the sums back into your spatial zoning layer
zoning_joined <- zoning_cleaned %>%
  left_join(lot_summary, by = "Zoning_ID") %>%
  mutate(
    Actual_Units = ifelse(is.na(Actual_Units), 0, Actual_Units),
    
    # Calculate Headroom using the new variable name
    Headroom = MaxZoneUnits - Actual_Units,
    
    # Create an explicit true/false flag where actual unit count is less than maximum
    Has_Development_Room = ifelse(Actual_Units < MaxZoneUnits, "Under Capacity (Has Room)", "At/Over Capacity")
  )


# -------------------------------------------------------------------------
# 3c. VISUALIZE INTRINSIC CAPACITY (Map Previews)
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# 3c. VISUALIZE INTRINSIC CAPACITY (Map Previews)
# -------------------------------------------------------------------------

# Modify classification text to align with regulatory chokepoint terminology
zoning_joined <- zoning_joined %>%
  mutate(
    Zoning_Status = ifelse(Actual_Units < MaxZoneUnits, "Under Zoned Limit (Has Room)", "At/Over Zoned Limit")
    
    zoning_dimensional_rules <- zoning_joined %>%
      mutate(
        # 1. Clean up or isolate the zone code from the description string
        Zone_Code = sub(".*\\((.*)\\).*", "\\1", desc_),
        
        # 2. Assign standard Clark County Title 40 dimensional limits
        Max_Height_Ft = case_when(
          grepl("R1-|R-6|R-7.5|R-10|RLD", Zone_Code) ~ 35,
          grepl("R-12|R-18|R-22", Zone_Code)        ~ 35,
          grepl("R-30|R-43", Zone_Code)             ~ 45,
          grepl("OR-15|OR-18|OR-22", Zone_Code)     ~ 45,
          grepl("OR-30|OR-43", Zone_Code)     ~ 60,
          grepl("IH|IL|ML|IR", Zone_Code)           ~ 100, # Industrial limits are much higher
          TRUE                                      ~ 35   # Baseline fallback
        ),
        
        Front_Setback_Ft = case_when(
          grepl("R1-20|R1-10", Zone_Code)           ~ 20,
          grepl("R1-5|R1-6|R1-7.5|R-6|R-7.5", Zone_Code) ~ 10, # 10ft for house, 20ft for garage
          grepl("R-12|R-18|R-22|R-30|R-43", Zone_Code)   ~ 10,
          grepl("RLD", Zone_Code)                   ~ 15,
          TRUE                                      ~ 15   # Baseline fallback
        )
      )
    
        
# -------------------------------------------------------------------------
# 3c. VISUALIZE INTRINSIC CAPACITY (Map Previews - Fixed)
# -------------------------------------------------------------------------
    
# --- MAP 1: Categorical Status (No Outlines) ---
ggplot(data = zoning_joined %>% 
         mutate(Zoning_Status = ifelse(Actual_Units < MaxZoneUnits, 
                                       "Under Zoned Limit (Has Room)", 
                                       "At/Over Zoned Limit"))) +
  geom_sf(aes(fill = Zoning_Status), color = NA) + 
  scale_fill_manual(
    values = c("Under Zoned Limit (Has Room)" = "#21918c", "At/Over Zoned Limit" = "#440154"),
    name = "Zoning Limitations"
  ) +
  labs(
    title = "Zoning Limitations & Growth Potential",
    subtitle = "Categorical status based on current Zoned Limits",
    caption = "Projection: NAD83 / Washington South (ftUS)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    legend.position = "bottom"
  )


# --- MAP 2: Headroom Gradient (No Outlines) ---
# -------------------------------------------------------------------------
# LOAD & PREPARE CITY BOUNDARIES LAYER
# -------------------------------------------------------------------------
# Load the Urban Growth Area/City boundary layer from your exact folder
ugabnd <- st_read("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/Ugabnd.shp") %>%
  st_transform(target_crs) %>%
  st_make_valid()

# Aggregate polygons by City name to get clear, single outlines for each city
city_outlines <- ugabnd %>%
  group_by(City) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup()

# Get coordinate centroids for city labels so they place nicely on the map
city_labels <- city_outlines %>%
  st_centroid()


# -------------------------------------------------------------------------
# --- MAP 2: Headroom Gradient with City Limit Outlines ---
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# LOAD & PREPARE CITY BOUNDARIES LAYER (Refined)
# -------------------------------------------------------------------------
# Load the Urban Growth Area/City boundary layer
ugabnd <- st_read("C:/Users/gmann/Downloads/Clark_County_GIS_Atlas/Ugabnd.shp") %>%
  st_transform(target_crs) %>%
  st_make_valid()

# Filter out unincorporated areas and keep only actual cities
city_outlines <- ugabnd %>%
  filter(!is.na(City), 
         City != "", 
         !grepl("Unincorporated|County", City, ignore.case = TRUE)) %>%
  group_by(City) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup()

# Extract the horizontal center from the centroid
city_centroids <- st_centroid(city_outlines)
label_x_coords <- st_coordinates(city_centroids)[, "X"]

# Extract the absolute highest northern point (ymax) for each city geometry
label_y_coords <- sapply(st_geometry(city_outlines), function(geom) {
  st_bbox(geom)[["ymax"]]
})

# Combine them into a fresh sf layer, shifting the Y coordinates up by 4,000 feet
city_labels <- city_outlines %>%
  st_drop_geometry() %>%
  mutate(
    X = label_x_coords,
    Y = label_y_coords + 4000  # Adjust this feet offset up or down if needed
  ) %>%
  st_as_sf(coords = c("X", "Y"), crs = target_crs)



# --- MAP 2: Headroom Gradient with Shifted Semi-Transparent Labels ---
zoning_gradient <- zoning_joined %>%
  mutate(
    Zoning_Status = ifelse(Actual_Units < MaxZoneUnits, "Under Zoned Limit (Has Room)", "At/Over Zoned Limit"),
    Headroom_Display = ifelse(Zoning_Status == "At/Over Zoned Limit", NA, Headroom)
  )

ggplot() +
  # 1. Base layer: Everything is drawn in light grey first
  geom_sf(data = zoning_gradient, fill = "#D3D3D3", color = NA) +
  
  # 2. Overlay layer: Dynamic zoning headroom values (Light Yellow to Red)
  geom_sf(data = filter(zoning_gradient, !is.na(Headroom_Display)), 
          aes(fill = Headroom_Display), color = NA) +
  
  # 3. City Outlines Layer: Thin, dark grey boundaries for actual cities
  geom_sf(data = city_outlines, fill = NA, color = "#4A4A4A", size = 0.5, linetype = "solid") +
  
  # 4. City Labels Layer: Shifted to the top edge and set to semi-transparent (alpha = 0.6)
  geom_sf_text(data = city_labels, aes(label = City), 
               color = "black", fontface = "bold", size = 3, alpha = 0.6, check_overlap = TRUE) +
  
  # Custom continuous gradient with your preferred name format
  scale_fill_gradient(
    low = "#FFFFE0",   
    high = "#FF0000",  
    name = "Potential\nnew homes\nunder current\nzoning limitation",
    na.value = "#D3D3D3"
  ) +
  labs(
    title = "Available Room Under Zoned Limits",
    subtitle = "Continuous gradient showing headroom before hitting regulatory ceilings",
    caption = "City borders outlined in dark grey. Areas at/over zoned limits shaded in light grey."
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    # Removes the X and Y coordinate axis titles entirely
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "right"
  )

# -------------------------------------------------------------------------
# 1. CONSOLIDATE ZONING DESCRIPTIONS (Binary Classification)
# -------------------------------------------------------------------------
# Define keywords that indicate residential permission
res_keywords <- "Residential|Resid|Single-family|Single Family|Multifamily|Multiple-family|Mobile Home|MHP|MDR|LDR|HDR|RLD|Mixed Use|Mixed-Use|WMU|Office Residential|Downtown|Town Center|Village"

zoning_binary <- zoning_joined %>%
  mutate(
    Is_Residential = ifelse(
      grepl(res_keywords, desc_, ignore.case = TRUE) & 
        !grepl("Airport/Residential", desc_, ignore.case = TRUE), # Exclude pure airparks if desired
      "Residential Zoning", 
      "Non-Residential / Commercial / Other"
    )
  )

# -------------------------------------------------------------------------
# 2. RENDER THE RESIDENTIAL VS NON-RESIDENTIAL MAP (Light Palette)
# -------------------------------------------------------------------------
ggplot() +
  # 1. Base Layer: Solid color filled zones with NO outlines (color = NA)
  geom_sf(data = zoning_binary, aes(fill = Is_Residential), color = NA) +
  
  # 2. City Outlines Layer: Thin borders for actual cities
  geom_sf(data = city_outlines, fill = NA, color = "#4A4A4A", size = 0.5, linetype = "solid") +
  
  # 3. City Labels Layer: Shifted above borders, perfectly legible over lighter background colors
  geom_sf_text(data = city_labels, aes(label = City), 
               color = "black", fontface = "bold", size = 3, alpha = 0.6, check_overlap = TRUE) +
  
  # High-visibility light categorical color palette
  scale_fill_manual(
    values = c(
      "Residential Zoning" = "#B19FF1",                 # Light Periwinkle
      "Non-Residential / Commercial / Other" = "#CCCCFF" # Light Violet
    ),
    name = "Zoning Type"
  ) +
  labs(
    title = "Residential vs Non-Residential Zoning Footprint",
    subtitle = "Consolidated view of regulatory frameworks allowing housing",
    caption = "City borders outlined in dark grey. Borders removed from individual zoning parcels."
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    legend.position = "bottom"
  )

# -------------------------------------------------------------------------
# EXPORT IMAGES TO SPECIFIED DIRECTORY
# -------------------------------------------------------------------------

# Define the target folder path
output_dir <- "C:\\Users\\gmann\\Documents\\ClarkCountyZoning"

# Create the folder automatically if it doesn't already exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# --- 1. Generate and Save Map 2 (The Gradient Map) ---
map_gradient <- ggplot() +
  geom_sf(data = zoning_gradient, fill = "#D3D3D3", color = NA) +
  geom_sf(data = filter(zoning_gradient, !is.na(Headroom_Display)), 
          aes(fill = Headroom_Display), color = NA) +
  geom_sf(data = city_outlines, fill = NA, color = "#4A4A4A", size = 0.5) +
  geom_sf_text(data = city_labels, aes(label = City), 
               color = "black", fontface = "bold", size = 3, alpha = 0.6, check_overlap = TRUE) +
  scale_fill_gradient(
    low = "#FFFFE0",   
    high = "#FF0000",  
    name = "Potential\nnew homes\nunder current\nzoning limitation",
    na.value = "#D3D3D3"
  ) +
  labs(
    title = "Available Room Under Zoned Limits",
    subtitle = "Continuous gradient showing headroom before hitting regulatory ceilings",
    caption = "City borders outlined in dark grey. Areas at/over zoned limits shaded in light grey."
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    legend.position = "right"
  )

# Save the Gradient Map
ggsave(
  filename = file.path(output_dir, "Zoning_Headroom_Gradient.png"),
  plot = map_gradient,
  width = 10,       # Width in inches
  height = 8,       # Height in inches
  dpi = 300,        # High print resolution (300 dots per inch)
  bg = "white"      # Prevents transparent background rendering issues
)


# --- 2. Generate and Save Map 3 (The Residential Footprint Map) ---
map_residential <- ggplot() +
  geom_sf(data = zoning_binary, aes(fill = Is_Residential), color = NA) +
  geom_sf(data = city_outlines, fill = NA, color = "#4A4A4A", size = 0.5) +
  geom_sf_text(data = city_labels, aes(label = City), 
               color = "black", fontface = "bold", size = 3, alpha = 0.6, check_overlap = TRUE) +
  scale_fill_manual(
    values = c(
      "Residential Zoning" = "#CCCCFF",                 
      "Non-Residential / Commercial / Other" = "#B19FF1" 
    ),
    name = "Zoning Type"
  ) +
  labs(
    title = "Residential vs Non-Residential Zoning Footprint",
    subtitle = "Consolidated view of regulatory frameworks allowing housing",
    caption = "City borders outlined in dark grey. Borders removed from individual zoning parcels."
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    legend.position = "bottom"
  )

# Save the Residential Map
ggsave(
  filename = file.path(output_dir, "Zoning_Residential_Footprint.png"),
  plot = map_residential,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)


# -------------------------------------------------------------------------
# 4. ENVIRONMENTAL ANALYSIS: RASTER WETLAND INTERSECTION
# -------------------------------------------------------------------------

# 1. Load the Raster Layer from inside the Geodatabase
# Replace with your actual path. If it's a standalone TIFF instead of a GDB layer,
# just use: raster_path <- "path/to/your/wetlands.tif"
gdb_path <- "C:/Users/gmann/Downloads/SEA_BIO_WetlandsInventory/Wetlands_Inventory.gdb"

# List layers if needed to find the exact raster name inside the GDB: 
# st_layers(gdb_path)
wetlands_raster <- rast(gdb_path, lyrs = "wetlands_inventory_2016")

# 2. Match Projections 
# The raster must match your feet-based target_crs (EPSG 2927) for precise area math
if (crs(wetlands_raster, proj = TRUE) != crs(zoning_binary, proj = TRUE)) {
  # Reproject raster using nearest neighbor to preserve the discrete 0-23 land codes
  wetlands_raster <- project(wetlands_raster, crs(zoning_binary), method = "near")
}

# 3. Filter down to your Residential footprint layer created in the last step
residential_zones <- zoning_binary %>% 
  filter(Is_Residential == "Residential Zoning")

# 4. Extract Raster Values per Zone
# exact_extract evaluates fractions of pixels crossing borders for extreme accuracy
cat("Extracting wetland pixels across residential zones...\n")
extracted_data <- exact_extract(wetlands_raster, residential_zones, include_cols = "Zoning_ID")

# 5. Bind data list and calculate area
# Under NOAA C-CAP, values 13-18 and 22-23 represent active wetlands
wetland_summary <- bind_rows(extracted_data) %>%
  mutate(
    Is_Wetland = value %in% c(13:18, 22, 23),
    # Account for partial pixels cut off by zone boundaries
    Wetland_Coverage_Weight = coverage_fraction * Is_Wetland
  ) %>%
  group_by(Zoning_ID) %>%
  summarise(
    # Count total effective pixels inside the zone
    Total_Zone_Pixels = sum(coverage_fraction),
    Total_Wetland_Pixels = sum(Wetland_Coverage_Weight),
    
    # Calculate percentages
    Pct_Wetland = (Total_Wetland_Pixels / Total_Zone_Pixels) * 100
  )

# 6. Join back to your spatial layer and convert pixel count to structural Acres
# Note: Determine your pixel cell size (e.g. 30x30m or 10x10ft) to compute raw acres.
# Assuming a standard 30-meter C-CAP cell reprojected, or using live polygon calculation:
zoning_wetland_impact <- residential_zones %>%
  left_join(wetland_summary, by = "Zoning_ID") %>%
  mutate(
    Pct_Wetland = ifelse(is.na(Pct_Wetland), 0, Pct_Wetland),
    # The clean acreage calculation: Multiply your calculated zone acres by the wetland ratio
    Wetland_Acres = Zone_Acres * (Pct_Wetland / 100),
    Net_Buildable_Acres = Zone_Acres - Wetland_Acres
  )

# -------------------------------------------------------------------------
# EXPOSE WETLAND CEILINGS
# -------------------------------------------------------------------------
# Quickly isolate any residential zone where wetlands eat up more than 50% of the land
high_risk_wetlands <- zoning_wetland_impact %>% 
  filter(Pct_Wetland > 50)

# !!! STOPPING POINT !!!
# Run the script up to this line to inspect your plot. 
# Once you review your environmental constraint layers, you can uncomment and edit Section 4 below.

# -------------------------------------------------------------------------
# 4. ENVIRONMENTAL DIFFERENCE & NET AREA (Pending Review)
# -------------------------------------------------------------------------
# steep_slope <- st_read("path_to_your_data/SteepSlopes.shp") %>% st_transform(target_crs) %>% st_make_valid()
# constraints <- st_union(wetlands, steep_slope)
# zoning_net  <- st_difference(zoning_cleaned, st_union(constraints))