# ==============================================================================
# Interactive Leaflet & Shiny Viewer for Statewide Housing Capacity Model
# ==============================================================================

library(shiny)
library(leaflet)
library(sf)
library(data.table)

# ------------------------------------------------------------------------------
# 1. Load & Prepare Data
# ------------------------------------------------------------------------------
# Point to your masked non-zero active parcels GeoPackage
# Use a relative path so it works universally on GitHub and deployment servers
gpkg_path <- "data/parcels_fully_masked_nonzero.gpkg"

message("Loading active buildable parcels for interactive map...")
parcels_sf <- st_read(gpkg_path, layer = "active_buildable_parcels", quiet = TRUE)

# Leaflet requires standard WGS84 (EPSG:4327) geographic coordinates
if (st_crs(parcels_sf)$epsg != 4327) {
  parcels_sf <- st_transform(parcels_sf, crs = 4327)
}

# Convert to data.table for lightning-fast attribute handling if needed
dt <- as.data.table(st_drop_geometry(parcels_sf))

# Define a clean color palette for story bins or net additions
pal <- colorFactor(
  palette = c("#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#3f007d"),
  levels  = c("2 Stories", "3-4 Stories", "5-6 Stories", "6-9 Stories", "10-15 Stories", "20+ Stories"),
  na.color = "#cccccc"
)

# ------------------------------------------------------------------------------
# 2. Shiny UI Layout
# ------------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Washington State Housing Capacity & TOD Impact Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Interactive Filtering"),
      p("Click any parcel on the map to inspect its metadata, zoning tier, and policy breakdown."),
      hr(),
      
      # Optional County Filter if available in attributes
      if ("COUNTY_NM" %in% names(parcels_sf)) {
        selectInput("county_filter", "Select County:",
                    choices = c("All Counties", sort(unique(parcels_sf$COUNTY_NM))),
                    selected = "All Counties")
      } else {
        NULL
      },
      
      # Story Bin Filter
      checkboxGroupInput("story_filter", "Display Story Bins:",
                         choices = levels(parcels_sf$story_bin),
                         selected = levels(parcels_sf$story_bin)),
      
      hr(),
      helpText("Source: WA State Parcel GIS & Legislative Policy Model")
    ),
    
    mainPanel(
      width = 9,
      leafletOutput("map", height = "800px")
    )
  )
)

# ------------------------------------------------------------------------------
# 3. Shiny Server Logic
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Reactive filtering based on sidebar controls
  filtered_data <- reactive({
    df <- parcels_sf
    
    if (!is.null(input$county_filter) && input$county_filter != "All Counties") {
      df <- df[df$COUNTY_NM == input$county_filter, ]
    }
    
    if (!is.null(input$story_filter)) {
      df <- df[df$story_bin %in% input$story_filter, ]
    }
    
    return(df)
  })
  
  # Render Base Leaflet Map with OSM Basemap
  output$map <- renderLeaflet({
    leaflet(parcels_sf) %>%
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
               attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors') %>%
      setView(lng = -122.3321, lat = 47.6062, zoom = 11) # Centered on Seattle region
  })
  
  # Update map polygons dynamically when filters change
  observe({
    df <- filtered_data()
    
    # Construct clean HTML popup content safely handling field variations
    popup_content <- paste0(
      "<div style='font-family: sans-serif; font-size: 12px; line-height: 1.4;'>",
      "<b style='font-size: 14px; color: #3f007d;'>Parcel Details</b><br>",
      "<hr style='margin: 4px 0;'>",
      "<b>Address / Situs:</b> ", ifelse("SITUS_ADDR" %in% names(df), df$SITUS_ADDR, "N/A"), "<br>",
      "<b>City:</b> ", ifelse("SITUS_CITY_NM" %in% names(df), df$SITUS_CITY_NM, "Unincorporated"), "<br>",
      "<b>County:</b> ", ifelse("COUNTY_NM" %in% names(df), df$COUNTY_NM, "N/A"), "<br>",
      "<hr style='margin: 4px 0;'>",
      "<b>Land Use Code (LU):</b> ", ifelse("lu_code" %in% names(df), df$lu_code, "N/A"), "<br>",
      "<b>Current FAR:</b> ", round(df$current_far, 2), "<br>",
      "<b>Baseline Units:</b> ", df$total_baseline_units, "<br>",
      "<hr style='margin: 4px 0;'>",
      "<b style='color: #2b5c8f;'>Growth Potential (Net Units):</b><br>",
      "&bull; HB 1337 (ADUs): +", df$net_1337_add, "<br>",
      "&bull; E2SSB 5466 (TOD): +", df$net_tod_add, "<br>",
      "&bull; HB 1110 (Middle Housing): +", df$net_1110_add, "<br>",
      "<b>Total Net Capacity: +</b>", <b>df$max_policy_net_add,</b> "<br>",
      "<b>Story Tier:</b> ", as.character(df$story_bin),
      "</div>"
    )
    
    leafletProxy("map", data = df) %>%
      clearShapes() %>%
      addPolygons(
        fillColor   = ~pal(story_bin),
        fillOpacity = 0.75,
        color       = "#ffffff",
        weight      = 0.5,
        popup       = popup_content,
        highlightOptions = highlightOptions(
          weight       = 2,
          color        = "#000000",
          fillOpacity  = 0.9,
          bringToFront = TRUE
        )
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)