
# UI function
mapTileUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("dta_map"))
}

# Server function
mapTile <- function(input, output, session, 
                    city, dta_source, all_cities, forecast_dta) {
  output$dta_map <- renderUI({
    req(city(), dta_source())
    
    cities <- ifelse(all_cities(),
                     list(forecast_dta$get_all_cities()),
                     city())
    map_dta <- forecast_dta$get_map_dta(cities[[1]], dta_source())

    # Take of the namespace
    ns <- session$ns
    
    output$dm <- renderLeaflet({
      leaflet() %>%
        addTiles() %>% 
        addMarkers(lng = map_dta$LNG, 
                   lat = map_dta$LAT, 
                   label = map_dta$CITY,
                   popup = map_dta$LABEL) %>% 
        setView(lng = 9.5, lat = 51.317, zoom = 6)
    })
    # Construct the actual output.
    tagList(
      leaflet::leafletOutput(ns("dm"))
    )
  })
}