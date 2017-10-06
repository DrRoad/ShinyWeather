
# UI function
qualityTileUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("dta_quality"))
}

# Server function
qualityTile <- function(input, output, session, 
                        dta_sources, forecast_dta, variable) {
  output$dta_quality <- renderUI({
    plot_dta <- forecast_dta$get_quality_dta(dta_sources())
    # Take of the namespace
    ns <- session$ns
    output$dc <- renderHighchart({
      highcharter::highchart() %>% 
        highcharter::hc_xAxis(plot_dta$FORECAST_PERIOD) %>% 
        highcharter::hc_add_series(name = "weather.com",
                                   data = (plot_dta %>%
                                             dplyr::filter(SOURCE == "www.weather.com") %>% 
                                             dplyr::select(variable))[[1]]) %>%
        highcharter::hc_add_series(name = "wetter.com",
                                   data = (plot_dta %>%
                                             dplyr::filter(SOURCE == "www.wetter.com") %>% 
                                             dplyr::select(variable))[[1]]) %>%
        highcharter::hc_add_series(name = "wetter.de",
                                   data = (plot_dta %>%
                                             dplyr::filter(SOURCE == "www.wetter.de") %>% 
                                             dplyr::select(variable))[[1]]) %>%
        highcharter::hc_add_series(name = "wetterkontor.de",
                                   data = (plot_dta %>%
                                             dplyr::filter(SOURCE == "www.wetterkontor.de") %>% 
                                             dplyr::select(variable))[[1]]) %>%
        highcharter::hc_chart(type = "column") %>% 
        highcharter::hc_tooltip(valueDecimals = 1) %>% 
        highcharter::hc_add_theme(highcharter::hc_theme_smpl())
    })
    # Construct the actual output.
    tagList(
      highcharter::highchartOutput(ns("dc"))
    )
  })
}