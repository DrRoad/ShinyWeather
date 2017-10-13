
# UI function
qualityTileUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("dta_quality"))
}

# Server function
qualityTile <- function(input, output, session, forecast_dta, variable) {
  output$dta_quality <- renderUI({
    plot_dta <- forecast_dta$get_quality_dta(list("www.weather.com",
                                                  "www.wetter.com",
                                                  "www.wetter.de",
                                                  "www.wetterkontor.de"))
    # Take of the namespace
    ns <- session$ns
    output$dc <- renderHighchart({
      highcharter::highchart() %>% 
        highcharter::hc_xAxis(plot_dta$FORECAST_PERIOD) %>% 
        highcharter::hc_add_series(name = "Provider A",
                                   data = (plot_dta %>%
                                             dplyr::filter(SOURCE == "www.weather.com") %>% 
                                             dplyr::select(variable))[[1]]) %>%
        highcharter::hc_add_series(name = "Provider B",
                                   data = (plot_dta %>%
                                             dplyr::filter(SOURCE == "www.wetter.com") %>% 
                                             dplyr::select(variable))[[1]]) %>%
        highcharter::hc_add_series(name = "Provider C",
                                   data = (plot_dta %>%
                                             dplyr::filter(SOURCE == "www.wetter.de") %>% 
                                             dplyr::select(variable))[[1]]) %>%
        highcharter::hc_add_series(name = "Provider D",
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