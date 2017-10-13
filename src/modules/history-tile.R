
# UI function
historyTileUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("dta_history"))
}

# Server function
historyTile <- function(input, output, session, 
                        city, dte, dta_sources, 
                        forecast_dta, variable) {
  output$dta_history <- renderUI({
    plot_dta <- forecast_dta$get_history_dta(city(), dte(), dta_sources())
    # Take of the namespace
    ns <- session$ns
    output$hc <- renderHighchart({
      highcharter::highchart() %>% 
        highcharter::hc_xAxis(categories = plot_dta$SAMPLE_DATE) %>% 
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
        highcharter::hc_add_theme(highcharter::hc_theme_smpl())
    })
    # Construct the actual output.
    tagList(
      highcharter::highchartOutput(ns("hc"))
    )
  })
}