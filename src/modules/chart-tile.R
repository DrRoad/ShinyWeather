
# UI function
chartTileUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("dta_chart"))
}

# Server function
chartTile <- function(input, output, session, 
                      city, dta_source, number_of_days, 
                      forecast_dta) {
  output$dta_chart <- renderUI({
    plot_dta <- forecast_dta$get_forecast_chart_dta(city(), 
                                                    dta_source(),
                                                    number_of_days())
    plot_dta$LOWER_RAIN_PROB[plot_dta$LOWER_RAIN_PROB < 0] <- 0
    plot_dta$UPPER_RAIN_PROB[plot_dta$UPPER_RAIN_PROB > 100] <- 100
    # Take of the namespace
    ns <- session$ns

    output$dc <- renderHighchart({
      highcharter::highchart() %>% 
        highcharter::hc_xAxis(categories = plot_dta$FORECAST_DATE) %>% 
        highcharter::hc_yAxis_multiples(list(
                                          labels = list(format = '{value}%',
                                                        style = list(color = "lightblue")),
                                          title = list(text = "Rain Probability",
                                                       style = list(color = "lightblue")),
                                          opposite = "true",
                                          min = 0, 
                                          max = 100 
                                        ),
                                        list(
                                          labels = list(format = '{value}°C',
                                                        style = list(color = "red")),
                                          title = list(text = "Temperature",
                                                       style = list(color = "red"))
                                        ),
                                        list(
                                          labels = list(format = '{value}km/h'),
                                          title = list(text = "Wind Speed"),
                                          opposite = "true" 
                                        )) %>% 
        highcharter::hc_add_series(id = "RainProb",
                                   name = "Rain Probability", 
                                   data = plot_dta$RAIN_PROB,
                                   type = "column",
                                   color = "lightblue",
                                   fillOpacity = 0.1,
                                   tooltip = list(valueSuffix = " %")) %>% 
        highcharter::hc_add_series(id = "Temp",
                                   name = "Temperature", 
                                   data = plot_dta$TEMPERATURE,
                                   yAxis = 1,
                                   type = "spline",
                                   color = "red",
                                   tooltip = list(valueSuffix = " °C")) %>%
        highcharter::hc_add_series(id = "WindSp",
                                   name = "Wind Speed", 
                                   data = plot_dta$WIND_SPEED,
                                   yAxis = 2,
                                   type = "spline",
                                   color = "black",
                                   dashStyle = "shortdot",
                                   tooltip = list(valueSuffix = " km/h")) %>% 
        highcharter::hc_add_series(id = "RainProbError",
                                   name = "Rain Probability Error",
                                   data = select(plot_dta, LOWER_RAIN_PROB,
                                                           UPPER_RAIN_PROB),
                                   type = "errorbar",
                                   hcaes(low = LOWER_RAIN_PROB,
                                         high = UPPER_RAIN_PROB),
                                   color = "blue",
                                   enableMouseTracking = FALSE) %>%
        highcharter::hc_add_series(id = "TempError",
                                   name = "Temperature Error",
                                   data = select(plot_dta, LOWER_TEMPERATURE,
                                                           UPPER_TEMPERATURE),
                                   yAxis = 1,
                                   type = "areasplinerange",
                                   hcaes(low = LOWER_TEMPERATURE,
                                         high = UPPER_TEMPERATURE),
                                   lineWidth = 0,
                                   color = "#FF0000",
                                   fillOpacity = 0.1,
                                   showInLegend = FALSE,
                                   enableMouseTracking = FALSE) %>%
        highcharter::hc_add_series(id = "WindSpError",
                                   name = "Wind Speed Error",
                                   data = select(plot_dta, LOWER_WIND_SPEED,
                                                           UPPER_WIND_SPEED),
                                   yAxis = 2,
                                   type = "areasplinerange",
                                   hcaes(low = LOWER_WIND_SPEED,
                                         high = UPPER_WIND_SPEED),
                                   lineWidth = 0,
                                   color = "black",
                                   fillOpacity = 0.1,
                                   showInLegend = FALSE,
                                   enableMouseTracking = FALSE) %>%
        hc_legend(layout = "vertical",
                  align = "left",
                  x = 80,
                  verticalAlign = "top",
                  y = 15,
                  floating = TRUE) %>% 
        hc_tooltip(shared = TRUE)
        # highcharter::hc_plotOptions(series = list(
        #                               events = list(
        #                                 show = JS('function () {
        #                                              var i = chart.series.length - 1;
        #                                              while (i--) {  
        #                                                var sid = chart.series[i].options.id.concat("Error");
        #                                                var series = chart.get(sid);
        #                                                if (series) {
        #                                                  series.show();
        #                                                }
        #                                              }
        #                                            }'),
        #                                 hide = JS('function () {
        #                                              var i = chart.series.length - 1;
        #                                              while (i--) {  
        #                                                var sid = chart.series[i].options.id.concat("Error");
        #                                                var series = chart.get(sid);
        #                                                if (series) {
        #                                                  series.show();
        #                                                }
        #                                              }
        #                                            }'),
        #                               ledgendItemClick = JS("function() {
        #                                                        
        #                                                          return false;
        #                                                        
        #                                                      }")
        #                             )))
    })
    # Construct the actual output.
    tagList(
      h4(city()),
      div(
        highcharter::highchartOutput(ns("dc")), 
        style = "height: 90%"
      )
    )
  })
}