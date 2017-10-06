
# UI function
tableTileUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("dta_table"))
}

# Server function
tableTile <- function(input, output, session, 
                      city, dta_source, number_of_days,
                      forecast_dta) {
  output$dta_table <- renderUI({
    table_dta <- forecast_dta$get_table_dta(city(), 
                                            dta_source(), 
                                            number_of_days())
    # Take of the namespace
    ns <- session$ns
    # Construct the data table output, which is to be shown.
    output$dt <- DT::renderDataTable({
      DT::datatable(table_dta, 
                    colnames = c("Date" = "FORECAST_DATE", 
                                 "°C" = "TEMPERATURE", 
                                 "Rain %" = "RAIN_PROB", 
                                 "Rain l/m2" = "RAIN_AMMOUNT", 
                                 "Wind km/h" = "WIND_SPEED"),
                    selection = "none",
                    style = "bootstrap",
                    rownames = FALSE,
                    options = list(paging = FALSE,
                                   ordering = FALSE,
                                   searching = FALSE,
                                   scrollY = FALSE,
                                   scrollX = FALSE,
                                   info = FALSE,
                                   rowCallback = JS("function(r,d) {
                                                    $(r).attr('height', '10px')}"),
                                   columnDefs = list(list(width = '60px', 
                                                          targets = c(1:4)),
                                                     list(className = 'dt-left',
                                                          targets = c(0:4)))))
    })
    # Construct the actual output.
    tagList(
      h4(city()),
      div(DT::dataTableOutput(ns("dt")), style = "font-size: 90%; width: 95%; 
                                                  overflow-x: hidden;
                                                  overflow-y: hidden;")
    )
  }) 
}