
# UI function
dataTileUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("dta_table"))
}

# Server function
dataTile <- function(input, output, session, forecast_dta) {
  output$dta_table <- renderUI({
    table_dta <- forecast_dta$get_all_dta()
    # Take of the namespace
    ns <- session$ns
    # Construct the data table output, which is to be shown.
    output$dt <- DT::renderDataTable({
      DT::datatable(table_dta, 
                    selection = "none",
                    style = "bootstrap",
                    rownames = FALSE,
                    options = list(paging = TRUE,
                                  ordering = TRUE,
                                  searching = TRUE,
                                  scrollY = '500px',
                                  scrollX = TRUE,
                                  info = TRUE))
      }, server = TRUE)
    # Construct the actual output.
    tagList(
      DT::dataTableOutput(ns("dt"))
    )
  }) 
}