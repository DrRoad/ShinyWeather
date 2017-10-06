library(R6)

# This R6 class handles data ingestion. It provides methods for extracting 
# specific subsets of data, which are plotted by the modules. On initialization 
# the data is read from .csv files. Public get_* methods return subsets of the 
# data.
forecast_dta_R6 <- R6Class("forecast_dta_R6",
  public = list(
    # On initialization data is fetched. (See private function fetch_dta.)
    initialize = function(dta_path = NULL) {
      private$fetch_dta(dta_path)
    },
    
    # Returns a vector of strings with all cities, for which forecast data is 
    # provided.
    get_all_cities = function() {
      # As city names do not change we only 
      # need to compute them once.
      if (is.null(private$cities)) {
        private$cities <- private$raw_dta %>% 
          select(CITY) %>% 
          distinct() %>% 
          as.list(all.names = TRUE)
      }
      private$cities$CITY
    },
    
    # Returns a vector strings with all sources from which forecast data is
    # scraped.
    get_sources = function() {
      # As sources do not change they only
      # need to be computed once.
      if (is.null(private$sources)) {
      private$sources <- private$raw_dta %>% 
        select(SOURCE) %>% 
        distinct()
      }
      private$sources$SOURCE
    },
    
    # Returns a tibble with forecast data for a given city and source for a 
    # given number of days (len). The type of forecast dta is converted to char.
    # Retuned columns:
    #
    # FORECAST_DATE TEMPERATURE RAIN_PROB RAIN_AMMOUNT WIND_SPEED
    # <chr>       <chr>     <chr>        <chr>      <chr>
    # 
    get_table_dta = function(city = NULL, dta_source = NULL, len = 5) { 
      # Make sure that parameters are given.
      if (is.null(city) || is.null(dta_source)) return(NULL)
      private$extract_forecast_dta(city, dta_source) %>% 
        dplyr::mutate(FORECAST_DATE = as.character(FORECAST_DATE),
                      TEMPERATURE = as.character(TEMPERATURE),
                      RAIN_PROB = as.character(RAIN_PROB),
                      RAIN_AMMOUNT = as.character(RAIN_AMMOUNT),
                      WIND_SPEED = as.character(WIND_SPEED), 
                      FORECAST_DATE = lubridate::as_date(FORECAST_DATE) %>% 
                        format("%d.%m.%Y")) %>%
        dplyr::select(CITY, FORECAST_DATE, TEMPERATURE, RAIN_PROB, 
                      RAIN_AMMOUNT, WIND_SPEED) %>% 
        dplyr::slice(1:(len))
    },
    
    # Returns a tibble with forecast data for a given city and source for a 
    # given number of days. The dates are formatted to dd.mm.yyyy. 
    # Returned columns:
    # 
    # FORECAST_DATE TEMPERATURE RAIN_PROB RAIN_AMMOUNT WIND_SPEED
    # <chr>       <dbl>     <dbl>        <dbl>      <dbl>
    #
    get_forecast_chart_dta = function(city = NULL, dta_source = NULL, len = 5) { 
      # Make sure that parameters are given.
      if (is.null(city) || is.null(dta_source)) return(NULL)
      private$extract_forecast_dta(city, dta_source) %>%
        dplyr::mutate(TEMPERATURE = as.numeric(TEMPERATURE),
                      RAIN_PROB = as.numeric(RAIN_PROB),
                      RAIN_AMMOUNT = as.numeric(RAIN_AMMOUNT),
                      WIND_SPEED = as.numeric(WIND_SPEED), 
                      FORECAST_DATE = lubridate::as_date(FORECAST_DATE) %>% 
                        format("%d.%m.%Y")) %>% 
        dplyr::mutate(LOWER_TEMPERATURE = TEMPERATURE - MEAN_DIFF_TEMPERATURE, 
                      UPPER_TEMPERATURE = TEMPERATURE + MEAN_DIFF_TEMPERATURE, 
                      LOWER_RAIN_PROB = RAIN_PROB - MEAN_DIFF_RAIN_PROB, 
                      UPPER_RAIN_PROB = RAIN_PROB + MEAN_DIFF_RAIN_PROB, 
                      LOWER_RAIN_AMMOUNT = RAIN_AMMOUNT - MEAN_DIFF_RAIN_AMMOUNT, 
                      UPPER_RAIN_AMMOUNT = RAIN_AMMOUNT + MEAN_DIFF_RAIN_AMMOUNT, 
                      LOWER_WIND_SPEED = WIND_SPEED - MEAN_DIFF_WIND_SPEED, 
                      UPPER_WIND_SPEED = WIND_SPEED + MEAN_DIFF_WIND_SPEED) %>% 
        dplyr::select(CITY, FORECAST_DATE, 
                      TEMPERATURE, LOWER_TEMPERATURE, UPPER_TEMPERATURE,
                      RAIN_PROB, LOWER_RAIN_PROB, UPPER_RAIN_PROB,
                      RAIN_AMMOUNT, LOWER_RAIN_AMMOUNT, UPPER_RAIN_AMMOUNT,
                      WIND_SPEED, LOWER_WIND_SPEED, UPPER_WIND_SPEED) %>% 
        dplyr::slice(1:len)
    },
    
    # Returns a tibble with spatial and forecast data, which is to be displayed
    # in a map. For every spatial point a lable is constructed. 
    # Returned columns:
    # 
    # LNG   LAT   SAMPLE_DATE FORECAST_DATE TEMPERATURE RAIN_PROB RAIN_AMMOUNT 
    # <dbl> <dbl> <date>      <date>        <chr>       <chr>     <chr>  
    # WIND_SPEED CITY  SOURCE LABEL 
    # <chr>      <chr> <chr>  <chr>
    #
    get_map_dta = function(city = NULL, dta_source = NULL) {
      # Make sure that parameters are given.
      if (is.null(city) || is.null(dta_source)) return(NULL)
      
      # Function to create HTML code which is used to display lables.
      make_label <- function(city, date, temp, rain_prob, rain_ammount, 
                             wind_speed, source) {
        htmltools::HTML(paste0("<b>", city,", ", format(as.Date(date), "%m.%d.%Y"), 
                               "</b><br>", "Temperature: ", temp, " °C, <br>",
                               "Rain Probability: ", rain_prob, " %, <br>", 
                               "Rain Ammount: ", rain_ammount, " l/m<sup>2</sup>, 
                               <br>", "Wind Speed: ", wind_speed, " km/h <br>",
                               "<sub>Source: ", source, "<sub>"))  
      }
      
      # Format city strings to make them comparrable.
      city <- tolower(city) 
      private$spatial_dta %>%
        dplyr::filter(CITY_KEY %in% city) %>%
        dplyr::left_join(private$raw_dta, 
                         by = "CITY_KEY", 
                         suffix = c(".x", "")) %>%
        dplyr::select(-CITY.x) %>% 
        dplyr::filter(FORECAST_DATE == max(SAMPLE_DATE)) %>%
        dplyr::filter(SAMPLE_DATE == max(SAMPLE_DATE)) %>% 
        dplyr::rowwise() %>%
        dplyr::mutate(SOURCE = if_else(dta_source == "mean of all",
                                       "mean of all",
                                       SOURCE)) %>% 
        dplyr::filter(SOURCE == dta_source) %>% 
        dplyr::group_by(CITY) %>% 
        dplyr::mutate(TEMPERATURE = mean(TEMPERATURE, na.rm = TRUE) %>% 
                        format(digits = 1), 
                      RAIN_PROB = mean(RAIN_PROB, na.rm = TRUE) %>% 
                        format(digits = 1), 
                      RAIN_AMMOUNT = mean(RAIN_AMMOUNT, na.rm = TRUE) %>% 
                        format(digits = 1), 
                      WIND_SPEED = mean(WIND_SPEED, na.rm = TRUE) %>% 
                        format(digits = 1)) %>% 
        dplyr::distinct(CITY, .keep_all = TRUE) %>% 
        dplyr::rowwise() %>% 
        dplyr::mutate(LABEL = make_label(CITY, FORECAST_DATE, TEMPERATURE, 
                                         RAIN_PROB, RAIN_AMMOUNT, 
                                         WIND_SPEED, SOURCE)) %>% 
        dplyr::select(-CITY_KEY)
    },
  
    # This function calculates the mean difference of forecast values from 
    # "true" values as a measure of forecast quality.
    # Returned columns:
    #
    # SOURCE FORECAST_PERIOD MEAN_DIFF_TEMPERATURE MEAN_DIFF_RAIN_PROB 
    # <chr>  <time>          <dbl>                 <dbl> 
    # MEAN_DIFF_RAIN_AMMOUNT MEAN_DIFF_WIND_SPEED
    # <dbl>                  <dbl>
    #
    get_quality_dta = function(sources) {
      # Make sure that parameter is given.
      if (is.null(sources)) return(NULL)
      private$raw_dta %>% 
        dplyr::filter(SOURCE %in% sources) %>% 
        dplyr::select(SOURCE, FORECAST_PERIOD, MEAN_DIFF_TEMPERATURE, 
                      MEAN_DIFF_RAIN_PROB, MEAN_DIFF_RAIN_AMMOUNT, 
                      MEAN_DIFF_WIND_SPEED) %>% 
        dplyr::distinct(FORECAST_PERIOD, SOURCE, .keep_all = TRUE)
    },  
    
    # Returns the forecast data for a given city and a given date from a given
    # data source, i.e. the forcasts date - 7 days, date - 6 days, ..., date.
    # Returned columns:
    # 
    # SAMPLE_DATE FORECAST_DATE TEMPERATURE RAIN_PROB RAIN_AMMOUNT WIND_SPEED 
    # <chr>       <date>        <int>       <dbl>     <dbl>        <int>
    # CITY  SOURCE CITY_KEY
    # <chr> <chr>  <chr>
    #
    get_history_dta = function(city, dte, dta_source) {
      # Make sure that parameters are given.
      if (is.null(city) || is.null(dte) || is.null(dta_source)) return(NULL)
      private$raw_dta %>% 
        dplyr::filter(CITY == city) %>% 
        dplyr::filter(FORECAST_DATE == dte) %>% 
        dplyr::distinct(SAMPLE_DATE, SOURCE, .keep_all = TRUE) %>% 
        dplyr::filter(SOURCE %in% dta_source) %>% 
        dplyr::filter(SAMPLE_DATE > FORECAST_DATE - 7) %>% 
        dplyr::mutate(SAMPLE_DATE = lubridate::as_date(SAMPLE_DATE) %>% 
                        format("%d.%m.%Y")) %>% 
        dplyr::select(-CITY_KEY)
    },
    
    # Returns all provided spatio temporal forecast data.
    # Returned columns:
    #
    # SAMPLE_DATE FORECAST_DATE TEMPERATURE RAIN_PROB RAIN_AMMOUNT WIND_SPEED
    # <date>      <date>        <int>       <dbl>     <dbl>        <int>  
    # SOURCE CITY_KEY CITY  LNG   LAT
    # <chr>  <chr>    <chr> <dbl> <dbl>
    #
    get_all_dta = function() {
      private$raw_dta %>% 
        dplyr::left_join(private$spatial_dta, 
                         by = "CITY_KEY", 
                         suffix = c(".x", "")) %>%
        dplyr::select(-CITY.x) %>% 
        dplyr::distinct(SAMPLE_DATE, FORECAST_DATE, 
                        SOURCE, CITY_KEY, .keep_all = TRUE) 
    }
  ),
  
  private = list(
    raw_dta = NULL,
    spatial_dta = NULL,
    cities = NULL,
    sources = NULL,

    # Funktion to fetch data from the data source.
    # WARNING: Loading all data globally at once is only  
    #          advisable if the data is not too large.
    fetch_dta = function(dta_path = NULL) {
      if (is.null(dta_path)) return(NULL)
      # Load all the forecast data
      dta_wetter_de <- 
        read.csv2(file.path(dta_path, 
                            "forecast-data-wetter_de.csv"), as.is = TRUE) %>% 
        dplyr::as_tibble() %>% 
        dplyr::mutate(TEMPERATURE = as.integer(TEMPERATURE),
                      RAIN_PROB = as.integer(RAIN_PROB),
                      RAIN_AMMOUNT = as.double(RAIN_AMMOUNT),
                      WIND_SPEED = as.integer(WIND_SPEED))
      dta_wetter_com <- 
        read.csv2(file.path(dta_path,
                            "forecast-data-wetter_com.csv"), as.is = TRUE) %>%
        dplyr::as_tibble() %>% 
        dplyr::mutate(TEMPERATURE = as.integer(TEMPERATURE),
                      RAIN_PROB = as.integer(RAIN_PROB),
                      RAIN_AMMOUNT = as.double(RAIN_AMMOUNT),
                      WIND_SPEED = as.integer(WIND_SPEED))
      dta_weather_com <- 
        read.csv2(file.path(dta_path,
                            "forecast-data-weather_com.csv"), as.is = TRUE) %>% 
        dplyr::as_tibble() %>% 
        dplyr::mutate(TEMPERATURE = as.integer(TEMPERATURE),
                      RAIN_PROB = as.integer(RAIN_PROB),
                      RAIN_AMMOUNT = as.double(RAIN_AMMOUNT),
                      WIND_SPEED = as.integer(WIND_SPEED)) %>% 
        dplyr::filter(FORECAST_DATE >= SAMPLE_DATE)
      dta_wetterkontor_de <- 
        read.csv2(file.path(dta_path,
                            "forecast-data-wetterkontor_de.csv"), as.is = TRUE) %>%             
        dplyr::as_tibble() %>% 
        dplyr::mutate(TEMPERATURE = as.integer(TEMPERATURE),
                      RAIN_PROB = as.integer(RAIN_PROB),
                      RAIN_AMMOUNT = as.double(RAIN_AMMOUNT),
                      WIND_SPEED = as.integer(WIND_SPEED))
      
      # Combine all the forecast data to one table
      private$raw_dta <- dta_weather_com %>% 
        dplyr::bind_rows(dta_wetter_com) %>% 
        dplyr::bind_rows(dta_wetter_de) %>% 
        dplyr::bind_rows(dta_wetterkontor_de) %>% 
        dplyr::mutate(RAIN_PROB = as.numeric(RAIN_PROB)) %>% 
        dplyr::mutate(RAIN_AMMOUNT = as.numeric(RAIN_AMMOUNT)) %>% 
        dplyr::mutate(SAMPLE_DATE = lubridate::as_date(SAMPLE_DATE)) %>% 
        dplyr::mutate(FORECAST_DATE = lubridate::as_date(FORECAST_DATE)) %>% 
        dplyr::mutate(CITY_KEY = tolower(CITY)) %>% 
        dplyr::filter(SAMPLE_DATE > "2017-08-31")
      
      # Calculate the mean of measurements from all data 
      # sources for each day (i.e. no forecast data) and
      # each city. We assume this to be the "real" value 
      # for any given day and city.
      mean_values <- private$raw_dta %>% 
        dplyr::filter(FORECAST_DATE == SAMPLE_DATE) %>% 
        dplyr::group_by(SAMPLE_DATE, CITY) %>%  
        dplyr::summarise(MEAN_TEMPERATURE = mean(TEMPERATURE, na.rm = TRUE),
                         MEAN_RAIN_PROB = mean(RAIN_PROB, na.rm = TRUE),
                         MEAN_RAIN_AMMOUNT = mean(RAIN_AMMOUNT, na.rm = TRUE),
                         MEAN_WIND_SPEED = mean(WIND_SPEED, na.rm = TRUE)) %>% 
        dplyr::ungroup()

      # Combine "real" values for each city and date with 
      # all forcast data and calculate for each forcast 
      # from each source the difference between "real" 
      # value an forecast value. 
      # Then calculate the mean difference between forcast
      # value and "real" value.
      private$raw_dta <- private$raw_dta %>% 
        dplyr::left_join(mean_values) %>% 
        dplyr::mutate(DIFF_TEMPERATURE = abs(MEAN_TEMPERATURE - TEMPERATURE),
                      DIFF_RAIN_PROB = abs(MEAN_RAIN_PROB - RAIN_PROB),
                      DIFF_RAIN_AMMOUNT = abs(MEAN_RAIN_AMMOUNT - RAIN_AMMOUNT),
                      DIFF_WIND_SPEED = abs(MEAN_WIND_SPEED - WIND_SPEED)) %>% 
        dplyr::mutate(FORECAST_PERIOD = difftime(FORECAST_DATE, 
                                                 SAMPLE_DATE,
                                                 units = "day")) %>% 
        dplyr::group_by(SOURCE, FORECAST_PERIOD) %>% 
        dplyr::mutate(MEAN_DIFF_TEMPERATURE = mean(DIFF_TEMPERATURE, 
                                                      na.rm = TRUE),
                         MEAN_DIFF_RAIN_PROB = mean(DIFF_RAIN_PROB, 
                                                    na.rm = TRUE),
                         MEAN_DIFF_RAIN_AMMOUNT = mean(DIFF_RAIN_AMMOUNT, 
                                                       na.rm = TRUE),
                         MEAN_DIFF_WIND_SPEED = mean(DIFF_WIND_SPEED, 
                                                     na.rm = TRUE)) %>% 
        dplyr::ungroup() %>% 
        dplyr::filter(FORECAST_PERIOD < 8)
      
      
      # Load data about the cities for which forecast is available
      dta_cities <- 
        read.csv2(file.path(dta_path,
                            "city-data_200.csv"), as.is = TRUE) %>%             
        dplyr::as_tibble() %>% 
        dplyr::mutate(ort = tolower(ort)) %>% 
        dplyr::select(ort)
      dta_geo_coord <-
        read.csv2(file.path(dta_path,
                            "city-geo-coordinates.csv"), as.is = TRUE) %>%             
        dplyr::as_tibble() %>% 
        dplyr::mutate(ort = tolower(ORT)) %>% 
        dplyr::select(-ORT)
      # Combine the city data to one table
      private$spatial_dta <- dta_cities %>% 
        dplyr::left_join(dta_geo_coord) %>% 
        dplyr::mutate(CITY = paste0(toupper(substr(ort, 1, 1)), 
                                    tolower(substr(ort, 2, nchar(ort))))) %>% 
        dplyr::select(-ort) %>%
        dplyr::mutate(LNG = as.numeric(Länge), 
                      LAT = as.numeric(Breite)) %>% 
        dplyr::filter(!is.na(LNG)) %>% 
        dplyr::select(-Länge, -Breite) %>% 
        dplyr::mutate(CITY_KEY = tolower(CITY))
    },
    
    # Returns the forecast data for a given city and date source. This function
    # is a private helper function to public functions.
    # Returned columns:
    #
    # FORECAST_DATE TEMPERATURE RAIN_PROB RAIN_AMMOUNT WIND_SPEED CITY_KEY
    # <date>        <chr>       <chr>     <chr>        <chr>      <chr>
    #
    extract_forecast_dta = function(city, dta_source) {
      if (is.null(city) || is.null(dta_source)) return(NULL)
      # Prepare data for forecasts in table and plot
      private$raw_dta %>% 
        dplyr::filter(CITY == city) %>% 
        dplyr::filter(SAMPLE_DATE == max(SAMPLE_DATE)) %>% 
        dplyr::rowwise() %>%
        dplyr::mutate(SOURCE = if_else(dta_source == "mean of all",
                                       "mean of all",
                                       SOURCE)) %>% 
        dplyr::filter(SOURCE == dta_source) %>%
        dplyr::group_by(CITY, FORECAST_DATE) %>% 
        dplyr::mutate(TEMPERATURE = mean(TEMPERATURE, na.rm = TRUE) %>% 
                        format(digits = 1), 
                      RAIN_PROB = mean(RAIN_PROB, na.rm = TRUE) %>% 
                        format(digits = 1), 
                      RAIN_AMMOUNT = mean(RAIN_AMMOUNT, na.rm = TRUE) %>% 
                        format(digits = 1), 
                      WIND_SPEED = mean(WIND_SPEED, na.rm = TRUE) %>% 
                        format(digits = 1)) %>% 
        dplyr::ungroup() %>% 
        dplyr::select(-SAMPLE_DATE, -SOURCE) %>% 
        dplyr::distinct(CITY, FORECAST_DATE, .keep_all = TRUE)
    }
  )
)
