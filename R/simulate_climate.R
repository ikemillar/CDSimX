#' Simulate Integrated Climate Dataset
#'
#' Generates a complete synthetic climate dataset
#' by internally calling all climate simulation
#' modules and merging their outputs into a
#' single dataframe.
#'
#' The function currently simulates:
#' - temperature
#' - rainfall
#' - relative humidity
#' - dew point
#' - wind speed
#' - wind direction
#' - solar radiation
#' - evapotranspiration
#'
#' @param stations Output from
#' create_stations().
#'
#' @param time_index Output from
#' generate_time_index().
#'
#' @param seed Optional random seed
#' for reproducibility.
#'
#' @return A merged dataframe containing
#' all simulated climate variables.
#'
#' @examples
#' \dontrun{
#'
#' stations <- create_stations(
#'   n = 10
#' )
#'
#' tindex <- generate_time_index(
#'   start_date = "2020-01-01",
#'   end_date   = "2020-12-31",
#'   frequency  = "monthly"
#' )
#'
#' climate <- simulate_climate(
#'   stations,
#'   tindex
#' )
#'
#' head(climate)
#'
#' }
#'
#' @export

simulate_climate <- function(
    stations,
    time_index,
    seed = NULL
){
  # -----------------------------------
  # Validation
  # -----------------------------------
  if(missing(stations)){
    stop("stations is required. ", "Use create_stations().")
  }
  if(missing(time_index)){
    stop("time_index is required. ", "Use generate_time_index().")
  }
  if(!is.null(seed)){
    set.seed(seed)
  }
  # -----------------------------------
  # Temperature
  # -----------------------------------
  temp <- simulate_temperature(
    stations = stations,
    time_index = time_index,
    seed = seed
  )
  # -----------------------------------
  # Rainfall
  # -----------------------------------
  rain <- simulate_rainfall(
    stations = stations,
    time_index = time_index,
    seed = seed
  )
  # -----------------------------------
  # Relative humidity
  # -----------------------------------
  rh <- simulate_rh(
    stations = stations,
    time_index = time_index,
    rainfall = rain,
    seed = seed
  )
  # -----------------------------------
  # Dew point
  # -----------------------------------
  dew <- simulate_dewpoint(
    temperature = temp,
    rh = rh
  )
  # -----------------------------------
  # Wind speed
  # -----------------------------------
  ws <- simulate_wind_speed(
    stations = stations,
    time_index = time_index,
    seed = seed
  )
  # -----------------------------------
  # Wind direction
  # -----------------------------------
  wd <- simulate_wind_direction(
    stations = stations,
    time_index = time_index,
    wind_speed = ws$WindSpeed,
    seed = seed
  )
  # -----------------------------------
  # Solar radiation
  # -----------------------------------
  sr <- simulate_solar_radiation(
    stations = stations,
    time_index = time_index,
    rainfall = rain$Rainfall,
    rh = rh$RH,
    seed = seed
  )
  # -----------------------------------
  # Evapotranspiration
  # -----------------------------------
  et <- simulate_evapotranspiration(
    temperature = temp,
    rh = rh,
    solar_radiation = sr,
    wind_speed = ws,
    seed = seed
  )
  # -----------------------------------
  # Keep only variable-specific columns
  # -----------------------------------
  rain_merge <- rain[ , c(
    "Station",
    "DATE",
    "Rain_Days",
    "Rainfall",
    "Wet_Day",
    "Extreme_Event",
    "Rain_Anomaly"
  )]
  rh_merge <- rh[ , c(
    "Station",
    "DATE",
    "RH",
    "Humidity_Anomaly"
  )]
  dew_merge <- dew[ , c(
    "Station",
    "DATE",
    "DewPoint",
    "Dewpoint_Depression"
  )]
  ws_merge <- ws[ , c(
    "Station",
    "DATE",
    "WindSpeed",
    "Extreme_Wind",
    "Wind_Anomaly"
  )]
  wd_merge <- wd[ , c(
    "Station",
    "DATE",
    "WindDirection",
    "WindSector",
    "Prevailing_Direction",
    "Direction_Variability",
    "Extreme_Shift",
    "Wind_u",
    "Wind_v"
  )]
  sr_merge <- sr[ , c(
    "Station",
    "DATE",
    "Solar_Radiation",
    "Clear_Sky_Radiation",
    "Cloud_Factor",
    "Sunshine_Fraction",
    "Radiation_Anomaly"
  )]
  et_merge <- et[ , c(
    "Station",
    "DATE",
    "Atmospheric_Pressure",
    "VPD",
    "ET0",
    "Dryness_Index",
    "Dryness_Class",
    "ET_Anomaly"
  )]
  # -----------------------------------
  # Merge outputs
  # -----------------------------------
  climate <- Reduce(
    function(x, y){
      merge(x, y, by = c("Station", "DATE"), all = TRUE)
    }, list(temp, rain_merge, rh_merge, dew_merge, ws_merge, wd_merge,
           sr_merge, et_merge)
  )
  # -----------------------------------
  # Remove duplicated columns
  # -----------------------------------
  climate <- climate[ , !duplicated(names(climate))]
  # -----------------------------------
  # Sort output
  # -----------------------------------
  climate <- climate[order(climate$Station, climate$DATE), ]
  # -----------------------------------
  # Final message
  # -----------------------------------
  message(
    "Integrated climate simulation complete for ",
    length(unique(climate$Station)),
    " stations."
  )
  return(climate)
}
