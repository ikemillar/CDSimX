#' Simulate Dew Point Temperature
#'
#' Generates synthetic dew point temperature series for multiple
#' stations using simulated air temperature and relative humidity.
#'
#' Dew point is physically linked to:
#' - air temperature,
#' - atmospheric moisture,
#' - rainfall regimes,
#' - coastal humidity effects,
#' - elevation controls.
#'
#' The simulation uses a Magnus-type approximation for realistic
#' atmospheric thermodynamics.
#'
#' @param temperature data.frame from simulate_temperature()
#'
#' @param rh data.frame from simulate_rh()
#'
#' @param min_dewpoint Numeric.
#' Minimum allowable dew point (°C).
#' Default = -5
#'
#' @param max_dewpoint Numeric.
#' Maximum allowable dew point (°C).
#' Default = 35
#'
#' @param noise_sd Numeric.
#' Stochastic variability in dew point.
#' Default = 0.5
#'
#' @param seed Optional numeric seed.
#'
#' @return data.frame containing:
#' \describe{
#'   \item{Station}{Station name}
#'   \item{LON}{Longitude}
#'   \item{LAT}{Latitude}
#'   \item{ELEV}{Elevation}
#'   \item{DATE}{Simulation timestamp}
#'   \item{Year}{Calendar year}
#'   \item{Month}{Calendar month}
#'   \item{Season}{Climatological season}
#'   \item{Tmean}{Mean air temperature (°C)}
#'   \item{RH}{Relative humidity (%)}
#'   \item{DewPoint}{Simulated dew point temperature (°C)}
#'   \item{Dewpoint_Depression}{Tmean − DewPoint}
#' }
#'
#' @examples
#' stations <- create_stations(
#'   n = 3,
#'   seed = 123
#' )
#'
#' tindex <- generate_time_index(
#'   start_date = "2000-01-01",
#'   end_date = "2005-12-31",
#'   frequency = "monthly"
#' )
#'
#' temp <- simulate_temperature(
#'   stations,
#'   tindex
#' )
#'
#' rh <- simulate_rh(
#'   stations,
#'   tindex
#' )
#'
#' dew <- simulate_dewpoint(
#'   temperature = temp,
#'   rh = rh
#' )
#'
#' head(dew)
#'
#' @export

simulate_dewpoint <- function(
    temperature,
    rh,
    min_dewpoint = -5,
    max_dewpoint = 35,
    noise_sd = 0.5,
    seed = NULL
){
  # -----------------------------------
  # Validation
  # -----------------------------------
  if(missing(temperature)){
    stop(
      "temperature data is required. ",
      "Use simulate_temperature() first."
    )
  }
  if(missing(rh)){
    stop(
      "Relative humidity data is required. ",
      "Use simulate_rh() first."
    )
  }
  if(!is.null(seed)) set.seed(seed)
  required_temp <- c(
    "Station",
    "DATE",
    "Avg.Temp"
  )
  required_rh <- c(
    "Station",
    "DATE",
    "RH"
  )
  if(!all(required_temp %in% names(temperature))){
    stop(
      "temperature must contain: ",
      paste(required_temp, collapse = ", ")
    )
  }
  if(!all(required_rh %in% names(rh))){
    stop(
      "rh must contain: ",
      paste(required_rh, collapse = ", ")
    )
  }
  # -----------------------------------
  # Merge temperature + RH
  # -----------------------------------
  sim_df <- merge(temperature, rh[, c("Station", "DATE", "RH")],
    by = c("Station", "DATE"))
  # -----------------------------------
  # Magnus dew point equation
  # -----------------------------------
  a <- 17.27
  b <- 237.7
  gamma_val <-
    (
      a * sim_df$Avg.Temp /
        (b + sim_df$Avg.Temp)
    ) + log(sim_df$RH / 100)
  sim_df$DewPoint <- (b * gamma_val) / (a - gamma_val)
  # -----------------------------------
  # Add stochastic variability
  # -----------------------------------
  sim_df$DewPoint <-
    sim_df$DewPoint +
    rnorm(
      nrow(sim_df),
      mean = 0,
      sd = noise_sd
    )
  # -----------------------------------
  # Physical constraints
  # -----------------------------------
  # Dew point cannot exceed air temperature
  sim_df$DewPoint <- pmin(sim_df$DewPoint, sim_df$Avg.Temp)
  # Apply bounds
  sim_df$DewPoint <-
    pmax(
      min_dewpoint,
      pmin(
        max_dewpoint,
        sim_df$DewPoint
      )
    )

  # -----------------------------------
  # Derived metrics
  # -----------------------------------
  sim_df$Dewpoint_Depression <- sim_df$Avg.Temp - sim_df$DewPoint
  # -----------------------------------
  # Round outputs
  # -----------------------------------
  numeric_cols <- c(
    "Avg.Temp",
    "RH",
    "DewPoint",
    "Dewpoint_Depression"
  )
  sim_df[numeric_cols] <- round(sim_df[numeric_cols], 2)
  # -----------------------------------
  # Final output
  # -----------------------------------
  keep_cols <- c(
    "Station",
    "LON",
    "LAT",
    "ELEV",
    "DATE",
    "Year",
    "Month",
    "Season",
    "Avg.Temp",
    "RH",
    "DewPoint",
    "Dewpoint_Depression"
  )
  sim_df <- sim_df[, keep_cols]
  message(
    "Dew point simulation complete for ",
    length(unique(sim_df$Station)),
    " stations."
  )
  return(sim_df)
}
