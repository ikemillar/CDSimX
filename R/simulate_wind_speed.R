#' Simulate Wind Speed Time Series
#'
#' Generates synthetic wind speed climate series
#' for multiple stations using stochastic atmospheric dynamics.
#'
#' The simulation incorporates:
#' - seasonal circulation variability,
#' - coastal enhancement,
#' - elevation acceleration,
#' - temporal persistence,
#' - stochastic turbulence,
#' - extreme wind events,
#' - climate-zone effects.
#'
#' Wind speed is simulated in m/s.
#'
#' @param stations data.frame from create_stations()
#'
#' @param time_index data.frame from generate_time_index()
#'
#' @param ar_coeff Numeric.
#' AR(1) persistence coefficient.
#' Default = 0.6
#'
#' @param seasonal_strength Numeric.
#' Controls seasonal wind variability.
#' Default = 1
#'
#' @param noise_sd Numeric.
#' Standard deviation of stochastic turbulence.
#' Default = 1.5
#'
#' @param extreme_event_prob Numeric.
#' Probability of extreme wind events.
#' Default = 0.01
#'
#' @param extreme_multiplier Numeric.
#' Multiplier applied during extreme events.
#' Default = 2
#'
#' @param min_ws Numeric.
#' Minimum allowable wind speed.
#' Default = 0
#'
#' @param max_ws Numeric.
#' Maximum allowable wind speed.
#' Default = 40
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
#'   \item{WindSpeed}{Simulated wind speed (m/s)}
#'   \item{Extreme_Wind}{Extreme wind event indicator}
#'   \item{Wind_Anomaly}{Wind speed anomaly}
#' }
#'
#' @examples
#' stations <- create_stations(
#'   n = 5,
#'   seed = 123
#' )
#'
#' time_index <- generate_time_index(
#'   start_date = "2000-01-01",
#'   end_date = "2005-12-31",
#'   frequency = "monthly"
#' )
#'
#' wind <- simulate_wind_speed(
#'   stations,
#'   time_index
#' )
#'
#' head(wind)
#'
#' @export

simulate_wind_speed <- function(
    stations,
    time_index,
    ar_coeff = 0.6,
    seasonal_strength = 1,
    noise_sd = 1.5,
    extreme_event_prob = 0.01,
    extreme_multiplier = 2,
    min_ws = 0,
    max_ws = 40,
    seed = NULL
){

  # -----------------------------------
  # Validation
  # -----------------------------------

  if(missing(stations)){

    stop(
      "stations is required. ",
      "Use create_stations() first."
    )

  }

  if(missing(time_index)){

    stop(
      "time_index is required. ",
      "Use generate_time_index() first."
    )

  }

  if(!is.null(seed)) set.seed(seed)

  required_cols <- c(
    "Station",
    "LON",
    "LAT",
    "ELEV",
    "CLIMATE_ZONE",
    "COASTAL_INDEX"
  )

  if(!all(required_cols %in% names(stations))){

    stop(
      "stations must contain: ",
      paste(required_cols, collapse = ", ")
    )

  }

  # -----------------------------------
  # Expand station × time grid
  # -----------------------------------

  sim_df <- merge(
    stations,
    time_index,
    by = NULL
  )

  sim_df <- sim_df[
    order(sim_df$Station, sim_df$DATE),
  ]

  # -----------------------------------
  # Frequency controls
  # -----------------------------------

  freq <- unique(sim_df$Frequency)

  if(freq == "daily"){

    cycle_index  <- sim_df$DOY
    cycle_length <- 365
    ar_coeff     <- 0.8

  }

  if(freq == "monthly"){

    cycle_index  <- sim_df$Month
    cycle_length <- 12
    ar_coeff     <- 0.5

  }

  if(freq == "yearly"){

    cycle_index  <- sim_df$Year -
      min(sim_df$Year) + 1

    cycle_length <- max(cycle_index)

    ar_coeff     <- 0.2

  }

  # -----------------------------------
  # Seasonal wind cycle
  # -----------------------------------

  sim_df$Seasonal_Component <-

    seasonal_strength *

    (
      2 *
        sin(
          2 * pi *
            cycle_index /
            cycle_length
        )
    )

  # -----------------------------------
  # Climate-zone baseline
  # -----------------------------------

  sim_df$Base_WS <-

    ifelse(
      sim_df$CLIMATE_ZONE == "Coastal",

      6,

      ifelse(
        sim_df$CLIMATE_ZONE == "Forest",

        3.5,

        4.5
      )
    )

  # -----------------------------------
  # Coastal enhancement
  # -----------------------------------

  sim_df$Coastal_Effect <-

    4 * (1 - sim_df$COASTAL_INDEX)

  # -----------------------------------
  # Elevation acceleration
  # -----------------------------------

  sim_df$Elevation_Effect <-

    0.0025 * sim_df$ELEV

  # -----------------------------------
  # Latitude effect
  # -----------------------------------

  sim_df$Latitude_Effect <-

    0.2 *
    (
      sim_df$LAT -
        mean(sim_df$LAT)
    )

  # -----------------------------------
  # Combined wind climatology
  # -----------------------------------

  sim_df$Wind_Climatology <-

    sim_df$Base_WS +
    sim_df$Seasonal_Component +
    sim_df$Coastal_Effect +
    sim_df$Elevation_Effect +
    sim_df$Latitude_Effect

  # -----------------------------------
  # Temporal persistence
  # -----------------------------------

  sim_df$Persistence <- NA

  for(st in unique(sim_df$Station)){

    idx <- which(sim_df$Station == st)

    n <- length(idx)

    pers <- numeric(n)

    pers[1] <- rnorm(1, 0, noise_sd)

    for(i in 2:n){

      pers[i] <-

        ar_coeff * pers[i - 1] +

        rnorm(1, 0, noise_sd)

    }

    sim_df$Persistence[idx] <- pers

  }

  # -----------------------------------
  # Turbulent variability
  # -----------------------------------

  sim_df$Noise <-

    rnorm(
      nrow(sim_df),
      mean = 0,
      sd = noise_sd
    )

  # -----------------------------------
  # Wind speed generation
  # -----------------------------------

  sim_df$WindSpeed <-

    sim_df$Wind_Climatology +
    sim_df$Persistence +
    sim_df$Noise

  # -----------------------------------
  # Extreme wind events
  # -----------------------------------

  sim_df$Extreme_Wind <- 0

  extreme_idx <- which(

    runif(nrow(sim_df)) <

      extreme_event_prob

  )

  if(length(extreme_idx) > 0){

    sim_df$WindSpeed[extreme_idx] <-

      sim_df$WindSpeed[extreme_idx] *

      extreme_multiplier

    sim_df$Extreme_Wind[extreme_idx] <- 1

  }

  # -----------------------------------
  # Physical bounds
  # -----------------------------------

  sim_df$WindSpeed <-

    pmax(
      min_ws,
      pmin(
        max_ws,
        sim_df$WindSpeed
      )
    )

  # -----------------------------------
  # Wind anomaly
  # -----------------------------------

  sim_df$Wind_Anomaly <-

    sim_df$WindSpeed -

    ave(
      sim_df$WindSpeed,
      sim_df$Station,
      sim_df$Month,
      FUN = mean
    )

  # -----------------------------------
  # Rounding
  # -----------------------------------

  sim_df$WindSpeed <-

    round(sim_df$WindSpeed, 2)

  sim_df$Wind_Anomaly <-

    round(sim_df$Wind_Anomaly, 2)

  # -----------------------------------
  # Final output
  # -----------------------------------

  keep_cols <- c(
    "Station",
    "LON",
    "LAT",
    "ELEV",
    "CLIMATE_ZONE",
    "DATE",
    "Year",
    "Month",
    "Season",
    "WindSpeed",
    "Extreme_Wind",
    "Wind_Anomaly"
  )

  sim_df <- sim_df[, keep_cols]

  message(
    "Wind speed simulation complete for ",
    length(unique(sim_df$Station)),
    " stations."
  )

  return(sim_df)

}
