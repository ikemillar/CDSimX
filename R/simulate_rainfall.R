#' Simulate Rainfall Time Series
#'
#' Generates synthetic rainfall series for multiple stations
#' using stochastic climate dynamics and climate-zone controls.
#'
#' The simulation incorporates:
#' - wet/dry occurrence processes,
#' - seasonal rainfall regimes,
#' - spatial climate variability,
#' - coastal moisture effects,
#' - elevation enhancement,
#' - temporal persistence,
#' - extreme rainfall events,
#' - Gamma-distributed rainfall amounts.
#'
#' Supports:
#' - daily simulations,
#' - monthly simulations,
#' - yearly simulations.
#'
#' @param stations data.frame from create_stations()
#'
#' @param time_index data.frame from generate_time_index()
#'
#' @param wetday_prob Numeric.
#' Base wet-day probability.
#' Used mainly for daily simulations.
#' Default = 0.35
#'
#' @param gamma_shape Numeric.
#' Shape parameter for Gamma rainfall generation.
#' Default = 2
#'
#' @param gamma_scale Numeric.
#' Scale parameter for Gamma rainfall generation.
#' Default = 8
#'
#' @param ar_coeff Numeric.
#' Temporal persistence coefficient.
#' Default = 0.4
#'
#' @param seasonal_strength Numeric.
#' Controls rainfall seasonality intensity.
#' Default = 1
#'
#' @param extreme_event_prob Numeric.
#' Probability of extreme rainfall occurrence.
#' Default = 0.01
#'
#' @param extreme_multiplier Numeric.
#' Multiplier applied during extreme events.
#' Default = 3
#'
#' @param max_rainfall Numeric.
#' Maximum allowable rainfall amount.
#' Default = 500
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
#'   \item{Rainfall}{Simulated rainfall amount (mm)}
#'   \item{Wet_Day}{Wet occurrence indicator}
#'   \item{Extreme_Event}{Extreme rainfall indicator}
#'   \item{Rain_Anomaly}{Rainfall anomaly}
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
#' rain <- simulate_rainfall(
#'   stations = stations,
#'   time_index = time_index,
#'   seed = 123
#' )
#'
#' head(rain)
#'
#' @export

simulate_rainfall <- function(
    stations,
    time_index,
    wetday_prob = 0.35,
    gamma_shape = 2,
    gamma_scale = 8,
    ar_coeff = 0.4,
    seasonal_strength = 1,
    extreme_event_prob = 0.01,
    extreme_multiplier = 3,
    max_rainfall = 800,
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
    "COASTAL_INDEX",
    "RAIN_REGIME"
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
  sim_df <- merge(stations, time_index, by = NULL)
  sim_df <- sim_df[order(sim_df$Station, sim_df$DATE), ]
  # -----------------------------------
  # Frequency controls
  # -----------------------------------
  freq <- unique(sim_df$Frequency)
  if(freq == "daily"){
    cycle_index  <- sim_df$DOY
    cycle_length <- 365
    ar_coeff     <- 0.7

  }
  if(freq == "monthly"){
    cycle_index  <- sim_df$Month
    cycle_length <- 12
    ar_coeff     <- 0.5
  }

  if(freq == "yearly"){
    cycle_index  <- sim_df$Year - min(sim_df$Year) + 1
    cycle_length <- max(cycle_index)
    ar_coeff     <- 0.2
  }
  # -----------------------------------
  # Seasonal rainfall climatology
  # -----------------------------------
  sim_df$Seasonal_Component <- 0
  # Coastal bimodal
  coastal_idx <- which(
    sim_df$RAIN_REGIME == "Bimodal"
  )
  sim_df$Seasonal_Component[coastal_idx] <-
    seasonal_strength *
    (
      80 *
        exp(
          -((sim_df$Month[coastal_idx] - 6)^2)/4
        ) +
        50 *
        exp(
          -((sim_df$Month[coastal_idx] - 10)^2)/3
        )
    )
  # Forest humid
  forest_idx <- which(
    sim_df$RAIN_REGIME == "Humid"
  )
  sim_df$Seasonal_Component[forest_idx] <-
    seasonal_strength *
    (
      100 *
        sin(
          2 * pi *
            cycle_index[forest_idx] /
            cycle_length
        )^2
    )
  # Savannah monomodal
  sav_idx <- which(
    sim_df$RAIN_REGIME == "Monomodal"
  )
  sim_df$Seasonal_Component[sav_idx] <-
    seasonal_strength *
    (
      120 *
        exp(
          -((sim_df$Month[sav_idx] - 8)^2)/6
        )
    )

  # -----------------------------------
  # Spatial effects
  # -----------------------------------
  sim_df$Elevation_Effect <- 0.03 * sim_df$ELEV
  sim_df$Coastal_Effect <- 40 * (1 - sim_df$COASTAL_INDEX)
  sim_df$Latitude_Effect <- 8 * (sim_df$LAT - mean(sim_df$LAT))
  # -----------------------------------
  # Base rainfall field
  # -----------------------------------
  sim_df$Base_Rainfall <-
    sim_df$Seasonal_Component +
    sim_df$Elevation_Effect +
    sim_df$Coastal_Effect -
    sim_df$Latitude_Effect
  sim_df$Base_Rainfall <- pmax(sim_df$Base_Rainfall, 0)
  # -----------------------------------
  # Temporal persistence
  # -----------------------------------
  sim_df$Persistence <- NA
  for(st in unique(sim_df$Station)){
    idx <- which(sim_df$Station == st)
    n <- length(idx)
    pers <- numeric(n)
    pers[1] <- rnorm(1, 0, 5)
    for(i in 2:n){
      pers[i] <- ar_coeff * pers[i - 1] + rnorm(1, 0, 5)
    }
    sim_df$Persistence[idx] <- pers
  }
  # -----------------------------------
  # Wet-day occurrence / rain days
  # -----------------------------------
  if(freq == "daily"){
    prob_vec <-
      plogis(
        (
          sim_df$Base_Rainfall / 100
        ) - 1
      )
    sim_df$Wet_Day <-
      rbinom(
        nrow(sim_df),
        size = 1,
        prob = pmin(
          pmax(prob_vec, 0.01),
          0.95
        )
      )
    sim_df$Rain_Days <- sim_df$Wet_Day
  } else {
    # -----------------------------------
    # Climate-dependent rain-day means
    # -----------------------------------
    mean_rain_days <-
      ifelse(
        sim_df$CLIMATE_ZONE == "Coastal",
        sim_df$Base_Rainfall / 8,
        ifelse(
          sim_df$CLIMATE_ZONE == "Forest",
          sim_df$Base_Rainfall / 10,
          sim_df$Base_Rainfall / 15
        )
      )

    # -----------------------------------
    # Generate rain days
    # -----------------------------------

    sim_df$Rain_Days <-
      round(
        pmax(
          0,
          rnorm(
            nrow(sim_df),
            mean = mean_rain_days,
            sd = 3
          )
        )
      )

    # -----------------------------------
    # Frequency-specific limits
    # -----------------------------------
    if(freq == "monthly"){
      sim_df$Rain_Days <- pmin(sim_df$Rain_Days, 31)
    }
    if(freq == "yearly"){
      sim_df$Rain_Days <- pmin(sim_df$Rain_Days, 365)
    }
    sim_df$Wet_Day <-
      ifelse(sim_df$Rain_Days > 0, 1, 0)
  }
  # -----------------------------------
  # Gamma rainfall generation
  # -----------------------------------
  sim_df$Rainfall <- 0
  wet_idx <- which(sim_df$Wet_Day == 1)
  sim_df$Rainfall[wet_idx] <-
    rgamma(
      length(wet_idx),
      shape = gamma_shape + sim_df$Base_Rainfall[wet_idx] / 100,
      scale = gamma_scale + abs(sim_df$Persistence[wet_idx]) / 10
    )
  # -----------------------------------
  # Scale rainfall using rain days
  # -----------------------------------
  sim_df$Rainfall[wet_idx] <-
    sim_df$Rainfall[wet_idx] * sqrt(sim_df$Rain_Days[wet_idx])
  # -----------------------------------
  # Climate scaling
  # -----------------------------------
  sim_df$Rainfall[wet_idx] <- sim_df$Rainfall[wet_idx] *
    sqrt(sim_df$Base_Rainfall[wet_idx] / 20)
  # -----------------------------------
  # Extreme rainfall events
  # -----------------------------------
  sim_df$Extreme_Event <- 0
  extreme_idx <- which(runif(nrow(sim_df)) < extreme_event_prob)
  if(length(extreme_idx) > 0){
    sim_df$Rainfall[extreme_idx] <-
      sim_df$Rainfall[extreme_idx] * extreme_multiplier
    sim_df$Extreme_Event[extreme_idx] <- 1
  }
  # -----------------------------------
  # Bounds
  # -----------------------------------

  sim_df$Rainfall <-
    pmax(
      0,
      pmin(
        max_rainfall,
        sim_df$Rainfall
      )
    )

  # -----------------------------------
  # Rainfall anomaly
  # -----------------------------------
  if(freq %in% c("daily", "monthly")){
    climatology <-
      ave(
        sim_df$Rainfall,
        sim_df$Station,
        sim_df$Month,
        FUN = mean
      )
  } else {
    climatology <-
      ave(
        sim_df$Rainfall,
        sim_df$Station,
        FUN = mean
      )
  }
  sim_df$Rain_Anomaly <- sim_df$Rainfall - climatology
  sim_df$Rainfall <- pmax(sim_df$Rainfall, 0)
  # -----------------------------------
  # Rounding
  # -----------------------------------
  sim_df$Rainfall <- round(sim_df$Rainfall, 2)
  sim_df$Rain_Anomaly <- round(sim_df$Rain_Anomaly, 2)
  # -----------------------------------
  # Final output
  # -----------------------------------
  keep_cols <- c(
    "Station",
    "LON",
    "LAT",
    "ELEV",
    "CLIMATE_ZONE",
    "RAIN_REGIME",
    "DATE",
    "Year",
    "Month",
    "Season",
    "Rain_Days",
    "Rainfall",
    "Wet_Day",
    "Extreme_Event",
    "Rain_Anomaly"
  )
  sim_df <- sim_df[, keep_cols]
  message(
    "Rainfall simulation complete for ",
    length(unique(sim_df$Station)),
    " stations."
  )
  return(sim_df)
}
