#' Simulate Relative Humidity Time Series
#'
#' Generates synthetic Relative Humidity (RH) series
#' for multiple climate stations using stochastic
#' hydro-climatic relationships.
#'
#' The simulation incorporates:
#' - seasonal humidity cycles,
#' - rainfall-humidity coupling,
#' - temperature-humidity interaction,
#' - coastal moisture effects,
#' - elevation drying effects,
#' - temporal persistence,
#' - stochastic atmospheric variability,
#' - physically realistic RH bounds.
#'
#' Higher rainfall generally increases RH,
#' while higher temperature lowers RH.
#'
#' @param stations data.frame from create_stations()
#'
#' @param time_index data.frame from generate_time_index()
#'
#' @param rainfall Optional rainfall data.frame from
#' simulate_rainfall().
#'
#' @param temperature Optional temperature data.frame
#' from simulate_temperature().
#'
#' @param ar_coeff Numeric.
#' AR(1) persistence coefficient.
#' Default = 0.7
#'
#' @param seasonal_strength Numeric.
#' Controls seasonal RH variability.
#' Default = 8
#'
#' @param rain_sensitivity Numeric.
#' RH increase per rainfall unit.
#' Default = 0.04
#'
#' @param temp_sensitivity Numeric.
#' RH decrease per temperature unit.
#' Default = 0.6
#'
#' @param coastal_moisture Numeric.
#' Coastal humidity enhancement factor.
#' Default = 12
#'
#' @param noise_sd Numeric.
#' Standard deviation of stochastic noise.
#' Default = 3
#'
#' @param min_rh Numeric.
#' Minimum allowable RH (%).
#' Default = 15
#'
#' @param max_rh Numeric.
#' Maximum allowable RH (%).
#' Default = 100
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
#'   \item{RH}{Relative humidity (%)}
#'   \item{Humidity_Anomaly}{Humidity anomaly}
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
#'   stations,
#'   time_index
#' )
#'
#' temp <- simulate_temperature(
#'   stations,
#'   time_index
#' )
#'
#' rh <- simulate_rh(
#'   stations,
#'   time_index,
#'   rainfall = rain,
#'   temperature = temp
#' )
#'
#' head(rh)
#'
#' @export

simulate_rh <- function(
    stations,
    time_index,
    rainfall = NULL,
    temperature = NULL,
    ar_coeff = 0.7,
    seasonal_strength = 8,
    rain_sensitivity = 0.04,
    temp_sensitivity = 0.6,
    coastal_moisture = 12,
    noise_sd = 3,
    min_rh = 15,
    max_rh = 100,
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
  sim_df <- merge(stations, time_index, by = NULL)
  sim_df <- sim_df[order(sim_df$Station, sim_df$DATE), ]
  # -----------------------------------
  # Frequency controls
  # -----------------------------------
  freq <- unique(sim_df$Frequency)
  if(freq == "daily"){
    cycle_index  <- sim_df$DOY
    cycle_length <- 365
    ar_coeff     <- 0.85
  }
  if(freq == "monthly"){
    cycle_index  <- sim_df$Month
    cycle_length <- 12
    ar_coeff     <- 0.6
  }
  if(freq == "yearly"){
    cycle_index  <- sim_df$Year - min(sim_df$Year) + 1
    cycle_length <- max(cycle_index)
    ar_coeff     <- 0.3
  }
  # -----------------------------------
  # Seasonal humidity cycle
  # -----------------------------------
  sim_df$Seasonal_Component <-
    seasonal_strength * sin(2 * pi * cycle_index / cycle_length)
  # -----------------------------------
  # Coastal moisture effect
  # -----------------------------------
  sim_df$Coastal_Effect <- coastal_moisture * (1 - sim_df$COASTAL_INDEX)
  # -----------------------------------
  # Elevation drying effect
  # -----------------------------------
  sim_df$Elevation_Effect <- -0.01 * sim_df$ELEV
  # -----------------------------------
  # Climate-zone baseline RH
  # -----------------------------------
  sim_df$RH_Base <-
    ifelse(sim_df$CLIMATE_ZONE == "Coastal", 78,
      ifelse(sim_df$CLIMATE_ZONE == "Forest", 72, 58))
  # -----------------------------------
  # Rainfall coupling
  # -----------------------------------
  sim_df$Rainfall_Effect <- 0
  if(!is.null(rainfall)){
    rain_merge <- rainfall[, c("Station", "DATE", "Rainfall")]
    sim_df <- merge(sim_df, rain_merge, by = c("Station", "DATE"), all.x = TRUE)
    sim_df$Rainfall[is.na(sim_df$Rainfall)] <- 0
    sim_df$Rainfall_Effect <- log1p(sim_df$Rainfall) * rain_sensitivity * 10
  }
  # -----------------------------------
  # Temperature coupling
  # -----------------------------------
  sim_df$Temperature_Effect <- 0
  if(!is.null(temperature)){
    temp_merge <- temperature[, c("Station", "DATE", "Avg.Temp")]
    sim_df <- merge(sim_df, temp_merge, by = c("Station", "DATE"), all.x = TRUE)
    sim_df$Avg.Temp[is.na(sim_df$Avg.Temp)] <- mean(sim_df$Avg.Temp, na.rm = TRUE)
    sim_df$Temperature_Effect <- -temp_sensitivity * ( sim_df$Avg.Temp - 25)
  }
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
      pers[i] <- ar_coeff * pers[i - 1] + rnorm(1, 0, noise_sd)
    }
    sim_df$Persistence[idx] <- pers
  }

  # -----------------------------------
  # Final RH generation
  # -----------------------------------
  sim_df$RH <-
    sim_df$RH_Base +
    sim_df$Seasonal_Component +
    sim_df$Coastal_Effect +
    sim_df$Elevation_Effect +
    sim_df$Rainfall_Effect +
    sim_df$Temperature_Effect +
    sim_df$Persistence
  # -----------------------------------
  # Bounds
  # -----------------------------------
  sim_df$RH <-
    pmax(
      min_rh,
      pmin(
        max_rh,
        sim_df$RH
      )
    )
  # -----------------------------------
  # Humidity anomaly
  # -----------------------------------
  sim_df$Humidity_Anomaly <-
    sim_df$RH -
    ave(
      sim_df$RH,
      sim_df$Station,
      sim_df$Month,
      FUN = mean
    )
  # -----------------------------------
  # Rounding
  # -----------------------------------
  sim_df$RH <- round(sim_df$RH, 2)
  sim_df$Humidity_Anomaly <- round(sim_df$Humidity_Anomaly, 2)
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
    "RH",
    "Humidity_Anomaly"
  )
  sim_df <- sim_df[, keep_cols]
  message(
    "Relative humidity simulation complete for ",
    length(unique(sim_df$Station)),
    " stations."
  )
  return(sim_df)
}
