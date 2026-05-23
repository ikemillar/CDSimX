#' Simulate Wind Direction Time Series
#'
#' Generates synthetic wind direction fields for multiple stations
#' using stochastic atmospheric circulation dynamics.
#'
#' The simulation incorporates:
#' - prevailing regional wind regimes,
#' - seasonal directional shifts,
#' - coastal circulation effects,
#' - temporal persistence,
#' - wind-speed-dependent directional variability,
#' - stochastic directional turbulence,
#' - extreme wind-direction shifts,
#' - optional wind-speed coupling,
#' - vector wind components (u and v).
#'
#' @param stations data.frame from create_stations()
#'
#' @param time_index data.frame from generate_time_index()
#'
#' @param wind_speed Optional numeric vector.
#' Wind speed values used to dynamically adjust
#' directional variability.
#'
#' If NULL, synthetic wind speed is generated internally.
#'
#' @param base_direction Numeric.
#' Default prevailing wind direction (degrees).
#' Default = 225
#'
#' @param seasonal_shift Numeric.
#' Controls seasonal directional oscillation.
#' Default = 30
#'
#' @param noise_sd Numeric.
#' Base directional variability.
#' Default = 30
#'
#' @param extreme_shift_prob Numeric.
#' Probability of abrupt directional shifts.
#' Default = 0.01
#'
#' @param max_shift Numeric.
#' Maximum extreme directional deviation.
#' Default = 90
#'
#' @param seed Optional numeric seed.
#'
#' @return data.frame containing:
#' \describe{
#'   \item{Station}{Station name}
#'   \item{LON}{Longitude}
#'   \item{LAT}{Latitude}
#'   \item{ELEV}{Elevation}
#'   \item{CLIMATE_ZONE}{Climate classification}
#'   \item{DATE}{Simulation timestamp}
#'   \item{Year}{Calendar year}
#'   \item{Month}{Calendar month}
#'   \item{Season}{Climatological season}
#'   \item{WindSpeed}{Wind speed (m/s)}
#'   \item{WindDirection}{Wind direction (degrees)}
#'   \item{WindSector}{Compass sector}
#'   \item{Prevailing_Direction}{Mean prevailing direction}
#'   \item{Direction_Variability}{Directional variability}
#'   \item{Extreme_Shift}{Extreme directional event flag}
#'   \item{Wind_u}{Zonal wind component}
#'   \item{Wind_v}{Meridional wind component}
#' }
#'
#' @examples
#' stations <- create_stations(
#'   n = 3,
#'   seed = 123
#' )
#'
#' time_index <- generate_time_index(
#'   start_date = "2000-01-01",
#'   end_date = "2005-12-31",
#'   frequency = "monthly"
#' )
#'
#' ws <- simulate_wind_speed(
#'   stations,
#'   time_index
#' )
#'
#' wd <- simulate_wind_direction(
#'   stations,
#'   time_index,
#'   wind_speed = ws$WindSpeed
#' )
#'
#' head(wd)
#'
#' @export

simulate_wind_direction <- function(
    stations,
    time_index,
    wind_speed = NULL,
    base_direction = 225,
    seasonal_shift = 30,
    noise_sd = 30,
    extreme_shift_prob = 0.01,
    max_shift = 90,
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
  sim_df <- sim_df[order(sim_df$Station, sim_df$DATE), ]
  # -----------------------------------
  # Frequency controls
  # -----------------------------------
  freq <- unique(sim_df$Frequency)
  if(freq == "daily"){
    cycle_index  <- sim_df$DOY
    cycle_length <- 365
  }
  if(freq == "monthly"){
    cycle_index  <- sim_df$Month
    cycle_length <- 12
  }
  if(freq == "yearly"){
    cycle_index  <- sim_df$Year -
      min(sim_df$Year) + 1
    cycle_length <- max(cycle_index)
  }
  # -----------------------------------
  # Wind speed handling
  # -----------------------------------
  if(is.null(wind_speed)){
    sim_df$WindSpeed <-
      rgamma(
        nrow(sim_df),
        shape = 3,
        scale = 2
      )
  } else {
    if(length(wind_speed) != nrow(sim_df)){
      stop(
        "Length of wind_speed vector must equal ",
        "number of simulated records."
      )
    }
    sim_df$WindSpeed <- wind_speed
  }
  # -----------------------------------
  # Prevailing circulation regimes
  # -----------------------------------
  sim_df$Prevailing_Direction <-
    ifelse(sim_df$CLIMATE_ZONE == "Coastal", 225,
      ifelse(sim_df$CLIMATE_ZONE == "Forest", 200, 180)
    )
  # -----------------------------------
  # Seasonal directional oscillation
  # -----------------------------------
  sim_df$Seasonal_Component <- seasonal_shift * sin(2 * pi * cycle_index /
        cycle_length)
  # -----------------------------------
  # Coastal circulation effect
  # -----------------------------------
  sim_df$Coastal_Component <- 25 * (1 - sim_df$COASTAL_INDEX)
  # -----------------------------------
  # Wind-speed-dependent variability
  # -----------------------------------
  sim_df$Direction_Variability <-
    ifelse(sim_df$WindSpeed > 12, noise_sd * 0.5,
      ifelse(sim_df$WindSpeed > 6, noise_sd, noise_sd * 2)
    )
  # -----------------------------------
  # Directional persistence
  # -----------------------------------
  sim_df$Directional_Noise <- NA
  for(st in unique(sim_df$Station)){
    idx <- which(sim_df$Station == st)
    n <- length(idx)
    dir_noise <- numeric(n)
    dir_noise[1] <-
      rnorm(
        1,
        mean = 0,
        sd = sim_df$Direction_Variability[idx[1]]
      )
    for(i in 2:n){
      persistence <-
        if(freq == "daily"){
          0.85
        } else if(freq == "monthly"){
          0.60
        } else {
          0.30
        }
      dir_noise[i] <-
        persistence * dir_noise[i - 1] +
        rnorm(1, mean = 0, sd = sim_df$Direction_Variability[idx[i]])
    }
    sim_df$Directional_Noise[idx] <- dir_noise
  }
  # -----------------------------------
  # Extreme directional shifts
  # -----------------------------------
  sim_df$Extreme_Shift <- 0
  extreme_idx <- which(runif(nrow(sim_df)) < extreme_shift_prob)
  if(length(extreme_idx) > 0){
    sim_df$Directional_Noise[extreme_idx] <-
      sim_df$Directional_Noise[extreme_idx] +
      runif(length(extreme_idx), -max_shift, max_shift)
    sim_df$Extreme_Shift[extreme_idx] <- 1
  }
  # -----------------------------------
  # Final wind direction
  # -----------------------------------
  sim_df$WindDirection <-
    sim_df$Prevailing_Direction +
    sim_df$Seasonal_Component +
    sim_df$Coastal_Component +
    sim_df$Directional_Noise
  # -----------------------------------
  # Convert to 0–360°
  # -----------------------------------
  sim_df$WindDirection <- sim_df$WindDirection %% 360
  # -----------------------------------
  # Wind sectors
  # -----------------------------------
  sector_breaks <- c(
    0, 22.5, 67.5, 112.5,
    157.5, 202.5, 247.5,
    292.5, 337.5, 360
  )
  sector_labels <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N")
  sim_df$WindSector <-
    as.character(
      cut(sim_df$WindDirection,
        breaks = sector_breaks,
        labels = sector_labels,
        include.lowest = TRUE,
        right = FALSE
      )
    )
  sim_df$WindSector[sim_df$WindDirection >= 337.5] <- "N"
  # -----------------------------------
  # Wind vector components
  # -----------------------------------
  theta <- sim_df$WindDirection * pi / 180
  sim_df$Wind_u <- -sim_df$WindSpeed * sin(theta)
  sim_df$Wind_v <- -sim_df$WindSpeed * cos(theta)
  # -----------------------------------
  # Rounding
  # -----------------------------------
  sim_df$WindDirection <- round(sim_df$WindDirection, 2)
  sim_df$WindSpeed <- round(sim_df$WindSpeed, 2)
  sim_df$Wind_u <- round(sim_df$Wind_u, 2)
  sim_df$Wind_v <- round(sim_df$Wind_v, 2)
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
    "WindDirection",
    "WindSector",
    "Prevailing_Direction",
    "Direction_Variability",
    "Extreme_Shift",
    "Wind_u",
    "Wind_v"
  )
  sim_df <- sim_df[, keep_cols]
  message(
    "Wind direction simulation complete for ",
    length(unique(sim_df$Station)),
    " stations."
  )
  return(sim_df)
}
