#' Simulate Solar Radiation Time Series
#'
#' Generates synthetic incoming solar radiation series
#' for multiple stations using physically consistent
#' astronomical and atmospheric controls.
#'
#' The simulation incorporates:
#' - annual solar cycle,
#' - latitude-dependent extraterrestrial radiation,
#' - cloud/rainfall attenuation,
#' - humidity effects,
#' - elevation enhancement,
#' - seasonal variability,
#' - stochastic atmospheric variability,
#' - physically bounded radiation values.
#'
#' Solar radiation is simulated as:
#'
#' - daily total radiation (MJ/m²/day) for daily simulations
#' - monthly mean radiation for monthly simulations
#' - yearly mean radiation for yearly simulations
#'
#' @param stations data.frame from create_stations()
#'
#' @param time_index data.frame from generate_time_index()
#'
#' @param rainfall Optional numeric vector.
#' Rainfall series from simulate_rainfall().
#' Used for cloud attenuation effects.
#'
#' @param rh Optional numeric vector.
#' Relative humidity series from simulate_rh().
#' Used for atmospheric moisture attenuation.
#'
#' @param atmospheric_transmissivity Numeric.
#' Baseline atmospheric transmissivity coefficient.
#' Default = 0.65
#'
#' @param cloud_attenuation Numeric.
#' Radiation reduction factor from rainfall/cloudiness.
#' Default = 0.12
#'
#' @param humidity_attenuation Numeric.
#' Radiation reduction factor from RH.
#' Default = 0.08
#'
#' @param elevation_factor Numeric.
#' Radiation increase per meter elevation.
#' Default = 0.00012
#'
#' @param seasonal_strength Numeric.
#' Controls annual radiation seasonality.
#' Default = 1
#'
#' @param noise_sd Numeric.
#' Standard deviation of stochastic variability.
#' Default = 1.5
#'
#' @param min_radiation Numeric.
#' Minimum allowable solar radiation.
#' Default = 0
#'
#' @param max_radiation Numeric.
#' Maximum allowable solar radiation.
#' Default = 35
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
#'   \item{Solar_Radiation}{Simulated solar radiation (MJ/m²/day)}
#'   \item{Clear_Sky_Radiation}{Potential clear-sky radiation}
#'   \item{Cloud_Factor}{Cloud attenuation factor}
#'   \item{Sunshine_Fraction}{Fraction of available sunshine reaching the surface}
#'   \item{Radiation_Anomaly}{Solar radiation anomaly}
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
#' rain <- simulate_rainfall(
#'   stations,
#'   time_index
#' )
#'
#' rh <- simulate_rh(
#'   stations,
#'   time_index
#' )
#'
#' solar <- simulate_solar_radiation(
#'   stations = stations,
#'   time_index = time_index,
#'   rainfall = rain$Rainfall,
#'   rh = rh$RH,
#'   seed = 123
#' )
#'
#' head(solar)
#'
#' @export

simulate_solar_radiation <- function(
    stations,
    time_index,
    rainfall = NULL,
    rh = NULL,
    atmospheric_transmissivity = 0.65,
    cloud_attenuation = 0.12,
    humidity_attenuation = 0.08,
    elevation_factor = 0.00012,
    seasonal_strength = 1,
    noise_sd = 1.5,
    min_radiation = 0,
    max_radiation = 35,
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
    "CLIMATE_ZONE"
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
  # Day-of-year handling
  # -----------------------------------
  doy <- sim_df$DOY
  # -----------------------------------
  # Solar declination angle
  # -----------------------------------
  declination <- 23.45 * sin(2 * pi * (284 + doy) / 365)
  declination_rad <- declination * pi / 180
  latitude_rad <- sim_df$LAT * pi / 180
  # -----------------------------------
  # Sunset hour angle
  # -----------------------------------
  ws <-acos(-tan(latitude_rad) * tan(declination_rad))
  # -----------------------------------
  # Extraterrestrial radiation
  # FAO-56 approximation
  # -----------------------------------
  dr <- 1 + 0.033 * cos(2 * pi * doy / 365)
  Gsc <- 0.0820
  Ra <- (24 * 60 / pi) * Gsc * dr *
    (ws * sin(latitude_rad) * sin(declination_rad) +
        cos(latitude_rad) * cos(declination_rad) * sin(ws)
    )
  sim_df$Extraterrestrial_Radiation <- pmax(Ra, 0)
  # -----------------------------------
  # Clear-sky radiation
  # -----------------------------------
  sim_df$Clear_Sky_Radiation <- sim_df$Extraterrestrial_Radiation *
    atmospheric_transmissivity * (1 + elevation_factor * sim_df$ELEV)
  # -----------------------------------
  # Seasonal modulation
  # -----------------------------------
  sim_df$Seasonal_Component <- seasonal_strength * sin(2 * pi * doy / 365)
  sim_df$Seasonal_Component <- seasonal_strength *
    (0.7 * sin(2 * pi * doy / 365) + 0.3 * sin(4 * pi * doy / 365))
  # -----------------------------------
  # Cloud attenuation
  # -----------------------------------
  sim_df$Cloud_Factor <- runif(nrow(sim_df), 0.85, 1)
  if(!is.null(rainfall)){
    if(length(rainfall) != nrow(sim_df)){
      stop("Length of rainfall vector must equal ",
        "number of simulated records."
      )
    }
    sim_df$Rainfall <- rainfall
    sim_df$Cloud_Factor <- sim_df$Cloud_Factor -
      (log1p(sim_df$Rainfall) * cloud_attenuation / 10)
  }
  # -----------------------------------
  # Humidity attenuation
  # -----------------------------------
  if(!is.null(rh)){
    if(length(rh) != nrow(sim_df)){
      stop("Length of rh vector must equal ",
        "number of simulated records.")
    }
    sim_df$RH <- rh
    sim_df$Cloud_Factor <- sim_df$Cloud_Factor -
      ((sim_df$RH / 100) * humidity_attenuation)
  }
  # -----------------------------------
  # Ensure physical limits
  # -----------------------------------
  sim_df$Cloud_Factor <- pmin(pmax(sim_df$Cloud_Factor, 0.2), 1)
  # -----------------------------------
  # Sunshine fraction
  # -----------------------------------
  sim_df$Sunshine_Fraction <- sim_df$Cloud_Factor + rnorm(nrow(sim_df), 0, 0.03)
  sim_df$Sunshine_Fraction <- pmin(pmax(sim_df$Sunshine_Fraction, 0), 1)
  # -----------------------------------
  # Stochastic atmospheric variability
  # -----------------------------------
  sim_df$Noise <- rnorm(nrow(sim_df), mean = 0, sd = noise_sd)
  # -----------------------------------
  # Generate solar radiation
  # -----------------------------------
  sim_df$Solar_Radiation <- sim_df$Clear_Sky_Radiation * sim_df$Cloud_Factor +
    sim_df$Seasonal_Component + sim_df$Noise
  # -----------------------------------
  # Radiation anomaly
  # -----------------------------------
  sim_df$Radiation_Anomaly <-
    sim_df$Solar_Radiation -
    ave(
      sim_df$Solar_Radiation,
      sim_df$Station,
      sim_df$Month,
      FUN = mean
    )
  # -----------------------------------
  # Rounding
  # -----------------------------------
  numeric_cols <- c(
    "Solar_Radiation",
    "Clear_Sky_Radiation",
    "Cloud_Factor",
    "Radiation_Anomaly"
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
    "Solar_Radiation",
    "Clear_Sky_Radiation",
    "Cloud_Factor",
    "Sunshine_Fraction",
    "Radiation_Anomaly"
  )
  sim_df <- sim_df[, keep_cols]
  message(
    "Solar radiation simulation complete for ",
    length(unique(sim_df$Station)),
    " stations."
  )
  return(sim_df)
}
