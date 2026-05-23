#' Simulate Evapotranspiration
#'
#' Simulates reference evapotranspiration (ET0)
#' using temperature, relative humidity,
#' solar radiation, and wind speed.
#'
#' The function incorporates:
#' - temperature-driven evaporation
#' - humidity suppression
#' - solar radiation forcing
#' - aerodynamic wind enhancement
#' - elevation-based pressure adjustment
#' - stochastic environmental variability
#'
#' @param temperature Output from
#' simulate_temperature().
#'
#' @param rh Output from
#' simulate_rh().
#'
#' @param solar_radiation Output from
#' simulate_solar_radiation().
#'
#' @param wind_speed Output from
#' simulate_wind_speed().
#'
#' @param method Method used for
#' evapotranspiration estimation.
#' Currently supports:
#' \describe{
#'   \item{FAO56}{
#'   Simplified FAO-56
#'   Penman-Monteith inspired approach
#'   }
#' }
#' Default is "FAO56".
#'
#' @param crop_factor Numeric scaling
#' factor for evapotranspiration.
#' Default is 1.
#'
#' @param humidity_sensitivity Numeric
#' humidity reduction factor.
#' Default is 0.35.
#'
#' @param wind_sensitivity Numeric
#' aerodynamic enhancement factor.
#' Default is 0.08.
#'
#' @param radiation_sensitivity Numeric
#' solar radiation enhancement factor.
#' Default is 0.12.
#'
#' @param noise_sd Standard deviation
#' for stochastic variability.
#' Default is 0.25.
#'
#' @param min_et Minimum ET0 value.
#' Default is 0.
#'
#' @param max_et Maximum ET0 value.
#' Default is 15.
#'
#' @param seed Optional random seed.
#'
#' @return A data frame containing:
#' \describe{
#'   \item{Station}{Station identifier}
#'   \item{LON}{Longitude}
#'   \item{LAT}{Latitude}
#'   \item{ELEV}{Elevation (m)}
#'   \item{DATE}{Date}
#'   \item{Year}{Year}
#'   \item{Month}{Month}
#'   \item{Season}{Season category}
#'   \item{Avg.Temp}{Average temperature (°C)}
#'   \item{RH}{Relative humidity (%)}
#'   \item{Solar_Radiation}{Solar radiation (MJ/m²/day)}
#'   \item{WindSpeed}{Wind speed (m/s)}
#'   \item{Atmospheric_Pressure}{Estimated atmospheric pressure (kPa)}
#'   \item{VPD}{Vapour pressure deficit (kPa)}
#'   \item{ET0}{Reference evapotranspiration (mm/day)}
#'   \item{Dryness_Index}{Normalized dryness indicator}
#'   \item{Dryness_Class}{Categorical atmospheric moisture condition derived
#'   from the dryness index.Classes include: Humid, Moderate, Dry, and Very Dry}
#'   \item{ET_Anomaly}{Evapotranspiration anomaly}
#' }
#'
#' @examples
#' \dontrun{
#' temp <- simulate_temperature(stations, tindex_month)
#' rh   <- simulate_rh(stations, tindex_month)
#' sr   <- simulate_solar_radiation(stations, tindex_month)
#' ws   <- simulate_wind_speed(stations, tindex_month)
#'
#' et <- simulate_evapotranspiration(
#'   temperature = temp,
#'   rh = rh,
#'   solar_radiation = sr,
#'   wind_speed = ws
#' )
#' }
#'
#' @export

simulate_evapotranspiration <- function(
    temperature,
    rh,
    solar_radiation,
    wind_speed,
    method = "FAO56",
    crop_factor = 1,
    humidity_sensitivity = 0.35,
    wind_sensitivity = 0.08,
    radiation_sensitivity = 0.12,
    noise_sd = 0.25,
    min_et = 0,
    max_et = 15,
    seed = NULL
){
  # -----------------------------------
  # Validation
  # -----------------------------------
  if(missing(temperature)){
    stop(
      "temperature is required. ",
      "Use simulate_temperature()."
    )
  }
  if(missing(rh)){
    stop(
      "rh is required. ",
      "Use simulate_rh()."
    )
  }
  if(missing(solar_radiation)){
    stop(
      "solar_radiation is required. ",
      "Use simulate_solar_radiation()."
    )
  }
  if(missing(wind_speed)){
    stop(
      "wind_speed is required. ",
      "Use simulate_wind_speed()."
    )
  }
  valid_methods <- c("FAO56")
  if(!method %in% valid_methods){
    stop("method must be one of: ",
      paste(valid_methods, collapse = ", ")
    )
  }
  if(!is.null(seed)) set.seed(seed)
  # -----------------------------------
  # Merge datasets
  # -----------------------------------
  sim_df <- merge(
    temperature,
    rh[, c("Station", "DATE", "RH")],
    by = c("Station", "DATE")
  )
  sim_df <- merge(
    sim_df,
    solar_radiation[, c(
      "Station",
      "DATE",
      "Solar_Radiation"
    )],
    by = c("Station", "DATE")
  )
  sim_df <- merge(
    sim_df,
    wind_speed[, c(
      "Station",
      "DATE",
      "WindSpeed"
    )],
    by = c("Station", "DATE")
  )
  sim_df <- sim_df[ order(sim_df$Station, sim_df$DATE), ]
  # -----------------------------------
  # Atmospheric pressure
  # -----------------------------------
  sim_df$Atmospheric_Pressure <-
    101.3 *
    (
      (
        293 - 0.0065 * sim_df$ELEV
      ) / 293
    )^5.26
  # -----------------------------------
  # Saturation vapor pressure
  # -----------------------------------
  es <- 0.6108 * exp((17.27 * sim_df$Avg.Temp) /
        (sim_df$Avg.Temp + 237.3))
  # -----------------------------------
  # Actual vapor pressure
  # -----------------------------------
  ea <- es * (sim_df$RH / 100)
  # -----------------------------------
  # Vapour pressure deficit
  # -----------------------------------
  sim_df$VPD <- pmax(es - ea, 0)
  # -----------------------------------
  # Radiation contribution
  # -----------------------------------
  radiation_component <- sim_df$Solar_Radiation * radiation_sensitivity
  # -----------------------------------
  # Wind contribution
  # -----------------------------------
  wind_component <- sim_df$WindSpeed * wind_sensitivity
  # -----------------------------------
  # Humidity suppression
  # -----------------------------------
  humidity_component <- (1 - sim_df$RH / 100) * humidity_sensitivity
  # -----------------------------------
  # Temperature contribution
  # -----------------------------------
  temp_component <- 0.15 * sim_df$Avg.Temp
  # -----------------------------------
  # Random environmental variability
  # -----------------------------------
  noise <-
    rnorm(
      nrow(sim_df),
      mean = 0,
      sd = noise_sd
    )
  # -----------------------------------
  # Compute ET0
  # -----------------------------------
  if(method == "FAO56"){
    sim_df$ET0 <- (temp_component + radiation_component + wind_component +
          humidity_component + noise) * crop_factor
  }
  # -----------------------------------
  # Physical constraints
  # -----------------------------------
  sim_df$ET0 <- pmax(min_et, pmin(max_et, sim_df$ET0))
  # -----------------------------------
  # Dryness index
  # -----------------------------------
  #sim_df$Dryness_Index <- sim_df$VPD / (sim_df$Solar_Radiation + 1)
  sim_df$Dryness_Index <- (sim_df$VPD * 10) / (sim_df$Solar_Radiation + 1)
  sim_df$Dryness_Class <-
    cut(sim_df$Dryness_Index, breaks = c(-Inf, 0.15, 0.30, 0.50, Inf),
      labels = c("Humid", "Moderate", "Dry", "Very Dry")
    )
  # -----------------------------------
  # ET anomaly
  # -----------------------------------
  sim_df$ET_Anomaly <- sim_df$ET0 -
    ave(
      sim_df$ET0,
      sim_df$Station,
      sim_df$Month,
      FUN = mean
    )
  # -----------------------------------
  # Rounding
  # -----------------------------------
  sim_df$Atmospheric_Pressure <- round(sim_df$Atmospheric_Pressure, 2)
  sim_df$VPD <- round(sim_df$VPD, 2)
  sim_df$ET0 <- round(sim_df$ET0, 2)
  sim_df$Dryness_Index <- round(sim_df$Dryness_Index, 2)
  sim_df$ET_Anomaly <- round(sim_df$ET_Anomaly, 2)
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
    "Solar_Radiation",
    "WindSpeed",
    "Atmospheric_Pressure",
    "VPD",
    "ET0",
    "Dryness_Index",
    "Dryness_Class",
    "ET_Anomaly"
  )

  sim_df <- sim_df[, keep_cols]
  message(
    "Evapotranspiration simulation complete for ",
    length(unique(sim_df$Station)),
    " stations."
  )
  return(sim_df)

}
