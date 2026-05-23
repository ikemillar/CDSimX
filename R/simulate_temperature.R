#' Simulate Temperature Time Series
#'
#' Generates synthetic Tmin, Tmax, and Tmean climate series
#' for multiple stations using stochastic climate dynamics.
#'
#' The simulation incorporates:
#' - seasonal variability,
#' - autoregressive temporal persistence,
#' - spatial station effects,
#' - climate-zone variability,
#' - coastal moderation,
#' - elevation lapse-rate adjustment,
#' - long-term warming trends,
#' - stochastic climate variability,
#' - physically consistent Tmax > Tmin relationships,
#' - optional rainfall-temperature coupling.
#'
#' @param stations data.frame from create_stations()
#'
#' @param time_index data.frame from generate_time_index()
#'
#' @param ar_coeff Numeric.
#' AR(1) persistence coefficient.
#' Default = 0.7
#'
#' @param seasonal_amplitude Numeric.
#' Baseline annual temperature cycle amplitude.
#' Default = 3
#'
#' @param warming_trend Numeric.
#' Annual warming trend (°C/year).
#' Default = 0.02
#'
#' @param noise_sd Numeric.
#' Standard deviation of stochastic variability.
#' Default = 1
#'
#' @param mean_dtr Numeric.
#' Mean diurnal temperature range (Tmax − Tmin).
#' Default = 6
#'
#' @param rainfall Optional numeric vector.
#' Rainfall values used for rainfall-temperature coupling.
#'
#' @param cooling_factor Numeric.
#' Controls rainfall cooling strength on Tmax.
#' Default = 0.15
#'
#' @param min_tmin Numeric.
#' Minimum allowable Tmin.
#' Default = 10
#'
#' @param max_tmin Numeric.
#' Maximum allowable Tmin.
#' Default = 35
#'
#' @param min_tmax Numeric.
#' Minimum allowable Tmax.
#' Default = 15
#'
#' @param max_tmax Numeric.
#' Maximum allowable Tmax.
#' Default = 45
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
#'   \item{Tmin}{Simulated minimum temperature (°C)}
#'   \item{Tmax}{Simulated maximum temperature (°C)}
#'   \item{Avg.Temp}{Mean temperature (°C)}
#'   \item{DTR}{Diurnal temperature range (°C)}
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
#' temp <- simulate_temperature(
#'   stations = stations,
#'   time_index = time_index,
#'   seed = 123
#' )
#'
#' head(temp)
#'
#' @export

simulate_temperature <- function(
    stations,
    time_index,
    ar_coeff = 0.7,
    seasonal_amplitude = 3,
    warming_trend = 0.02,
    noise_sd = 1,
    mean_dtr = 6,
    rainfall = NULL,
    cooling_factor = 0.15,
    min_tmin = 10,
    max_tmin = 35,
    min_tmax = 15,
    max_tmax = 45,
    seed = NULL
){

  # -----------------------------------
  # Input validation
  # -----------------------------------

  if (missing(stations)) {
    stop(
      "stations is required. ",
      "Use create_stations() first."
    )
  }

  if (missing(time_index)) {
    stop(
      "time_index is required. ",
      "Use generate_time_index() first."
    )
  }

  if (!is.null(seed)) set.seed(seed)

  required_cols <- c(
    "Station",
    "LON",
    "LAT",
    "ELEV",
    "TEMP_BASE",
    "CLIMATE_ZONE",
    "COASTAL_INDEX"
  )

  if (!all(required_cols %in% names(stations))) {
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
  # Frequency handling
  # -----------------------------------

  freq <- unique(sim_df$Frequency)[1]

  if(freq == "daily"){

    cycle_index  <- sim_df$DOY
    cycle_length <- 365
    ar_coeff     <- 0.9

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
  # Seasonal amplitude
  # -----------------------------------

  sim_df$Seasonal_Amplitude <-

    ifelse(
      sim_df$CLIMATE_ZONE == "Savannah",
      4.5,

      ifelse(
        sim_df$CLIMATE_ZONE == "Forest",
        3,
        2
      )
    ) +

    0.15 *
    (
      sim_df$LAT -
        mean(sim_df$LAT)
    ) -

    0.8 * sim_df$COASTAL_INDEX

  # -----------------------------------
  # Seasonal temperature cycle
  # -----------------------------------

  sim_df$Seasonal_Component <-

    sim_df$Seasonal_Amplitude *

    sin(
      2 * pi *
        cycle_index /
        cycle_length
    )

  # -----------------------------------
  # Dynamic DTR by climate zone
  # -----------------------------------

  sim_df$Mean_DTR <-

    ifelse(
      sim_df$CLIMATE_ZONE == "Savannah",

      9,

      ifelse(
        sim_df$CLIMATE_ZONE == "Forest",
        6,
        4
      )
    )

  # -----------------------------------
  # Elevation effects
  # Tmax cools faster than Tmin
  # -----------------------------------

  sim_df$Elevation_Effect_Tmin <-
    -0.0055 * sim_df$ELEV

  sim_df$Elevation_Effect_Tmax <-
    -0.0070 * sim_df$ELEV

  # -----------------------------------
  # Latitude effect
  # -----------------------------------

  sim_df$Latitude_Effect <-

    0.25 *
    (
      sim_df$LAT -
        mean(sim_df$LAT)
    )

  # -----------------------------------
  # Long-term warming trend
  # -----------------------------------

  start_year <- min(sim_df$Year)

  sim_df$Trend_Component <-

    warming_trend *

    (
      sim_df$Year -
        start_year
    )

  # -----------------------------------
  # Base climatology
  # -----------------------------------

  sim_df$Base_Temp <-

    sim_df$TEMP_BASE +

    sim_df$Seasonal_Component +

    sim_df$Latitude_Effect +

    sim_df$Trend_Component

  # -----------------------------------
  # AR(1) stochastic variability
  # -----------------------------------

  sim_df$Noise <- NA

  for (st in unique(sim_df$Station)) {

    idx <- which(sim_df$Station == st)

    n <- length(idx)

    noise <- numeric(n)

    noise[1] <- rnorm(
      1,
      mean = 0,
      sd = noise_sd
    )

    for (i in 2:n) {

      noise[i] <-

        ar_coeff *
        noise[i - 1] +

        rnorm(
          1,
          mean = 0,
          sd = noise_sd
        )
    }

    sim_df$Noise[idx] <- noise
  }

  # -----------------------------------
  # Generate Tmin
  # -----------------------------------

  sim_df$Tmin <-

    sim_df$Base_Temp +

    sim_df$Elevation_Effect_Tmin -

    (sim_df$Mean_DTR / 2) +

    sim_df$Noise

  # -----------------------------------
  # Generate Tmax
  # -----------------------------------

  sim_df$Tmax <-

    sim_df$Base_Temp +

    sim_df$Elevation_Effect_Tmax +

    (sim_df$Mean_DTR / 2) +

    sim_df$Noise +

    rnorm(
      nrow(sim_df),
      mean = 0,
      sd = 0.5
    )

  # -----------------------------------
  # Rainfall-temperature coupling
  # -----------------------------------

  if(!is.null(rainfall)){

    if(length(rainfall) != nrow(sim_df)){

      stop(
        "Length of rainfall vector must equal ",
        "number of simulated records."
      )

    }

    sim_df$Rainfall <- rainfall

    sim_df$Rainfall_Effect <-

      log1p(sim_df$Rainfall) *
      cooling_factor

    sim_df$Tmax <-

      sim_df$Tmax -
      sim_df$Rainfall_Effect

  }

  # -----------------------------------
  # Apply physical bounds
  # -----------------------------------

  sim_df$Tmin <-

    pmax(
      min_tmin,
      pmin(max_tmin, sim_df$Tmin)
    )

  sim_df$Tmax <-

    pmax(
      min_tmax,
      pmin(max_tmax, sim_df$Tmax)
    )

  # -----------------------------------
  # Ensure Tmax > Tmin
  # -----------------------------------

  bad_idx <- which(
    sim_df$Tmax <= sim_df$Tmin
  )

  if(length(bad_idx) > 0){

    sim_df$Tmax[bad_idx] <-

      sim_df$Tmin[bad_idx] +

      runif(
        length(bad_idx),
        1,
        3
      )

  }

  # -----------------------------------
  # Derived variables
  # -----------------------------------

  sim_df$Avg.Temp <-

    (
      sim_df$Tmin +
        sim_df$Tmax
    ) / 2

  sim_df$DTR <-

    sim_df$Tmax -
    sim_df$Tmin

  # -----------------------------------
  # Round outputs
  # -----------------------------------

  numeric_cols <- c(
    "Tmin",
    "Tmax",
    "Avg.Temp",
    "DTR"
  )

  sim_df[numeric_cols] <-

    round(
      sim_df[numeric_cols],
      2
    )

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
    "Tmin",
    "Tmax",
    "Avg.Temp",
    "DTR",
    "CLIMATE_ZONE",
    "COASTAL_INDEX"
  )

  sim_df <- sim_df[, keep_cols]

  message(
    "Temperature simulation complete for ",
    length(unique(sim_df$Station)),
    " stations."
  )

  return(sim_df)

}
