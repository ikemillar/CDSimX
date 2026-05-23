#' Apply Physical Constraints to Climate Data
#'
#' Enforces physically realistic bounds and relationships
#' across simulated climate variables.
#'
#' This function is designed as a quality-control layer
#' for synthetic climate simulations and ensures that
#' impossible atmospheric or hydrological states are removed.
#'
#' The function automatically checks and corrects:
#'
#' \itemize{
#'   \item Tmin <= Tmax
#'   \item Avg.Temp lies between Tmin and Tmax
#'   \item Relative humidity remains within 0–100%
#'   \item Dew point does not exceed air temperature
#'   \item Rainfall and rain days remain non-negative
#'   \item Wind speed remains non-negative
#'   \item Solar radiation remains non-negative
#'   \item Evapotranspiration remains non-negative
#'   \item Vapor pressure deficit remains non-negative
#'   \item Sunshine fraction remains within 0–1
#'   \item Cloud factor remains within 0–1
#' }
#'
#' @param climate_data A dataframe generated from
#'   \code{simulate_climate()} or related simulation functions.
#'
#' @param verbose Logical. If TRUE, prints correction summaries.
#'   Default is TRUE.
#'
#' @param tolerance Numeric tolerance used when comparing
#'   floating-point values. Default is 0.
#'
#' @return A corrected climate dataframe with physically
#'   realistic values enforced.
#'
#' @examples
#' stations <- create_stations(n = 3)
#' time_index <- generate_time_index(
#'   start_date = "2010-01-01",
#'   end_date = "2012-12-31"
#' )
#'
#' climate <- simulate_climate(
#'   stations = stations,
#'   time_index = time_index
#' )
#'
#' climate <- apply_physical_constraints(climate)
#'
#' @export

apply_physical_constraints <- function(
    climate_data,
    verbose = TRUE,
    tolerance = 0
){

  # -----------------------------------
  # Validation
  # -----------------------------------

  if(missing(climate_data)){

    stop(
      "climate_data is required."
    )

  }

  if(!is.data.frame(climate_data)){

    stop(
      "climate_data must be a dataframe."
    )

  }

  sim_df <- climate_data

  correction_count <- 0

  # -----------------------------------
  # Tmin <= Tmax
  # -----------------------------------

  if(all(c("Tmin", "Tmax") %in% names(sim_df))){

    bad_idx <- which(
      sim_df$Tmin >
        sim_df$Tmax + tolerance
    )

    if(length(bad_idx) > 0){

      temp_swap <- sim_df$Tmin[bad_idx]

      sim_df$Tmin[bad_idx] <-
        sim_df$Tmax[bad_idx]

      sim_df$Tmax[bad_idx] <-
        temp_swap

      correction_count <-
        correction_count + length(bad_idx)

    }

  }

  # -----------------------------------
  # Avg.Temp bounds
  # -----------------------------------

  if(all(c(
    "Avg.Temp",
    "Tmin",
    "Tmax"
  ) %in% names(sim_df))){

    sim_df$Avg.Temp <-

      pmax(
        sim_df$Tmin,

        pmin(
          sim_df$Avg.Temp,
          sim_df$Tmax
        )
      )

  }

  # -----------------------------------
  # Relative Humidity
  # -----------------------------------

  if("RH" %in% names(sim_df)){

    before <- sim_df$RH

    sim_df$RH <-

      pmin(
        pmax(sim_df$RH, 0),
        100
      )

    correction_count <-
      correction_count +
      sum(before != sim_df$RH, na.rm = TRUE)

  }

  # -----------------------------------
  # Dew Point <= Avg.Temp
  # -----------------------------------

  if(all(c(
    "DewPoint",
    "Avg.Temp"
  ) %in% names(sim_df))){

    before <- sim_df$DewPoint

    sim_df$DewPoint <-

      pmin(
        sim_df$DewPoint,
        sim_df$Avg.Temp
      )

    correction_count <-
      correction_count +
      sum(before != sim_df$DewPoint, na.rm = TRUE)

  }

  # -----------------------------------
  # Rainfall >= 0
  # -----------------------------------

  if("Rainfall" %in% names(sim_df)){

    before <- sim_df$Rainfall

    sim_df$Rainfall <-
      pmax(sim_df$Rainfall, 0)

    correction_count <-
      correction_count +
      sum(before != sim_df$Rainfall, na.rm = TRUE)

  }

  # -----------------------------------
  # Rain_Days >= 0
  # -----------------------------------

  if("Rain_Days" %in% names(sim_df)){

    before <- sim_df$Rain_Days

    sim_df$Rain_Days <-

      round(
        pmax(sim_df$Rain_Days, 0)
      )

    correction_count <-
      correction_count +
      sum(before != sim_df$Rain_Days, na.rm = TRUE)

  }

  # -----------------------------------
  # WindSpeed >= 0
  # -----------------------------------

  if("WindSpeed" %in% names(sim_df)){

    before <- sim_df$WindSpeed

    sim_df$WindSpeed <-

      pmax(sim_df$WindSpeed, 0)

    correction_count <-
      correction_count +
      sum(before != sim_df$WindSpeed, na.rm = TRUE)

  }

  # -----------------------------------
  # Solar Radiation >= 0
  # -----------------------------------

  if("Solar_Radiation" %in% names(sim_df)){

    before <- sim_df$Solar_Radiation

    sim_df$Solar_Radiation <-

      pmax(sim_df$Solar_Radiation, 0)

    correction_count <-
      correction_count +
      sum(before != sim_df$Solar_Radiation, na.rm = TRUE)

  }

  # -----------------------------------
  # ET0 >= 0
  # -----------------------------------

  if("ET0" %in% names(sim_df)){

    before <- sim_df$ET0

    sim_df$ET0 <-

      pmax(sim_df$ET0, 0)

    correction_count <-
      correction_count +
      sum(before != sim_df$ET0, na.rm = TRUE)

  }

  # -----------------------------------
  # VPD >= 0
  # -----------------------------------

  if("VPD" %in% names(sim_df)){

    before <- sim_df$VPD

    sim_df$VPD <-

      pmax(sim_df$VPD, 0)

    correction_count <-
      correction_count +
      sum(before != sim_df$VPD, na.rm = TRUE)

  }

  # -----------------------------------
  # Sunshine Fraction [0,1]
  # -----------------------------------

  if("Sunshine_Fraction" %in% names(sim_df)){

    before <- sim_df$Sunshine_Fraction

    sim_df$Sunshine_Fraction <-

      pmin(
        pmax(
          sim_df$Sunshine_Fraction,
          0
        ),
        1
      )

    correction_count <-
      correction_count +
      sum(
        before != sim_df$Sunshine_Fraction,
        na.rm = TRUE
      )

  }

  # -----------------------------------
  # Cloud Factor [0,1]
  # -----------------------------------

  if("Cloud_Factor" %in% names(sim_df)){

    before <- sim_df$Cloud_Factor

    sim_df$Cloud_Factor <-

      pmin(
        pmax(
          sim_df$Cloud_Factor,
          0
        ),
        1
      )

    correction_count <-
      correction_count +
      sum(
        before != sim_df$Cloud_Factor,
        na.rm = TRUE
      )

  }

  # -----------------------------------
  # Recompute DTR
  # -----------------------------------

  if(all(c(
    "Tmin",
    "Tmax"
  ) %in% names(sim_df))){

    sim_df$DTR <-

      round(
        sim_df$Tmax -
          sim_df$Tmin,
        2
      )

  }

  # -----------------------------------
  # Recompute Dewpoint Depression
  # -----------------------------------

  if(all(c(
    "Avg.Temp",
    "DewPoint"
  ) %in% names(sim_df))){

    sim_df$Dewpoint_Depression <-

      round(
        sim_df$Avg.Temp -
          sim_df$DewPoint,
        2
      )

  }

  # -----------------------------------
  # Messages
  # -----------------------------------

  if(verbose){

    message(
      "Physical constraints applied. ",
      correction_count,
      " corrections made."
    )

  }

  return(sim_df)

}
