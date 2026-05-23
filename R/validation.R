# =========================================================
# VALIDATE CLIMATE DATASET
# =========================================================

#' Validate Simulated Climate Dataset
#'
#' Performs statistical and physical validation checks on a
#' simulated climate dataset generated using
#' \code{simulate_climate()}.
#'
#' The function computes:
#' \itemize{
#'   \item Descriptive statistics
#'   \item Missing value diagnostics
#'   \item Physical consistency checks
#'   \item Correlation structure
#'   \item Seasonal summaries
#'   \item Station summaries
#'   \item Extreme-event frequencies
#'   \item Validation summary metrics
#' }
#'
#' @param climate_data Data frame generated from
#'   \code{simulate_climate()}.
#'
#' @param digits Number of decimal places for summaries.
#'   Default is 2.
#'
#' @param return_data Logical. If TRUE, returns all validation
#'   outputs as a list. Default is TRUE.
#'
#' @return A list containing:
#'
#' \describe{
#'
#' \item{summary_statistics}{
#' Descriptive statistics for all numeric climate variables.
#' }
#'
#' \item{missing_values}{
#' Count and percentage of missing values.
#' }
#'
#' \item{physical_checks}{
#' Number of physical inconsistencies detected.
#' }
#'
#' \item{correlation_matrix}{
#' Correlation matrix among major climate variables.
#' }
#'
#' \item{seasonal_summary}{
#' Mean climate conditions by season.
#' }
#'
#' \item{station_summary}{
#' Mean climate conditions by station.
#' }
#'
#' \item{extreme_summary}{
#' Frequency of extreme rainfall and wind events.
#' }
#'
#' \item{validation_summary}{
#' Overall validation metrics for the climate dataset.
#' }
#'
#' }
#'
#' @examples
#' \dontrun{
#'
#' cd <- simulate_climate(stations, tindex_month)
#'
#' vc <- validate_climate(cd)
#'
#' vc$summary_statistics
#' vc$correlation_matrix
#' vc$physical_checks
#' vc$validation_summary
#'
#' }
#'
#' @export

validate_climate <- function(climate_data, digits = 2, return_data = TRUE){
  # -------------------------------------------------------
  # Check input
  # -------------------------------------------------------
  if(!is.data.frame(climate_data)){
    stop("climate_data must be a data.frame.")
  }

  # -------------------------------------------------------
  # Numeric variables
  # -------------------------------------------------------
  numeric_vars <- names(
    climate_data[sapply(climate_data, is.numeric)]
  )
  # -------------------------------------------------------
  # Summary statistics
  # -------------------------------------------------------
  summary_statistics <- data.frame(
    Variable = numeric_vars,
    Mean = NA,
    SD = NA,
    Min = NA,
    Max = NA,
    Skewness = NA
  )

  for(i in seq_along(numeric_vars)){
    x <- climate_data[[numeric_vars[i]]]
    x <- x[is.finite(x)]
    summary_statistics$Mean[i] <- mean(x)
    summary_statistics$SD[i] <- sd(x)
    summary_statistics$Min[i] <- min(x)
    summary_statistics$Max[i] <- max(x)

    # Skewness
    summary_statistics$Skewness[i] <-
      mean(((x - mean(x)) / sd(x))^3)
  }

  summary_statistics[, -1] <-
    round(summary_statistics[, -1], digits)

  # -------------------------------------------------------
  # Missing values
  # -------------------------------------------------------
  missing_values <- data.frame(
    Variable = names(climate_data),
    Missing_Count = sapply(
      climate_data,
      function(x) sum(is.na(x))
    )
  )

  missing_values$Missing_Percent <-
    round(
      100 *
        missing_values$Missing_Count /
        nrow(climate_data),
      digits
    )

  # -------------------------------------------------------
  # Physical consistency checks
  # -------------------------------------------------------
  physical_checks <- list()
  # Temperature consistency
  if(all(c("Tmin", "Avg.Temp", "Tmax") %in% names(climate_data))){
    physical_checks$Temperature_Inconsistency <-
      sum(
        climate_data$Tmin > climate_data$Avg.Temp |
          climate_data$Avg.Temp > climate_data$Tmax,
        na.rm = TRUE
      )
  }

  # RH bounds
  if("RH" %in% names(climate_data)){
    physical_checks$RH_Out_Of_Bounds <-
      sum(climate_data$RH < 0 | climate_data$RH > 100, na.rm = TRUE)
  }

  # Rainfall bounds
  if("Rainfall" %in% names(climate_data)){
    physical_checks$Negative_Rainfall <-
      sum(climate_data$Rainfall < 0, na.rm = TRUE)
  }
  # Wind speed bounds
  if("WindSpeed" %in% names(climate_data)){
    physical_checks$Negative_WindSpeed <-
      sum(climate_data$WindSpeed < 0, na.rm = TRUE)
  }
  # Wind direction bounds
  if("WindDirection" %in% names(climate_data)){
    physical_checks$Invalid_WindDirection <-
      sum(
        climate_data$WindDirection < 0 |
          climate_data$WindDirection > 360,
        na.rm = TRUE
      )
  }
  # Dew point consistency
  if(all(c("DewPoint", "Avg.Temp") %in% names(climate_data))){
    physical_checks$Invalid_DewPoint <-
      sum(
        climate_data$DewPoint >
          climate_data$Avg.Temp,
        na.rm = TRUE
      )
  }
  # Sunshine fraction
  if("Sunshine_Fraction" %in% names(climate_data)){
    physical_checks$Invalid_SunshineFraction <-
      sum(
        climate_data$Sunshine_Fraction < 0 |
          climate_data$Sunshine_Fraction > 1,
        na.rm = TRUE
      )
  }

  # ET0
  if("ET0" %in% names(climate_data)){
    physical_checks$Negative_ET0 <-
      sum(
        climate_data$ET0 < 0,
        na.rm = TRUE
      )
  }

  # -------------------------------------------------------
  # Correlation matrix
  # -------------------------------------------------------
  corr_vars <- c(
    "Tmin",
    "Tmax",
    "Avg.Temp",
    "Rainfall",
    "RH",
    "DewPoint",
    "WindSpeed",
    "Solar_Radiation",
    "ET0"
  )
  corr_vars <- corr_vars[corr_vars %in% names(climate_data)]
  correlation_matrix <-
    round(
      cor(
        climate_data[, corr_vars],
        use = "pairwise.complete.obs"
      ),
      digits
    )

  # -------------------------------------------------------
  # Seasonal summary
  # -------------------------------------------------------
  seasonal_summary <- NULL

  if("Season" %in% names(climate_data)){
    seasonal_summary <-
      aggregate(
        climate_data[, corr_vars],
        by = list(
          Season = climate_data$Season
        ),
        FUN = mean,
        na.rm = TRUE
      )
    seasonal_summary[, -1] <-
      round(
        seasonal_summary[, -1],
        digits
      )
  }

  # -------------------------------------------------------
  # Station summary
  # -------------------------------------------------------
  station_summary <- NULL
  if("Station" %in% names(climate_data)){
    station_summary <-
      aggregate(
        climate_data[, corr_vars],
        by = list(
          Station = climate_data$Station
        ),
        FUN = mean,
        na.rm = TRUE
      )
    station_summary[, -1] <- round(station_summary[, -1], digits)
  }

  # -------------------------------------------------------
  # Extreme-event summary
  # -------------------------------------------------------
  extreme_summary <- data.frame(
    Metric = character(),
    Value = numeric(),
    stringsAsFactors = FALSE
  )

  # Rainfall extremes
  if("Extreme_Event" %in%
     names(climate_data)){

    extreme_summary <- rbind(
      extreme_summary,
      data.frame(
        Metric = "Extreme_Rainfall_Events",
        Value = sum(
          climate_data$Extreme_Event,
          na.rm = TRUE
        )
      )
    )
  }

  # Wind extremes
  if("Extreme_Wind" %in%
     names(climate_data)){

    extreme_summary <- rbind(
      extreme_summary,
      data.frame(
        Metric = "Extreme_Wind_Events",
        Value = sum(
          climate_data$Extreme_Wind,
          na.rm = TRUE
        )
      )
    )
  }

  # -------------------------------------------------------
  # Validation summary
  # -------------------------------------------------------
  validation_summary <- data.frame(
    Metric = character(),
    Value = numeric(),
    stringsAsFactors = FALSE
  )
  # Mean Tmin
  if("Tmin" %in% names(climate_data)){
    validation_summary <- rbind(
      validation_summary,
      data.frame(
        Metric = "Mean_Tmin",
        Value = round(
          mean(
            climate_data$Tmin,
            na.rm = TRUE
          ),
          digits
        )
      )
    )
  }
  # Mean Tmax
  if("Tmax" %in% names(climate_data)){
    validation_summary <- rbind(
      validation_summary,
      data.frame(
        Metric = "Mean_Tmax",
        Value = round(
          mean(
            climate_data$Tmax,
            na.rm = TRUE
          ),
          digits
        )
      )
    )
  }
  # Mean temperature
  if("Avg.Temp" %in% names(climate_data)){
    validation_summary <- rbind(
      validation_summary,
      data.frame(
        Metric = "Mean_Temperature",
        Value = round(
          mean(
            climate_data$Avg.Temp,
            na.rm = TRUE
          ),
          digits
        )
      )
    )
  }
  # Mean rainfall
  if("Rainfall" %in% names(climate_data)){
    validation_summary <- rbind(
      validation_summary,
      data.frame(
        Metric = "Mean_Rainfall",
        Value = round(
          mean(
            climate_data$Rainfall,
            na.rm = TRUE
          ),
          digits
        )
      )
    )
  }
  # Mean RH
  if("RH" %in% names(climate_data)){
    validation_summary <- rbind(
      validation_summary,
      data.frame(
        Metric = "Mean_RH",
        Value = round(
          mean(
            climate_data$RH,
            na.rm = TRUE
          ),
          digits
        )
      )
    )
  }
  # Mean wind speed
  if("WindSpeed" %in% names(climate_data)){
    validation_summary <- rbind(
      validation_summary,
      data.frame(
        Metric = "Mean_WindSpeed",
        Value = round(
          mean(
            climate_data$WindSpeed,
            na.rm = TRUE
          ),
          digits
        )
      )
    )
  }
  # Mean solar radiation
  if("Solar_Radiation" %in%
     names(climate_data)){
    validation_summary <- rbind(
      validation_summary,
      data.frame(
        Metric = "Mean_SolarRadiation",
        Value = round(
          mean(
            climate_data$Solar_Radiation,
            na.rm = TRUE
          ),
          digits
        )
      )
    )
  }

  # Mean ET0
  if("ET0" %in% names(climate_data)){
    validation_summary <- rbind(
      validation_summary,
      data.frame(
        Metric = "Mean_ET0",
        Value = round(
          mean(
            climate_data$ET0,
            na.rm = TRUE
          ),
          digits
        )
      )
    )
  }

  # -------------------------------------------------------
  # Validation output
  # -------------------------------------------------------
  validation_output <- list(
    summary_statistics = summary_statistics,
    missing_values = missing_values,
    physical_checks = physical_checks,
    correlation_matrix = correlation_matrix,
    seasonal_summary = seasonal_summary,
    station_summary = station_summary,
    extreme_summary = extreme_summary,
    validation_summary = validation_summary
  )
  # -------------------------------------------------------
  # Console message
  # -------------------------------------------------------
  cat(
    "Climate dataset validation complete.\n"
  )
  # -------------------------------------------------------
  # Return
  # -------------------------------------------------------
  if(return_data){
    return(validation_output)
  }
}
