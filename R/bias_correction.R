# =========================================================
# BIAS CORRECTION OF CLIMATE VARIABLES
# =========================================================
#' Bias Correction for Simulated Climate Data
#'
#' Applies statistical bias correction to simulated climate
#' variables using observed reference data.
#'
#' Supported correction methods include:
#' \itemize{
#'   \item Mean scaling
#'   \item Additive correction
#'   \item Multiplicative correction
#' }
#'
#' Physical constraints are automatically enforced after
#' correction to ensure climatological realism.
#'
#' @param simulated_data Simulated climate data frame.
#'
#' @param observed_data Observed/reference climate data frame.
#'
#' @param variables Character vector of variables to correct.
#'
#' @param method Bias-correction method.
#' Options are:
#' \code{"mean_scaling"},
#' \code{"additive"},
#' \code{"multiplicative"}.
#'
#' @param digits Number of decimal places.
#'
#' @param return_factors Logical. If TRUE, returns correction
#' factors and bias statistics.
#'
#' @return A list containing:
#'
#' \describe{
#'
#' \item{corrected_data}{
#' Bias-corrected climate dataset.
#' }
#'
#' \item{correction_factors}{
#' Bias-correction factors and bias statistics applied to
#' each variable.
#' }
#'
#' }
#'
#' @examples
#' \dontrun{
#'
#' bc <- bias_correction(
#'   simulated_data = cd,
#'   observed_data = obs_data,
#'   variables = c("Rainfall", "Avg.Temp")
#' )
#'
#' head(bc$corrected_data)
#' bc$correction_factors
#'
#' }
#'
#' @export

bias_correction <- function(
    simulated_data,
    observed_data,
    variables = c(
      "Tmin",
      "Tmax",
      "Avg.Temp",
      "Rainfall",
      "RH",
      "WindSpeed",
      "Solar_Radiation",
      "ET0"
    ),
    method = "mean_scaling",
    digits = 2,
    return_factors = TRUE
){

  # -------------------------------------------------------
  # Input checks
  # -------------------------------------------------------
  if(!is.data.frame(simulated_data)){
    stop("simulated_data must be a data.frame.")
  }
  if(!is.data.frame(observed_data)){
    stop("observed_data must be a data.frame.")
  }
  # -------------------------------------------------------
  # Supported methods
  # -------------------------------------------------------
  supported_methods <- c(
    "mean_scaling",
    "additive",
    "multiplicative"
  )
  if(!method %in% supported_methods){
    stop(
      "method must be one of: ",
      paste(supported_methods, collapse = ", ")
    )
  }
  # -------------------------------------------------------
  # Initialize outputs
  # -------------------------------------------------------
  corrected_data <- simulated_data
  correction_factors <- data.frame(
    Variable = character(),
    Simulated_Mean = numeric(),
    Observed_Mean = numeric(),
    Correction_Factor = numeric(),
    Original_Bias = numeric(),
    Corrected_Bias = numeric(),
    stringsAsFactors = FALSE
  )
  # -------------------------------------------------------
  # Bias correction loop
  # -------------------------------------------------------
  for(v in variables){
    if(v %in% names(simulated_data) && v %in% names(observed_data)){
      sim_mean <- mean(simulated_data[[v]], na.rm = TRUE)
      obs_mean <- mean(observed_data[[v]], na.rm = TRUE)
      # ---------------------------------------------------
      # Mean scaling
      # ---------------------------------------------------
      if(method == "mean_scaling"){
        factor <- ifelse(sim_mean == 0, 1, obs_mean / sim_mean)
        corrected_data[[v]] <- simulated_data[[v]] * factor
      }
      # ---------------------------------------------------
      # Additive correction
      # ---------------------------------------------------
      if(method == "additive"){
        factor <- obs_mean - sim_mean
        corrected_data[[v]] <- simulated_data[[v]] + factor
      }
      # ---------------------------------------------------
      # Multiplicative correction
      # ---------------------------------------------------
      if(method == "multiplicative"){
        factor <- ifelse(sim_mean == 0, 1, obs_mean / sim_mean)
        corrected_data[[v]] <- simulated_data[[v]] * factor
      }
      # ---------------------------------------------------
      # Physical constraints
      # ---------------------------------------------------
      if(v == "RH"){
        corrected_data[[v]] <-
          pmin(
            pmax(corrected_data[[v]], 0),
            100
          )
      }
      if(v == "Rainfall"){
        corrected_data[[v]] <-
          pmax(corrected_data[[v]], 0)
      }

      if(v == "WindSpeed"){
        corrected_data[[v]] <-
          pmax(corrected_data[[v]], 0)
      }
      if(v == "Solar_Radiation"){
        corrected_data[[v]] <-
          pmax(corrected_data[[v]], 0)
      }
      if(v == "ET0"){
        corrected_data[[v]] <-
          pmax(corrected_data[[v]], 0)
      }
      # ---------------------------------------------------
      # Bias statistics
      # ---------------------------------------------------
      corrected_bias <-
        mean(corrected_data[[v]], na.rm = TRUE) - obs_mean
      original_bias <- sim_mean - obs_mean

      # ---------------------------------------------------
      # Store correction factors
      # ---------------------------------------------------
      correction_factors <- rbind(
        correction_factors,
        data.frame(
          Variable = v,
          Simulated_Mean = round(sim_mean, digits),
          Observed_Mean = round(obs_mean, digits),
          Correction_Factor = round(factor, digits),
          Original_Bias = round(original_bias, digits),
          Corrected_Bias = round(corrected_bias, digits),
          stringsAsFactors = FALSE
        )
      )
    }
  }
  # -------------------------------------------------------
  # Console message
  # -------------------------------------------------------
  cat(
    "Bias correction complete for",
    nrow(correction_factors),
    "variables.\n"
  )
  # -------------------------------------------------------
  # Return output
  # -------------------------------------------------------
  output <- list(
    corrected_data = corrected_data
  )
  if(return_factors){
    output$correction_factors <- correction_factors
  }
  return(output)
}
