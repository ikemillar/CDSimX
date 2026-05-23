# =========================================================
# COPULA-BASED DEPENDENCE MODELING
# =========================================================

#' Apply Copula Dependence Structure to Climate Variables
#'
#' Introduces multivariate dependence among simulated climate
#' variables using Gaussian or t copulas.
#'
#' This function improves realism by preserving correlations
#' and inter-variable dependency structures commonly observed
#' in climate systems.
#'
#' Supported variables include:
#' \itemize{
#'   \item Tmin
#'   \item Tmax
#'   \item Avg.Temp
#'   \item Rainfall
#'   \item RH
#'   \item WindSpeed
#'   \item Solar_Radiation
#'   \item ET0
#' }
#'
#' @param climate_data Climate data frame.
#'
#' @param variables Variables used in dependence modeling.
#'
#' @param copula_type Type of copula.
#' Options are:
#' \code{"gaussian"} or \code{"t"}.
#'
#' @param df Degrees of freedom for t copula.
#' Default is 4.
#'
#' @param seed Random seed for reproducibility.
#'
#' @param digits Number of decimal places.
#'
#' @return A list containing:
#'
#' \describe{
#'
#' \item{adjusted_data}{
#' Climate dataset with copula-adjusted dependence structure.
#' }
#'
#' \item{correlation_matrix}{
#' Empirical correlation matrix used in copula fitting.
#' }
#'
#' \item{copula_type}{
#' Copula family used.
#' }
#'
#' }
#'
#' @examples
#' \dontrun{
#'
#' cp <- copula_dependence(cd)
#'
#' head(cp$adjusted_data)
#'
#' }
#'
#' @export

copula_dependence <- function(
    climate_data,
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
    copula_type = "gaussian",
    df = 4,
    seed = 123,
    digits = 2
){
  # -------------------------------------------------------
  # Required package
  # -------------------------------------------------------
  if(!requireNamespace("copula", quietly = TRUE)){
    stop("Package 'copula' is required but not installed.")
  }
  # -------------------------------------------------------
  # Input checks
  # -------------------------------------------------------
  if(!is.data.frame(climate_data)){
    stop("climate_data must be a data.frame.")
  }
  # -------------------------------------------------------
  # Select variables
  # -------------------------------------------------------
  vars <- variables[variables %in% names(climate_data)]
  if(length(vars) < 2){
    stop("At least two variables are required.")
  }
  # -------------------------------------------------------
  # Extract numeric matrix
  # -------------------------------------------------------
  X <- climate_data[, vars]
  X <- as.data.frame(X)
  # -------------------------------------------------------
  # Remove missing rows
  # -------------------------------------------------------
  complete_rows <- complete.cases(X)
  X_complete <- X[complete_rows, ]
  # -------------------------------------------------------
  # Rank transformation
  # -------------------------------------------------------
  U <- apply(X_complete, 2,
    function(x)
      rank(x) / (length(x) + 1)
  )
  # -------------------------------------------------------
  # Correlation matrix
  # -------------------------------------------------------
  corr_matrix <- cor(X_complete)
  # -------------------------------------------------------
  # Set seed
  # -------------------------------------------------------
  set.seed(seed)
  # -------------------------------------------------------
  # Build copula
  # -------------------------------------------------------
  d <- ncol(U)

  if(copula_type == "gaussian"){
    cop <- copula::normalCopula(
      param = corr_matrix[lower.tri(corr_matrix)], dim = d, dispstr = "un"
    )
  } else if(copula_type == "t"){
    cop <- copula::tCopula(
      param =
        corr_matrix[
          lower.tri(corr_matrix)
        ],
      dim = d,
      dispstr = "un",
      df = df
    )
  } else {
    stop("copula_type must be ", "'gaussian' or 't'.")
  }
  # -------------------------------------------------------
  # Simulate dependent uniforms
  # -------------------------------------------------------
  U_sim <- copula::rCopula(nrow(X_complete), cop)
  # -------------------------------------------------------
  # Transform back to original distributions
  # -------------------------------------------------------
  adjusted <- X_complete
  for(i in seq_along(vars)){
    adjusted[, i] <-
      quantile(
        X_complete[, i],
        probs = U_sim[, i],
        na.rm = TRUE,
        type = 8
      )
  }

  # -------------------------------------------------------
  # Insert adjusted variables back
  # -------------------------------------------------------
  adjusted_data <- climate_data
  adjusted_data[complete_rows, vars] <- round(adjusted, digits)
  # -------------------------------------------------------
  # Output
  # -------------------------------------------------------
  output <- list(
    adjusted_data = adjusted_data,
    correlation_matrix = round(corr_matrix, digits),
    copula_type = copula_type
  )
  # -------------------------------------------------------
  # Console message
  # -------------------------------------------------------
  cat(
    "Copula dependence modeling complete using",
    copula_type,
    "copula.\n"
  )
  return(output)
}
