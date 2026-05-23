#' Create or load station metadata
#'
#' Create a station metadata table either by:
#' - loading from a CSV file,
#' - accepting an existing data.frame,
#' - or auto-generating synthetic stations within a bounding box.
#'
#' The function also supports optional derivation of climate-related
#' station attributes used by the CDSimX simulation engine.
#'
#' @param source Path to CSV file OR a data.frame with Station/LON/LAT
#'   OR NULL (to generate synthetic stations).
#' @param n Integer number of stations to generate when source = NULL.
#'   Default = 10.
#' @param bbox Numeric vector:
#'   c(min_lon, max_lon, min_lat, max_lat).
#'   Default approximates Ghana's spatial extent.
#' @param derive_climate Logical. If TRUE, additional climate metadata
#'   are derived for each station. Default = TRUE.
#' @param seed Optional numeric seed for reproducibility.
#'
#' @return A data.frame containing:
#' \describe{
#'   \item{Station}{Station name}
#'   \item{LON}{Longitude}
#'   \item{LAT}{Latitude}
#'   \item{ELEV}{Synthetic elevation estimate (m)}
#'   \item{CLIMATE_ZONE}{Derived climate zone}
#'   \item{COASTAL_INDEX}{Relative coastal influence index}
#'   \item{TEMP_BASE}{Baseline temperature estimate}
#'   \item{RAIN_REGIME}{Derived rainfall regime}
#' }
#'
#' @examples
#' create_stations(n = 5, seed = 42)
#'
#' create_stations(
#'   data.frame(
#'     Station = "Accra",
#'     LON = -0.18,
#'     LAT = 5.60
#'   )
#' )
#'
#' @export
#'
#' @importFrom stats runif
#' @importFrom readr read_csv
#' @importFrom scales rescale
#' @importFrom dplyr case_when

create_stations <- function(
    source = NULL,
    n = 10,
    bbox = c(-3.5, 1.5, 4.5, 11.5),
    derive_climate = TRUE,
    seed = NULL
){
  # Reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
  }
  # Load or generate stations
  if (is.character(source)) {
    if (!file.exists(source)) {
      stop("File not found: ", source)
    }
    message("Loading station list from file: ", source)
    df <- readr::read_csv(source, show_col_types = FALSE)
  } else if (is.data.frame(source)) {
    message("Using station metadata from supplied data.frame")
    df <- source
  } else if (is.null(source)) {
    message("Generating synthetic station network...")
    if (length(bbox) != 4 || !is.numeric(bbox)) {
      stop("bbox must be numeric vector: ",
           "c(min_lon, max_lon, min_lat, max_lat)")
    }
    df <- data.frame(
      Station = paste0("Station_", seq_len(n)),
      LON = runif(n, bbox[1], bbox[2]),
      LAT = runif(n, bbox[3], bbox[4]),
      stringsAsFactors = FALSE
    )
    message("Generated ", n,
            " synthetic stations within bounding box.")
  } else {
    stop("`source` must be a file path, ", "a data.frame, or NULL.")
  }
  # Standardize names
  names(df) <- make.names(names(df))
  # Required columns
  required_cols <- c("Station", "LON", "LAT")
  if (!all(required_cols %in% names(df))) {
    stop(
      "Station metadata must contain columns named: ",
      paste(required_cols, collapse = ", ")
    )
  }
  # Coerce data types
  df$Station <- as.character(df$Station)
  df$LON <- as.numeric(df$LON)
  df$LAT <- as.numeric(df$LAT)
  rownames(df) <- NULL
  # Derive climate metadata
  if (derive_climate) {
    message("Deriving climate-aware station attributes...")
    # Synthetic elevation estimate
    if (!"ELEV" %in% names(df)) {
      df$ELEV <- round(
        runif(nrow(df), min = 0, max = 800),
        1
      )
    }
    # Climate zone classification
    if (!"CLIMATE_ZONE" %in% names(df)) {
      df$CLIMATE_ZONE <- dplyr::case_when(
        df$LAT < 6.5 ~ "Coastal",
        df$LAT >= 6.5 & df$LAT < 8.5 ~ "Forest",
        df$LAT >= 8.5 ~ "Savannah",
        TRUE ~ "Unknown"
      )
    }
    # Coastal influence index
    # Lower value = stronger coastal influence
    if (!"COASTAL_INDEX" %in% names(df)) {
      df$COASTAL_INDEX <- round(
        scales::rescale(abs(df$LAT - 5)),
        3
      )
    }
    # Baseline temperature estimate
    # Elevation-adjusted
    if (!"TEMP_BASE" %in% names(df)) {
      df$TEMP_BASE <- round(
        30 - (df$ELEV * 0.0065) +
          runif(nrow(df), -1, 1),
        2
      )
    }
    # Rainfall regime classification
    if (!"RAIN_REGIME" %in% names(df)) {
      df$RAIN_REGIME <- dplyr::case_when(
        df$CLIMATE_ZONE == "Coastal" ~ "Bimodal",
        df$CLIMATE_ZONE == "Forest" ~ "Humid",
        df$CLIMATE_ZONE == "Savannah" ~ "Monomodal",
        TRUE ~ "Unknown"
      )
    }
  }
  # Return final station table
  return(df)
}
