# =========================================================
# EXPORT CDSIMX DATA TO NETCDF
# =========================================================

#' Export CDSimX Data to NetCDF
#'
#' Exports climate simulation or forecasting data
#' into NetCDF format.
#'
#' @param data Climate dataframe.
#'
#' @param file Output NetCDF filename.
#'
#' @param date_col Date column name.
#'
#' @param station_col Station column name.
#'
#' @param lon_col Longitude column.
#'
#' @param lat_col Latitude column.
#'
#' @param variables Climate variables to export.
#'
#' @param fillvalue Missing value.
#'
#' @param overwrite Logical.
#'
#' @return Invisibly returns output filename.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' export_netcdf(climate_data)
#' }

export_netcdf <- function(
    data,
    file = "CDSimX_output.nc",
    date_col = "DATE",
    station_col = "Station",
    lon_col = "LON",
    lat_col = "LAT",
    variables = NULL,
    fillvalue = -9999,
    overwrite = TRUE
){
  # -------------------------------------------------------
  # Package check
  # -------------------------------------------------------
  if(!requireNamespace("ncdf4", quietly = TRUE)){
    stop("Package 'ncdf4' is required but not installed.")
  }

  if(!requireNamespace("tidyr", quietly = TRUE)){
    stop("Package 'tidyr' is required but not installed.")
  }

  if(!requireNamespace("dplyr", quietly = TRUE)){
    stop("Package 'dplyr' is required but not installed.")
  }

  # -------------------------------------------------------
  # Validation
  # -------------------------------------------------------
  if(!is.data.frame(data)){
    stop("data must be a data.frame.")
  }
  required_cols <- c(date_col, station_col, lon_col, lat_col)
  missing_cols <- required_cols[!(required_cols %in% names(data))]
  if(length(missing_cols) > 0){
    stop(
      paste(
        "Missing required columns:",
        paste(
          missing_cols,
          collapse = ", "
        )
      )
    )
  }

  # -------------------------------------------------------
  # Overwrite handling
  # -------------------------------------------------------
  if(file.exists(file)){
    if(overwrite){
      file.remove(file)
    } else {
      stop("File already exists.")
    }
  }

  # -------------------------------------------------------
  # Date formatting
  # -------------------------------------------------------
  data[[date_col]] <- as.Date(data[[date_col]])
  # -------------------------------------------------------
  # Auto-detect variables
  # -------------------------------------------------------
  if(is.null(variables)){
    exclude_cols <- c(date_col, station_col, lon_col, lat_col)
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    variables <- setdiff(numeric_cols, exclude_cols)
  }
  if(length(variables) == 0){
    stop("No climate variables found.")
  }
  # -------------------------------------------------------
  # Sort data
  # -------------------------------------------------------
  data <- data[
    order(data[[station_col]], data[[date_col]]),
  ]
  # -------------------------------------------------------
  # Station metadata
  # -------------------------------------------------------
  station_info <- unique(
    data[, c(station_col, lon_col, lat_col)]
  )
  stations <- station_info[[station_col]]
  dates <- sort(unique(data[[date_col]]))
  # -------------------------------------------------------
  # Dimensions
  # -------------------------------------------------------
  station_dim <- ncdf4::ncdim_def(
    "station",
    "",
    vals = 1:length(stations),
    create_dimvar = FALSE
  )
  time_vals <- as.numeric(dates - min(dates))
  time_dim <- ncdf4::ncdim_def(
    "time",
    units = paste0("days since ", min(dates)),
    vals = time_vals
  )
  # -------------------------------------------------------
  # Coordinate variables
  # -------------------------------------------------------
  lon_var <- ncdf4::ncvar_def(
    "longitude",
    "degrees_east",
    list(station_dim),
    fillvalue
  )
  lat_var <- ncdf4::ncvar_def(
    "latitude",
    "degrees_north",
    list(station_dim),
    fillvalue
  )
  # -------------------------------------------------------
  # Climate variable definitions
  # -------------------------------------------------------
  var_defs <- list()
  for(v in variables){
    var_defs[[v]] <- ncdf4::ncvar_def(
      name = v,
      units = "",
      dim = list(station_dim, time_dim),
      missval = fillvalue,
      prec = "double"
    )
  }
  # -------------------------------------------------------
  # Create NetCDF
  # -------------------------------------------------------
  nc <- ncdf4::nc_create(
    file,
    vars = c(list(lon_var, lat_var), var_defs)
  )
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  # -------------------------------------------------------
  # Write coordinates
  # -------------------------------------------------------
  ncdf4::ncvar_put(nc, lon_var, station_info[[lon_col]])
  ncdf4::ncvar_put(nc, lat_var, station_info[[lat_col]])
  # -------------------------------------------------------
  # Write climate variables
  # -------------------------------------------------------
  for(v in variables){
    wide <- tidyr::pivot_wider(
      data[, c(station_col, date_col, v)],
      names_from = all_of(date_col),
      values_from = all_of(v)
    )
    mat <- as.matrix(wide[, -1])
    ncdf4::ncvar_put(nc, var_defs[[v]], mat)
  }
  # -------------------------------------------------------
  # Metadata
  # -------------------------------------------------------
  ncdf4::ncatt_put(nc, 0, "title", "CDSimX Climate Dataset")
  ncdf4::ncatt_put(nc, 0, "package", "CDSimX")
  ncdf4::ncatt_put(nc, 0, "creation_date", as.character(Sys.time()))
  ncdf4::ncatt_put(nc, 0, "author", "CDSimX")
  # -------------------------------------------------------
  # Completion message
  # -------------------------------------------------------
  cat("NetCDF export completed successfully.\n", "Saved to:",
    normalizePath(file), "\n")
  invisible(file)
}
