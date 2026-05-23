# =========================================================
# EXPORT DATA TO CSV
# =========================================================

#' Export CDSimX Data to CSV
#'
#' Exports any CDSimX dataframe to a CSV file.
#'
#' @param data A dataframe to export.
#'
#' @param file Output CSV filename.
#'
#' @param row_names Logical. Include row names?
#'
#' @return Invisibly returns the file path.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' export_csv(climate_data)
#' }

export_csv <- function(
    data,
    file = "CDSimX_output.csv",
    row_names = FALSE
){
  # -------------------------------------------------------
  # Validation
  # -------------------------------------------------------
  if(!is.data.frame(data)){
    stop("data must be a dataframe.")
  }
  # -------------------------------------------------------
  # Ensure extension
  # -------------------------------------------------------
  if(!grepl("\\.csv$", file, ignore.case = TRUE)){
    file <- paste0(file, ".csv")
  }
  # -------------------------------------------------------
  # Export
  # -------------------------------------------------------
  utils::write.csv(
    x = data,
    file = file,
    row.names = row_names
  )
  cat(
    "CSV export completed successfully.\n",
    "Saved to:",
    normalizePath(file),
    "\n"
  )
  invisible(file)
}
