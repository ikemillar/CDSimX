#' Generate Climate Simulation Time Index
#'
#' Creates a standardized temporal framework for CDSimX simulations.
#' Supports daily, monthly, and yearly temporal resolutions.
#'
#' This function becomes the backbone of all climate simulations,
#' ensuring that all variables share a synchronized temporal structure.
#'
#' @param start_date Character or Date object.
#'   Simulation start date.
#'   Default = "1990-01-01"
#'
#' @param end_date Character or Date object.
#'   Simulation end date.
#'   Default = "1999-12-31"
#'
#' @param frequency Temporal resolution:
#'   - "daily"
#'   - "monthly"
#'   - "yearly"
#'
#' @param calendar Calendar type.
#'   Currently supports:
#'   - "standard"
#'   - "noleap"
#'
#' @return A data.frame containing:
#' \describe{
#'   \item{START_DATE}{Start date}
#'   \item{END_DATE}{End date}
#'   \item{DATE}{Date index}
#'   \item{Year}{Calendar year}
#'   \item{Month}{Month number}
#'   \item{Day}{Day of month}
#'   \item{DOY}{Day of year}
#'   \item{Week}{Week number}
#'   \item{Quarter}{Quarter}
#'   \item{Season}{Climatological season}
#'   \item{Frequency}{Simulation resolution}
#' }
#'
#' @examples
#' daily_index <- generate_time_index(
#'   start_date = "2000-01-01",
#'   end_date   = "2000-12-31",
#'   frequency  = "daily"
#' )
#'
#' monthly_index <- generate_time_index(
#'   start_date = "1990-01-01",
#'   end_date   = "2020-12-31",
#'   frequency  = "monthly"
#' )
#'
#' @export
#'
#' @importFrom lubridate year month day yday week quarter
#' @importFrom dplyr case_when

generate_time_index <- function(
    start_date = "1990-01-01",
    end_date   = "1999-12-31",
    frequency  = "daily",
    calendar   = "standard"
){
  # -------------------------------------------------------
  # Validate simulation dates
  # -------------------------------------------------------
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  start_year <- as.numeric(format(start_date, "%Y"))
  end_year <- as.numeric(format(end_date, "%Y"))
  if(start_year > current_year){
    stop(
      paste("start_date year cannot exceed current year (",current_year,").")
    )
  }
  if(end_year > current_year){
    stop(
      paste("end_date year cannot exceed current year (",current_year,").")
    )
  }
  if(end_date < start_date){
    stop("end_date must be greater than or equal to start_date.")
  }
  # normalize aliases
  frequency <- tolower(frequency)
  frequency <- switch(
    frequency,
    "day"   = "daily",
    "days"  = "daily",
    "month" = "monthly",
    "months" = "monthly",
    "year"  = "yearly",
    "years" = "yearly",
    frequency
  )
  # Validate frequency
  allowed_freq <- c("daily", "monthly", "yearly")
  if (!frequency %in% allowed_freq) {
    stop("frequency must be one of: ",
      paste(allowed_freq, collapse = ", ")
    )
  }
  # Convert dates
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)
  if (start_date >= end_date) {
    stop("start_date must be earlier than end_date")
  }
  # Generate temporal sequence
  if (frequency == "daily") {
    start_seq <- seq.Date(
      from = start_date,
      to   = end_date,
      by   = "day"
    )
    end_seq <- start_seq
    mid_seq <- start_seq
  } else if (frequency == "monthly") {
    start_seq <- seq.Date(
      from = as.Date(format(start_date, "%Y-%m-01")),
      to   = as.Date(format(end_date, "%Y-%m-01")),
      by   = "month"
    )
    end_seq <- as.Date(
      sapply(start_seq, function(x) {
        seq(x, by = "month", length.out = 2)[2] - 1
      }),
      origin = "1970-01-01"
    )
    mid_seq <- start_seq + floor((end_seq - start_seq) / 2)
  } else if (frequency == "yearly") {
    years <- seq(
      lubridate::year(start_date),
      lubridate::year(end_date),
      by = 1
    )
    start_seq <- as.Date(
      paste0(years, "-01-01")
    )
    end_seq <- as.Date(
      paste0(years, "-12-31")
    )
    mid_seq <- start_seq + floor((end_seq - start_seq) / 2)
  }
  # Create temporal dataframe
  time_df <- data.frame(
    START_DATE = start_seq,
    END_DATE   = end_seq,
    DATE       = mid_seq,
    stringsAsFactors = FALSE
  )
  # Extract temporal components
  time_df$Year    <- lubridate::year(time_df$DATE)
  time_df$Month   <- lubridate::month(time_df$DATE)
  time_df$Day     <- lubridate::day(time_df$DATE)
  time_df$DOY     <- lubridate::yday(time_df$DATE)
  time_df$Week    <- lubridate::week(time_df$DATE)
  time_df$Quarter <- lubridate::quarter(time_df$DATE)
  # Climatological seasons
  # Tropical Africa-oriented classification
  time_df$Season <- dplyr::case_when(
    time_df$Month %in% c(12, 1, 2)     ~ "Dry",
    time_df$Month %in% c(3, 4, 5)      ~ "Pre-Wet",
    time_df$Month %in% c(6, 7, 8, 9)   ~ "Wet",
    time_df$Month %in% c(10, 11)       ~ "Post-Wet",
    TRUE ~ "Unknown"
  )
  # Frequency label
  time_df$Frequency <- frequency
  # Leap year handling
  if (calendar == "noleap") {
    time_df <- subset(time_df, !(Month == 2 & Day == 29))
  }
  # Reorder columns
  time_df <- time_df[, c(
    "START_DATE",
    "END_DATE",
    "DATE",
    "Year",
    "Month",
    "Day",
    "DOY",
    "Week",
    "Quarter",
    "Season",
    "Frequency"
  )]
  # Final message
  message("Generated ", nrow(time_df), " time steps at ",
          frequency, " resolution.")
  return(time_df)
}
