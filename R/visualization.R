# =========================================================
# VISUALIZATION FUNCTIONS FOR CDSimX
# =========================================================

#' Visualization Functions for Climate Data
#'
#' Flexible visualization tools for CDSimX climate datasets.
#'
#' @name visualization
NULL

# =========================================================
# STATION TIME SERIES PLOT
# =========================================================

#' Plot Station Climate Time Series
#'
#' @description
#' Creates highly customizable climate time-series
#' visualizations with automatic seasonal detection.
#'
#' Supports:
#' \itemize{
#'   \item Custom line colors
#'   \item Seasonal coloring
#'   \item LOESS smoothing
#'   \item Trend lines
#'   \item Dark/light themes
#'   \item Flexible date handling
#'   \item Faceting
#' }
#'
#' @param df Climate dataframe.
#'
#' @param station Station name.
#'
#' @param var Climate variable to plot.
#'
#' @param smooth Logical.
#' Add LOESS smoothing line.
#'
#' @param smooth_span LOESS span parameter.
#'
#' @param show_points Logical.
#' Show points on plot.
#'
#' @param point_size Point size.
#'
#' @param line_size Line width.
#'
#' @param line_color Main line color.
#'
#' @param smooth_color Smoothing line color.
#'
#' @param seasonal_colors Named vector of colors.
#'
#' @param use_season_colors Logical.
#' Color points by season.
#'
#' @param alpha Transparency level.
#'
#' @param theme_style Plot theme.
#' Options are:
#' \code{"minimal"},
#' \code{"dark"},
#' \code{"classic"},
#' \code{"bw"}.
#'
#' @param date_breaks X-axis date interval.
#'
#' @param date_labels Date label format.
#'
#' @param facet Logical.
#' Facet by season.
#'
#' @param show_trend Logical.
#' Add linear trend line.
#'
#' @param trend_color Trend line color.
#'
#' @param title Plot title.
#'
#' @param subtitle Plot subtitle.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#'
#' plot_station_timeseries(
#'   climate_data,
#'   station = "Station_1",
#'   var = "Tmin"
#' )
#'
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point
#' @importFrom ggplot2 geom_smooth labs scale_x_date
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 theme_minimal theme_dark
#' @importFrom ggplot2 theme_classic theme_bw
#' @importFrom ggplot2 theme element_text facet_wrap
#' @importFrom dplyr filter mutate case_when `%>%`
#' @importFrom lubridate month
#' @importFrom rlang sym
#' @importFrom stats median
#'
#' @export

plot_station_timeseries <- function(
    df,
    station,
    var = "Tmin",
    smooth = TRUE,
    smooth_span = 0.25,
    show_points = TRUE,
    point_size = 2,
    line_size = 1,
    line_color = NULL,
    smooth_color = NULL,
    seasonal_colors = NULL,
    use_season_colors = TRUE,
    alpha = 0.8,
    theme_style = c(
      "minimal",
      "dark",
      "classic",
      "bw"
    ),
    date_breaks = "2 years",
    date_labels = "%Y",
    facet = FALSE,
    show_trend = FALSE,
    trend_color = "black",
    title = NULL,
    subtitle = NULL
){
  # -------------------------------------------------------
  # Match arguments
  # -------------------------------------------------------
  theme_style <- match.arg(theme_style)
  # -------------------------------------------------------
  # Detect date column
  # -------------------------------------------------------
  if("DATE" %in% names(df)){
    df$DATE <- as.Date(df$DATE)
    date_col <- "DATE"
  } else if("Date" %in% names(df)){
    df$Date <- as.Date(df$Date)
    date_col <- "Date"
  } else {
    stop("Dataset must contain either DATE or Date column.")
  }
  # -------------------------------------------------------
  # Variable checks
  # -------------------------------------------------------
  if(!(var %in% names(df))){
    stop(paste("Variable", var, "not found in dataframe."))
  }
  # -------------------------------------------------------
  # Station check
  # -------------------------------------------------------
  if(!("Station" %in% names(df))){
    stop("Station column not found.")
  }
  # -------------------------------------------------------
  # Latitude check
  # -------------------------------------------------------
  if(!("LAT" %in% names(df))){
    stop("LAT column not found.")
  }
  # -------------------------------------------------------
  # Subset station
  # -------------------------------------------------------
  dsub <- df %>% dplyr::filter(Station == station)
  if(nrow(dsub) == 0){
    stop(paste("Station", station, "not found."))
  }
  # -------------------------------------------------------
  # Hemisphere detection
  # -------------------------------------------------------
  hemi <- if(
    abs(stats::median(dsub$LAT, na.rm = TRUE)) < 5
  ){
    "Equatorial"
  } else if(
    stats::median(dsub$LAT, na.rm = TRUE) > 0
  ){
    "Northern"
  } else {
    "Southern"
  }
  # -------------------------------------------------------
  # Month extraction
  # -------------------------------------------------------
  dsub$Month <- lubridate::month(dsub[[date_col]])
  # -------------------------------------------------------
  # Seasonal classification
  # -------------------------------------------------------
  dsub <- dsub %>%
    dplyr::mutate(
      Season = dplyr::case_when(
        hemi == "Equatorial" & Month %in% c(11,12,1,2,3) ~ "Dry Season",
        hemi == "Equatorial" ~ "Wet Season",
        hemi == "Northern" & Month %in% c(12,1,2) ~ "Winter",
        hemi == "Northern" & Month %in% c(3,4,5) ~ "Spring",
        hemi == "Northern" & Month %in% c(6,7,8) ~ "Summer",
        hemi == "Northern" & Month %in% c(9,10,11) ~ "Autumn",
        hemi == "Southern" & Month %in% c(6,7,8) ~ "Winter",
        hemi == "Southern" & Month %in% c(9,10,11) ~ "Spring",
        hemi == "Southern" & Month %in% c(12,1,2) ~ "Summer",
        hemi == "Southern" & Month %in% c(3,4,5) ~ "Autumn",
        TRUE ~ "Unknown"
      )
    )
  # -------------------------------------------------------
  # Default colors
  # -------------------------------------------------------
  default_colors <- list(
    "Tmin" = list(line = "#3182bd", smooth = "#08519c"),
    "Tmax" = list(line = "#e31a1c", smooth = "#a50f15"),
    "Rainfall" = list(line = "#2b8cbe", smooth = "#045a8d"),
    "Avg.Temp" = list(line = "#fd8d3c", smooth = "#d94801"),
    "RH" = list(line = "#756bb1", smooth = "#54278f"),
    "WindSpeed" = list(line = "#636363", smooth = "#252525"),
    "Solar_Radiation" = list(line = "#fdae6b", smooth = "#e6550d"),
    "ET0" = list(line = "#31a354", smooth = "#006d2c")
  )
  # -------------------------------------------------------
  # Assign default colors
  # -------------------------------------------------------
  if(is.null(line_color)){
    if(var %in% names(default_colors)){
      line_color <- default_colors[[var]]$line
    } else {
      line_color <- "#2c7fb8"
    }
  }
  if(is.null(smooth_color)){
    if(var %in% names(default_colors)){
      smooth_color <- default_colors[[var]]$smooth
    } else {
      smooth_color <- "#045a8d"
    }
  }
  # -------------------------------------------------------
  # Seasonal palette
  # -------------------------------------------------------
  if(is.null(seasonal_colors)){
    seasonal_colors <- c(
      "Winter" = "#81b1d2",
      "Spring" = "#4daf4a",
      "Summer" = "#e41a1c",
      "Autumn" = "#ff7f00",
      "Wet Season" = "#1b9e77",
      "Dry Season" = "#d95f02",
      "Unknown" = "grey60"
    )
  }

  # -------------------------------------------------------
  # Base plot
  # -------------------------------------------------------
  p <- ggplot2::ggplot(dsub,
    ggplot2::aes(x = .data[[date_col]], y = .data[[var]])) +
    ggplot2::geom_line(linewidth = line_size, color = line_color, alpha = alpha)
  # -------------------------------------------------------
  # Add points
  # -------------------------------------------------------
  if(show_points){
    if(use_season_colors){
      p <- p + ggplot2::geom_point(ggplot2::aes(color = Season),
          size = point_size,
          alpha = alpha
        ) +
        ggplot2::scale_color_manual(values = seasonal_colors)
    } else {
      p <- p + ggplot2::geom_point(
          color = line_color,
          size = point_size,
          alpha = alpha
        )
    }
  }

  # -------------------------------------------------------
  # Add smoothing
  # -------------------------------------------------------
  if(smooth){
    p <- p +
      ggplot2::geom_smooth(
        method = "loess",
        span = smooth_span,
        linewidth = 1,
        color = smooth_color,
        alpha = 0.25
      )
  }

  # -------------------------------------------------------
  # Add trend line
  # -------------------------------------------------------
  if(show_trend){
    p <- p +
      ggplot2::geom_smooth(
        method = "lm",
        se = FALSE,
        linewidth = 1,
        linetype = "dashed",
        color = trend_color
      )
  }
  # -------------------------------------------------------
  # Faceting
  # -------------------------------------------------------
  if(facet){
    p <- p + ggplot2::facet_wrap(~Season, scales = "free_y")
  }
  # -------------------------------------------------------
  # Default titles
  # -------------------------------------------------------
  if(is.null(title)){
    title <- paste(var, "Time Series at", station)
  }
  if(is.null(subtitle)){
    subtitle <- paste(
      "Hemisphere:",
      hemi,
      "| Period:",
      format(
        min(dsub[[date_col]]),
        "%Y"
      ),
      "-",
      format(
        max(dsub[[date_col]]),
        "%Y"
      )
    )
  }

  # -------------------------------------------------------
  # Labels and scales
  # -------------------------------------------------------
  p <- p +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Date",
      y = var,
      color = "Season"
    ) +
    ggplot2::scale_x_date(
      date_breaks = date_breaks,
      date_labels = date_labels
    )
  # -------------------------------------------------------
  # Themes
  # -------------------------------------------------------
  if(theme_style == "minimal"){
    p <- p + ggplot2::theme_minimal(base_size = 13)
  }
  if(theme_style == "dark"){
    p <- p + ggplot2::theme_dark(base_size = 13)
  }
  if(theme_style == "classic"){
    p <- p + ggplot2::theme_classic(base_size = 13)
  }
  if(theme_style == "bw"){
    p <- p + ggplot2::theme_bw(base_size = 13)
  }
  # -------------------------------------------------------
  # Common formatting
  # -------------------------------------------------------
  p <- p +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 16
      ),
      plot.subtitle = ggplot2::element_text(
        size = 11
      ),
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1
      ),
      legend.position = "bottom"
    )
  return(p)
}
# -------------------------------------------------------
# Global variables
# -------------------------------------------------------
utils::globalVariables(c("Season", "Month"))
