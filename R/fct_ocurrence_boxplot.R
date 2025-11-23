#' Create boxplots of measured values by unit type
#'
#' Creates faceted boxplots showing the distribution of measured values
#' for each unit type over time. Useful for visualising data spread and
#' identifying outliers across different measurement units.
#'
#' @param data A tibble containing measurement data
#' @param value_column Character string. Name of column containing measurement values.
#'   Default is "MEASURED_VALUE"
#' @param unit_column Character string. Name of column containing measurement units.
#'   Default is "MEASURED_UNIT"
#' @param date_column Character string. Name of column containing sampling dates.
#'   Default is "SAMPLING_DATE"
#' @param show_points Logical. Whether to overlay individual points on boxplots.
#'   Default is TRUE
#'
#' @return A ggplot object with faceted boxplots
#'
#' @examples
#' \dontrun{
#' make_measurement_boxplot(literature_clean)
#'
#' make_measurement_boxplot(
#'   data = literature_clean,
#'   value_column = "MEASURED_VALUE_STANDARD",
#'   unit_column = "MEASURED_UNIT_STANDARD"
#' )
#' }
#'
#' @export
make_measurement_boxplot <- function(
  data,
  value_column = "MEASURED_VALUE",
  unit_column = "MEASURED_UNIT",
  date_column = "SAMPLING_DATE",
  show_points = TRUE
) {
  plot <- ggplot(
    data = data,
    mapping = aes(x = .data[[date_column]], y = .data[[value_column]]) # TODO: How do we stop NA being added as a facet here?
  ) +
    geom_boxplot() +
    facet_wrap(
      facets = vars(.data[[unit_column]]),
      drop = TRUE,
      ncol = 1,
      scales = "free_y"
    )

  if (show_points) {
    plot <- plot + geom_point()
  }

  plot
}
