#' Create Ridge Plot for Copper Measurements
#'
#' @param data Filtered measurement data
#' @param compartment Environmental compartment to plot
#' @param sub_compartments Character vector of sub-compartments to include
#' @param thresholds Threshold data (from generate_copper_thresholds())
#' @param max_threshold Maximum threshold value to display
#' @param n_points Number of points for density calculation
#'
#' @importFrom dplyr filter mutate uncount
#' @importFrom ggplot2 ggplot aes geom_vline labs facet_wrap
#' @importFrom ggridges geom_ridgeline theme_ridges
#' @importFrom stats dnorm
#'
#' @export
plot_copper_ridges <- function(
  data,
  compartment,
  sub_compartments = NULL,
  thresholds,
  max_threshold = 1,
  n_points = 100
) {
  # Filter data for compartment
  plot_data <- data |>
    filter(
      ENVIRON_COMPARTMENT == compartment,
      UNCERTAINTY_TYPE == "Standard Deviation",
      !is.na(MEASURED_N),
      MEASURED_UNIT_STANDARD == "mg/L"
    ) |>
    mutate(
      SD_STANDARDISED = UNCERTAINTY_UPPER_STANDARD - MEASURED_VALUE_STANDARD
    )

  # Filter for sub-compartments if specified
  if (!is.null(sub_compartments)) {
    plot_data <- plot_data |>
      filter(ENVIRON_COMPARTMENT_SUB %in% sub_compartments)
  }

  # Generate density distributions
  ridge_data <- plot_data |>
    mutate(
      low = MEASURED_VALUE_STANDARD - 3 * SD_STANDARDISED,
      high = MEASURED_VALUE_STANDARD + 3 * SD_STANDARDISED
    ) |>
    uncount(n_points, .id = "row") |>
    mutate(
      x = (1 - row / n_points) * low + row / n_points * high,
      density = dnorm(x, MEASURED_VALUE_STANDARD, SD_STANDARDISED)
    )

  # Filter thresholds
  threshold_data <- thresholds |>
    filter(
      ENVIRON_COMPARTMENT == compartment,
      THRESHOLD_VALUE <= max_threshold
    )

  if (!is.null(sub_compartments)) {
    threshold_data <- threshold_data |>
      filter(ENVIRON_COMPARTMENT_SUB %in% sub_compartments)
  }

  # Create plot
  p <- ggplot(ridge_data, aes(x = x, y = SAMPLE_ID, height = density)) +
    geom_ridgeline(
      aes(fill = ENVIRON_COMPARTMENT_SUB),
      alpha = 0.7,
      scale = 0.5
    ) +
    geom_vline(
      data = threshold_data,
      aes(xintercept = THRESHOLD_VALUE, color = THRESHOLD_CLASS),
      linetype = "dashed",
      linewidth = 0.8,
      alpha = 0.6
    ) +
    labs(
      x = "Concentration (mg/L)",
      y = "Sample ID"
    ) +
    theme_ridges() +
    facet_wrap(
      facets = vars(ENVIRON_COMPARTMENT_SUB),
      ncol = 1,
      scales = "free_y"
    )

  return(p)
}

#' Create Boxplot for Copper Measurements from Summary Statistics
#'
#' @param data Filtered measurement data with summary statistics
#' @param compartment Environmental compartment to plot
#' @param sub_compartments Character vector of sub-compartments to include. Leave NULL to include all
#' @param thresholds Threshold data
#' @param max_threshold Maximum threshold value to display
#'
#' @importFrom dplyr filter mutate
#' @importFrom ggplot2 ggplot aes stat_boxplot geom_vline labs facet_wrap
#' @importFrom stats qnorm
#'
#' @export
plot_copper_boxplots <- function(
  data,
  compartment,
  sub_compartments = NULL,
  thresholds,
  max_threshold = 1
) {
  # Filter and prepare data
  plot_data <- data |>
    filter(
      ENVIRON_COMPARTMENT == compartment,
      UNCERTAINTY_TYPE == "Standard Deviation",
      !is.na(MEASURED_N),
      MEASURED_UNIT_STANDARD == "mg/L"
    ) |>
    mutate(
      SD_STANDARDISED = UNCERTAINTY_UPPER_STANDARD - MEASURED_VALUE_STANDARD,
      # Calculate boxplot statistics from mean and SD assuming normal distribution
      box_ymin = MEASURED_VALUE_STANDARD - 1.5 * 1.35 * SD_STANDARDISED, # ~whisker (Q1 - 1.5*IQR)
      box_lower = MEASURED_VALUE_STANDARD - 0.675 * SD_STANDARDISED, # Q1
      box_middle = MEASURED_VALUE_STANDARD, # median (= mean for normal)
      box_upper = MEASURED_VALUE_STANDARD + 0.675 * SD_STANDARDISED, # Q3
      box_ymax = MEASURED_VALUE_STANDARD + 1.5 * 1.35 * SD_STANDARDISED, # ~whisker (Q3 + 1.5*IQR)
      # Create hover text
      hover_text = paste0(
        "Sample: ",
        SAMPLE_ID,
        "\n",
        "Mean: ",
        round(MEASURED_VALUE_STANDARD, 3),
        " mg/L\n",
        "SD: ",
        round(SD_STANDARDISED, 3),
        "\n",
        "N: ",
        MEASURED_N,
        "\n"
      )
    )

  if (!is.null(sub_compartments)) {
    plot_data <- plot_data |>
      filter(ENVIRON_COMPARTMENT_SUB %in% sub_compartments)
  }

  # Filter thresholds
  threshold_data <- thresholds |>
    filter(
      ENVIRON_COMPARTMENT == compartment,
      THRESHOLD_VALUE <= max_threshold
    )

  if (!is.null(sub_compartments)) {
    threshold_data <- threshold_data |>
      filter(ENVIRON_COMPARTMENT_SUB %in% sub_compartments)
  }

  # Create plot
  p <- ggplot(
    plot_data,
    aes(
      x = box_middle,
      y = SAMPLE_ID,
      text = hover_text
    )
  ) +
    geom_boxplot(
      aes(
        xmin = box_ymin,
        lower = box_lower,
        middle = box_middle, # FIXME: why doesn't this work!?!?!?!
        upper = box_upper,
        xmax = box_ymax,
        fill = ENVIRON_COMPARTMENT_SUB
      ),
      stat = "identity",
      alpha = 0.7
    ) +
    geom_vline(
      data = threshold_data,
      aes(xintercept = THRESHOLD_VALUE, color = THRESHOLD_CLASS),
      linetype = "dashed",
      linewidth = 0.8,
      alpha = 0.6
    ) +
    labs(
      x = "Concentration (mg/L)",
      y = "Sample ID"
    ) +
    theme_minimal() +
    facet_wrap(
      facets = vars(ENVIRON_COMPARTMENT_SUB),
      ncol = 1,
      scales = "free_y"
    )

  return(p)
}
