#' Plot Reference-Compartment Heatmap
#'
#' Creates a heatmap showing the count of observations for each combination of
#' reference and environmental compartment, with references ordered by year.
#'
#' @param data A data frame containing the main table data
#' @param compartment_order Character vector of compartment names in desired order.
#'   If NULL, will attempt to use `environ_compartments_sub_vocabulary()`.
#' @param x_var Character string specifying the column name for x-axis (compartment).
#'   Default is "ENVIRON_COMPARTMENT_SUB".
#' @param y_var Character string specifying the column name for y-axis (reference).
#'   Default is "REFERENCE_ID".
#' @param year_var Character string specifying the column name for year ordering.
#'   Default is "YEAR".
#' @param fill_label Character string for the legend title. Default is "Sample Size (n)".
#' @param x_label Character string for x-axis label. Default is "Environmental Sub-Compartment".
#' @param text_threshold Threshold for switching text color from black to white.
#'   Can be "mean", "median", or a numeric value. Default is "mean".
#' @param rotate_x Angle to rotate x-axis labels. Default is 45.
#'
#' @return A ggplot2 object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plot_reference_compartment_heatmap(main_table)
#' plot_reference_compartment_heatmap(main_table, text_threshold = "median")
#' plot_reference_compartment_heatmap(main_table, text_threshold = 5)
#' }
plot_reference_compartment_heatmap <- function(
  data,
  compartment_order = NULL,
  x_var = "ENVIRON_COMPARTMENT_SUB",
  y_var = "REFERENCE_ID",
  year_var = "YEAR",
  fill_label = "Sample Size (n)",
  x_label = "Environmental Sub-Compartment",
  text_threshold = "mean",
  rotate_x = 45
) {
  # Get compartment order if not provided ----
  if (is.null(compartment_order)) {
    compartment_order <- unlist(
      environ_compartments_sub_vocabulary(),
      use.names = FALSE
    )
  }

  # Prepare data for plotting ----
  plot_data <- data |>
    group_by(across(all_of(c(y_var, x_var)))) |>
    distinct() |>
    reframe(count = sum(MEASURED_N, na.rm = TRUE)) |>
    # Add factor levels for x-axis ordering
    mutate(
      {{ x_var }} := factor(
        .data[[x_var]],
        levels = compartment_order
      )
    ) |>
    # Join year data for y-axis ordering
    left_join(
      data |> distinct(across(all_of(c(y_var, year_var)))),
      by = y_var
    ) |>
    mutate({{ y_var }} := fct_reorder(.data[[y_var]], .data[[year_var]]))

  # Calculate text color threshold ----
  threshold_value <- switch(
    text_threshold,
    "mean" = mean(plot_data$count, na.rm = TRUE),
    "median" = median(plot_data$count, na.rm = TRUE),
    as.numeric(text_threshold)
  )

  # Create plot ----
  ggplot(
    plot_data,
    aes(
      y = .data[[y_var]],
      x = .data[[x_var]],
      fill = count
    )
  ) +
    geom_tile() +
    geom_text(aes(
      label = count,
      color = if_else(count > threshold_value, "black", "white")
    )) +
    scale_fill_viridis(name = fill_label) +
    scale_color_identity() +
    theme_aep_plots() +
    theme(
      axis.text.x = element_text(angle = rotate_x, hjust = 1),
      axis.title.y = element_blank(),
      legend.position = "none"
    ) +
    labs(x = x_label)
}


plot_reference_species_heatmap <- function(
  data,
  x_var = "SAMPLE_SPECIES",
  y_var = "REFERENCE_ID",
  year_var = "YEAR",
  fill_label = "Sample Size (n)",
  x_label = "Sampled Species",
  text_threshold = "mean",
  rotate_x = 45
) {
  # Prepare data for plotting ----
  plot_data <- data |>
    group_by(across(all_of(c(y_var, x_var)))) |>
    distinct() |>
    reframe(count = sum(MEASURED_N, na.rm = TRUE), SPECIES_GROUP) |>
    # Join year data for y-axis ordering
    left_join(
      data |> distinct(across(all_of(c(y_var, year_var)))),
      by = y_var
    ) |>
    filter(!is.na(SAMPLE_SPECIES))

  # Calculate text color threshold ----
  threshold_value <- switch(
    text_threshold,
    "mean" = mean(plot_data$count, na.rm = TRUE),
    "median" = median(plot_data$count, na.rm = TRUE),
    as.numeric(text_threshold)
  )

  # Create plot ----
  ggplot(
    plot_data,
    aes(
      y = .data[[y_var]],
      x = .data[[x_var]],
      fill = count
    )
  ) +
    geom_tile() +
    geom_text(aes(
      label = count,
      color = if_else(count > threshold_value, "black", "white")
    )) +
    scale_fill_viridis(name = fill_label) +
    scale_color_identity() +
    theme_aep_plots() +
    theme(
      axis.text.x = element_text(angle = rotate_x, hjust = 1),
      axis.title.y = element_blank(),
      legend.position = "none"
    ) +
    labs(x = x_label) +
    facet_wrap(
      facets = vars(SPECIES_GROUP),
      space = "free_x",
      scales = "free_x",
      nrow = 1
    )
}
