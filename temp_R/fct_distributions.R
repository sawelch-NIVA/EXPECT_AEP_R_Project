generate_density_distributions <- function(
  data,
  value_col = "MEASURED_VALUE_STANDARD",
  upper_col = "UNCERTAINTY_UPPER_STANDARD",
  lower_col = "UNCERTAINTY_LOWER_STANDARD",
  uncertainty_type_col = "UNCERTAINTY_TYPE",
  n_points = 1001
) {
  valid_types <- c(
    "Standard Deviation",
    "95% Confidence Interval",
    "First-Third Quartile Range"
  )

  # Calculate x_range once for all rows
  data_filtered <- data |>
    filter(.data[[uncertainty_type_col]] %in% valid_types) |>
    drop_na(
      .data[[value_col]],
      .data[[upper_col]],
      .data[[lower_col]]
    ) |>
    mutate(
      sd = case_when(
        .data[[uncertainty_type_col]] == "Standard Deviation" ~
          .data[[upper_col]],
        .data[[uncertainty_type_col]] == "95% Confidence Interval" ~
          (.data[[upper_col]] - .data[[lower_col]]) / (2 * 1.96),
        .data[[uncertainty_type_col]] == "First-Third Quartile Range" ~
          (.data[[upper_col]] - .data[[lower_col]]) / 1.35
      )
    )

  x_min <- min(data_filtered[[value_col]] - 4 * data_filtered$sd)
  x_max <- max(data_filtered[[value_col]] + 4 * data_filtered$sd)
  x_range <- seq(from = x_min, to = x_max, length.out = n_points)

  # Add nested density distribution for each row
  data_filtered |>
    mutate(
      density_dist = list(
        tibble(
          x = x_range,
          density = dnorm(x_range, mean = get(value_col), sd = sd)
        )
      )
    ) |>
    ungroup()
}
