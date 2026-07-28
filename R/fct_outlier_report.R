#' Flag Outliers Within Each Level of a Facet Column
#'
#' Computes outlier flags (see [flag_outliers()]) separately within each
#' distinct value of `facet_col`, e.g. so that flags reflect "is this point
#' unusual for its sampling year" rather than "unusual across the whole
#' group". Used to build the per-facet distribution plots in the generated
#' outlier notebooks (see [plot_outlier_distribution()]).
#'
#' @param data A data frame with a `MEASURED_VALUE_STANDARD` column.
#' @param facet_col Column name (string) to group by, e.g. `"SAMPLING_YEAR"`.
#' @param min_n Minimum sample size required to compute flags per level.
#' @return `data` with flag columns added, plus a `facet_label` column
#'   combining the facet level and its n (Markdown-formatted, for use as an
#'   axis label).
#' @export
flag_by_facet <- function(data, facet_col, min_n = 10) {
  # data may already carry whole-group flag columns from
  # outlier_group_analysis(); drop them so the facet-level recomputation
  # below doesn't collide with (and get silently renamed away from) them.
  data |>
    dplyr::select(
      -dplyr::any_of(c("RMZ", "outlier_RMZ", "outlier_IQR", "dot_fill"))
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(facet_col))) |>
    dplyr::group_modify(
      ~ dplyr::bind_cols(.x, flag_outliers(.x$MEASURED_VALUE_STANDARD, min_n = min_n))
    ) |>
    dplyr::mutate(
      facet_label = paste0(
        "**",
        .data[[facet_col]],
        "**<br>(n = ",
        dplyr::n(),
        ")"
      )
    ) |>
    dplyr::ungroup()
}

#' Dip Test Within Each Level of a Facet Column
#'
#' Companion to [flag_by_facet()]: runs [dip_test_safe()] separately within
#' each distinct value of `facet_col` (and, if given, `panel_facet`, purely
#' so that column survives into the result for plotting).
#'
#' @param data A data frame with a `MEASURED_VALUE_STANDARD` column.
#' @param facet_col Column name (string) to group by.
#' @param min_n Minimum sample size required to run the test per level.
#' @param panel_facet Optional additional column name (string) to group by,
#'   e.g. a geographic feature used to facet the plot into panels.
#' @return A tibble: `facet_col` (and `panel_facet` if given), `n`, `dip_p`,
#'   `bimodal`, `draw_dip_p` (whether n was large enough to plot a p-value).
#' @export
dip_by_facet <- function(data, facet_col, min_n = 10, panel_facet = NULL) {
  group_cols <- c(facet_col, panel_facet)
  data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::group_modify(~ {
      dip <- dip_test_safe(.x$MEASURED_VALUE_STANDARD, min_n = min_n)
      tibble::tibble(n = nrow(.x), dip_p = dip$dip_p, bimodal = isTRUE(dip$bimodal))
    }) |>
    dplyr::ungroup() |>
    dplyr::mutate(draw_dip_p = n >= min_n)
}

#' Suggested Figure Height for a Faceted Outlier Distribution Plot
#'
#' Grows with the number of facet rows, but capped well below the point at
#' which `ragg` refuses to open a device (its hard limit is 50000px; at the
#' 300dpi used throughout these notebooks that's about 166in). A group with
#' hundreds or thousands of distinct sites/campaigns would otherwise request
#' a figure large enough to crash the render.
#'
#' @param x A vector whose distinct values become plot rows (e.g. a facet
#'   column).
#' @param height_per_row Height (inches) allotted per distinct value.
#' @param base Fixed additional height (inches), e.g. for axis/legend space.
#' @param max_height Hard cap (inches) on the returned height.
#' @return A single numeric value, for use as a `fig-height` chunk option.
#' @export
outlier_fig_height <- function(x, height_per_row = 0.5, base = 3, max_height = 80) {
  min(dplyr::n_distinct(x, na.rm = TRUE) * height_per_row + base, max_height)
}

#' Log10 X-Scale With a Minimum Span
#'
#' A `scale_x_log10()` sized to the data, but never narrower than
#' `min_span` orders of magnitude. Without this, a small/tightly-clustered
#' group gets a panel that auto-zooms to its own tiny range: axis labels
#' collapse to a single value and the plot conveys no sense of scale. This
#' also indirectly controls dot size in [plot_outlier_distribution()], since
#' a wider panel makes a fixed-data-unit dot occupy less of the plot.
#'
#' @param x Numeric vector of (positive) values the scale needs to cover.
#' @param min_span Minimum width of the axis, in orders of magnitude.
#' @return A `ggplot2::scale_x_log10()` object.
#' @export
outlier_log10_scale <- function(x, min_span = 2) {
  x_pos <- x[!is.na(x) & x > 0]
  if (length(x_pos) == 0) {
    lo <- -1
    hi <- 1
  } else {
    lo <- floor(min(log10(x_pos)))
    hi <- ceiling(max(log10(x_pos)))
    deficit <- min_span - (hi - lo)
    if (deficit > 0) {
      pad <- ceiling(deficit / 2)
      lo <- lo - pad
      hi <- hi + pad
    }
  }
  breaks_seq <- lo:hi
  ggplot2::scale_x_log10(
    limits = 10^c(lo, hi),
    breaks = 10^breaks_seq,
    labels = scales::label_log(),
    minor_breaks = outer(1:9, 10^breaks_seq) |> as.vector()
  )
}

#' Dotplot Binwidth Scaled to a Group's Own Data Range
#'
#' A fixed `binwidth` tuned for one dataset looks wildly oversized on a
#' small, narrow-range group and undersized on a huge, wide-range one.
#' Scaling `binwidth` to roughly `bins` bins across the data's own log10
#' range keeps dot size sensible ("one dot, one sample") regardless of the
#' group's scale.
#'
#' @param x Numeric vector of (positive) values.
#' @param bins Target number of bins across the data's own range.
#' @param min_binwidth Floor on the returned binwidth (log10 units), so
#'   near-identical values don't collapse to a zero-width bin.
#' @return A single numeric value, for use as `geom_dotplot(binwidth = )`.
#' @export
outlier_dotplot_binwidth <- function(x, bins = 40, min_binwidth = 0.005) {
  x_pos <- x[!is.na(x) & x > 0]
  if (length(x_pos) < 2) {
    return(min_binwidth)
  }
  log_range <- diff(range(log10(x_pos)))
  max(log_range / bins, min_binwidth)
}

#' Build a Flag-Coloured Distribution Plot with a Dip-Test Panel
#'
#' Reproduces the "distribution by <facet>" figures used throughout the
#' outlier notebooks: a box+dot plot coloured by outlier flag (computed
#' within each level of `facet_col`), alongside a narrow panel of per-level
#' dip-test p-values.
#'
#' @param data A data frame with a `MEASURED_VALUE_STANDARD` column.
#' @param facet_col Column name (string) to group/facet the y-axis by, e.g.
#'   `"SAMPLING_YEAR"`, `"SITE_CODE"`, `"CAMPAIGN_NAME_SHORT"`.
#' @param x_label X-axis label.
#' @param y_label Y-axis label.
#' @param panel_facet Optional column name (string) to further split the
#'   plot into panels via `facet_wrap()`, e.g. `"SITE_GEOGRAPHIC_FEATURE"`.
#' @param min_n Minimum sample size required to compute flags/dip test.
#' @return A composed `patchwork` object (main plot + dip-test panel).
#' @export
plot_outlier_distribution <- function(
  data,
  facet_col,
  x_label = "Measured value",
  y_label = NULL,
  panel_facet = NULL,
  min_n = 10
) {
  flagged <- flag_by_facet(data, facet_col, min_n = min_n)
  facet_label_map <- dplyr::distinct(
    flagged,
    dplyr::pick(dplyr::all_of(facet_col)),
    facet_label
  )
  dip_results <- dip_by_facet(data, facet_col, min_n = min_n, panel_facet = panel_facet) |>
    dplyr::left_join(facet_label_map, by = facet_col)

  fill_values <- c(
    "neither" = "black",
    "IQR" = "#0077BB",
    "RMZ" = "#cc6811",
    "both" = "#d51bcf",
    "not tested" = "grey70"
  )
  fill_labels <- c(
    "neither" = "Neither",
    "IQR" = "IQR only",
    "RMZ" = "RMZ only",
    "both" = "Both",
    "not tested" = "n < min_n (not tested)"
  )

  main_plot <- flagged |>
    ggplot2::ggplot(ggplot2::aes(y = facet_label, x = MEASURED_VALUE_STANDARD)) +
    ggplot2::geom_boxplot(
      outliers = FALSE,
      whisker.colour = "darkgrey",
      staple.colour = "darkgrey",
      box.colour = "darkgrey"
    ) +
    ggplot2::geom_dotplot(
      ggplot2::aes(fill = dot_fill),
      dotsize = 1,
      binwidth = outlier_dotplot_binwidth(flagged$MEASURED_VALUE_STANDARD),
      stackdir = "centerwhole",
      binpositions = "all",
      stroke = 0
    ) +
    ggplot2::scale_fill_manual(
      values = fill_values,
      labels = fill_labels,
      name = "Outlier flags",
      drop = FALSE
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(colour = NA, size = 5))) +
    outlier_log10_scale(flagged$MEASURED_VALUE_STANDARD) +
    ggplot2::labs(y = y_label, x = x_label) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      axis.text.y.left = ggtext::element_markdown(),
      panel.background = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "grey90"),
      panel.grid.minor = ggplot2::element_line(colour = "grey95"),
      plot.background = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )

  dip_panel <- dip_results |>
    ggplot2::ggplot(ggplot2::aes(
      y = facet_label,
      x = 0,
      label = ifelse(draw_dip_p, format(round(dip_p, 3), nsmall = 3), ""),
      colour = bimodal
    )) +
    ggplot2::geom_text(size = 14 / .pt) +
    ggplot2::scale_colour_manual(
      values = c("FALSE" = "grey40", "TRUE" = "#CC3311"),
      na.value = "grey40"
    ) +
    ggplot2::labs(x = "p(not unimodal)", y = NULL) +
    ggplot2::theme_void(base_size = 14) +
    ggplot2::theme(legend.position = "none", axis.title.x = ggplot2::element_text())

  if (!is.null(panel_facet)) {
    main_plot <- main_plot +
      ggplot2::facet_wrap(facets = panel_facet, ncol = 1, scales = "free_y")
    dip_panel <- dip_panel +
      ggplot2::facet_wrap(facets = panel_facet, ncol = 1, scales = "free_y") +
      ggplot2::theme(strip.text = ggplot2::element_blank())
  }

  main_plot + dip_panel + patchwork::plot_layout(widths = c(6, 1), guides = "collect")
}

#' Plot the Effect of Outlier Treatment on a Group's Distribution
#'
#' Reproduces the "effect of outlier treatment" comparison plot: raw values
#' vs. 98%/90% Winsorized vs. double-flagged-trimmed, as violins with
#' overlaid boxplots. Consumes the flagged/Winsorized `data` from
#' [outlier_group_analysis()] directly -- no statistics are recomputed here.
#'
#' @param result A list as returned by [outlier_group_analysis()].
#' @param x_label X-axis label for the concentration values.
#' @return A ggplot object.
#' @export
plot_outlier_treatment_comparison <- function(result, x_label = "Measured value") {
  data <- result$data
  n_double <- sum(data$dot_fill == "both", na.rm = TRUE)
  trimmed_label <- paste0(
    "Trimmed (double-flagged, n = ",
    n_double,
    " removed)"
  )

  plot_data <- dplyr::bind_rows(
    dplyr::transmute(data, value = MEASURED_VALUE_STANDARD, treatment = "Unprocessed"),
    dplyr::transmute(data, value = value_winsor_98, treatment = "Winsorized (98%)"),
    dplyr::transmute(data, value = value_winsor_90, treatment = "Winsorized (90%)"),
    data |>
      dplyr::filter(is.na(dot_fill) | dot_fill != "both") |>
      dplyr::transmute(value = MEASURED_VALUE_STANDARD, treatment = trimmed_label)
  ) |>
    dplyr::mutate(
      treatment = factor(
        treatment,
        levels = c("Unprocessed", "Winsorized (98%)", "Winsorized (90%)", trimmed_label)
      )
    )

  plot_data |>
    ggplot2::ggplot(ggplot2::aes(x = value, y = treatment, fill = treatment)) +
    ggplot2::geom_violin(scale = "width", colour = NA, alpha = 0.65, width = 0.8) +
    ggplot2::geom_boxplot(
      fill = NA,
      width = 0.08,
      outliers = FALSE,
      whisker.colour = "grey40",
      staple.colour = "grey40",
      box.colour = "grey40"
    ) +
    outlier_log10_scale(plot_data$value) +
    ggplot2::scale_fill_brewer(palette = "BuGn") +
    ggplot2::labs(x = x_label, y = NULL, title = result$group_label) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(legend.position = "none")
}

#' Summarise Flagged Measurements by Comment and Campaign
#'
#' Reproduces the "flagged measurements" summary tables used throughout the
#' outlier notebooks: one row per (MEASUREMENT_COMMENT, CAMPAIGN_NAME_SHORT)
#' combination among flagged points, with n and mean.
#'
#' @param data A data frame with `dot_fill`, `MEASUREMENT_COMMENT`,
#'   `CAMPAIGN_NAME_SHORT` and `MEASURED_VALUE_STANDARD` columns.
#' @return A `flextable` object, or `NULL` if no rows are flagged.
#' @export
outlier_flag_table <- function(data) {
  flagged <- data |>
    dplyr::filter(!is.na(dot_fill), !dot_fill %in% c("neither", "not tested"))

  if (nrow(flagged) == 0) {
    return(NULL)
  }

  flagged |>
    dplyr::group_by(MEASUREMENT_COMMENT, CAMPAIGN_NAME_SHORT) |>
    dplyr::reframe(n = dplyr::n(), mean = mean(MEASURED_VALUE_STANDARD)) |>
    dplyr::arrange(dplyr::desc(mean)) |>
    flextable::flextable() |>
    flextable::autofit() |>
    flextable::set_table_properties(width = 1, layout = "autofit") |>
    flextable::colformat_double(j = "mean") |>
    flextable::set_header_labels(
      MEASUREMENT_COMMENT = "Sample Comment",
      CAMPAIGN_NAME_SHORT = "Campaign",
      n = "n",
      mean = "Mean"
    )
}
