# Group triage plots (PLAN.md P1.1).
#
# One function per plot, each returning a ggplot. Plots are written to
# individual PNGs by write_triage_plots(); nothing here is ever stored in a
# targets object, because a ggplot captures its whole input data and drawing
# happens at print time anyway (CLAUDE.md 4.4).
#
# PILOT SCOPE: these are being trialled on 5 randomly sampled groups before
# being generalised. Expect the aesthetics not to fit every group yet.

# ---- Group selection ---------------------------------------------------

#' Columns That Define a Sample Group
#' @return A character vector of column names.
triage_group_cols <- function() {
  c(
    "ENVIRON_COMPARTMENT",
    "ENVIRON_COMPARTMENT_SUB",
    "SPECIES_GROUP",
    "SAMPLE_SPECIES",
    "SAMPLE_TISSUE",
    "SITE_GEOGRAPHIC_FEATURE",
    "SITE_GEOGRAPHIC_FEATURE_SUB",
    "MEASURED_UNIT_STANDARD"
  )
}

#' Sample Groups for Triage
#'
#' Picks groups from the summary table for which triage plots are worth making.
#'
#' CAVEAT on `min_n`: `n` in `summarise_literature_data` is `sum(MEASURED_N)`,
#' i.e. a count of *measurements*, whereas the plots draw one mark per *row*.
#' A group can therefore clear `min_n` on aggregated measurements while having
#' very few rows to plot. `n_rows` is returned alongside so you can see when
#' that happens.
#'
#' @param summary_data The `summarise_literature_data` target.
#' @param data The `literature_analysis_ready` target, used to count rows.
#' @param min_n Minimum `n` (measurements) for a group to be considered.
#' @param n_sample Number of groups to sample. `Inf` takes all of them.
#' @param seed Random seed, so the pilot selection is reproducible.
#' @return A tibble of group-defining columns plus `n`, `n_rows`, and a
#'   filesystem-safe `group_slug`.
#' @export
sample_triage_groups <- function(
  summary_data,
  data,
  min_n = 100,
  n_sample = 5,
  seed = 20260729
) {
  group_cols <- triage_group_cols()

  row_counts <- data |>
    dplyr::count(dplyr::across(dplyr::all_of(group_cols)), name = "n_rows")

  eligible <- summary_data |>
    dplyr::filter(.data$n >= min_n) |>
    dplyr::select(dplyr::all_of(group_cols), "n") |>
    dplyr::left_join(row_counts, by = group_cols)

  withr::with_seed(seed, {
    picked <- if (is.infinite(n_sample) || n_sample >= nrow(eligible)) {
      eligible
    } else {
      dplyr::slice_sample(eligible, n = n_sample)
    }
  })

  picked |>
    dplyr::mutate(
      group_slug = slugify_name(triage_group_label(picked, sep = "_"))
    )
}

#' Human-Readable Label for a Group
#'
#' Biota groups are labelled by taxonomy; everything else by compartment. The
#' geography and unit are appended so that two groups differing only by site
#' type or unit do not collide.
#'
#' @param grp A one-or-more-row tibble of group-defining columns.
#' @param sep Separator between label fragments.
#' @return A character vector of labels.
#' @export
triage_group_label <- function(grp, sep = " / ") {
  taxon <- paste(
    dplyr::coalesce(grp$SPECIES_GROUP, "Unknown"),
    dplyr::coalesce(grp$SAMPLE_SPECIES, "spp."),
    dplyr::coalesce(grp$SAMPLE_TISSUE, "whole"),
    sep = sep
  )
  compartment <- paste(
    grp$ENVIRON_COMPARTMENT,
    grp$ENVIRON_COMPARTMENT_SUB,
    sep = sep
  )
  paste(
    dplyr::if_else(grp$ENVIRON_COMPARTMENT == "Biota", taxon, compartment),
    dplyr::coalesce(grp$SITE_GEOGRAPHIC_FEATURE, "Unknown site"),
    grp$MEASURED_UNIT_STANDARD,
    sep = sep
  )
}

#' Subset Data to a Single Group
#'
#' Matches on all group-defining columns, treating `NA` as a value to match
#' (a plain `==` filter would silently drop `NA` groups, which are common in
#' the non-biota compartments).
#'
#' @param data The `literature_analysis_ready` target.
#' @param grp A one-row tibble of group-defining columns.
#' @param exclude_cols Group columns to ignore when matching. Used by the
#'   overall-distribution plot, which deliberately keeps every unit for an
#'   otherwise-identical group so dry and wet weight can be compared.
#' @return A filtered data frame.
#' @export
filter_to_group <- function(data, grp, exclude_cols = character(0)) {
  keep <- rep(TRUE, nrow(data))
  for (col in setdiff(triage_group_cols(), exclude_cols)) {
    want <- grp[[col]][1]
    have <- data[[col]]
    keep <- keep &
      if (is.na(want)) is.na(have) else (!is.na(have) & have == want)
  }
  data[keep, , drop = FALSE]
}

# ---- Presentation helpers ----------------------------------------------

#' Should This Plot Show Points Rather Than Bins?
#'
#' Below `threshold` observations a 2D bin or density is mostly empty cells and
#' conveys less than the raw points; above it, points overplot into a solid
#' block and the render slows to a crawl. One helper owns this decision so the
#' switch is consistent across every triage plot.
#'
#' @param x A vector (or data frame) whose length/rows is the observation count.
#' @param threshold Cutoff below which points are preferred.
#' @return `TRUE` if points should be drawn.
#' @export
triage_use_points <- function(x, threshold = 30) {
  n <- if (is.data.frame(x)) nrow(x) else length(x)
  n < threshold
}

#' Standard Unit Label for Triage Plot Axes
#'
#' Where a subset spans more than one unit (only the overall-distribution
#' plot, which is deliberately unit-agnostic), the unit is carried by the
#' colour legend instead and the axis label stays generic.
#'
#' @param data A group subset.
#' @return A single string.
#' @export
triage_unit_label <- function(data) {
  units <- unique(data$MEASURED_UNIT_STANDARD)
  if (length(units) != 1) {
    return("Measured value")
  }
  paste0("Measured value (", units, ")")
}

#' Tidy Vannmiljø Campaign Names for Display
#'
#' Vannmiljø campaigns arrive as `Vm_2010_2025 (Polluted Seabed)`. The prefix
#' is constant across 44 of the 72 campaigns, so it costs axis space without
#' distinguishing anything; strip it and keep the parenthesised activity.
#' Non-Vannmiljø campaign names (`NorSeal1988` and friends) are returned
#' unchanged.
#'
#' NB: the versions of this in `docs/NBXX-Outliers.qmd` and
#' `scripts/reference_triage_plots.R` use a `case_when()` with no `.default`,
#' so every non-Vannmiljø campaign silently becomes `NA`. That is 28 of 72
#' campaigns. This function does not have that bug.
#'
#' @param x A character vector of campaign names.
#' @return A character vector the same length as `x`.
#' @export
prettify_campaign_name <- function(x) {
  dplyr::case_when(
    stringr::str_detect(x, "Vm_2010_2025") ~
      x |>
        stringr::str_remove("^Vm_2010_2025\\s*\\(") |>
        stringr::str_remove("\\)$"),
    .default = x
  )
}

# ---- The five plots ----------------------------------------------------

#' Triage Plot: Overall Distribution, Split by Unit
#'
#' Unlike the other four views this one is deliberately **unit-agnostic**: it
#' should be passed a subset that still contains every unit for the group (see
#' the `exclude_cols` argument of [filter_to_group()]). The whole point of the
#' plot is to show how far dry and wet weight concentrations diverge, and since
#' the group key includes the unit, filtering by it would collapse exactly the
#' comparison the plot exists to make.
#'
#' @param data A group subset, retaining all units.
#' @param label Group label for the subtitle.
#' @return A ggplot.
#' @export
triage_plot_density <- function(data, label = NULL) {
  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data$MEASURED_VALUE_STANDARD,
      colour = .data$MEASURED_UNIT_STANDARD
    )
  )

  p <- if (triage_use_points(data)) {
    p +
      ggplot2::geom_dotplot(
        ggplot2::aes(fill = .data$MEASURED_UNIT_STANDARD),
        method = "histodot",
        binwidth = 0.05
      )
  } else {
    p + ggplot2::geom_density() + ggplot2::geom_rug(alpha = 0.1)
  }

  p +
    ggplot2::scale_x_log10() +
    ggplot2::labs(
      x = triage_unit_label(data),
      y = "Density",
      colour = "Unit",
      fill = "Unit",
      title = "a) Overall distribution",
      subtitle = label
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(legend.position = "bottom")
}

#' Triage Plot: Concentration by Sampling Date
#' @param data A group subset. @param label Group label for the subtitle.
#' @return A ggplot.
#' @export
triage_plot_by_date <- function(data, label = NULL) {
  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$SAMPLING_DATE, y = .data$MEASURED_VALUE_STANDARD)
  )

  p <- if (triage_use_points(data)) {
    p + ggplot2::geom_point(alpha = 0.7)
  } else {
    p +
      ggplot2::geom_bin2d(bins = 60) +
      ggplot2::scale_fill_viridis_b(option = "plasma", name = "Count")
  }

  p +
    ggplot2::geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
    ggplot2::scale_y_log10() +
    ggplot2::labs(
      x = "Sampling date",
      y = triage_unit_label(data),
      title = "b) Concentration by date",
      subtitle = label
    )
}

#' Triage Plot: Distribution by a Categorical Facet
#'
#' Shared implementation behind the by-campaign and by-site-type plots: both
#' are "distribution of value, split by some category on the y axis", and
#' differ only in which column and how the labels are tidied.
#'
#' @param data A group subset.
#' @param facet_col Column name (string) to put on the y axis.
#' @param title Plot title.
#' @param label Group label for the subtitle.
#' @param min_facet_n Drop categories with fewer than this many rows.
#' @param wrap_width Width at which to wrap category labels.
#' @param label_fn Function applied to the category labels before plotting,
#'   e.g. [prettify_campaign_name()]. Defaults to leaving them alone.
#' @return A ggplot.
#' @export
triage_plot_by_category <- function(
  data,
  facet_col,
  title,
  label = NULL,
  min_facet_n = 10,
  wrap_width = 15,
  label_fn = identity
) {
  plot_data <- data |>
    dplyr::filter(!is.na(.data[[facet_col]])) |>
    dplyr::add_count(.data[[facet_col]], name = ".facet_n") |>
    dplyr::filter(.data$.facet_n >= min_facet_n) |>
    dplyr::mutate(
      .facet = forcats::fct_reorder(
        label_fn(as.character(.data[[facet_col]])),
        .data$MEASURED_VALUE_STANDARD,
        stats::median,
        .na_rm = TRUE
      ) |>
        forcats::fct_relabel(stringr::str_wrap, width = wrap_width)
    )

  if (nrow(plot_data) == 0) {
    return(triage_empty_plot(
      title,
      paste0("no category with n >= ", min_facet_n)
    ))
  }

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$MEASURED_VALUE_STANDARD, y = .data$.facet)
  )

  p <- if (triage_use_points(plot_data)) {
    p + ggplot2::geom_point(alpha = 0.7)
  } else {
    p +
      ggplot2::geom_bin2d(bins = 40) +
      ggplot2::scale_fill_viridis_b(name = "Count")
  }

  p +
    ggplot2::scale_x_log10() +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      x = triage_unit_label(data),
      y = NULL,
      title = title,
      subtitle = paste0(
        label,
        if (!is.null(label)) "  ",
        "(n ≥ ",
        min_facet_n,
        " per row)"
      )
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = ggplot2::rel(0.6))
    )
}

#' Triage Plot: Spatial Distribution
#'
#' Median concentration per hex cell over a coastline base map. Falls back to
#' points where there are too few sites to bin meaningfully.
#'
#' @param data A group subset. @param label Group label for the subtitle.
#' @return A ggplot.
#' @export
triage_plot_spatial <- function(data, label = NULL) {
  spatial <- data |>
    dplyr::filter(!is.na(.data$LONGITUDE), !is.na(.data$LATITUDE))

  if (nrow(spatial) == 0) {
    return(triage_empty_plot("e) Spatial distribution", "no coordinates"))
  }

  # NB: map_data() is exported by ggplot2, not by maps (maps is only needed as
  # the underlying database). Passing a maps::map() object straight to
  # geom_polygon() routes through ggplot2's deprecated fortify.map(), which
  # errors with "subscript out of bounds" in names[df$group, 1].
  world_map <- ggplot2::map_data("world")
  bbox <- get_study_area_bbox()

  base <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = world_map,
      ggplot2::aes(x = .data$long, y = .data$lat, group = .data$group),
      fill = "lightgray",
      colour = "white"
    )

  layer <- if (triage_use_points(spatial)) {
    ggplot2::geom_point(
      data = spatial,
      ggplot2::aes(
        x = .data$LONGITUDE,
        y = .data$LATITUDE,
        colour = .data$MEASURED_VALUE_STANDARD
      ),
      size = 2
    )
  } else {
    ggplot2::stat_summary_hex(
      data = spatial,
      ggplot2::aes(
        x = .data$LONGITUDE,
        y = .data$LATITUDE,
        z = .data$MEASURED_VALUE_STANDARD
      ),
      fun = "median",
      bins = 60,
      alpha = 0.75
    )
  }

  scale_layer <- if (triage_use_points(spatial)) {
    ggplot2::scale_colour_viridis_c(
      name = triage_unit_label(data),
      trans = "log10",
      option = "rocket"
    )
  } else {
    ggplot2::scale_fill_viridis_b(
      name = triage_unit_label(data),
      trans = "log10",
      n.breaks = 6,
      option = "rocket"
    )
  }

  base +
    layer +
    scale_layer +
    ggplot2::coord_fixed(
      ratio = 2,
      xlim = c(bbox[[1]], bbox[[3]]),
      ylim = c(50, bbox[[4]])
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = "e) Spatial distribution",
      subtitle = paste0(label, if (!is.null(label)) "  ", "(median per cell)")
    ) +
    ggplot2::theme(legend.position = "right")
}

#' Placeholder Plot for Groups a Given View Cannot Describe
#'
#' Returning a labelled blank rather than erroring keeps one awkward group from
#' killing a whole batch of triage plots, and makes the gap visible on the
#' contact sheet rather than silent.
#'
#' @param title Plot title. @param reason Short explanation.
#' @return A ggplot.
#' @export
triage_empty_plot <- function(title, reason) {
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text",
      x = 0,
      y = 0,
      label = paste0("Not available:\n", reason),
      size = 5,
      colour = "grey40"
    ) +
    ggplot2::labs(title = title) +
    ggplot2::theme_void()
}

# ---- Writing -----------------------------------------------------------

#' Write All Triage Plots for One Group
#'
#' @param data The `literature_analysis_ready` target.
#' @param grp A one-row tibble from [sample_triage_groups()].
#' @param dir Output directory.
#' @param width,height,dpi PNG canvas. Fixed on purpose: a 40,000-row group and
#'   a 150-row group must produce the same-sized artefact, or the contact sheet
#'   becomes unreadable.
#' @return A character vector of written file paths.
#' @export
write_triage_plots_for_group <- function(
  data,
  grp,
  dir = "_triage",
  width = 8,
  height = 5,
  dpi = 150
) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  group_data <- filter_to_group(data, grp)
  # Plot (a) keeps every unit for the group on purpose; see
  # triage_plot_density(). The other four stay unit-specific.
  group_data_all_units <- filter_to_group(
    data,
    grp,
    exclude_cols = "MEASURED_UNIT_STANDARD"
  )
  label <- triage_group_label(grp)
  slug <- grp$group_slug[1]

  # List names carry the a/b/c/d/e prefix so the written files sort into
  # reading order in a file browser.
  plots <- list(
    a_density = triage_plot_density(group_data_all_units, label),
    b_date = triage_plot_by_date(group_data, label),
    c_campaign = triage_plot_by_category(
      group_data,
      "CAMPAIGN_NAME_SHORT",
      "c) Distribution by campaign",
      label,
      label_fn = prettify_campaign_name
    ),
    d_site_type = triage_plot_by_category(
      group_data,
      "SITE_GEOGRAPHIC_FEATURE_SUB",
      "d) Distribution by site type",
      label
    ),
    e_spatial = triage_plot_spatial(group_data, label)
  )

  paths <- character(0)
  for (nm in names(plots)) {
    path <- file.path(dir, paste0(slug, "_", nm, ".png"))
    ggplot2::ggsave(
      filename = path,
      plot = plots[[nm]],
      width = width,
      height = height,
      dpi = dpi,
      device = ragg::agg_png
    )
    paths <- c(paths, path)
  }
  paths
}

#' Write Triage Plots for Several Groups
#'
#' @param data The `literature_analysis_ready` target.
#' @param groups Output of [sample_triage_groups()].
#' @param dir Output directory.
#' @param ... Passed to [write_triage_plots_for_group()].
#' @return A character vector of all written file paths, for `format = "file"`.
#' @export
write_triage_plots <- function(data, groups, dir = "_triage", ...) {
  paths <- purrr::map(
    seq_len(nrow(groups)),
    function(i) {
      grp <- groups[i, , drop = FALSE]
      message("Triage plots: ", triage_group_label(grp))
      write_triage_plots_for_group(data, grp, dir = dir, ...)
    }
  )
  unlist(paths, use.names = FALSE)
}
