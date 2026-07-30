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
#' @return A tibble of group-defining columns plus `n`, `n_sources` (distinct
#'   REFERENCE_ID), `n_rows`, a filesystem-safe `group_slug`, a heading-anchor
#'   `heading_slug` shared by every unit variant of the same group, and
#'   `n_heading` (measurements summed across unit variants), sorted for
#'   hierarchical presentation.
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
    dplyr::select(
      dplyr::all_of(group_cols),
      "n",
      "n_sources",
      # Carried through so the notebook can print each group's flags under its
      # heading from the same source the summary table highlights from. any_of()
      # rather than all_of(): sample_triage_groups() is also called in tests with
      # a bare summary fixture that has none of these.
      dplyr::any_of(c(
        "species_common_name",
        "n_units",
        "dip_p",
        "multimodal",
        "outlier_fraction",
        "prop_dropped",
        "flag_outliers",
        "flag_multimodal"
      ))
    ) |>
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
      group_slug = slugify_name(triage_group_label(picked, sep = "_")),
      heading_slug = heading_anchor(picked)
    ) |>
    sort_triage_groups()
}

#' Columns Forming the Heading Hierarchy
#'
#' The group key minus the unit. Unit is deliberately **not** a heading level:
#' at most two units occur per group, so they sit as separate plot rows under a
#' shared heading rather than splitting the tree.
#'
#' @return A character vector of column names, outermost first.
#' @export
triage_heading_cols <- function() {
  setdiff(triage_group_cols(), "MEASURED_UNIT_STANDARD")
}

#' Sort Groups for Hierarchical Presentation
#'
#' Two requirements pull against each other here: nested headings need the tree
#' traversed in order, while a triage sheet should lead with the groups carrying
#' the most data. The resolution is to nest, and order **siblings within each
#' parent** by descending measurement count. So the heaviest compartment comes
#' first, and within it the heaviest sub-compartment, and so on down to the leaf.
#'
#' Weights are `sum(MEASURED_N)` summed across unit variants but nothing else,
#' per the 2026-07-30 decision: a group split only by dry and wet weight is one
#' group for ordering purposes.
#'
#' @param groups Output of [sample_triage_groups()] before sorting.
#' @return The same tibble, reordered, with `n_heading` added.
#' @export
sort_triage_groups <- function(groups) {
  heading_cols <- triage_heading_cols()

  weighted <- groups |>
    dplyr::group_by(dplyr::across(dplyr::all_of(heading_cols))) |>
    dplyr::mutate(n_heading = sum(.data$n)) |>
    dplyr::ungroup()

  if (nrow(weighted) == 0) {
    return(weighted)
  }

  # Build the sort key as plain vectors and hand them to order(). Doing this
  # through arrange() would need quoted column references built at runtime; the
  # vector form is shorter and much easier to reason about.
  keys <- list()
  for (depth in seq_along(heading_cols)) {
    cols <- heading_cols[seq_len(depth)]
    # Weight of this row's ancestor node at this depth, i.e. the total carried by
    # the sibling it competes with at this level.
    ancestor_weight <- weighted |>
      dplyr::group_by(dplyr::across(dplyr::all_of(cols))) |>
      dplyr::mutate(.w = sum(.data$n)) |>
      dplyr::ungroup() |>
      dplyr::pull(".w")
    keys <- c(
      keys,
      list(-ancestor_weight),
      # Name breaks ties, so two equally weighted siblings order deterministically
      # rather than by whatever the sample happened to produce.
      list(dplyr::coalesce(weighted[[heading_cols[depth]]], ""))
    )
  }
  keys <- c(keys, list(-weighted$n))

  weighted[do.call(order, keys), , drop = FALSE]
}

#' Stable Heading Anchor for a Group
#'
#' Every unit variant of a group shares one heading, so the anchor is built from
#' the heading columns only.
#'
#' Deliberately does **not** use [slugify_name()], which ends in `make.unique()`.
#' Two things break under that here: unit variants legitimately share an anchor
#' and would be handed `_1` / `_2` suffixes, and the suffix a given group
#' receives depends on what else is in the vector, so the anchor computed over
#' 245 summary-table rows would not match the one computed over 25 triaged
#' groups. Silently linking to the wrong section is worse than failing, so
#' uniqueness is asserted against the distinct key instead.
#'
#' @param grp A tibble of group-defining columns.
#' @return A character vector of anchors, prefixed `grp-`.
#' @export
heading_anchor <- function(grp) {
  heading_cols <- triage_heading_cols()
  # NA levels are omitted rather than spelled "NA": every non-biota group has
  # three NA taxonomy columns, which turned every abiotic anchor into
  # "...-freshwater-na-na-na-river...". Dropping them cannot introduce an
  # ambiguity that the assertion below would not catch.
  key <- vapply(
    seq_len(nrow(grp)),
    function(i) {
      parts <- vapply(
        heading_cols,
        function(col) as.character(grp[[col]][i]),
        character(1)
      )
      paste(parts[!is.na(parts)], collapse = "_")
    },
    character(1)
  )
  slug <- key |>
    stringr::str_replace_all("[^A-Za-z0-9]+", "-") |>
    stringr::str_replace_all("^-+|-+$", "") |>
    tolower()

  # A collision would mean two distinct heading keys slugging to one anchor,
  # which silently merges two sections. Fail instead.
  distinct_keys <- length(unique(key))
  distinct_slugs <- length(unique(slug))
  if (distinct_slugs != distinct_keys) {
    stop(
      "heading_anchor(): ",
      distinct_keys,
      " distinct group keys collapsed to ",
      distinct_slugs,
      " anchors. Two sections would share one link target."
    )
  }

  paste0("grp-", slug)
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
    # SITE_GEOGRAPHIC_FEATURE_SUB is part of the group key, so omitting it made
    # distinct groups share a label. slugify_name() then disambiguated them
    # with make.unique() suffixes (_1, _2), which (a) put two identically
    # titled headings in the notebook and (b) left the unsuffixed slug as a
    # string prefix of the suffixed one, breaking filename matching.
    dplyr::coalesce(grp$SITE_GEOGRAPHIC_FEATURE_SUB, "Unknown sub-site"),
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

# ---- Shared scales -----------------------------------------------------

#' Compute Shared Value-Axis Limits
#'
#' Triage plots are only comparable if they share axes. Computing limits from
#' each group's own data (the ggplot2 default) means every panel silently
#' rescales, so two groups an order of magnitude apart can look identical.
#' This derives limits once from the whole dataset and they are then passed
#' into every plot.
#'
#' Grouped by `ENVIRON_COMPARTMENT` by default. Be aware of what that does and
#' does not buy you: as of 2026-07-29 Aquatic alone spans 12.3 orders of
#' magnitude, which is the entire global range, so for ~90% of the rows a
#' per-compartment limit is a global limit. Adding `MEASURED_UNIT_STANDARD` to
#' `by` only narrows Aquatic/mg-L to 9.8 orders; the spread is genuinely within
#' unit, not an artefact of mixing them. Widen or narrow via `by` as needed.
#'
#' No epsilon is added for the log scale: `literature_analysis_ready` has
#' already dropped zeros and negatives, so every value is strictly positive.
#'
#' @param data The `literature_analysis_ready` target.
#' @param by Columns defining a scale group.
#' The **date** range is deliberately global and never grouped. Time is the one
#' axis where a per-group scale is always wrong: a group sampled only in 2019
#' would otherwise fill the whole panel and look identical to one sampled over
#' thirty years.
#'
#' @param pad Multiplicative padding applied to each end of the value axis, so
#'   points do not sit exactly on the panel edge. Dates are not padded.
#' @return A tibble of `by` columns plus `value_min`, `value_max`, and the
#'   global `date_min` / `date_max`.
#' @export
compute_triage_scale_limits <- function(
  data,
  by = "ENVIRON_COMPARTMENT",
  pad = 1.5
) {
  date_range <- range(data$SAMPLING_DATE, na.rm = TRUE)

  data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::summarise(
      value_min = min(.data$MEASURED_VALUE_STANDARD, na.rm = TRUE) / pad,
      value_max = max(.data$MEASURED_VALUE_STANDARD, na.rm = TRUE) * pad,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      date_min = as.Date(date_range[1]),
      date_max = as.Date(date_range[2])
    )
}

#' Global Date Limits from a Scale-Limits Table
#'
#' Constant across every row by construction; see
#' [compute_triage_scale_limits()].
#'
#' @param limits Output of [compute_triage_scale_limits()].
#' @return A length-2 Date vector, or `NULL`.
#' @export
triage_date_limits <- function(limits) {
  if (is.null(limits) || !all(c("date_min", "date_max") %in% names(limits))) {
    return(NULL)
  }
  c(limits$date_min[1], limits$date_max[1])
}

#' Look Up Shared Limits for One Group
#'
#' Falls back to `NULL` (i.e. let ggplot2 choose) when the group has no
#' matching entry, so an unexpected compartment degrades to the old behaviour
#' rather than erroring mid-batch.
#'
#' @param limits Output of [compute_triage_scale_limits()].
#' @param grp A one-row tibble of group-defining columns.
#' @return A length-2 numeric vector, or `NULL`.
#' @export
triage_limits_for <- function(limits, grp) {
  if (is.null(limits)) {
    return(NULL)
  }
  by <- setdiff(
    names(limits),
    c("value_min", "value_max", "date_min", "date_max")
  )
  row <- limits
  for (col in by) {
    row <- row[row[[col]] == grp[[col]][1], , drop = FALSE]
  }
  if (nrow(row) != 1) {
    return(NULL)
  }
  c(row$value_min[1], row$value_max[1])
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

# ---- Threshold reference lines -----------------------------------------
#
# v2, 2026-07-30. The first attempt annotated each line with rotated in-panel
# text. It did not survive contact with real data: the shared per-compartment
# value axis spans up to 12.3 orders of magnitude while the M-608 boundaries sit
# inside about one, so three labels landed within 7% of the panel width and
# stacked into an unreadable block. In-panel text is also the most
# resize-sensitive thing on a plot.
#
# The class names now go on a secondary axis and the panel carries no text at
# all. Severity reads off colour and linetype, both keyed on the class number.

#' Thresholds Visible Within the Axis Limits
#'
#' Dropped rather than clamped: a threshold that does not apply to the visible
#' range should vanish, not pile up on the panel edge.
#'
#' @param thresholds Output of [thresholds_for_group()].
#' @param limits Shared value-axis limits, or `NULL` to keep everything.
#' @return A possibly-empty subset of `thresholds`.
#' @export
thresholds_in_limits <- function(thresholds, limits = NULL) {
  if (is.null(thresholds) || nrow(thresholds) == 0) {
    return(empty_threshold_match())
  }
  if (is.null(limits) || !all(is.finite(limits))) {
    return(thresholds)
  }
  thresholds |>
    dplyr::filter(
      .data$THRESHOLD_VALUE_STANDARD >= limits[1],
      .data$THRESHOLD_VALUE_STANDARD <= limits[2]
    )
}

#' Threshold Reference Lines for a Triage Panel
#'
#' Returns a list of layers, so a panel with no applicable threshold adds nothing
#' and needs no branching at the call site. No labels: those live on the
#' secondary axis, via [triage_threshold_sec_axis()].
#'
#' No halo under these lines. The secondary axis already says where they sit, and
#' the class colours contrast with the viridis fills on their own.
#'
#' @param thresholds Output of [thresholds_for_group()].
#' @param orientation `"vertical"` where the measured value is on x (panels a, c,
#'   d), `"horizontal"` where it is on y (panel b).
#' @param limits Shared value-axis limits, used to drop off-scale lines.
#' @param linewidth Line width.
#' @return A list of ggplot2 layers, possibly empty.
#' @export
triage_threshold_layers <- function(
  thresholds,
  orientation = c("vertical", "horizontal"),
  limits = NULL,
  linewidth = 0.7
) {
  orientation <- match.arg(orientation)
  thresholds <- thresholds_in_limits(thresholds, limits)
  if (nrow(thresholds) == 0) {
    return(list())
  }

  cls <- as.character(threshold_class_number(thresholds$THRESHOLD_CLASS))
  colours <- unname(threshold_class_colours()[cls])
  linetypes <- unname(threshold_class_linetypes()[cls])

  geom <- if (orientation == "vertical") {
    ggplot2::geom_vline
  } else {
    ggplot2::geom_hline
  }
  mapping <- if (orientation == "vertical") {
    ggplot2::aes(xintercept = .data$THRESHOLD_VALUE_STANDARD)
  } else {
    ggplot2::aes(yintercept = .data$THRESHOLD_VALUE_STANDARD)
  }

  list(geom(
    data = thresholds,
    mapping,
    colour = colours,
    linetype = linetypes,
    linewidth = linewidth
  ))
}

#' Secondary Axis Naming the Threshold Classes
#'
#' Breaks at the threshold values, labelled with the class numeral (or the
#' threshold type where there is no class, since PROREF and BAC are styled as
#' class I but are not Norwegian classification classes). The axis title names
#' the source.
#'
#' Returns a `waiver()` where nothing applies, which is what `sec.axis` expects
#' when there is no secondary axis, so call sites need no branching.
#'
#' @param thresholds Output of [thresholds_for_group()].
#' @param limits Shared value-axis limits, used to drop off-scale breaks.
#' @return A `ggplot2::dup_axis()` specification, or `ggplot2::waiver()`.
#' @export
triage_threshold_sec_axis <- function(thresholds, limits = NULL) {
  thresholds <- thresholds_in_limits(thresholds, limits)
  if (nrow(thresholds) == 0) {
    return(ggplot2::waiver())
  }
  ggplot2::dup_axis(
    breaks = thresholds$THRESHOLD_VALUE_STANDARD,
    labels = threshold_axis_label(thresholds),
    # Several sources can coexist on the unit-agnostic overall-distribution
    # panel, so this is a set rather than a single name.
    name = paste(unique(thresholds$REFERENCE_ID), collapse = " / ")
  )
}

#' Theme Tweaks for the Threshold Secondary Axis
#'
#' Roman numerals at the default axis text size read as tick marks rather than
#' labels, so they are bolder and slightly larger. Applied separately from
#' [triage_theme()] because only the panels carrying a secondary axis want it.
#'
#' @param position `"top"` for a vertical-line panel, `"right"` for panel b.
#' @return A ggplot2 theme.
#' @export
triage_sec_axis_theme <- function(position = c("top", "right")) {
  position <- match.arg(position)
  if (position == "top") {
    ggplot2::theme(
      axis.text.x.top = ggplot2::element_text(
        size = ggplot2::rel(0.9),
        face = "bold"
      ),
      axis.title.x.top = ggplot2::element_text(size = ggplot2::rel(0.8))
    )
  } else {
    ggplot2::theme(
      axis.text.y.right = ggplot2::element_text(
        size = ggplot2::rel(0.9),
        face = "bold"
      ),
      axis.title.y.right = ggplot2::element_text(size = ggplot2::rel(0.8))
    )
  }
}

#' Shared Theme for the Triage Panels
#'
#' `theme_minimal()` plus the tweaks every panel wants. One function so the look
#' can be changed in one place.
#'
#' **This has to be added before any per-panel `theme()` call.**
#' `theme_minimal()` replaces the whole theme, whereas `theme()` modifies it, so
#' `theme(legend.position = "bottom") + theme_minimal()` silently discards the
#' legend position. Every call site below therefore puts this first.
#'
#' It also cannot live inside [triage_threshold_layers()]: that returns a bare
#' list of layers, and ggplot2 refuses to add a theme to a geom outside a plot
#' ("Cannot add ggproto objects together").
#'
#' @return A list of ggplot2 theme components.
#' @export
triage_theme <- function() {
  list(
    ggplot2::theme_minimal(),
    ggplot2::theme(
      # The panel is busy enough with tiles, thresholds and their labels.
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.85))
    )
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
#' @param limits Shared value-axis limits from [triage_limits_for()].
#' @param thresholds The `copper_toxicity_thresholds` target, or `NULL` for no
#'   reference lines.
#' @param grp The one-row group tibble, needed to match thresholds. Because this
#'   panel spans units, thresholds are matched **once per unit present** and the
#'   unit is prepended to each label; matching on the group's own unit alone
#'   would draw wet-weight lines across a dry-weight curve.
#' @return A ggplot.
#' @export
triage_plot_density <- function(
  data,
  label = NULL,
  limits = NULL,
  thresholds = NULL,
  grp = NULL
) {
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
    p +
      ggplot2::geom_density() +
      ggplot2::geom_rug(alpha = 0.15, linewidth = 0.7)
  }

  thr <- thresholds_for_group_by_unit(
    thresholds,
    grp,
    unique(data$MEASURED_UNIT_STANDARD)
  )

  p +
    triage_threshold_layers(thr, orientation = "vertical", limits = limits) +
    ggplot2::scale_x_log10(
      limits = limits,
      sec.axis = triage_threshold_sec_axis(thr, limits = limits)
    ) +
    ggplot2::labs(
      x = triage_unit_label(data),
      y = "Density",
      colour = "Unit",
      fill = "Unit",
      title = "a) Overall distribution",
      subtitle = label
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    triage_theme() +
    triage_sec_axis_theme("top") +
    ggplot2::theme(legend.position = "bottom")
}

#' Thresholds Across Several Units
#'
#' Used only by the unit-agnostic overall-distribution panel. Where more than one
#' unit is present the unit is prepended to each label, since two lines an order
#' of magnitude apart otherwise look like disagreeing sources rather than
#' different bases of measurement.
#'
#' @param thresholds The `copper_toxicity_thresholds` target, or `NULL`.
#' @param grp A one-row group tibble, or `NULL`.
#' @param units Character vector of units present in the subset.
#' @return A tibble as [thresholds_for_group()], possibly zero-row.
#' @export
thresholds_for_group_by_unit <- function(thresholds, grp, units) {
  if (is.null(thresholds) || is.null(grp)) {
    return(empty_threshold_match())
  }
  units <- stats::na.omit(unique(units))
  matched <- purrr::map(
    units,
    function(u) {
      m <- thresholds_for_group(thresholds, grp, unit = u)
      if (nrow(m) > 0 && length(units) > 1) {
        m$threshold_label <- paste0(u, ": ", m$threshold_label)
      }
      m
    }
  )
  dplyr::bind_rows(matched)
}

#' Triage Plot: Concentration by Sampling Date
#' @param data A group subset. @param label Group label for the subtitle.
#' @param limits Shared value-axis limits from [triage_limits_for()].
#' @param date_limits Global date-axis limits from [triage_date_limits()].
#'   Always supply these: a per-group date axis makes a group sampled in one
#'   year look like one sampled over thirty.
#' @param thresholds The `copper_toxicity_thresholds` target, or `NULL`.
#' @param grp The one-row group tibble, needed to match thresholds.
#' @return A ggplot.
#' @export
triage_plot_by_date <- function(
  data,
  label = NULL,
  limits = NULL,
  date_limits = NULL,
  thresholds = NULL,
  grp = NULL
) {
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
    # A white halo underneath, so the trend line survives both the white panel of
    # theme_minimal() and the dark indigo end of the viridis fill. This is just a
    # second geom_smooth with different aesthetics: the fit is computed twice,
    # which is negligible next to the draw.
    #
    # The halo is SOLID while the line on top is dotted, deliberately. R
    # specifies dash patterns in multiples of the line width, so a wider halo
    # with a matched linetype gets proportionally longer dashes and drifts out of
    # phase along the line, leaving the grey dots sometimes on the halo and
    # sometimes off it.
    ggplot2::geom_smooth(
      method = "lm",
      se = FALSE,
      formula = y ~ x,
      colour = "white",
      alpha = 0.35,
      linewidth = 1.5
    ) +
    # Dotted and mid-grey on purpose. A solid coloured trend line reads as a
    # fitted model; this is an unweighted OLS fit of log10 concentration on date,
    # taking no account of unequal sampling effort, so it is an eye guide only.
    ggplot2::geom_smooth(
      method = "lm",
      se = FALSE,
      formula = y ~ x,
      linetype = "dotted",
      colour = "grey60",
      linewidth = 0.8
    ) +
    # Lines but no secondary axis on this panel. The classes would land on a
    # secondary *y* axis, where vertical space is far tighter than horizontal:
    # II and IV collide, and the rotated title sits awkwardly between the labels
    # and the legend. The classes are legible on panels a, c and d, and these are
    # triage plots. Flagged in PLAN.md P1.1g to revisit.
    triage_threshold_layers(
      thresholds_for_group(thresholds, grp),
      orientation = "horizontal",
      limits = limits
    ) +
    ggplot2::scale_x_date(limits = date_limits) +
    ggplot2::scale_y_log10(limits = limits) +
    ggplot2::labs(
      x = "Sampling date",
      y = triage_unit_label(data),
      title = "b) Concentration by date",
      subtitle = label
    ) +
    triage_theme()
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
#' No minimum category size is imposed. These panels answer "what campaigns and
#' site types are represented, and do their values differ", which is a coverage
#' question rather than a statistical one, so a category with two observations
#' is still worth seeing. Cardinality is bounded in practice (at most 31
#' campaigns and 5 site types per group), so this cannot produce the
#' unreadably tall figures that sank the first attempt.
#'
#' @param wrap_width Width at which to wrap category labels.
#' @param label_fn Function applied to the category labels before plotting,
#'   e.g. [prettify_campaign_name()]. Defaults to leaving them alone.
#' @param limits Shared value-axis limits from [triage_limits_for()].
#' @param thresholds The `copper_toxicity_thresholds` target, or `NULL`.
#' @param grp The one-row group tibble, needed to match thresholds.
#' @param x_bins Number of bins along the value axis. The category axis is always
#'   binned at exactly one category per bin; see below.
#' @return A ggplot.
#' @export
triage_plot_by_category <- function(
  data,
  facet_col,
  title,
  label = NULL,
  wrap_width = 15,
  label_fn = identity,
  limits = NULL,
  thresholds = NULL,
  grp = NULL,
  x_bins = 40
) {
  plot_data <- data |>
    dplyr::filter(!is.na(.data[[facet_col]])) |>
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
    return(triage_empty_plot(title, paste0("no non-missing ", facet_col)))
  }

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$MEASURED_VALUE_STANDARD, y = .data$.facet)
  )

  bw <- category_x_binwidth(plot_data, limits, x_bins)

  p <- if (triage_use_points(plot_data)) {
    p + ggplot2::geom_point(alpha = 0.7)
  } else {
    # Counted here and drawn with geom_tile() rather than handed to geom_bin2d().
    # Two reasons, both learned the hard way on 2026-07-30:
    #
    # 1. `bins = 40` (the original setting) bins BOTH axes. A discrete y scale is
    #    mapped to integer positions 1..k before the stat runs, so 40 bins across
    #    that range produced bands (k-1)/40 tall inside a row pitch of 1: thin
    #    stripes with visible gaps, wasting most of the panel height. Measured at
    #    0.179 against a pitch of 1.0.
    # 2. stat_bin2d() takes its binning range from the *shared scale*, not from
    #    its own layer. The threshold labels are placed at `y = Inf`, which is
    #    ordinary practice for annotating the top of a panel, and that pushed the
    #    y range to infinity: the stat then asked for more than a million bins
    #    and failed outright, drawing no heatmap at all.
    #
    # Counting explicitly decouples the two. Bands are exactly one category tall
    # by construction, and nothing another layer does to the scales can break it.
    p +
      ggplot2::geom_tile(
        data = count_by_category_bin(plot_data, bw, origin = limits[1]),
        ggplot2::aes(x = .data$value_mid, y = .data$.facet, fill = .data$count),
        width = bw,
        height = 1,
        inherit.aes = FALSE
      ) +
      ggplot2::scale_fill_viridis_b(name = "Count")
  }

  thr <- thresholds_for_group(thresholds, grp)

  p +
    triage_threshold_layers(thr, orientation = "vertical", limits = limits) +
    ggplot2::scale_x_log10(
      limits = limits,
      sec.axis = triage_threshold_sec_axis(thr, limits = limits)
    ) +
    # Additive 0.5 makes the outermost bands sit flush with the panel edge. The
    # ggplot2 default for discrete scales is 0.6, which leaves a sliver of dead
    # space above the top band and below the bottom one.
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = 0.5)) +
    ggplot2::labs(
      x = triage_unit_label(data),
      y = NULL,
      title = title,
      subtitle = label
    ) +
    triage_theme() +
    triage_sec_axis_theme("top") +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = ggplot2::rel(0.6))
      # Category bands are contiguous, so a horizontal grid line inside them adds
      # nothing and shows through the lighter viridis fills.
      # panel.grid.major.y = ggplot2::element_blank()
    )
}

#' Count Observations per Value Bin per Category
#'
#' The counting half of the categorical heatmap. Bins are computed in log10
#' space, because that is the space the panel is drawn in, and the returned
#' `value_mid` is back-transformed so it can be plotted against an untransformed
#' `scale_x_log10()`.
#'
#' Bins are anchored at `origin` (the left-hand end of the shared axis where one
#' is supplied) rather than at log10 = 0. Anchoring at zero left the outermost
#' bin's midpoint able to fall outside the drawn limits, so ggplot2 dropped the
#' tile and warned. Anchoring at the axis start also keeps bin edges identical
#' across every group sharing a scale, which is the point of the shared limits.
#'
#' @param data A plot subset carrying `MEASURED_VALUE_STANDARD` and `.facet`.
#' @param binwidth Bin width in log10 units, from [category_x_binwidth()].
#' @param origin Left-hand end of the value axis, untransformed. `NULL` anchors
#'   at the subset's own minimum.
#' @return A tibble of `value_mid`, `.facet`, `count`. Empty bins are absent
#'   rather than zero-filled, so they draw as panel background.
#' @export
count_by_category_bin <- function(data, binwidth, origin = NULL) {
  values <- data$MEASURED_VALUE_STANDARD
  keep <- !is.na(values) & values > 0
  data <- data[keep, , drop = FALSE]
  if (nrow(data) == 0) {
    return(tibble::tibble(
      value_mid = numeric(0),
      .facet = data$.facet[0],
      count = integer(0)
    ))
  }

  origin_log <- if (!is.null(origin) && is.finite(origin) && origin > 0) {
    log10(origin)
  } else {
    min(log10(data$MEASURED_VALUE_STANDARD))
  }

  data |>
    dplyr::mutate(
      .bin = floor(
        (log10(.data$MEASURED_VALUE_STANDARD) - origin_log) / binwidth
      )
    ) |>
    dplyr::count(.data$.facet, .data$.bin, name = "count") |>
    dplyr::mutate(
      value_mid = 10^(origin_log + (.data$.bin + 0.5) * binwidth)
    ) |>
    dplyr::select("value_mid", ".facet", "count")
}

#' Value-Axis Bin Width for the Categorical Panels
#'
#' `geom_bin2d()` takes `binwidth` in the **transformed** space, and these panels
#' use `scale_x_log10()`, so the width is in log10 units. Derived from the shared
#' scale limits where available, so bin width is identical across every group in
#' a compartment and two panels can be compared directly; falls back to the
#' subset's own range otherwise.
#'
#' @param data A plot subset.
#' @param limits Shared value-axis limits, or `NULL`.
#' @param bins Target number of bins across the axis.
#' @return A single positive number, in log10 units.
#' @export
category_x_binwidth <- function(data, limits = NULL, bins = 40) {
  span <- if (!is.null(limits) && all(is.finite(limits)) && all(limits > 0)) {
    diff(log10(limits))
  } else {
    rng <- range(data$MEASURED_VALUE_STANDARD, na.rm = TRUE)
    if (!all(is.finite(rng)) || any(rng <= 0)) {
      return(0.1)
    }
    diff(log10(rng))
  }
  # A single-valued group gives a zero span; any positive width will do, since
  # every observation lands in one bin regardless.
  if (!is.finite(span) || span <= 0) {
    return(0.1)
  }
  span / bins
}

#' Triage Plot: Spatial Distribution
#'
#' Median concentration per hex cell over a coastline base map. Falls back to
#' points where there are too few sites to bin meaningfully.
#'
#' @param data A group subset. @param label Group label for the subtitle.
#' @param limits Shared colour-scale limits from [triage_limits_for()].
#' @return A ggplot.
#' @export
triage_plot_spatial <- function(data, label = NULL, limits = NULL) {
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

  # Both branches binned, with the same limits and breaks, so a hex map and a
  # points fallback remain visually comparable. Previously the points branch
  # used a continuous scale and the hex branch a binned one.
  scale_layer <- if (triage_use_points(spatial)) {
    ggplot2::scale_colour_viridis_b(
      name = triage_unit_label(data),
      trans = "log10",
      n.breaks = 6,
      limits = limits,
      option = "rocket"
    )
  } else {
    ggplot2::scale_fill_viridis_b(
      name = triage_unit_label(data),
      trans = "log10",
      n.breaks = 6,
      limits = limits,
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
    triage_theme() +
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
#' @param scale_limits Output of [compute_triage_scale_limits()], so every
#'   panel and every group share a value axis.
#' @param thresholds The `copper_toxicity_thresholds` target, or `NULL` for no
#'   reference lines. Read the header of `R/fct_threshold_match.R` before
#'   interpreting them: the comparators are borrowed across compartments,
#'   species and tissues, and are a sanity check rather than an assessment.
#' @param width,height,dpi PNG canvas. Fixed on purpose: a 40,000-row group and
#'   a 150-row group must produce the same-sized artefact, or the contact sheet
#'   becomes unreadable.
#' @return A character vector of written file paths.
#' @export
write_triage_plots_for_group <- function(
  data,
  grp,
  dir = "triage",
  scale_limits = NULL,
  thresholds = NULL,
  width = 8,
  height = 5,
  dpi = 150
) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  group_data <- filter_to_group(data, grp)
  # Plot (a) keeps every unit for the group on purpose; see
  # triage_plot_density().
  group_data_all_units <- filter_to_group(
    data,
    grp,
    exclude_cols = "MEASURED_UNIT_STANDARD"
  )
  # Plot (d) likewise relaxes geography. SITE_GEOGRAPHIC_FEATURE(_SUB) are part
  # of the group key, so within a strict group there is exactly one site type
  # and the panel is a single degenerate row. Relaxing them shows how the same
  # species/compartment/unit varies across site types, which is the question
  # the panel is actually for.
  group_data_all_geography <- filter_to_group(
    data,
    grp,
    exclude_cols = c("SITE_GEOGRAPHIC_FEATURE", "SITE_GEOGRAPHIC_FEATURE_SUB")
  )
  label <- triage_group_label(grp)
  slug <- grp$group_slug[1]
  lims <- triage_limits_for(scale_limits, grp)
  date_lims <- triage_date_limits(scale_limits)

  # List names carry the a/b/c/d/e prefix so the written files sort into
  # reading order in a file browser.
  plots <- list(
    a_density = triage_plot_density(
      group_data_all_units,
      label,
      limits = lims,
      thresholds = thresholds,
      grp = grp
    ),
    b_date = triage_plot_by_date(
      group_data,
      label,
      limits = lims,
      date_limits = date_lims,
      thresholds = thresholds,
      grp = grp
    ),
    c_campaign = triage_plot_by_category(
      group_data,
      "CAMPAIGN_NAME_SHORT",
      "c) Distribution by campaign",
      label,
      label_fn = prettify_campaign_name,
      limits = lims,
      thresholds = thresholds,
      grp = grp
    ),
    d_site_type = triage_plot_by_category(
      group_data_all_geography,
      "SITE_GEOGRAPHIC_FEATURE_SUB",
      "d) Distribution by site type (all geographies)",
      label,
      limits = lims,
      thresholds = thresholds,
      grp = grp
    ),
    # No thresholds on the spatial panel: the measured value is a colour there,
    # not a position, so there is no line to draw.
    e_spatial = triage_plot_spatial(group_data, label, limits = lims)
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
#' @param ... Passed to [write_triage_plots_for_group()], notably `scale_limits`
#'   and `thresholds`.
#' @return A character vector of all written file paths, for `format = "file"`.
#' @export
write_triage_plots <- function(data, groups, dir = "triage", ...) {
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
