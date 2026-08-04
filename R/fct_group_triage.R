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
#' @param ids The `group_ids` ledger, attached so each group carries its stable
#'   id into the notebook headings. `NULL` skips it.
#' @param must_include Group ids to include whatever their size, and whatever the
#'   sampling does. For groups that matter for a reason unrelated to `n`: the two
#'   algae groups sit at 70 and 68 measurements, and no `min_n` reaches them
#'   without admitting seven unrelated groups as well. Unknown ids are an error,
#'   not a silent omission.
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
  seed = 20260729,
  ids = NULL,
  must_include = character(0)
) {
  group_cols <- triage_group_cols()
  # Attached before filtering so the id travels with the group into the notebook
  # headings. NULL is allowed so the tests can build groups without a ledger.
  if (!is.null(ids)) {
    summary_data <- attach_group_ids(summary_data, ids)
  }

  if (length(must_include) > 0 && !"group_id" %in% names(summary_data)) {
    stop("must_include needs group ids; pass `ids` as well.")
  }
  # A must_include id that matches nothing is a typo, and a silent one would leave
  # a group Sam explicitly asked for quietly missing from the contact sheet.
  unknown <- setdiff(must_include, summary_data$group_id)
  if (length(unknown) > 0) {
    stop(
      "must_include names unknown group id(s): ",
      paste(sQuote(unknown), collapse = ", ")
    )
  }

  # n_rows and the reference list come from the same pass over `data`.
  #
  # The references are named, not just counted. "from 2 sources (distinct
  # REFERENCE_ID)" tells you the shape of the evidence but not whose it is, and
  # whether a group is two Vannmiljø campaigns or two independent papers is
  # exactly the thing a lump/split judgement turns on. Naming them is affordable:
  # across the 27 pilot groups the maximum is 3 references and the longest
  # rendered list is 88 characters.
  row_counts <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      n_rows = dplyr::n(),
      reference_ids = paste(
        sort(unique(stats::na.omit(.data$REFERENCE_ID))),
        collapse = ", "
      ),
      .groups = "drop"
    )

  # Two independent routes in: big enough, or named explicitly. `must_include`
  # exists because "this group matters for a reason unrelated to its size" is a
  # judgement, and encoding it as a threshold catches bystanders. Concretely: the
  # two algae groups sit at n = 70 and n = 68, and no cutoff reaches them without
  # also admitting seven unrelated groups, because eight others are interleaved
  # between 68 and 99.
  #
  # Named by group id rather than by key, which is the whole point of the ids
  # existing (R/fct_group_ids.R).
  forced <- summary_data$group_id %in% must_include
  eligible <- summary_data |>
    dplyr::filter(.data$n >= min_n | forced) |>
    dplyr::select(
      dplyr::all_of(group_cols),
      "n",
      "n_sources",
      # Carried through so the notebook can print each group's flags under its
      # heading from the same source the summary table highlights from. any_of()
      # rather than all_of(): sample_triage_groups() is also called in tests with
      # a bare summary fixture that has none of these.
      dplyr::any_of(c(
        "group_id",
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

  # Sampling applies only to the size-eligible groups. A named group is named
  # because it is wanted, so it must not be able to lose a coin toss; without this
  # split, `must_include` would be silently advisory whenever n_sample is finite.
  if ("group_id" %in% names(eligible)) {
    kept <- eligible[eligible$group_id %in% must_include, , drop = FALSE]
    poolable <- eligible[!eligible$group_id %in% must_include, , drop = FALSE]
  } else {
    kept <- eligible[0, , drop = FALSE]
    poolable <- eligible
  }

  withr::with_seed(seed, {
    room <- n_sample - nrow(kept)
    sampled <- if (is.infinite(n_sample) || room >= nrow(poolable)) {
      poolable
    } else if (room <= 0) {
      poolable[0, , drop = FALSE]
    } else {
      dplyr::slice_sample(poolable, n = room)
    }
  })
  picked <- dplyr::bind_rows(kept, sampled)

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

#' Shorten an eData Reference Id for Display
#'
#' `REFERENCE_ID` is a run-on of year, first author and title
#' (`2019SimonsenLabilityOfToxic`), up to 36 characters with no spaces in it.
#' As a category label that is unwrappable and unreadable at the size these
#' panels are drawn; as "Simonsen 2019" it is both, and the full id is one grep
#' away in the data.
#'
#' The author is taken as everything before the next capitalised word, which
#' clips a double-barrelled surname to its first element
#' (`2021LehmannKonEffectsOf` becomes "Lehmann 2021"). That is accepted: these
#' are axis labels, not citations.
#'
#' Two ids shortening to the same label would silently merge two studies into
#' one band, so where that happens **both** keep their full id rather than being
#' quietly conflated. Ids not matching the year-then-author shape are returned
#' unchanged.
#'
#' @param x A character vector of reference ids.
#' @return A character vector the same length as `x`.
#' @export
prettify_reference_id <- function(x) {
  x <- as.character(x)
  if (length(x) == 0) {
    return(character(0))
  }

  year <- stringr::str_extract(x, "^\\d{4}")
  rest <- stringr::str_remove(x, "^\\d{4}")
  # Split at the first lower-to-upper transition, i.e. where the title starts.
  author <- stringr::str_split_i(rest, "(?<=[a-z])(?=[A-Z])", 1)
  # A few ids drop a Norwegian initial letter and start lower case
  # (2015verjordetToxicAndEssential, from Overjordet), which reads as a typo
  # unless it is capitalised here.
  author <- sub("^(.)", "\\U\\1", author, perl = TRUE)

  short <- ifelse(
    is.na(year) | is.na(author) | author == "",
    x,
    paste(author, year)
  )

  # Collision check on the distinct ids, not on the vector, so a reference
  # appearing 13,000 times does not count as clashing with itself.
  pairs <- unique(data.frame(id = x, short = short))
  clashing <- pairs$short[duplicated(pairs$short)]
  ifelse(short %in% clashing, x, short)
}

#' Label a Row by Its Campaign or Its Reference
#'
#' Neither column alone works as the category for panel c. Vannmiljø is a single
#' `REFERENCE_ID` covering 44 campaigns and ~90% of the rows, so keying on the
#' reference collapses almost the whole dataset into one band. Literature data is
#' the mirror image: one campaign per study, named by an internal code
#' (`ARKIXb1993FramGrnld`) that says nothing about whose data it is, so keying on
#' the campaign hides the thing a lump/split judgement turns on.
#'
#' So: campaign where the row is Vannmiljø, reference otherwise. A group drawing
#' on both shows both kinds of band, which is correct, because for a mixed group
#' "which source is this" genuinely has two different answers.
#'
#' Vannmiljø rows are detected on the `Vm_` campaign prefix rather than on the
#' reference id, which contains a non-ASCII character.
#'
#' @param campaign `CAMPAIGN_NAME_SHORT`.
#' @param reference `REFERENCE_ID`.
#' @return A character vector the same length as `campaign`.
#' @export
triage_source_label <- function(campaign, reference) {
  campaign <- as.character(campaign)
  reference <- as.character(reference)
  if (length(campaign) == 0) {
    return(character(0))
  }

  is_vm <- !is.na(campaign) & stringr::str_starts(campaign, "Vm_")
  out <- ifelse(
    is_vm,
    prettify_campaign_name(campaign),
    prettify_reference_id(reference)
  )
  # A row with no reference falls back to its campaign rather than becoming NA,
  # which triage_plot_by_category() would drop from the panel without saying so.
  ifelse(is.na(out), campaign, out)
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
    # Source AND matrix, via threshold_source_title(). The bare REFERENCE_ID
    # said which document the boundaries came from but not what they were set
    # for, and the match is loose enough (genus-level for biota, many-to-one for
    # compartments) that the reader cannot infer it. Several sources can coexist
    # on the unit-agnostic overall-distribution panel, so this is a set rather
    # than a single name.
    name = threshold_source_title(thresholds)
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

#' Log Value Axis, Labelled at Every Power of Ten
#'
#' A shared replacement for bare `scale_x_log10()` / `scale_y_log10()` on the
#' triage panels, so every value axis is read the same way.
#'
#' Every power of ten gets a label and a major gridline; the 2 to 9 subdivisions
#' get minor ticks. Sam's requirement: it was impossible to read a concentration
#' off the panels, because ggplot2's default log breaks label only two or three
#' points across an axis this wide.
#'
#' **Known limitation.** The Aquatic value axis spans 12.6 orders of magnitude
#' (9.3e-08 to 405,000), so this puts 13 labels on it. At full size, which is what
#' the lightbox shows, that reads fine. In a `layout-ncol=5` grid each panel is
#' about a fifth of page width and the labels will be dense. That span is a
#' consequence of sharing limits per compartment (P1.1d) so panels are comparable,
#' and comparability was the more valuable property. Drop the grid to 3 across if
#' the thumbnails need to be readable too.
#'
#' @param limits Shared value-axis limits, or `NULL`.
#' @param axis `"x"` or `"y"`.
#' @return A ggplot2 scale.
#' @export
triage_value_scale <- function(limits = NULL, axis = c("x", "y"), ...) {
  axis <- match.arg(axis)
  scale_fn <- if (axis == "x") ggplot2::scale_x_log10 else ggplot2::scale_y_log10

  scale_fn(
    limits = limits,
    # Powers of ten are now MAJOR breaks, so they are labelled. They used to be
    # minor breaks (triage_log_minor_breaks()), which drew a gridline at each
    # power but no number against it, and left the axis unreadable: ggplot2's
    # default log breaks label only two or three points across a span this wide.
    #
    # A fixed vector rather than a function of the limits, for the reason given
    # at triage_log_minor_breaks(): ggplot2 silently drops breaks outside the
    # range, so this needs no knowledge of the limits and avoids the
    # data-space-versus-transformed-space ambiguity in break functions.
    breaks = triage_log_minor_breaks(),
    # No minor breaks on the value axis. The 1:9-per-decade grid is ~96 lines
    # across a 12-decade axis and fights the threshold lines and the secondary
    # class axis, which is the judgement already recorded at
    # triage_log_minor_breaks(). Promoting the powers of ten to major breaks is
    # what makes the axis readable; adding a second tier back would undo it.
    minor_breaks = NULL,
    # "1e-07" rather than 10^-7, per Sam's phrasing, and it stays legible when
    # the labels are dense.
    #
    # The NA guard is load-bearing. `breaks` is a fixed vector spanning far more
    # range than any real axis, so ggplot2 hands the labeller an NA for every
    # break outside the limits. formatC() renders those as the literal string
    # "NA", which ggplot2 then draws: without this, an axis limited to 1e-3..1e3
    # came back with seven real labels and eighteen reading "NA".
    labels = function(x) {
      ifelse(is.na(x), NA_character_, formatC(x, format = "e", digits = 0))
    },
    ...
  )
}

#' Date Axis with a Tick and Gridline per Year
#'
#' Major break and label every five years, minor break every year, minor ticks
#' drawn. The sampling window is 1988 to 2025, so that is 8 labels and 38 minor
#' divisions, which is comfortable.
#'
#' @param limits Global date limits from [triage_date_limits()].
#' @return A ggplot2 scale.
#' @export
triage_date_scale <- function(limits = NULL) {
  ggplot2::scale_x_date(
    limits = limits,
    date_breaks = "5 years",
    date_minor_breaks = "1 year",
    date_labels = "%Y",
    guide = ggplot2::guide_axis(minor.ticks = TRUE)
  )
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
      # The panel is busy enough with tiles, thresholds and their labels. Blanked
      # in both directions here and then re-enabled on the **concentration axis
      # only**, by triage_minor_grid_theme(), because a decade grid is what makes
      # a wide log axis readable while a minor grid on the date or category axis
      # is just clutter.
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.85))
    )
  )
}

#' Minor Breaks at Every Power of Ten
#'
#' The concentration axes span up to 12 orders of magnitude, and ggplot2's
#' default log breaks thin out accordingly: panels a-d were labelling 1e-4, 1e0,
#' 1e4 and nothing between, so a point could only be placed to within four
#' decades by eye.
#'
#' The fix keeps those sparse **major** breaks (adding labels at every decade
#' would overrun the axis) and puts an unlabelled gridline at each intervening
#' power of ten. Reading a value is then "two lines right of 1e0".
#'
#' Deliberately **not** the 1:9-per-decade grid used by
#' [outlier_log10_scale()]: at ~96 lines across a 12-decade axis that fights the
#' threshold lines and the secondary axis rather than helping.
#'
#' Returned as a plain vector covering far more range than any real axis, rather
#' than as a function of the limits. ggplot2 silently drops breaks outside the
#' scale range, so a fixed vector needs no knowledge of the limits and avoids the
#' data-space-versus-transformed-space ambiguity in `minor_breaks` functions.
#'
#' @return A numeric vector of powers of ten.
#' @export
triage_log_minor_breaks <- function() {
  10^(-12:12)
}

#' Re-enable the Minor Grid on the Concentration Axis
#'
#' Must be added **after** [triage_theme()], which blanks the minor grid in both
#' directions. Colour and linewidth are given explicitly rather than inherited:
#' the parent `panel.grid.minor` is an `element_blank()` at that point, and
#' relying on what a child inherits from a blanked parent is exactly the kind of
#' silent no-op that is hard to spot in a written PNG.
#'
#' Lighter and thinner than the major grid so it reads as subordinate. Minor
#' lines are drawn before major ones, so a minor break coinciding with a major
#' break is painted over and needs no special handling.
#'
#' **Minor tick marks are added as well, and they are the part that matters on
#' the category panels.** Those panels are wall-to-wall `geom_tile()`, and a
#' geom draws over the panel grid, so inside the data region the gridlines are
#' invisible however dark they are: the first attempt at grey95 read as no
#' change at all. Ticks sit outside the panel where nothing can cover them.
#'
#' @param axis `"x"` where the measured value is on x (panels a, c, d), `"y"`
#'   where it is on y (panel b).
#' @return A ggplot2 theme.
#' @export
triage_minor_grid_theme <- function(axis = c("x", "y")) {
  axis <- match.arg(axis)
  grid <- ggplot2::element_line(colour = "grey88", linewidth = 0.3)
  # The tick ELEMENTS have to be set, not just their lengths. theme_minimal()
  # blanks axis.ticks outright, so setting only axis.minor.ticks.length gives a
  # length to something that is never drawn: the first attempt looked identical
  # to no change at all.
  major <- ggplot2::element_line(colour = "grey30", linewidth = 0.3)
  minor <- ggplot2::element_line(colour = "grey60", linewidth = 0.25)
  len <- ggplot2::unit(3, "pt")

  if (axis == "x") {
    ggplot2::theme(
      panel.grid.minor.x = grid,
      # Bottom only. The top axis on panels a, c and d is the threshold-class
      # secondary axis, whose breaks are threshold values; ticking it at every
      # decade would imply a structure it does not have.
      axis.ticks.x.bottom = major,
      axis.minor.ticks.x.bottom = minor,
      axis.ticks.length.x.bottom = len,
      axis.minor.ticks.length.x.bottom = ggplot2::rel(0.6)
    )
  } else {
    ggplot2::theme(
      panel.grid.minor.y = grid,
      axis.ticks.y.left = major,
      axis.minor.ticks.y.left = minor,
      axis.ticks.length.y.left = len,
      axis.minor.ticks.length.y.left = ggplot2::rel(0.6)
    )
  }
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
    triage_value_scale(
      limits = limits,
      axis = "x",
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
    # No triage_minor_grid_theme() here any more: powers of ten are major breaks
    # now, so the value axis has no minor breaks and there is nothing for it to
    # style.
    triage_value_text_theme("x") +
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
      colour = "#ffffff48",
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
    triage_date_scale(limits = date_limits) +
    triage_value_scale(limits = limits, axis = "y") +
    ggplot2::labs(
      x = "Sampling date",
      y = triage_unit_label(data),
      title = "b) Concentration by date",
      subtitle = label
    ) +
    triage_theme() +
    triage_value_text_theme("y") +
    # "x", not "y". The minor breaks moved: the value axis (y here) now labels
    # every power of ten as a MAJOR break and has no minor breaks at all, while
    # the date axis (x) gained a minor break per year. Left on "y" this would
    # have styled a grid with nothing to draw and silently dropped the yearly
    # lines Sam asked for.
    triage_minor_grid_theme("x")
}

#' De-emphasise the Concentration-Axis Labels
#'
#' The shared per-compartment limits put up to 13 decade labels on the value
#' axis (see [triage_value_scale()]), and at full weight that row of `1e-07`,
#' `1e-06`, ... competes with the panel for attention when it is really just a
#' ruler. Smaller and greyer, so it reads as scale furniture.
#'
#' Font size rather than shortening the labels. Dropping the mantissa to leave
#' `-07`, `-06` was the other option Sam offered and is fewer characters, but a
#' bare exponent is not a concentration and the axis title does not say
#' "log10", so it invites a reader to take -07 as a value. The labels stay as
#' [triage_value_scale()] writes them.
#'
#' Applied to one axis only. The category axis on panels c and d already sets
#' its own size, and the two must not fight.
#'
#' @param axis `"x"` where the measured value is on x (panels a, c, d), `"y"`
#'   where it is on y (panel b).
#' @return A ggplot2 theme.
#' @export
triage_value_text_theme <- function(axis = c("x", "y")) {
  axis <- match.arg(axis)
  el <- ggplot2::element_text(size = ggplot2::rel(0.7), colour = "grey45")
  if (axis == "x") {
    # .bottom, not the bare axis.text.x: the top axis on these panels is the
    # threshold-class secondary axis, which triage_sec_axis_theme() deliberately
    # makes bolder and larger. Setting axis.text.x would fight it, and which one
    # won would depend on the order the themes were added.
    ggplot2::theme(axis.text.x.bottom = el)
  } else {
    ggplot2::theme(axis.text.y.left = el)
  }
}

#' Per-Category Outlier Flags for a Triage Panel
#'
#' Recomputes the two outlier criteria **within each category of the panel**
#' (each campaign or reference on panels c and d), rather than reusing the
#' sample-group flags that `summarise_literature_data` puts in the triage table.
#'
#' Why the difference is deliberate, and why both are right (Sam, 2026-08-04:
#' "we're doing the same operation here but at a different level of grouping"):
#'
#' * The sample-group fence pools every campaign in the group. Measured on
#'   Aquatic / Freshwater / River, that made **one** mining campaign produce 998
#'   of the group's 1,316 flags, purely by being a mining campaign. Its values
#'   are unremarkable for a mine and extreme for the group.
#' * The per-campaign fence asks whether a value is unusual *for its own
#'   campaign*. On the same group it flags 687 rows, Mine Impact drops to 0, and
#'   dispersed campaigns that the pooled fence hid (Reference Rivers, 2 to 53)
#'   become visible.
#'
#' Neither subsumes the other, and this does **not** change
#' `n_double_outliers` or anything the triage table ranks on.
#'
#' Categories below `min_n` are not tested at all, and are reported as
#' `tested = FALSE` rather than as zero. A fence fitted to a handful of points
#' is noise: Urban Fjord Contaminants has 7 rows in this group, two of which are
#' the 1e3 block, and no fence fitted to that sample can call 29% of itself an
#' outlier. Such rows say so in their label; see [triage_category_labels()].
#'
#' @param data A plot subset carrying `MEASURED_VALUE_STANDARD` and `.facet`.
#' @param min_n Minimum category size for the flags to be computed.
#' @return `data` with `.outlier` (logical, `FALSE` where untested) and
#'   `.tested` (logical) added.
#' @export
triage_flag_by_category <- function(data, min_n = 10) {
  if (nrow(data) == 0) {
    data$.outlier <- logical(0)
    data$.tested <- logical(0)
    return(data)
  }
  data |>
    dplyr::group_by(.data$.facet) |>
    dplyr::mutate(
      .dot_fill = flag_outliers(.data$MEASURED_VALUE_STANDARD, min_n = min_n)$
        dot_fill,
      # `%in%` rather than `==`: dot_fill is a factor and an untested category
      # yields "not tested", which must read as FALSE here and not as NA.
      .outlier = .data$.dot_fill %in% "both",
      .tested = !(.data$.dot_fill %in% "not tested")
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-".dot_fill")
}

#' Right-Margin Count Labels for the Categorical Panels
#'
#' `"1,193 (53)"`: rows in the category, and how many of them the per-category
#' fences flagged. Sam's call, 2026-08-04, and it earns its place because the
#' shared value axis leaves a wide empty margin on the right of most panels.
#'
#' Untested categories read `"7 (n < 10)"` rather than `"7 (0)"`, which would
#' claim a test that did not run. The threshold is interpolated from `min_n`
#' rather than written into the string, so the label cannot drift if the default
#' changes.
#'
#' @param data Output of [triage_flag_by_category()].
#' @param min_n The same `min_n` passed to [triage_flag_by_category()].
#' @return A tibble of `.facet`, `n`, `k`, `tested`, `label`.
#' @export
triage_category_labels <- function(data, min_n = 10) {
  data |>
    dplyr::group_by(.data$.facet) |>
    dplyr::summarise(
      n = dplyr::n(),
      # all(), not any(): flag_outliers() decides per category, so the value is
      # constant within a group and all() simply picks it up.
      tested = all(.data$.tested),
      k = sum(.data$.outlier),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      label = dplyr::if_else(
        .data$tested,
        paste0(format(.data$n, big.mark = ",", trim = TRUE), " (", .data$k, ")"),
        paste0(
          format(.data$n, big.mark = ",", trim = TRUE),
          " (n < ",
          min_n,
          ")"
        )
      )
    )
}

#' Distribution Overlay for the Categorical Panels
#'
#' The layers drawn on top of the heatmap: a boxplot per category, tick marks at
#' the per-category outliers, and the count labels in the right margin.
#'
#' **These layers must be added before [triage_threshold_layers()].** The
#' threshold lines have to draw last or the boxplot covers them, which was the
#' first thing wrong with this overlay.
#'
#' Design notes, all settled by rendering against the real data on 2026-08-04:
#'
#' * **`outlier.shape = NA` is mandatory.** The boxplot's own outlier points are
#'   one mark per row, which on a 42,000-row group is precisely the thing
#'   CLAUDE.md 4.4 forbids. Suppressed here; the tick layer draws the outliers
#'   that survive the stricter two-criteria test instead.
#' * **The box is drawn twice**, a dark halo under a white line. The viridis
#'   fill is light at the modal bins, which is exactly where the box sits, so a
#'   dark line alone is swallowed there; the reverse pairing (white halo, dark
#'   line) was tried and put fat white whiskers through the tile rows.
#' * **Ticks, not points.** Points overplot into solid runs on the dense
#'   categories; ticks read as texture, and the density of the fringe is itself
#'   informative. White, not red: red over-dramatised what is mostly ordinary
#'   distribution tail, and white borrows the language the box already set.
#' * **`width = 0.45`** leaves a visible sliver of the tile band above and below,
#'   so the row still reads as a heatmap row rather than as a broken boxplot.
#'
#' Box statistics are computed in log10 space, because [triage_value_scale()]
#' transforms before the stat runs. Quartiles are unaffected (quantiles are
#' equivariant under a monotone transform) but the whiskers are 1.5 x IQR **in
#' log units**, which is the same fence [flag_outliers()] uses and is the right
#' definition for data spanning this many decades.
#'
#' @param data Output of [triage_flag_by_category()].
#' @param limits Shared value-axis limits, used to place the right-margin
#'   labels. Falls back to the data's own maximum where absent.
#' @param min_n Passed to [triage_category_labels()] for the untested wording.
#' @param ticks Draw the outlier ticks. `FALSE` on the small-n panels, where
#'   every observation is already drawn as a point and ticks would be clutter.
#' @return A list of ggplot2 layers.
#' @export
triage_category_overlay <- function(
  data,
  limits = NULL,
  min_n = 10,
  ticks = TRUE
) {
  if (nrow(data) == 0) {
    return(list())
  }

  box <- function(colour, linewidth) {
    ggplot2::geom_boxplot(
      data = data,
      ggplot2::aes(x = .data$MEASURED_VALUE_STANDARD, y = .data$.facet),
      inherit.aes = FALSE,
      fill = NA,
      colour = colour,
      linewidth = linewidth,
      outlier.shape = NA,
      width = 0.45
    )
  }
  layers <- list(box("grey15", 0.7), box("white", 0.3))

  flagged <- data[data$.outlier, , drop = FALSE]
  if (ticks && nrow(flagged) > 0) {
    # Lower half of each band, clear of the box (width 0.45, so the box occupies
    # +/- 0.225). as.numeric() on the factor gives the row's position on the
    # discrete scale, which is how ggplot2 places the tiles too.
    tick <- function(colour, linewidth) {
      ggplot2::geom_linerange(
        data = flagged,
        ggplot2::aes(
          x = .data$MEASURED_VALUE_STANDARD,
          ymin = as.numeric(.data$.facet) - 0.46,
          ymax = as.numeric(.data$.facet) - 0.24
        ),
        inherit.aes = FALSE,
        colour = colour,
        linewidth = linewidth
      )
    }
    layers <- c(layers, list(tick("grey15", 0.75), tick("white", 0.35)))
  }

  labels <- triage_category_labels(data, min_n = min_n)
  x_at <- if (!is.null(limits) && all(is.finite(limits))) {
    limits[2]
  } else {
    max(data$MEASURED_VALUE_STANDARD, na.rm = TRUE)
  }
  # Two layers rather than one with a colour mapping. A scale_colour_manual()
  # here would be a second colour scale on a panel that may already have one,
  # and ggplot2 resolves that by warning and replacing.
  text_layer <- function(rows, colour) {
    if (nrow(rows) == 0) {
      return(NULL)
    }
    ggplot2::geom_text(
      data = rows,
      ggplot2::aes(x = x_at, y = .data$.facet, label = .data$label),
      inherit.aes = FALSE,
      colour = colour,
      size = 2.1,
      hjust = 1
    )
  }
  c(
    layers,
    Filter(
      Negate(is.null),
      list(
        text_layer(labels[labels$tested, , drop = FALSE], "grey15"),
        # Greyer, so an untested row's label does not read with the same
        # authority as a tested one.
        text_layer(labels[!labels$tested, , drop = FALSE], "grey60")
      )
    )
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
#' @param overlay Draw the boxplot, outlier ticks and count labels on top of the
#'   heatmap. See [triage_category_overlay()]. `FALSE` gives the bare heatmap.
#' @param outlier_min_n Minimum category size for the outlier flags to be
#'   computed; smaller categories are reported as untested. See
#'   [triage_flag_by_category()].
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
  x_bins = 40,
  overlay = TRUE,
  outlier_min_n = 10
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

  plot_data <- triage_flag_by_category(plot_data, min_n = outlier_min_n)

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
      # Log fill, not linear. Counts span roughly four orders of magnitude
      # within a single panel (1 to 6,616 on Aquatic Sediment), so a linear
      # scale puts everything except the modal bin of the largest category into
      # the bottom colour and the panel degrades to presence/absence. On a log
      # scale each row's distribution shape is legible, which is the whole
      # question these panels exist to answer.
      #
      # Continuous rather than binned: the gradient carries more of the
      # distribution, and a binned scale's bands implied count thresholds that
      # mean nothing. Safe under log10 because count_by_category_bin() omits
      # empty bins rather than zero-filling them, so the minimum count is 1.
      ggplot2::scale_fill_viridis_c(
        name = "Count",
        transform = "log10",
        breaks = scales::breaks_log(n = 5),
        labels = scales::label_log()
      )
  }

  thr <- thresholds_for_group(thresholds, grp)

  # ORDER IS LOAD-BEARING. The overlay goes on before the threshold lines, so
  # the lines draw last and stay readable; with the overlay added afterwards the
  # boxplot sits on top of them and the class boundaries become hard to follow.
  # Ticks are suppressed on the points branch, where every observation is
  # already drawn.
  if (overlay) {
    p <- p +
      triage_category_overlay(
        plot_data,
        limits = limits,
        min_n = outlier_min_n,
        ticks = !triage_use_points(plot_data)
      )
  }

  p +
    triage_threshold_layers(thr, orientation = "vertical", limits = limits) +
    triage_value_scale(
      limits = limits,
      axis = "x",
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
    # No triage_minor_grid_theme() here any more: powers of ten are major breaks
    # now, so the value axis has no minor breaks and there is nothing for it to
    # style.
    triage_value_text_theme("x") +
    triage_sec_axis_theme("top") +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(
        size = ggplot2::rel(0.6),
        # campaign names are often quite long, we don't want text to overlap
        lineheight = 0.75
      )
      # Category bands are contiguous, so a horizontal grid line inside them adds
      # nothing and shows through the lighter viridis fills.
      # panel.grid.major.y = ggplot2::element_blank()
    )
}

#' Triage Plot: Distribution by Source
#'
#' Panel c. A thin wrapper on [triage_plot_by_category()] that derives the
#' category with [triage_source_label()]: Vannmiljø campaign where the row is
#' Vannmiljø, reference where it is not. See that function for why neither
#' column works on its own.
#'
#' @param data A group subset.
#' @param label Group label for the subtitle.
#' @param ... Passed to [triage_plot_by_category()], notably `limits`,
#'   `thresholds` and `grp`.
#' @return A ggplot.
#' @export
triage_plot_by_source <- function(data, label = NULL, ...) {
  if (!"REFERENCE_ID" %in% names(data)) {
    stop("triage_plot_by_source() needs a REFERENCE_ID column.")
  }
  data$.source <- triage_source_label(
    data$CAMPAIGN_NAME_SHORT,
    data$REFERENCE_ID
  )

  triage_plot_by_category(
    data,
    ".source",
    "c) Distribution by campaign or reference",
    label,
    # Already prettified by triage_source_label(), and the two kinds of label
    # need different treatment, so there is nothing left for a single label_fn
    # to do here.
    label_fn = identity,
    ...
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
#' @param bins Hex bins. Doubled from 60 to 120 on 2026-08-04: at 60 the cells
#'   were wide enough to average a fjord together with the open coast beside it,
#'   so several groups read as spatially uniform when whether they are is the
#'   question the panel exists to answer. Halving the cell quarters the rows
#'   behind each median, so read sparse groups with care; below
#'   `triage_use_points()` the panel drops to raw points anyway.
#'
#'   **`bins` is not bins across the visible map.** ggplot2 takes the hex width
#'   as `diff(scale range) / bins`, and the scale here is shared with the world
#'   basemap, which spans the globe. So 120 gives 360/120 = 3 degrees of
#'   longitude, not a 120-cell grid over Norway, and the visible extent set by
#'   `coord_fixed()` below has no effect on it. Two consequences: cell size is
#'   identical across every group, which is what makes the panels comparable,
#'   and the latitude width comes out at roughly half the longitude width, which
#'   `ratio = 2` happens to draw as a regular hexagon. Pass an explicit
#'   `binwidth` to `stat_summary_hex()`
#'   if a genuinely fine grid is ever wanted; `bins` cannot get there without
#'   absurd numbers.
#' @return A ggplot.
#' @export
triage_plot_spatial <- function(data, label = NULL, limits = NULL, bins = 120) {
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
      colour = "white",
      # A quarter of the ggplot2 default of 0.5. At panel size the default
      # coastline reads as a thick white band that eats the fjords, which is
      # most of the Norwegian coast and most of where the data is.
      linewidth = 0.125
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
      bins = bins,
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
    # Renamed from c_campaign on 2026-08-04, when the panel stopped being about
    # campaigns alone; see triage_plot_by_source(). The letter prefix is what the
    # notebooks key their subfigure ids on, so those are unaffected, but the
    # written filename changes and the image links in docs/groups/*.qmd were
    # updated to match by hand (the generator is append-only and will not do it).
    c_source = triage_plot_by_source(
      group_data,
      label,
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
