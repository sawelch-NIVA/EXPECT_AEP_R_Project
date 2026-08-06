# Parent-level triage overviews (PLAN.md P2.3 support).
#
# The five per-group panels (a-e) answer "what does this group look like".
# They cannot answer "should these groups be one group", because each panel is
# drawn strictly inside one group key and the thing you need to compare is
# sitting in the panel next door.
#
# These two panels sit one level up, at the sub-compartment, and compare
# the children below it. That is the view the `lump` / `split` decision in
# data/clean/decisions/group_decisions.csv actually needs.
#
# They are deliberately NOT a new plot type. triage_plot_by_category() already
# draws "distribution of value, split by some category", which is exactly this
# question with a different subset and a different column, so it is reused
# unchanged. Only the subsetting and the choice of column are new.

#' Group-Key Columns Below the Sub-Compartment
#'
#' The candidate split axes, in hierarchy order. Everything in
#' [triage_heading_cols()] below `ENVIRON_COMPARTMENT_SUB`.
#'
#' @return A character vector of column names, outermost first.
#' @export
triage_overview_candidate_cols <- function() {
  heading <- triage_heading_cols()
  heading[seq(match("ENVIRON_COMPARTMENT_SUB", heading) + 1, length(heading))]
}

#' Readable Names for the Split Axes
#'
#' Column names go in plot titles, and `SITE_GEOGRAPHIC_FEATURE_SUB` in a title
#' reads as a database artefact rather than as a question about the data.
#'
#' @param x A character vector of column names.
#' @return A character vector of readable labels, unknown names passed through.
#' @export
triage_level_label <- function(x) {
  lookup <- c(
    SPECIES_GROUP = "species group",
    SAMPLE_SPECIES = "species",
    SAMPLE_TISSUE = "tissue",
    SITE_GEOGRAPHIC_FEATURE = "site type",
    SITE_GEOGRAPHIC_FEATURE_SUB = "site subtype"
  )
  dplyr::coalesce(unname(lookup[x]), x)
}

#' Levels That Have Their Own Node Tier
#'
#' A sub-compartment overview stops at these rather than descending past them,
#' because the level below is covered by its own, more focused figure.
#'
#' `SPECIES_GROUP` is here because [triage_species_nodes()] gives every species
#' group its own by-species panel. Before that existed, the sub-compartment
#' overview carried the species comparison itself, across all 76 species of
#' `Biota, Aquatic` truncated to the largest 25. It was the only panel in the set
#' that dropped data, and pushing species down one level removes the truncation
#' from everything except Fish.
#'
#' @return A character vector of column names.
#' @export
triage_overview_stop_cols <- function() {
  "SPECIES_GROUP"
}

#' Which Levels Vary Below This Parent
#'
#' Returns up to two candidate columns that are both **populated** and
#' **varying** within `data`, stopping early at any column in `stop_cols`.
#'
#' The first two conditions are why this is derived from the data rather than
#' fixed:
#'
#' * *Populated.* The taxonomy columns are entirely `NA` for every abiotic
#'   sub-compartment, so a fixed rule of "the next two columns" would hand
#'   Aquatic Sediment two empty panels. Skipping them lands on site type and
#'   site subtype, which is the comparison that sub-compartment needs.
#' * *Varying.* A column with one distinct value draws a single band, which
#'   tells you nothing and costs a file and a figure slot.
#'
#' The third is `stop_cols`: a level with its own node tier is *included* and
#' then descent halts, so `Biota, Aquatic` returns `SPECIES_GROUP` alone rather
#' than `SPECIES_GROUP` plus `SAMPLE_SPECIES`. Abiotic nodes are unaffected and
#' still return both geography levels.
#'
#' Empty strings count as missing alongside `NA`: the decisions CSV round-trips
#' `NA` to `""`, so anything derived from it can carry either.
#'
#' @param data Rows under one parent node.
#' @param stop_cols Columns to stop after. See [triage_overview_stop_cols()].
#' @return A character vector of 0, 1 or 2 column names.
#' @export
triage_overview_levels <- function(
  data,
  stop_cols = triage_overview_stop_cols()
) {
  candidates <- triage_overview_candidate_cols()
  varies <- vapply(
    candidates,
    function(col) {
      if (!col %in% names(data)) {
        return(FALSE)
      }
      v <- as.character(data[[col]])
      v <- v[!is.na(v) & v != ""]
      length(unique(v)) >= 2
    },
    logical(1)
  )
  kept <- candidates[varies]
  hit <- which(kept %in% stop_cols)
  if (length(hit) > 0) {
    kept <- kept[seq_len(hit[1])]
  }
  utils::head(kept, 2)
}

#' Subset Data to One Parent Node
#'
#' The parent-level equivalent of [filter_to_group()]: matches on compartment,
#' sub-compartment and unit only, and leaves every other key column free.
#'
#' `NA` is matched as a value rather than dropped, for the same reason as in
#' [filter_to_group()].
#'
#' @param data The `literature_analysis_ready` target.
#' @param node A one-row tibble from [triage_overview_nodes()].
#' @return A filtered data frame.
#' @export
filter_to_overview_node <- function(data, node) {
  keep <- rep(TRUE, nrow(data))
  for (col in triage_overview_node_cols()) {
    want <- node[[col]][1]
    have <- data[[col]]
    keep <- keep &
      if (is.na(want)) is.na(have) else (!is.na(have) & have == want)
  }
  data[keep, , drop = FALSE]
}

#' Columns Identifying a Parent Node
#'
#' The unit is part of the node, not just of the groups below it. Sub-compartments
#' genuinely split on it (`Biota, Aquatic` is 129 groups of mg/kg wet against 66
#' of mg/kg dry), so a single mixed panel would show a units artefact separated by
#' three orders of magnitude and read as a real biological split.
#'
#' @return A character vector of column names.
#' @export
triage_overview_node_cols <- function() {
  c("ENVIRON_COMPARTMENT", "ENVIRON_COMPARTMENT_SUB", "MEASURED_UNIT_STANDARD")
}

#' Parent Nodes Worth an Overview
#'
#' One row per compartment x sub-compartment x unit carrying at least `min_n`
#' measurements, with the levels to compare resolved per node by
#' [triage_overview_levels()].
#'
#' `min_n` is `sum(MEASURED_N)`, the **same currency** as the `min_n` of
#' [sample_triage_groups()] and the `n` of `summarise_literature_data`, and it
#' defaults to the same 100. That alignment is the point: an overview whose
#' sub-compartment contains no group large enough to appear in the notebook is
#' written and then never displayed, because the notebook only opens headings for
#' groups it is showing. Counting rows instead let those two thresholds disagree
#' and produced exactly that orphan (Brackish/Transitional Water: 48 measurements
#' across 48 rows, over a 30-row bar and under a 100-measurement one).
#'
#' `n_rows` is reported alongside because the panels draw one mark per *row*, so a
#' node can clear `min_n` on aggregated measurements while having few rows to
#' plot; same caveat as [sample_triage_groups()].
#'
#' Nodes where no candidate column varies are dropped: there is nothing to
#' compare, and the per-group panels already cover them.
#'
#' @param data The `literature_analysis_ready` target. Must carry `MEASURED_N`.
#' @param min_n Minimum measurements for a node to qualify.
#' @param groups The `triage_pilot_groups` target. Nodes with no displayed group
#'   beneath them are dropped; see [filter_reachable_nodes()]. `NULL` skips the
#'   check.
#' @return A tibble of node columns plus `n`, `n_rows`, `level_1`, `level_2`
#'   (`NA` where only one level varies), `node_label` and `node_slug`.
#' @export
triage_overview_nodes <- function(data, min_n = 100, groups = NULL) {
  node_cols <- triage_overview_node_cols()

  counts <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(node_cols))) |>
    dplyr::summarise(
      n = sum(.data$MEASURED_N),
      n_rows = dplyr::n(),
      # How many group keys sit under this node. Reported above the figure
      # because "27,288 measurements" and "27,288 measurements in 13 groups"
      # invite quite different lumping decisions.
      #
      # Only the key columns that are NOT node columns: pick() cannot see the
      # grouping variables, and it does not need to, since they are constant
      # within a node by construction.
      n_groups = dplyr::n_distinct(
        dplyr::pick(dplyr::all_of(setdiff(triage_group_cols(), node_cols)))
      ),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$n >= min_n) |>
    dplyr::arrange(dplyr::desc(.data$n))

  counts <- filter_reachable_nodes(counts, groups, node_cols)

  if (nrow(counts) == 0) {
    return(empty_overview_nodes())
  }

  levels_for <- lapply(
    seq_len(nrow(counts)),
    function(i) {
      lv <- triage_overview_levels(
        filter_to_overview_node(data, counts[i, , drop = FALSE])
      )
      c(lv, rep(NA_character_, 2 - length(lv)))
    }
  )

  out <- counts |>
    dplyr::mutate(
      level_1 = vapply(levels_for, `[`, character(1), 1),
      level_2 = vapply(levels_for, `[`, character(1), 2)
    ) |>
    dplyr::filter(!is.na(.data$level_1))

  if (nrow(out) == 0) {
    return(empty_overview_nodes())
  }

  out <- out |>
    dplyr::mutate(
      node_label = paste(
        .data$ENVIRON_COMPARTMENT,
        .data$ENVIRON_COMPARTMENT_SUB,
        .data$MEASURED_UNIT_STANDARD,
        sep = " / "
      ),
      node_slug = slugify_name(.data$node_label)
    )

  # slugify_name() ends in make.unique(), so a collision would be papered over
  # with a _1 suffix rather than reported, and the unsuffixed slug would remain a
  # string PREFIX of the suffixed one, which quietly breaks any filename matching
  # downstream. Same trap as the one documented on triage_group_label().
  if (anyDuplicated(sub("_[0-9]+$", "", out$node_slug)) > 0) {
    stop(
      "triage_overview_nodes(): two nodes slugged to the same name. ",
      "Node labels must be unique before slugification."
    )
  }

  # Carried as all-NA so the node is shaped like a group. thresholds_for_group()
  # reads grp$SPECIES_GROUP[1] unconditionally for Biota, and on a tibble without
  # that column it gets NULL, making `if (is.na(taxon))` fail with a zero-length
  # condition rather than returning no thresholds.
  for (col in setdiff(triage_group_cols(), node_cols)) {
    out[[col]] <- NA_character_
  }
  out
}

#' Drop Nodes the Notebook Will Never Show
#'
#' The notebook only opens a heading for groups it is displaying, and an overview
#' is emitted when its heading is emitted. A node with no displayed group beneath
#' it therefore gets its PNG written and then never referenced.
#'
#' Clearing the node's own `min_n` is not sufficient, because a node's total is a
#' sum over groups that may each be far below the bar: `Invertebrates` reaches
#' 211 measurements as a node while its largest single group does not reach 100.
#'
#' @param nodes A node table.
#' @param groups The `triage_pilot_groups` target, or `NULL` to skip the check.
#' @param by Columns on which a node and a group must agree.
#' @return `nodes`, filtered.
#' @keywords internal
filter_reachable_nodes <- function(nodes, groups, by) {
  if (is.null(groups) || nrow(nodes) == 0) {
    return(nodes)
  }
  missing <- setdiff(by, names(groups))
  if (length(missing) > 0) {
    stop(
      "filter_reachable_nodes(): groups is missing column(s): ",
      paste(missing, collapse = ", ")
    )
  }
  nodes[do.call(paste, nodes[by]) %in% do.call(paste, groups[by]), , drop = FALSE]
}

#' Zero-Row Node Table
#'
#' Same columns as a populated [triage_overview_nodes()] result, so a caller can
#' `nrow()` it or iterate over it without branching on `NULL`.
#'
#' @return A zero-row tibble.
#' @keywords internal
empty_overview_nodes <- function() {
  cols <- c(
    triage_group_cols(),
    "n", "n_rows", "n_groups", "level_1", "level_2", "node_label", "node_slug"
  )
  out <- tibble::as_tibble(
    stats::setNames(rep(list(character(0)), length(cols)), cols)
  )
  out$n <- integer(0)
  out$n_rows <- integer(0)
  out$n_groups <- integer(0)
  out
}

#' Keep the Largest Categories
#'
#' `SAMPLE_SPECIES` runs to 115 values under `Biota, Aquatic`. The by-category
#' panel is one band per category, so plotting all of them reproduces exactly the
#' unreadably tall figure that [triage_plot_by_category()] documents having sunk
#' the first attempt at these plots.
#'
#' Truncation is by row count and is **reported in the subtitle**, never silent:
#' the panel exists to inform a human judgement, so "there are 90 more of these"
#' is itself part of what it has to say.
#'
#' @param data Rows under one parent node.
#' @param col Category column name.
#' @param max_categories Maximum categories to keep.
#' @return A list of `data` (filtered) and `note` (a string, or `NULL`).
#' @keywords internal
truncate_categories <- function(data, col, max_categories = 25) {
  present <- data[[col]]
  present <- present[!is.na(present) & present != ""]
  n_total <- length(unique(present))
  if (n_total <= max_categories) {
    return(list(data = data, note = NULL))
  }
  keep <- names(sort(table(present), decreasing = TRUE))[seq_len(max_categories)]
  list(
    data = data[!is.na(data[[col]]) & data[[col]] %in% keep, , drop = FALSE],
    note = paste0(
      "showing the ", max_categories, " largest of ", n_total, " by row count"
    )
  )
}

#' Write the Two Overview Panels for One Parent Node
#'
#' @param data The `literature_analysis_ready` target.
#' @param node A one-row tibble from [triage_overview_nodes()].
#' @param dir Output directory.
#' @param scale_limits Output of [compute_triage_scale_limits()], so the
#'   overviews share the value axis with the per-group panels below them.
#' @param thresholds The `copper_toxicity_thresholds` target, or `NULL`.
#' @param max_categories Passed to [truncate_categories()].
#' @param width Figure width in inches.
#' @param height_per_category Figure height allowance per category band.
#' @param min_height,max_height Bounds on the computed height.
#' @param dpi Resolution.
#' @return A character vector of written file paths.
#' @export
write_triage_overview_for_node <- function(
  data,
  node,
  dir = "triage",
  scale_limits = NULL,
  thresholds = NULL,
  max_categories = 25,
  width = 8,
  height_per_category = 0.28,
  min_height = 3.5,
  max_height = 9,
  dpi = 150
) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  node_data <- filter_to_overview_node(data, node)
  lims <- triage_limits_for(scale_limits, node)
  slug <- node$node_slug[1]

  levels <- c(node$level_1[1], node$level_2[1])
  levels <- levels[!is.na(levels)]
  # a, b rather than f, g. These are their own figure in the notebook, with their
  # own caption and cross-reference, not a continuation of the per-group row, so
  # their lettering restarts. The `_overview_` marker in the filename is what
  # keeps them distinct from a group's own a/b panels.
  prefixes <- c("a", "b")[seq_along(levels)]

  paths <- character(0)
  for (i in seq_along(levels)) {
    col <- levels[i]
    trimmed <- truncate_categories(node_data, col, max_categories)
    subtitle <- paste(
      c(node$node_label[1], trimmed$note),
      collapse = " -- "
    )

    p <- triage_plot_by_category(
      trimmed$data,
      col,
      paste0(prefixes[i], ") Distribution by ", triage_level_label(col)),
      subtitle,
      limits = lims,
      thresholds = thresholds,
      grp = node
    )

    n_cat <- length(unique(trimmed$data[[col]]))
    height <- min(max(n_cat * height_per_category, min_height), max_height)

    path <- file.path(
      dir,
      paste0(slug, "_", prefixes[i], "_overview_", tolower(col), ".png")
    )
    ggplot2::ggsave(
      filename = path,
      plot = p,
      width = width,
      height = height,
      dpi = dpi,
      device = ragg::agg_png
    )
    paths <- c(paths, path)
  }
  paths
}

#' Write Overview Panels for Every Parent Node
#'
#' @param data The `literature_analysis_ready` target.
#' @param nodes Output of [triage_overview_nodes()].
#' @param dir Output directory.
#' @param ... Passed to [write_triage_overview_for_node()], notably
#'   `scale_limits` and `thresholds`.
#' @return A character vector of all written file paths, for `format = "file"`.
#' @export
write_triage_overviews <- function(data, nodes, dir = "triage", ...) {
  paths <- purrr::map(
    seq_len(nrow(nodes)),
    function(i) {
      node <- nodes[i, , drop = FALSE]
      message("Triage overview: ", node$node_label[1])
      write_triage_overview_for_node(data, node, dir = dir, ...)
    }
  )
  unlist(paths, use.names = FALSE)
}
