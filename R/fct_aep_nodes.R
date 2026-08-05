# The AEP node layer (PLAN.md P3.1-P3.4). Added 2026-08-05.
#
# WHY THIS IS NOT JUST group_decisions.csv WITH MORE COLUMNS.
#
# A sampling group is defined by triage_group_cols(): compartment, species,
# tissue, site type, unit. An AEP node is whatever Sam decides to assess as one
# thing, and the two are not the same. His own prototype proves it. In
# docs/NBXX-algae.qmd the marine node is:
#
#     filter(ENVIRON_COMPARTMENT_SUB == "Freshwater",
#            LATITUDE >= 66.5,
#            SITE_GEOGRAPHIC_FEATURE == "River, stream, canal")
#
# `LATITUDE >= 66.5` is not in the group key at all, and a few lines later the
# same node drops outliers. So a node can be one group, several groups, or a
# restricted slice of either.
#
# The design answer is a MEMBERSHIP FILE plus a FIXED SET OF RESTRICTION COLUMNS,
# not a filter expression in a CSV cell. Arbitrary R in a spreadsheet cannot be
# validated, fails at pipeline runtime rather than at read time, and cannot be
# diffed meaningfully in review. The restrictions here cover every case the
# prototype notebooks actually use; anything genuinely beyond them should become
# a new column with a name, not an escape hatch.
#
# SPLIT OF AUTHORITY, same as the decisions layer:
#   * the pipeline READS these files and never writes them;
#   * scripts/scaffold_aep_nodes.R appends, and never overwrites a judgement.

#' Latitude of the Arctic Circle
#'
#' @return A single numeric.
#' @export
arctic_circle_lat <- function() {
  66.5
}

#' Permitted Node Levels
#'
#' The vertical position of a node in the pathway, and the reason `y` is
#' hand-placed rather than laid out automatically: vertical position carries
#' source-to-exposure meaning, so an automatic graph layout is actively wrong
#' here (PLAN.md P5.1).
#'
#' * `source` -- a release: emissions, tonnage, an industrial sector.
#' * `medium` -- an environmental compartment carrying copper.
#' * `organism` -- a taxon or tissue in which copper is measured.
#' * `tse` -- a target site exposure, the end of the pathway.
#'
#' @return A character vector.
#' @export
aep_node_levels <- function() {
  c("source", "medium", "organism", "tse")
}

#' Permitted Node Types
#'
#' * `empirical` -- resolved from sampling groups in the data. Must have members.
#' * `external` -- carried from an assessment made outside this dataset, with the
#'   magnitude typed in. The emissions and REACH nodes are these: PLAN.md P3.6
#'   makes the point that those WoE assessments are already written as prose in
#'   `docs/NBXX-norske-utslipp.qmd` and need transcribing, not re-deriving.
#'   Must NOT have members, and carries `value` / `value_unit` instead.
#'
#' @return A character vector.
#' @export
aep_node_types <- function() {
  c("empirical", "external")
}

#' The Four EPEQ Score Columns and Their Justifications
#'
#' Adapted from Peng et al. 2022, and scored 1-3 exactly as in
#' `docs/NBXX-algae.qmd`, which is the reference implementation and is Sam's own
#' wording. Every score carries a written justification in the adjacent column,
#' because a bare number is not a weight of evidence assessment.
#'
#' @return A character vector of column names, scores and justifications
#'   interleaved.
#' @export
epeq_cols <- function() {
  c(
    "essentiality_score", "essentiality_justification",
    "plausibility_score", "plausibility_justification",
    "evidence_score", "evidence_justification",
    "quantification_score", "quantification_justification"
  )
}

#' Columns Owned by the Human
#'
#' Never overwritten by [scaffold_aep_nodes()].
#'
#' @return A character vector of column names.
#' @export
aep_node_human_cols <- function() {
  c(
    "label", "level", "node_type", "x", "y",
    "lat_min", "lat_max", "date_min", "date_max",
    "exclude_references", "drop_outliers",
    "value", "value_sd", "value_n", "value_unit",
    epeq_cols(),
    "notes"
  )
}

#' An Empty Nodes Table
#'
#' The schema in one place, so the scaffold, the reader and the tests cannot
#' drift apart.
#'
#' @return A zero-row tibble.
#' @export
empty_aep_nodes <- function() {
  tibble::tibble(
    node_id = character(0),
    label = character(0),
    level = character(0),
    node_type = character(0),
    x = numeric(0),
    y = numeric(0),
    lat_min = numeric(0),
    lat_max = numeric(0),
    date_min = as.Date(character(0)),
    date_max = as.Date(character(0)),
    exclude_references = character(0),
    drop_outliers = logical(0),
    value = numeric(0),
    value_sd = numeric(0),
    value_n = numeric(0),
    value_unit = character(0),
    essentiality_score = numeric(0),
    essentiality_justification = character(0),
    plausibility_score = numeric(0),
    plausibility_justification = character(0),
    evidence_score = numeric(0),
    evidence_justification = character(0),
    quantification_score = numeric(0),
    quantification_justification = character(0),
    notes = character(0)
  )
}

#' Read and Validate the AEP Nodes File
#'
#' Validates rather than trusts, for the same reason as
#' [read_group_decisions()]: this file is hand-edited, and a typo must fail here
#' rather than produce an empty or wrong node in a manuscript figure.
#'
#' @param path Where the CSV lives.
#' @return A tibble of nodes.
#' @export
read_aep_nodes <- function(path = here_rel("data/clean/aep_nodes.csv")) {
  if (!file.exists(path)) {
    stop(
      "No nodes file at ", path,
      ". Run scripts/scaffold_aep_nodes.R first."
    )
  }
  nodes <- readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = readr::cols(
      .default = readr::col_guess(),
      node_id = readr::col_character(),
      label = readr::col_character(),
      level = readr::col_character(),
      node_type = readr::col_character(),
      exclude_references = readr::col_character(),
      value_unit = readr::col_character(),
      notes = readr::col_character()
    )
  )

  missing <- setdiff(names(empty_aep_nodes()), names(nodes))
  if (length(missing) > 0) {
    stop("Nodes file is missing column(s): ", paste(missing, collapse = ", "))
  }

  dup <- nodes$node_id[duplicated(nodes$node_id)]
  if (length(dup) > 0) {
    stop("Duplicate node_id(s): ", paste(unique(dup), collapse = ", "))
  }

  bad_level <- setdiff(stats::na.omit(unique(nodes$level)), aep_node_levels())
  if (length(bad_level) > 0) {
    stop(
      "Unrecognised level(s): ", paste(sQuote(bad_level), collapse = ", "),
      ". Permitted: ", paste(aep_node_levels(), collapse = ", ")
    )
  }

  bad_type <- setdiff(stats::na.omit(unique(nodes$node_type)), aep_node_types())
  if (length(bad_type) > 0) {
    stop(
      "Unrecognised node_type(s): ", paste(sQuote(bad_type), collapse = ", "),
      ". Permitted: ", paste(aep_node_types(), collapse = ", ")
    )
  }

  # Scores are 1-3 or blank. A 0 or a 4 is a typo, and a typo that survives into
  # a figure is indistinguishable from a judgement.
  for (col in epeq_cols()[c(TRUE, FALSE)]) {
    v <- nodes[[col]]
    bad <- !is.na(v) & !(v %in% 1:3)
    if (any(bad)) {
      stop(
        sum(bad), " row(s) have an out-of-range ", col,
        ": scores are 1, 2 or 3, or blank if unscored."
      )
    }
  }

  nodes
}

#' Read and Validate the Node Membership File
#'
#' @param path Where the CSV lives.
#' @param nodes Optional nodes table, to check every `node_id` exists.
#' @param ids Optional group id ledger, to check every `group_id` exists.
#' @return A tibble of `node_id`, `group_id`, `notes`.
#' @export
read_aep_node_members <- function(
  path = here_rel("data/clean/aep_node_members.csv"),
  nodes = NULL,
  ids = NULL
) {
  if (!file.exists(path)) {
    stop(
      "No membership file at ", path,
      ". Run scripts/scaffold_aep_nodes.R first."
    )
  }
  members <- readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )

  missing <- setdiff(c("node_id", "group_id"), names(members))
  if (length(missing) > 0) {
    stop("Membership file is missing column(s): ", paste(missing, collapse = ", "))
  }

  dup <- members |>
    dplyr::count(.data$node_id, .data$group_id) |>
    dplyr::filter(.data$n > 1)
  if (nrow(dup) > 0) {
    stop(
      "Duplicate membership row(s): ",
      paste(dup$node_id, dup$group_id, collapse = ", ")
    )
  }

  if (!is.null(nodes)) {
    unknown <- setdiff(members$node_id, nodes$node_id)
    if (length(unknown) > 0) {
      stop(
        "Membership names ", length(unknown), " unknown node_id(s): ",
        paste(sQuote(utils::head(unknown, 5)), collapse = ", ")
      )
    }
  }
  if (!is.null(ids)) {
    unknown <- setdiff(members$group_id, ids$group_id)
    if (length(unknown) > 0) {
      stop(
        "Membership names ", length(unknown), " unknown group_id(s): ",
        paste(sQuote(utils::head(unknown, 5)), collapse = ", "),
        ". Run scripts/allocate_group_ids.R if these are new groups."
      )
    }
  }

  members
}

#' Resolve One Node to its Rows of Data
#'
#' Membership first, then the restriction columns, in that order.
#'
#' **Mixed units are refused, not averaged.** A node pooling `mg/kg (dry)` with
#' `mg/kg (wet)` would produce a mean that means nothing, and the difference is
#' routinely a factor of four or five in biota. This is the same reasoning that
#' makes the unit part of the group key in the first place, and the same reason
#' [parse_measured_unit()] refuses a bare `mg/kg`.
#'
#' `external` nodes resolve to zero rows by design and are not an error: their
#' magnitude is typed into `value`, having been assessed elsewhere.
#'
#' @param node A one-row nodes tibble.
#' @param members The membership table.
#' @param data The `literature_analysis_ready` target.
#' @param ids The group id ledger, to map `group_id` back to group-key columns.
#' @return A tibble of the node's rows, possibly zero-row.
#' @export
resolve_node_data <- function(node, members, data, ids) {
  stopifnot(nrow(node) == 1)

  if (identical(node$node_type[1], "external")) {
    return(data[0, , drop = FALSE])
  }

  my_groups <- members$group_id[members$node_id == node$node_id[1]]
  if (length(my_groups) == 0) {
    return(data[0, , drop = FALSE])
  }

  key <- triage_group_cols()
  keys <- ids |>
    dplyr::filter(.data$group_id %in% my_groups) |>
    dplyr::select(dplyr::all_of(key))

  out <- data |> dplyr::semi_join(keys, by = key)

  # --- restrictions, each skipped when blank -----------------------------
  if (!is.na(node$lat_min[1])) {
    out <- out[!is.na(out$LATITUDE) & out$LATITUDE >= node$lat_min[1], ]
  }
  if (!is.na(node$lat_max[1])) {
    out <- out[!is.na(out$LATITUDE) & out$LATITUDE <= node$lat_max[1], ]
  }
  if (!is.na(node$date_min[1])) {
    out <- out[!is.na(out$SAMPLING_DATE) &
      out$SAMPLING_DATE >= node$date_min[1], ]
  }
  if (!is.na(node$date_max[1])) {
    out <- out[!is.na(out$SAMPLING_DATE) &
      out$SAMPLING_DATE <= node$date_max[1], ]
  }
  # Semicolon-separated, because a comma cannot survive a CSV cell unquoted and
  # reference ids are already long enough to be mistyped.
  if (!is.na(node$exclude_references[1]) && nzchar(node$exclude_references[1])) {
    drop <- trimws(strsplit(node$exclude_references[1], ";", fixed = TRUE)[[1]])
    drop <- drop[nzchar(drop)]
    out <- out[!out$REFERENCE_ID %in% drop, ]
  }
  if (isTRUE(node$drop_outliers[1]) && nrow(out) > 0) {
    # Computed WITHIN the resolved node, not inherited from the sampling group.
    # A value that is an outlier against its own small group may be unremarkable
    # against the pooled node, and the node is the thing being assessed.
    flags <- flag_outliers(out$MEASURED_VALUE_STANDARD)
    out <- out[!(flags$outlier_RMZ %in% TRUE & flags$outlier_IQR %in% TRUE), ]
  }

  units <- unique(stats::na.omit(out$MEASURED_UNIT_STANDARD))
  if (length(units) > 1) {
    stop(
      "Node ", node$node_id[1], " (", node$label[1], ") pools ",
      length(units), " units: ", paste(units, collapse = ", "),
      ". Split it, or restrict its membership to one unit."
    )
  }

  out
}

#' Report Card for One Node
#'
#' The compact summary PLAN.md section 4.3 asks a node to carry, as one row.
#'
#' **Arctic coverage is reported, never filtered.** Sam's decision 2026-08-05,
#' chosen over a global `LATITUDE >= 66.5` cut that would have dropped 81% of
#' measurements and left the marine node on 258. So the AEP is Norwegian and
#' Arctic representativeness is a stated property of each node, in the same
#' spirit as `n_sources`: a visible weakness rather than a silent one.
#'
#' Geometric mean and GSD alongside the arithmetic pair, matching
#' `summarise_literature_data`: these concentrations are lognormal over orders of
#' magnitude, so the arithmetic mean sits above almost every observation.
#'
#' @param node A one-row nodes tibble.
#' @param members The membership table.
#' @param data The `literature_analysis_ready` target.
#' @param ids The group id ledger.
#' @return A one-row tibble.
#' @export
node_report_card <- function(node, members, data, ids) {
  d <- resolve_node_data(node, members, data, ids)

  if (nrow(d) == 0) {
    # An external node reports the magnitude that was typed in; an empirical one
    # with no rows reports nothing and is caught by validate_aep_nodes().
    return(tibble::tibble(
      node_id = node$node_id[1],
      label = node$label[1],
      level = node$level[1],
      node_type = node$node_type[1],
      n = node$value_n[1],
      n_rows = 0L,
      n_groups = 0L,
      n_sources = NA_integer_,
      unit = node$value_unit[1],
      mean = node$value[1],
      sd = node$value_sd[1],
      geo_mean = NA_real_,
      gsd = NA_real_,
      median = NA_real_,
      n_arctic = NA_real_,
      pct_arctic = NA_real_,
      lat_min = NA_real_,
      lat_max = NA_real_,
      date_min = as.Date(NA),
      date_max = as.Date(NA)
    ))
  }

  v <- d$MEASURED_VALUE_STANDARD
  w <- d$MEASURED_N
  lat <- d$LATITUDE
  arctic <- !is.na(lat) & lat >= arctic_circle_lat()

  tibble::tibble(
    node_id = node$node_id[1],
    label = node$label[1],
    level = node$level[1],
    node_type = node$node_type[1],
    # Measurements, per CLAUDE.md 4.4.-1. n_rows alongside, named as rows.
    n = sum(w, na.rm = TRUE),
    n_rows = nrow(d),
    n_groups = length(unique(members$group_id[members$node_id == node$node_id[1]])),
    n_sources = dplyr::n_distinct(d$REFERENCE_ID),
    unit = unique(d$MEASURED_UNIT_STANDARD)[1],
    mean = mean(v, na.rm = TRUE),
    sd = stats::sd(v, na.rm = TRUE),
    geo_mean = 10^mean(log10(v), na.rm = TRUE),
    gsd = 10^stats::sd(log10(v), na.rm = TRUE),
    median = stats::median(v, na.rm = TRUE),
    n_arctic = sum(w[arctic], na.rm = TRUE),
    pct_arctic = 100 * sum(w[arctic], na.rm = TRUE) / sum(w, na.rm = TRUE),
    lat_min = suppressWarnings(min(lat, na.rm = TRUE)),
    lat_max = suppressWarnings(max(lat, na.rm = TRUE)),
    date_min = suppressWarnings(min(d$SAMPLING_DATE, na.rm = TRUE)),
    date_max = suppressWarnings(max(d$SAMPLING_DATE, na.rm = TRUE))
  )
}

#' Report Cards for Every Node
#'
#' @param nodes The nodes table.
#' @param members The membership table.
#' @param data The `literature_analysis_ready` target.
#' @param ids The group id ledger.
#' @return A tibble, one row per node.
#' @export
aep_node_report_cards <- function(nodes, members, data, ids) {
  if (nrow(nodes) == 0) {
    return(node_report_card(
      dplyr::bind_rows(empty_aep_nodes(), tibble::tibble(node_id = NA_character_)),
      members, data, ids
    )[0, ])
  }
  purrr::list_rbind(purrr::map(
    seq_len(nrow(nodes)),
    function(i) node_report_card(nodes[i, , drop = FALSE], members, data, ids)
  ))
}

#' Validate the Node Layer as a Whole
#'
#' Cross-file checks that neither reader can make alone. Warnings rather than
#' errors throughout, because a half-built node layer is the normal state while
#' the assessment is in progress and the pipeline must still run.
#'
#' @param nodes The nodes table.
#' @param members The membership table.
#' @param cards Output of [aep_node_report_cards()].
#' @return `nodes`, invisibly.
#' @export
validate_aep_nodes <- function(nodes, members, cards) {
  problems <- character(0)

  empirical <- nodes$node_id[nodes$node_type %in% "empirical"]
  no_members <- setdiff(empirical, members$node_id)
  if (length(no_members) > 0) {
    problems <- c(problems, paste0(
      length(no_members), " empirical node(s) have no members: ",
      paste(no_members, collapse = ", ")
    ))
  }

  external_with_members <- intersect(
    nodes$node_id[nodes$node_type %in% "external"],
    members$node_id
  )
  if (length(external_with_members) > 0) {
    problems <- c(problems, paste0(
      length(external_with_members),
      " external node(s) have members, which are ignored: ",
      paste(external_with_members, collapse = ", ")
    ))
  }

  empty <- cards$node_id[cards$n_rows == 0 & cards$node_type %in% "empirical"]
  if (length(empty) > 0) {
    problems <- c(problems, paste0(
      length(empty), " empirical node(s) resolve to no data: ",
      paste(empty, collapse = ", "),
      " (check the restriction columns)"
    ))
  }

  unscored <- nodes$node_id[
    is.na(nodes$essentiality_score) | is.na(nodes$plausibility_score) |
      is.na(nodes$evidence_score) | is.na(nodes$quantification_score)
  ]
  if (length(unscored) > 0) {
    problems <- c(problems, paste0(
      length(unscored), " node(s) are not fully EPEQ scored: ",
      paste(utils::head(unscored, 8), collapse = ", ")
    ))
  }

  unplaced <- nodes$node_id[is.na(nodes$x) | is.na(nodes$y)]
  if (length(unplaced) > 0) {
    problems <- c(problems, paste0(
      length(unplaced), " node(s) have no x/y placement: ",
      paste(utils::head(unplaced, 8), collapse = ", ")
    ))
  }

  if (length(problems) > 0) {
    cli::cli_warn(c(
      "AEP node layer is incomplete:",
      stats::setNames(problems, rep("*", length(problems)))
    ))
  }

  invisible(nodes)
}

#' What Has Not Been Claimed by Any Node
#'
#' **The backlog view, and the reason it exists.** Sam abandoned sequential review
#' of all 245 groups on 2026-08-05 in favour of picking groups of interest and
#' expanding outwards. That is the right call, but it needs the complement: a
#' ranked list of what has *not* been picked, so stopping is an informed choice
#' rather than an omission nobody noticed.
#'
#' Ranked by measurements descending, so the largest unclaimed group is always
#' the first thing on screen.
#'
#' @param members The membership table.
#' @param summary_data The `summarise_literature_data` target.
#' @param ids The group id ledger.
#' @param decisions Optional decisions table, to carry `decision` through so a
#'   group deliberately dropped is distinguishable from one never looked at.
#' @return A tibble, one row per group, with `node_id` (or `NA`) and `claimed`.
#' @export
node_coverage <- function(members, summary_data, ids, decisions = NULL) {
  key <- triage_group_cols()

  claimed <- members |>
    dplyr::group_by(.data$group_id) |>
    dplyr::summarise(
      node_id = paste(sort(unique(.data$node_id)), collapse = "; "),
      .groups = "drop"
    )

  out <- summary_data |>
    add_coverage_columns() |>
    attach_group_ids(ids) |>
    dplyr::left_join(claimed, by = "group_id") |>
    dplyr::mutate(claimed = !is.na(.data$node_id))

  if (!is.null(decisions) && "decision" %in% names(decisions)) {
    out <- out |>
      dplyr::left_join(
        decisions |> dplyr::select("group_id", "decision"),
        by = "group_id"
      )
  }

  out |>
    dplyr::select(
      dplyr::any_of(c(
        "group_id", "rank", "n", "n_sources", "cum_pct", "tier",
        "node_id", "claimed", "decision"
      )),
      dplyr::all_of(key)
    ) |>
    dplyr::arrange(dplyr::desc(.data$n))
}

#' One-Line Summary of Node Coverage
#'
#' What share of the data the current node set accounts for. The number to watch
#' when deciding whether to add another node or stop.
#'
#' @param coverage Output of [node_coverage()].
#' @return A one-row tibble.
#' @export
node_coverage_summary <- function(coverage) {
  tibble::tibble(
    groups = nrow(coverage),
    groups_claimed = sum(coverage$claimed),
    measurements = sum(coverage$n),
    measurements_claimed = sum(coverage$n[coverage$claimed]),
    pct_measurements_claimed = round(
      100 * sum(coverage$n[coverage$claimed]) / sum(coverage$n),
      1
    )
  )
}
