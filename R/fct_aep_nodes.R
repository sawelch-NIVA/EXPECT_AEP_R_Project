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

#' Magnitude Columns for External Nodes Only
#'
#' **These are never read for an `empirical` node**, whose mean, sd, geometric
#' mean, GSD, median, n and source count are all computed from its constituent
#' groups by [node_report_card()]. They exist only for `external` nodes, where
#' there is no data in this dataset to compute from: a national emissions total,
#' a REACH tonnage, a crustal abundance.
#'
#' **Renamed from `value*` to `external_*` on 2026-08-05**, when Sam asked the
#' obvious question: "why are we specifying these manually rather than
#' calculating from constituent groups?" The answer was "we do calculate them,
#' just not for these nodes", which is a sign the columns were misnamed rather
#' than a sign the question was wrong. A column called `value` on a table of
#' nodes reads as *the* value of every node.
#'
#' [read_aep_nodes()] now **stops** if one of these is filled on an `empirical`
#' node, rather than ignoring it. A number typed into a column that is never
#' read is the same failure class as the untracked decisions file and the
#' unhashed package namespace: work that appears done and silently is not.
#'
#' @return A character vector of column names.
#' @export
external_value_cols <- function() {
  c("external_value", "external_sd", "external_n", "external_unit")
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
    "exclude_references", "exclude_campaigns", "drop_outliers",
    external_value_cols(),
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
    # Added 2026-08-06. A whole campaign can be defective in a way that is not a
    # unit error and so cannot be repaired by unit_corrections.csv: 20 of the 44
    # G. morhua muscle rows carry liver-like concentrations, isolated to two
    # campaigns, with no intermediate values and with liver flat over the same
    # period. That is a tissue-labelling fault, and there is no factor that
    # fixes it. Excluding the affected rows and scoring what remains is honest;
    # averaging over rows believed to be mislabelled and calling the result
    # low-quality evidence is not.
    #
    # NOT covered by drop_outliers: 20 of 44 rows is far too large a fraction
    # for Tukey fences to reach, and a mode that size is not an outlier in any
    # statistical sense. This is a provenance judgement, not a statistical one.
    exclude_campaigns = character(0),
    drop_outliers = logical(0),
    # External nodes only; see external_value_cols().
    external_value = numeric(0),
    external_sd = numeric(0),
    external_n = numeric(0),
    external_unit = character(0),
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

#' Parse a Date Bound, Accepting a Bare Year
#'
#' `date_min` and `date_max` accept either a full `YYYY-MM-DD` or a bare year.
#' A bare year expands to the **inclusive** end of its interval: `2010` as a
#' lower bound is `2010-01-01`, and as an upper bound `2010-12-31`. So
#' `date_min = 2010, date_max = 2020` means the eleven whole years you would
#' expect it to mean.
#'
#' **This exists because the alternative silently emptied every node.** Sam's
#' first pass entered `date_min = 1900, date_max = 2100`, which is the obvious
#' thing to type. `readr` parsed them as numbers, and comparing a `Date` to
#' `2100` coerces the date to days-since-1970, so the bound meant "before
#' mid-1975" and every node resolved to zero rows with no error. Refusing years
#' outright would be safe but obtuse; accepting them under a stated convention is
#' both safe and what the typist meant.
#'
#' Anything that is neither is an error rather than an `NA`, because an
#' unparseable restriction that quietly becomes "no restriction" is how a node
#' silently changes meaning.
#'
#' @param x A character, numeric or Date vector.
#' @param bound `"min"` or `"max"`, deciding which end of a bare year is taken.
#' @return A Date vector.
#' @export
parse_node_date <- function(x, bound = c("min", "max")) {
  bound <- match.arg(bound)
  if (length(x) == 0) {
    return(as.Date(character(0)))
  }
  if (inherits(x, "Date")) {
    return(x)
  }

  chr <- trimws(as.character(x))
  out <- as.Date(rep(NA, length(chr)))

  blank <- is.na(chr) | !nzchar(chr)
  year <- !blank & grepl("^[0-9]{4}$", chr)
  full <- !blank & !year

  if (any(year)) {
    out[year] <- as.Date(paste0(
      chr[year],
      if (bound == "min") "-01-01" else "-12-31"
    ))
  }
  if (any(full)) {
    parsed <- suppressWarnings(as.Date(chr[full], format = "%Y-%m-%d"))
    if (any(is.na(parsed))) {
      stop(
        "Unparseable date_", bound, " value(s): ",
        paste(sQuote(utils::head(chr[full][is.na(parsed)], 5)), collapse = ", "),
        ". Use YYYY-MM-DD, or a bare year."
      )
    }
    out[full] <- parsed
  }

  out
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
read_aep_nodes <- function(path = here_rel("data/clean/aep/aep_nodes.csv")) {
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
      exclude_campaigns = readr::col_character(),
      external_unit = readr::col_character(),
      notes = readr::col_character(),
      # Read as text, then parsed by parse_node_date(). Letting readr guess is
      # what allowed a bare year through as a number, which then compared
      # against a Date as days-since-1970.
      date_min = readr::col_character(),
      date_max = readr::col_character()
    )
  )

  nodes$date_min <- parse_node_date(nodes$date_min, "min")
  nodes$date_max <- parse_node_date(nodes$date_max, "max")

  inverted <- !is.na(nodes$date_min) & !is.na(nodes$date_max) &
    nodes$date_min > nodes$date_max
  if (any(inverted)) {
    stop(
      sum(inverted), " node(s) have date_min after date_max: ",
      paste(sQuote(nodes$node_id[inverted]), collapse = ", ")
    )
  }
  inverted_lat <- !is.na(nodes$lat_min) & !is.na(nodes$lat_max) &
    nodes$lat_min > nodes$lat_max
  if (any(inverted_lat)) {
    stop(
      sum(inverted_lat), " node(s) have lat_min above lat_max: ",
      paste(sQuote(nodes$node_id[inverted_lat]), collapse = ", ")
    )
  }

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

  # STOPS rather than warns, and rather than ignoring. An empirical node's
  # magnitude is computed from its member groups, so a number typed into these
  # columns is never read: the node would report a value the file does not
  # contain, and the file would show a value the node does not use. Silently
  # discarding hand-entered numbers is the failure this project has now hit three
  # times (untracked decisions file, unhashed package namespace, this).
  filled <- vapply(
    external_value_cols(),
    function(col) !is.na(nodes[[col]]),
    logical(nrow(nodes))
  )
  if (nrow(nodes) == 1) {
    filled <- matrix(filled, nrow = 1, dimnames = list(NULL, external_value_cols()))
  }
  offenders <- nodes$node_id[
    nodes$node_type %in% "empirical" & apply(filled, 1, any)
  ]
  if (length(offenders) > 0) {
    stop(
      length(offenders), " empirical node(s) have external_* values set: ",
      paste(sQuote(offenders), collapse = ", "),
      ". These columns are only read for node_type = 'external'; an empirical ",
      "node's magnitude is computed from its member groups. Either clear them ",
      "or change node_type."
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
  path = here_rel("data/clean/aep/aep_node_members.csv"),
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

#' Apply One Semicolon-Separated Exclusion Column
#'
#' Shared by `exclude_references` and `exclude_campaigns`, so the two cannot
#' drift apart in how they split, trim, or handle a blank cell.
#'
#' **Warns when a listed value matches nothing.** A stale exclusion silently
#' doing nothing is the same failure that has now bitten this project three
#' times (the missing `imports`, the untracked decisions file, and a correction
#' whose selector no longer matched). Here it is quieter and worse: the node
#' still resolves, still produces a mean, and the rows you believed you had
#' removed are back in it. A typo in a campaign name is easy and invisible
#' otherwise, since these strings carry spaces and parentheses.
#'
#' It warns rather than aborts, unlike the corrections layer, because a node
#' exclusion narrows an estimate rather than rewriting a measurement, and
#' because a legitimately empty match happens while a node is being built up.
#'
#' @param data The node's rows so far.
#' @param node A one-row nodes tibble.
#' @param col Name of the exclusion column on `node`.
#' @param target Name of the column in `data` to match against.
#' @return `data` with excluded rows removed.
#' @export
apply_node_exclusion <- function(data, node, col, target) {
  if (!col %in% names(node) || is.na(node[[col]][1]) ||
    !nzchar(node[[col]][1])) {
    return(data)
  }
  drop <- trimws(strsplit(node[[col]][1], ";", fixed = TRUE)[[1]])
  drop <- drop[nzchar(drop)]
  if (length(drop) == 0) {
    return(data)
  }
  if (!target %in% names(data)) {
    cli::cli_warn(
      "Node {.val {node$node_id[1]}} sets {.field {col}} but the data has no \\
       {.field {target}} column; the exclusion did nothing."
    )
    return(data)
  }

  unmatched <- drop[!drop %in% data[[target]]]
  if (length(unmatched) > 0) {
    cli::cli_warn(c(
      "Node {.val {node$node_id[1]}}: {length(unmatched)} value{?s} in \\
       {.field {col}} matched no rows.",
      "*" = "{.val {unmatched}}",
      "i" = "Check for a typo. The rows you meant to exclude are still in \\
             the node."
    ))
  }

  data[!data[[target]] %in% drop, , drop = FALSE]
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
  # Longitude arrives only from an AEP's bounding box (see aep_scope_nodes()),
  # so the columns are absent on a bare nodes table and their absence means "no
  # restriction" rather than an error.
  # `%in% names()`, not `$`: a tibble warns on `$` for a column it does not
  # have, and this runs once per node per AEP.
  if ("lon_min" %in% names(node) && !is.na(node$lon_min[1])) {
    out <- out[!is.na(out$LONGITUDE) & out$LONGITUDE >= node$lon_min[1], ]
  }
  if ("lon_max" %in% names(node) && !is.na(node$lon_max[1])) {
    out <- out[!is.na(out$LONGITUDE) & out$LONGITUDE <= node$lon_max[1], ]
  }
  # Dates, not numbers. Comparing a Date against a bare year silently reads the
  # year as days-since-1970 and empties the node; read_aep_nodes() converts, and
  # this catches any caller that bypassed it.
  for (col in c("date_min", "date_max")) {
    if (!is.na(node[[col]][1]) && !inherits(node[[col]], "Date")) {
      stop(
        col, " on node ", node$node_id[1], " is ", class(node[[col]])[1],
        ", not a Date. Read the file with read_aep_nodes(), which accepts a ",
        "bare year and converts it."
      )
    }
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
  out <- apply_node_exclusion(
    out, node, "exclude_references", "REFERENCE_ID"
  )
  # Campaign names contain commas and parentheses ("Vm_2010_2025 (Urban Fjord
  # Contaminants)"), so the semicolon separator matters more here still.
  out <- apply_node_exclusion(
    out, node, "exclude_campaigns", "CAMPAIGN_NAME_SHORT"
  )
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

#' Weighted Median
#'
#' No dependency for four lines. Ties and zero weights behave as you would
#' expect; an even split takes the lower of the two straddling values rather
#' than interpolating, which keeps the result a value that was actually
#' observed.
#'
#' @param x Numeric values. @param w Weights, same length.
#' @return A single number, or `NA_real_` where nothing is usable.
#' @export
weighted_median <- function(x, w) {
  keep <- !is.na(x) & !is.na(w) & w > 0
  if (!any(keep)) {
    return(NA_real_)
  }
  x <- x[keep]
  w <- w[keep]
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  x[which(cumsum(w) >= sum(w) / 2)[1]]
}

#' Why the Centre is Weighted and the Spread is Not
#'
#' Recorded here because it is the one methodological choice in the node layer,
#' and Sam asked for it to be explained rather than asserted (2026-08-05).
#'
#' A row in this project is one of two things. A Vannmiljø row is a single
#' measurement, `MEASURED_N = 1`. A literature row is a **summary**: `MEASURED_N
#' = 50` means the authors measured fifty samples and reported one number for
#' them. There are 368 such rows, carrying 6,056 of 95,816 measurements.
#'
#' **The centre is weighted.** If fifty mussels averaged 2.4 mg/kg, that fact
#' should carry the weight of fifty mussels rather than of one. An unweighted
#' mean over rows lets a single Vannmiljø observation outvote a fifty-sample
#' study, and makes the reported `n` describe a different population from the
#' reported mean. That was the inconsistency in the first version: node N003
#' reported `n = 5,498` beside a geometric mean computed over 3,093 rows, 45% of
#' the claimed n coming from 1.5% of the rows.
#'
#' **The spread is not weighted, and cannot honestly be.** We hold the study
#' *means*, not the study *values*. Weighting the spread would treat those fifty
#' mussels as fifty copies of one number, erasing the within-study variation and
#' reporting a dataset far tighter than it is. Reconstructing the real variance
#' would need a within-study spread for every aggregated row, and this dataset
#' has one for 202 of 368 rows in five non-interconvertible forms (standard
#' deviation, 95% confidence interval, geometric SD, interquartile range,
#' min-max). Converting between those needs distributional assumptions per row.
#'
#' CLAUDE.md's standing rule settles it: a spread statistic that cannot be
#' justified in the methods section is worse than none. So `sd` and `gsd` are
#' **per row**, `n_rows` sits beside them in the card, and the difference is
#' documented rather than papered over.
#'
#' @name node_statistic_weighting
NULL

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
      n = node$external_n[1],
      n_rows = 0L,
      n_groups = 0L,
      n_sources = NA_integer_,
      unit = node$external_unit[1],
      mean = node$external_value[1],
      sd = node$external_sd[1],
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
    # Every level of aggregation a node spans, per Sam 2026-08-05: "each node
    # [represents] 1+ group covering 1+ MEASURED_N and 1+ different references.
    # we need to report each of these levels".
    n = sum(w, na.rm = TRUE),
    n_rows = nrow(d),
    n_groups = length(unique(members$group_id[members$node_id == node$node_id[1]])),
    n_sources = dplyr::n_distinct(d$REFERENCE_ID),
    unit = unique(d$MEASURED_UNIT_STANDARD)[1],
    # CENTRE: weighted by MEASURED_N, so it describes the same population as the
    # `n` reported beside it. SPREAD: per row, because we hold study means and
    # not study values. See ?node_statistic_weighting.
    mean = stats::weighted.mean(v, w = w, na.rm = TRUE),
    sd = stats::sd(v, na.rm = TRUE),
    geo_mean = 10^stats::weighted.mean(log10(v), w = w, na.rm = TRUE),
    gsd = 10^stats::sd(log10(v), na.rm = TRUE),
    median = weighted_median(v, w),
    n_arctic = sum(w[arctic], na.rm = TRUE),
    pct_arctic = 100 * sum(w[arctic], na.rm = TRUE) / sum(w, na.rm = TRUE),
    lat_min = suppressWarnings(min(lat, na.rm = TRUE)),
    lat_max = suppressWarnings(max(lat, na.rm = TRUE)),
    # as.Date(), not the bare min(). SAMPLING_DATE is an IDate (data.table),
    # courtesy of standardise_IDate_all(), while the zero-row branch above
    # returns as.Date(NA). vctrs refuses to combine IDate with Date, so a node
    # set containing both an empirical and an external node failed to bind at
    # all: "Can't combine ..1$date_min <IDate> and ..6$date_min <date>".
    # Caught by the pipeline, not by the unit tests, whose fixtures use plain
    # Dates throughout.
    date_min = as.Date(suppressWarnings(min(d$SAMPLING_DATE, na.rm = TRUE))),
    date_max = as.Date(suppressWarnings(max(d$SAMPLING_DATE, na.rm = TRUE)))
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

  # The converse of the check in read_aep_nodes(). An external node with no
  # magnitude is the other half-finished state: it has no member groups to
  # compute from AND nothing typed in, so its card reports NA and says so
  # nowhere else.
  no_value <- nodes$node_id[
    nodes$node_type %in% "external" & is.na(nodes$external_value)
  ]
  if (length(no_value) > 0) {
    problems <- c(problems, paste0(
      length(no_value), " external node(s) have no external_value: ",
      paste(no_value, collapse = ", "),
      " (nothing to compute from and nothing entered)"
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
