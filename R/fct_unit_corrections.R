# Correcting unit errors in the SOURCE data (2026-08-06).
#
# Third file in the unit trilogy, and the only one that changes a number:
#
#   R/fct_units.R           conversions this project performs
#   R/fct_unit_anomalies.R  detection of errors that arrived already made
#   R/fct_unit_corrections.R (this)  the human decision to override them
#
# The decision layer is data, not code: data/clean/decisions/unit_corrections.csv is
# hand-edited, the pipeline reads it and never writes it. Same contract as
# group_decisions.csv and aep_nodes.csv, and for the same reason -- overriding a
# national monitoring database is a scientific judgement that has to be
# reviewable in a diff and defensible in the methods, not buried in a mutate().
#
# Lower-case column names throughout, here and in the CSV: SCREAMING_SNAKE in
# this project means "column of the eData schema", and none of this is.
#
# WHY BOTH A SELECTOR AND A ROW ID LIST.
#
# Each correction carries how the rows were found (selector) *and* which rows
# that found (row_ids), and the pipeline requires the two to agree exactly.
# Either alone is weaker:
#
#   selector alone -- silently widens. If a re-export adds two rows matching the
#     same comment, they get corrected without anyone deciding they should be.
#   row_ids alone  -- silently narrows, and records no reasoning. A row that
#     should be corrected but was added later is simply missed, and a reader
#     cannot tell a deliberate omission from an oversight.
#
# Holding both turns a silent change of extent into a build failure that names
# the drift. That is the property that makes this an audit trail rather than a
# record, and it is what the row_id work exists to support.
#
# The scaffolding script resolves selector -> row_ids once so the file is
# self-contained; see scripts/scaffold_unit_corrections.R.

#' Value Columns a Unit Correction Scales
#'
#' The measured value and both detection limits. **The limits are included on
#' purpose** (Sam's call 2026-08-06): a submitter who multiplied their values by
#' 1000 multiplied the LOD and LOQ reported alongside them, so scaling the value
#' and leaving the limits behind would turn a consistent error into an
#' inconsistent one, and would corrupt the `x / sqrt(2)` imputation downstream.
#'
#' Imputed columns are included because they are derived from the limits and
#' would otherwise disagree with them.
#'
#' `MEASURED_VALUE` is deliberately **absent**. It is the as-reported number and
#' stays untouched as the audit trail, so a corrected row still shows what the
#' source actually said.
#'
#' @return A character vector of column names.
#' @export
unit_correction_value_cols <- function() {
  c(
    "MEASURED_VALUE_STANDARD",
    "LOD_VALUE_STANDARD",
    "LOQ_VALUE_STANDARD",
    "LOD_VALUE_STANDARD_IMPUTED",
    "LOQ_VALUE_STANDARD_IMPUTED"
  )
}

#' Selector Columns of a Unit Correction
#'
#' Fixed columns, never a filter expression in a cell. Same reasoning as the AEP
#' node restrictions (PLAN.md P3.3): arbitrary R in a spreadsheet cannot be
#' validated, fails at pipeline runtime rather than read time, and does not diff
#' usefully. Anything beyond these should become a named column here, not an
#' escape hatch.
#'
#' All are optional and combine with AND. A correction with none of them set is
#' refused rather than matching the whole dataset.
#'
#' @return A character vector of column names.
#' @export
unit_correction_selector_cols <- function() {
  c(
    "group_id",
    "campaign_name_short",
    "comment_match",
    "value_min",
    "value_max"
  )
}

#' Empty Unit Corrections Table
#'
#' The canonical column set and types, used to seed a new CSV and to return from
#' the degenerate read.
#'
#' @return A zero-row tibble.
#' @export
empty_unit_corrections <- function() {
  tibble::tibble(
    correction_id = character(0),
    group_id = character(0),
    campaign_name_short = character(0),
    comment_match = character(0),
    value_min = numeric(0),
    value_max = numeric(0),
    factor = numeric(0),
    row_ids = character(0),
    reason = character(0),
    evidence = character(0),
    date_added = character(0)
  )
}

#' Read and Validate the Unit Corrections File
#'
#' Every check here is an error rather than a warning. This file overwrites
#' measured values, so a correction that is stale, ambiguous or undocumented must
#' stop the build rather than quietly do something.
#'
#' @param path Path to the CSV. A missing file yields an empty table, so the
#'   pipeline runs before any correction has been decided.
#' @return A tibble of corrections.
#' @export
read_unit_corrections <- function(path) {
  if (is.null(path) || !file.exists(path)) {
    return(empty_unit_corrections())
  }

  # Header only, so the column and field-count checks below run before readr has
  # a chance to warn about a row it is already mis-parsing.
  header <- names(readr::read_csv(
    path,
    n_max = 0,
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE
  ))

  missing_cols <- setdiff(names(empty_unit_corrections()), header)
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "{.file {path}} is missing required column{?s}: {.field {missing_cols}}.",
      "i" = "Seed a new file with {.code write_unit_corrections_template()}."
    ))
  }

  # FIELD COUNT, because readr will not tell you.
  #
  # A row with one extra comma is silently truncated to the header width: no
  # error, no warning, and nothing in readr::problems(). Every column after the
  # extra comma shifts one place, so `factor` lands in `row_ids`, `reason` in
  # `evidence`, and the last value falls off the end entirely. Found on the
  # first real correction ever written, 2026-08-06.
  #
  # The downstream validator did catch it, but only as "factor must be finite
  # and positive", which points at the wrong thing. A shifted row in THIS file
  # could scale the wrong measurements, so it is worth naming exactly.
  #
  # After the column check, not before: a file with the wrong header should be
  # told that, rather than that its field count is odd.
  expected <- length(names(empty_unit_corrections()))
  counts <- utils::count.fields(path, sep = ",", quote = "\"")
  wrong <- which(!is.na(counts) & counts != expected)
  if (length(wrong) > 0) {
    detail <- paste0(
      "line ", wrong, ": found ", counts[wrong], ", expected ", expected
    )
    cli::cli_abort(c(
      "{.file {path}} has the wrong number of fields.",
      stats::setNames(detail, rep("*", length(detail))),
      "i" = "Usually one comma too many or too few. Every column after it \\
             shifts, so the values land in the wrong fields."
    ))
  }

  raw <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE
  )

  if (nrow(raw) == 0) {
    return(empty_unit_corrections())
  }

  out <- raw |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(c("value_min", "value_max", "factor")),
      ~ suppressWarnings(as.numeric(.x))
    )) |>
    dplyr::mutate(dplyr::across(
      dplyr::where(is.character),
      ~ dplyr::na_if(trimws(.x), "")
    ))

  validate_unit_corrections(out, path = path)
  out
}

#' Static Checks on a Corrections Table
#'
#' Everything checkable without the data. Row-level agreement is checked by
#' [apply_unit_corrections()], which is the only place the rows are known.
#'
#' @param corrections A corrections tibble.
#' @param path Path, used in messages only.
#' @return `invisible(corrections)`.
#' @export
validate_unit_corrections <- function(corrections, path = "unit_corrections") {
  if (nrow(corrections) == 0) {
    return(invisible(corrections))
  }

  err <- function(...) cli::cli_abort(c("In {.file {path}}:", ...))

  if (anyNA(corrections$correction_id)) {
    err("*" = "every correction needs a {.field correction_id}.")
  }
  dupes <- unique(corrections$correction_id[duplicated(
    corrections$correction_id
  )])
  if (length(dupes) > 0) {
    err("*" = "duplicate {.field correction_id}: {.val {dupes}}.")
  }

  bad_factor <- corrections$correction_id[
    !is.finite(corrections$factor) | corrections$factor <= 0
  ]
  if (length(bad_factor) > 0) {
    err(
      "*" = "{.field factor} must be finite and positive: {.val {bad_factor}}.",
      "i" = "A 1000x overstatement is {.code factor = 0.001}."
    )
  }

  # A correction that documents nothing cannot be defended in the methods, and
  # this file's whole purpose is to be defensible.
  undocumented <- corrections$correction_id[
    is.na(corrections$reason) | is.na(corrections$evidence)
  ]
  if (length(undocumented) > 0) {
    err(
      "*" = "{.field reason} and {.field evidence} are required: \\
             {.val {undocumented}}."
    )
  }

  # No selector at all would match every row in the dataset.
  sel <- intersect(unit_correction_selector_cols(), names(corrections))
  none <- corrections$correction_id[
    rowSums(!is.na(corrections[sel])) == 0
  ]
  if (length(none) > 0) {
    err(
      "*" = "no selector set: {.val {none}}.",
      "i" = "Set at least one of {.field {sel}}."
    )
  }

  bad_range <- corrections$correction_id[
    !is.na(corrections$value_min) &
      !is.na(corrections$value_max) &
      corrections$value_min >= corrections$value_max
  ]
  if (length(bad_range) > 0) {
    err(
      "*" = "{.field value_min} must be below {.field value_max}: {.val {bad_range}}."
    )
  }

  invisible(corrections)
}

#' Rows Matched by One Correction
#'
#' Selector semantics: unset columns do not constrain, set columns combine with
#' AND. `comment_match` is a **fixed** substring match, not a regex, because the
#' comments carry parentheses and slashes that would otherwise need escaping in
#' a spreadsheet cell. Both sides are passed through [normalise_unit_string()]
#' first, so `ug/g` in the CSV matches a real micro sign in the data; write
#' `ug`, never `µg` (CLAUDE.md 4.4.-2).
#'
#' @param data A measurements table carrying `row_id`.
#' @param correction A one-row corrections tibble.
#' @param ids The `group_ids` ledger, or `NULL` to skip `group_id` matching.
#' @return A logical vector, one element per row of `data`.
#' @export
match_unit_correction <- function(data, correction, ids = NULL) {
  keep <- rep(TRUE, nrow(data))

  gid <- correction$group_id[1]
  if (!is.na(gid)) {
    if (is.null(ids)) {
      cli::cli_abort(
        "{.val {correction$correction_id[1]}} selects on {.field group_id} but \\
         no group ledger was supplied."
      )
    }
    if (!gid %in% ids$group_id) {
      cli::cli_abort(
        "{.val {correction$correction_id[1]}} names unknown group {.val {gid}}."
      )
    }
    key <- intersect(names(ids), names(data))
    key <- setdiff(key, "group_id")
    target <- ids[ids$group_id == gid, key, drop = FALSE]
    in_group <- rep(TRUE, nrow(data))
    for (cn in key) {
      in_group <- in_group &
        (as.character(data[[cn]]) == as.character(target[[cn]][1])) %in% TRUE
    }
    keep <- keep & in_group
  }

  camp <- correction$campaign_name_short[1]
  if (!is.na(camp) && "CAMPAIGN_NAME_SHORT" %in% names(data)) {
    keep <- keep & (as.character(data$CAMPAIGN_NAME_SHORT) == camp) %in% TRUE
  }

  cmt <- correction$comment_match[1]
  if (!is.na(cmt) && "MEASUREMENT_COMMENT" %in% names(data)) {
    # Both sides through normalise_unit_string() first, so a micro sign in the
    # comment matches a plain `ug` typed in the spreadsheet.
    #
    # This is not politeness, it is CLAUDE.md 4.4.-2 applied where it bites
    # hardest. The comment that identifies the Urban Fjord fault reads
    # "Verdier oppgitt i ug/g (w.w.) og multiplisert med 1000." with a real
    # micro sign, and this project has already lost 18 rows for months to a
    # micro sign that did not survive a round trip. Requiring one to be typed
    # into a hand-edited CSV, on Windows, in Excel, to select rows for
    # overwriting, would be inviting the same failure into the least
    # forgiving place in the pipeline.
    keep <- keep &
      grepl(
        normalise_unit_string(cmt),
        normalise_unit_string(data$MEASUREMENT_COMMENT),
        fixed = TRUE
      ) %in%
        TRUE
  }

  vmin <- correction$value_min[1]
  if (!is.na(vmin)) {
    keep <- keep & (data$MEASURED_VALUE_STANDARD >= vmin) %in% TRUE
  }
  vmax <- correction$value_max[1]
  if (!is.na(vmax)) {
    keep <- keep & (data$MEASURED_VALUE_STANDARD <= vmax) %in% TRUE
  }

  keep
}

#' Apply Unit Corrections to the Measurements Table
#'
#' Scales [unit_correction_value_cols()] by each correction's `factor`, and
#' records provenance in `unit_correction_id` and `unit_correction_factor`.
#'
#' Aborts, never warns, on:
#'
#' * a selector matching no rows (the correction is stale, and a stale
#'   correction that silently does nothing is the failure mode this whole layer
#'   was built to avoid);
#' * a selector whose matched rows differ from the recorded `row_ids`, in either
#'   direction, naming the drift both ways;
#' * a row matched by two corrections, since the order of compounding would then
#'   decide the answer.
#'
#' @param data A measurements table carrying `row_id`.
#' @param corrections A validated corrections tibble.
#' @param ids The `group_ids` ledger.
#' @param id_col Name of the row id column.
#' @return `data` with values scaled and two provenance columns added.
#' @export
apply_unit_corrections <- function(
  data,
  corrections,
  ids = NULL,
  id_col = "row_id"
) {
  data$unit_correction_id <- NA_character_
  data$unit_correction_factor <- NA_real_

  if (nrow(corrections) == 0) {
    return(data)
  }
  if (!id_col %in% names(data)) {
    cli::cli_abort(
      "{.arg data} has no {.field {id_col}}; corrections cannot be verified."
    )
  }

  cols <- intersect(unit_correction_value_cols(), names(data))

  # TWO PASSES, and the split is load-bearing.
  #
  # Every selector is resolved against the data exactly as it arrived, before
  # anything is scaled. Matching and scaling in one loop looks equivalent and is
  # not: `value_min` / `value_max` would then be tested against values an
  # earlier correction had already multiplied, so which rows a correction
  # matches would depend on the order of the rows in the CSV. Reordering the
  # spreadsheet would silently change the numbers, and the overlap check below
  # would miss collisions because the first correction had moved its rows out of
  # the second's range. Caught by the two-disjoint-corrections test.
  hits <- lapply(seq_len(nrow(corrections)), function(i) {
    match_unit_correction(data, corrections[i, ], ids = ids)
  })

  for (i in seq_len(nrow(corrections))) {
    this <- corrections[i, ]
    cid <- this$correction_id[1]
    hit <- hits[[i]]

    if (!any(hit)) {
      cli::cli_abort(c(
        "Correction {.val {cid}} matches no rows.",
        "i" = "Stale corrections are refused: either the data changed or the \\
               selector is wrong."
      ))
    }

    matched <- sort(as.character(data[[id_col]][hit]))
    recorded <- sort(split_row_ids(this$row_ids[1]))

    if (length(recorded) > 0 && !identical(matched, recorded)) {
      gained <- setdiff(matched, recorded)
      lost <- setdiff(recorded, matched)
      cli::cli_abort(c(
        "Correction {.val {cid}} no longer matches the rows it recorded.",
        "*" = "{length(matched)} matched, {length(recorded)} recorded.",
        if (length(gained) > 0) {
          c("*" = "newly matched: {.val {utils::head(gained, 5)}}")
        },
        if (length(lost) > 0) {
          c("*" = "no longer present: {.val {utils::head(lost, 5)}}")
        },
        "i" = "Review the change, then re-run \\
               {.file scripts/scaffold_unit_corrections.R} to re-record."
      ))
    }

    clash <- hit & !is.na(data$unit_correction_id)
    if (any(clash)) {
      cli::cli_abort(c(
        "Correction {.val {cid}} overlaps {.val {unique(data$unit_correction_id[clash])}}.",
        "*" = "{sum(clash)} row{?s} matched twice, e.g. \\
               {.val {utils::head(data[[id_col]][clash], 3)}}.",
        "i" = "Corrections must partition the rows they touch; compounding \\
               order would otherwise decide the result."
      ))
    }

    for (cn in cols) {
      data[[cn]][hit] <- data[[cn]][hit] * this$factor[1]
    }
    data$unit_correction_id[hit] <- cid
    data$unit_correction_factor[hit] <- this$factor[1]
  }

  data
}

#' Split a Semicolon-Separated Row Id Cell
#'
#' Semicolons rather than commas, matching `aep_node_groups.csv`, because a
#' comma-separated list inside a CSV cell needs quoting that spreadsheets
#' routinely mangle.
#'
#' @param x A single string, or `NA`.
#' @return A character vector, empty where `x` is `NA` or blank.
#' @export
split_row_ids <- function(x) {
  if (length(x) == 0 || is.na(x) || !nzchar(trimws(x))) {
    return(character(0))
  }
  out <- trimws(strsplit(x, ";", fixed = TRUE)[[1]])
  out[nzchar(out)]
}

#' Per-Group Summary of What Was Corrected
#'
#' The reportable companion, joined onto the group summary so a corrected group
#' says so wherever it is shown.
#'
#' Counts measurements as `sum(MEASURED_N)` and rows separately, per CLAUDE.md
#' 4.4.-1: a Vannmiljo row is one measurement, a literature row can be fifty.
#'
#' @param data Output of [apply_unit_corrections()].
#' @param ids The `group_ids` ledger, or `NULL`.
#' @return A tibble, one row per affected group, empty where none.
#' @export
report_unit_corrections <- function(data, ids = NULL) {
  if (!"unit_correction_id" %in% names(data)) {
    return(tibble::tibble())
  }
  hit <- data[!is.na(data$unit_correction_id), , drop = FALSE]
  if (nrow(hit) == 0) {
    return(tibble::tibble())
  }

  key <- intersect(triage_group_cols(), names(hit))
  out <- hit |>
    dplyr::group_by(dplyr::across(dplyr::all_of(
      c(key, "unit_correction_id", "unit_correction_factor")
    ))) |>
    dplyr::summarise(
      n_rows_corrected = dplyr::n(),
      n_corrected = sum(.data$MEASURED_N, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$n_rows_corrected))

  if (!is.null(ids)) {
    join_key <- intersect(names(ids), names(out))
    if (length(join_key) > 0) {
      out <- dplyr::left_join(out, ids, by = join_key)
    }
  }
  out
}

#' Warn About Applied Corrections
#'
#' Corrections are loud on every build by design. Overriding a national
#' monitoring database should never become invisible through familiarity.
#'
#' @param report Output of [report_unit_corrections()].
#' @return `invisible(NULL)`.
#' @export
report_unit_correction_status <- function(report) {
  if (nrow(report) > 0) {
    cli::cli_inform(c(
      "!" = "{sum(report$n_rows_corrected)} row{?s} had measured values \\
             overridden by {length(unique(report$unit_correction_id))} unit \\
             correction{?s}.",
      "i" = "Read {.code tar_read(unit_correction_report)}. Source values are \\
             preserved in {.field MEASURED_VALUE}."
    ))
  }
  invisible(NULL)
}

#' Write an Empty Corrections File
#'
#' Seeds the hand-edited CSV with headers only. Refuses to overwrite, because
#' the file is irreplaceable human judgement.
#'
#' @param path Destination.
#' @return The path, invisibly.
#' @export
write_unit_corrections_template <- function(path) {
  if (file.exists(path)) {
    cli::cli_abort("{.file {path}} exists; refusing to overwrite.")
  }
  readr::write_csv(empty_unit_corrections(), path)
  invisible(path)
}
