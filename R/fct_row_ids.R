# Stable per-measurement identifiers (2026-08-06).
#
# An administrative key, not a scientific one, and named accordingly: `row_id`
# is lower case because SCREAMING_SNAKE in this project means "column of the
# eData schema". This is ours, it is not in the schema, and the casing is the
# only cue that says so. The same applies to anything else this layer adds.
#
# It exists so that a correction, an exclusion, or any other per-row judgement
# can name exactly the measurements it touches, in a hand-edited CSV, and have
# that reference survive a rebuild.
#
# WHY NOT A SEQUENTIAL COUNTER.
#
# The obvious scheme is R00001, R00002, ... over the loaded table. It is
# readable and it sorts, but it is *positional*: it encodes where a row sat, not
# what it is. Vannmiljo is re-exported periodically and the eData files are
# edited whenever an extraction error is found, so rows are inserted and removed
# routinely. One insertion shifts every subsequent id by one, and a corrections
# file keyed on R01234 then silently points at a different measurement. No
# error, no diff, and the wrong value gets overwritten.
#
# That is the same silent-success failure as the missing `imports = "STOPAEP"`
# and the untracked group_decisions.csv, and it would be a poor one to introduce
# into the one file whose job is overriding measured values.
#
# SAMPLE_ID is already content-derived: generate_sample_id_with_components() in
# R/fct_vm_eData.R builds it from site, parameter, compartment, date and
# subsample. Insert a row and every other id is unchanged. It also sorts by site
# then parameter then date, so it groups like with like as a property of what it
# is made of rather than by accident of ingestion order.
#
# So this file promotes what exists rather than inventing a scheme.

#' Columns Used to Break `SAMPLE_ID` Ties
#'
#' Ordered, and appended cumulatively: only as many as are needed to make the
#' id unique are used. Both are content-bearing, so a disambiguated id is still
#' a pure function of the measurement rather than of its position.
#'
#' Measured on the 90,221 loaded rows (2026-08-06): `SAMPLE_ID` alone is unique
#' for all 89,631 Vannmiljo rows and collides on 18 of the 590 literature rows.
#' `SUBSAMPLE` resolves every one of those, `MEASURED_TYPE` is carried as a
#' second line of defence.
#'
#' @return A character vector of column names.
#' @export
row_id_disambiguators <- function() {
  c("SUBSAMPLE", "MEASURED_TYPE")
}

#' Add a Stable Row Identifier
#'
#' `row_id` is `SAMPLE_ID`, with [row_id_disambiguators()] appended only for
#' those rows whose `SAMPLE_ID` is shared with another row.
#'
#' **The one wrinkle, stated plainly.** Because disambiguation is conditional, a
#' row's id can change if a *new* row later arrives sharing its `SAMPLE_ID`:
#' both then gain a suffix. Appending unconditionally would remove that, at the
#' cost of a redundant suffix on all 90,000 rows to defend against a case that
#' currently affects 18. The conditional form is chosen because the failure is
#' **loud rather than silent**: anything storing a `row_id` is expected to store
#' the selector that found it too, and the two disagreeing is an error the
#' pipeline raises. A positional scheme has no equivalent check, which is the
#' whole reason it was rejected.
#'
#' Errors rather than degrading in three cases, all of which mean a stored
#' `row_id` could point somewhere unintended:
#'
#' * `id_col` absent.
#' * Any `NA` in `id_col`, since there is no basis to build from.
#' * Ids still colliding after every disambiguator is applied. Suffixing those
#'   with a counter would reintroduce positional ids by the back door, so the
#'   duplicates are reported and the build stops.
#'
#' @param data A measurements table.
#' @param id_col Name of the column to build from.
#' @param disambiguate_by Columns tried, in order, to break ties.
#' @param col Name of the column to add.
#' @return `data` with `col` added as its first column.
#' @export
add_row_ids <- function(
  data,
  id_col = "SAMPLE_ID",
  disambiguate_by = row_id_disambiguators(),
  col = "row_id"
) {
  if (!id_col %in% names(data)) {
    cli::cli_abort(
      "{.arg data} has no {.field {id_col}} column, so {.field {col}} cannot be built."
    )
  }

  base <- as.character(data[[id_col]])

  n_missing <- sum(is.na(base))
  if (n_missing > 0) {
    cli::cli_abort(c(
      "{n_missing} row{?s} ha{?s/ve} a missing {.field {id_col}}.",
      "i" = "Every row needs one: it is the basis of {.field {col}}."
    ))
  }

  ids <- base
  available <- intersect(disambiguate_by, names(data))

  # Cumulative, and recomputed each pass, so a column is only used where the
  # previous pass left a genuine tie. Rows that were already unique keep the
  # bare SAMPLE_ID.
  for (i in seq_along(available)) {
    dup <- ids %in% unique(ids[duplicated(ids)])
    if (!any(dup)) {
      break
    }
    suffix <- do.call(
      paste,
      c(
        lapply(available[seq_len(i)], function(cn) {
          as.character(data[[cn]][dup])
        }),
        list(sep = "|")
      )
    )
    ids[dup] <- paste(base[dup], suffix, sep = "|")
  }

  still <- unique(ids[duplicated(ids)])
  if (length(still) > 0) {
    cli::cli_abort(c(
      "{length(still)} {.field {id_col}} value{?s} remain{?s/} ambiguous after \\
       disambiguating by {.field {available}}.",
      "*" = "{.val {utils::head(still, 5)}}",
      "i" = "Fix the source extraction. {.field {col}} is deliberately not \\
             made unique with a positional counter."
    ))
  }

  data[[col]] <- ids
  dplyr::relocate(data, dplyr::all_of(col))
}

#' Report Rows Whose `SAMPLE_ID` Needed Disambiguating
#'
#' A visible companion to [add_row_ids()], in the same spirit as
#' `literature_dropped_report` and `unit_anomaly_report`: the pipeline copes,
#' but it says so rather than absorbing the problem quietly.
#'
#' A non-empty result is a data-entry defect in the source extraction, not a
#' property of the data. Every collision found on 2026-08-06 was a `SAMPLE_ID`
#' written before `SUBSAMPLE` was refined, or one that never carried the
#' subsample at all.
#'
#' @param data Output of [add_row_ids()].
#' @param id_col Name of the column `row_id` was built from.
#' @param col Name of the row id column.
#' @return A tibble, one row per affected measurement, empty where none.
#' @export
report_row_id_collisions <- function(
  data,
  id_col = "SAMPLE_ID",
  col = "row_id"
) {
  if (!all(c(id_col, col) %in% names(data))) {
    return(empty_row_id_collisions())
  }

  hit <- data[data[[col]] != data[[id_col]], , drop = FALSE]
  if (nrow(hit) == 0) {
    return(empty_row_id_collisions())
  }

  # Explicit rather than `%||%`: an absent column yields NULL, and a NULL here
  # would silently become a zero-length column and misalign the bind_cols below.
  src <- if ("source_file_measurements" %in% names(hit)) {
    basename(as.character(hit[["source_file_measurements"]]))
  } else {
    rep(NA_character_, nrow(hit))
  }

  tibble::tibble(
    row_id = hit[[col]],
    sample_id = hit[[id_col]],
    source_file = src
  ) |>
    dplyr::bind_cols(
      hit |>
        dplyr::select(dplyr::any_of(c(
          "REFERENCE_ID", "SUBSAMPLE", "SAMPLE_SPECIES", "SAMPLE_TISSUE",
          "MEASURED_VALUE", "MEASURED_UNIT"
        )))
    ) |>
    dplyr::add_count(.data$sample_id, name = "n_sharing") |>
    dplyr::arrange(dplyr::desc(.data$n_sharing), .data$sample_id)
}

#' Zero-Row Collision Table
#' @return A zero-row tibble.
#' @export
empty_row_id_collisions <- function() {
  tibble::tibble(
    row_id = character(0),
    sample_id = character(0),
    source_file = character(0),
    n_sharing = integer(0)
  )
}

#' Warn About Row Id Collisions
#'
#' Called from the pipeline so the count reaches the build log.
#'
#' @param collisions Output of [report_row_id_collisions()].
#' @return `invisible(NULL)`.
#' @export
report_row_id_status <- function(collisions) {
  if (nrow(collisions) > 0) {
    cli::cli_warn(c(
      "{nrow(collisions)} row{?s} shared a {.field SAMPLE_ID} with another and \\
       {?was/were} disambiguated.",
      "i" = "This is a defect in the source extraction. Read \\
             {.code tar_read(row_id_collisions)}."
    ))
  }
  invisible(NULL)
}
