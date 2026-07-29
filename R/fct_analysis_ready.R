# Analysis-ready filtering (PLAN.md P1.0).
#
# One deliberate, documented data-hygiene step between load_literature_pqt and
# the downstream analysis, so that filtering decisions live in the pipeline
# rather than being patched inline in individual notebooks.

#' Default Grouping Columns for Drop Reporting
#'
#' Matches the grouping used by the `summarise_literature_data` target, so the
#' drop report can be joined straight onto the group summary table.
#'
#' @return A character vector of column names.
analysis_group_cols <- function() {
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

#' Drop Non-Positive and Missing Measured Values
#'
#' Removes rows whose measured value is `NA`, zero, or negative. This is the
#' only data-hygiene step applied before analysis; nothing more exotic (no
#' censored-data modelling, no LOD reconstruction) is attempted.
#'
#' Rationale for each case:
#'
#' * `NA` — no measurement to analyse.
#' * `0` — a true zero copper concentration is implausible in any environmental
#'   matrix. A stored zero is a "not detected" entry that lost its censoring
#'   flag somewhere upstream. Confirmed for 24 Aquatic Sediment rows from
#'   `Vm_2010_2025` campaigns, which carry no LOD/LOQ and no censoring flag.
#' * negative — physically impossible; usually a blank-corrected value that has
#'   gone below zero.
#'
#' Zero and negative values also break the log10 scales used throughout the
#' distribution plots, so removing them here avoids silent `-Inf` and `NaN`
#' propagation later.
#'
#' **Scope:** this filters on `value_col` only. It is deliberately *not* a
#' whole-row `tidyr::drop_na()`, which would gut the dataset, because many
#' eData columns (tissue, species, uncertainty bounds, LOD/LOQ) are legitimately
#' sparse.
#'
#' Rows dropped here are counted by [report_dropped_measurements()] rather than
#' vanishing silently.
#'
#' @param data A data frame with a measured value column.
#' @param value_col Name (string) of the measured value column to filter on.
#' @return `data` with offending rows removed.
#' @export
drop_nonpositive_measurements <- function(
  data,
  value_col = "MEASURED_VALUE_STANDARD"
) {
  x <- data[[value_col]]
  keep <- !is.na(x) & x > 0
  data[keep, , drop = FALSE]
}

#' Report What drop_nonpositive_measurements() Removes, Per Group
#'
#' Companion to [drop_nonpositive_measurements()]. Counting the losses rather
#' than dropping silently is what makes the filter defensible in the methods
#' section, and it flags any group that is mostly non-detects before that group
#' is used to build an AEP node.
#'
#' @param data A data frame, pre-filtering (i.e. `load_literature_pqt`).
#' @param value_col Name (string) of the measured value column to filter on.
#' @param group_cols Character vector of grouping columns. Defaults to
#'   [analysis_group_cols()].
#' @return A tibble with one row per group: `n_input`, `n_na`, `n_zero`,
#'   `n_negative`, `n_dropped`, `n_retained`, `prop_dropped`. Sorted by
#'   `n_dropped` descending, so the worst offenders are at the top.
#' @export
report_dropped_measurements <- function(
  data,
  value_col = "MEASURED_VALUE_STANDARD",
  group_cols = analysis_group_cols()
) {
  group_cols <- intersect(group_cols, names(data))

  data |>
    dplyr::mutate(.value = .data[[value_col]]) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      n_input = dplyr::n(),
      n_na = sum(is.na(.data$.value)),
      n_zero = sum(.data$.value == 0, na.rm = TRUE),
      n_negative = sum(.data$.value < 0, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      n_dropped = .data$n_na + .data$n_zero + .data$n_negative,
      n_retained = .data$n_input - .data$n_dropped,
      prop_dropped = .data$n_dropped / .data$n_input
    ) |>
    dplyr::arrange(dplyr::desc(.data$n_dropped))
}
