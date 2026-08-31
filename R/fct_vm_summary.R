# Descriptive summary of the Vannmiljø contribution to the joined dataset, for
# the manuscript's Materials & Methods > Vannmiljø section (index.qmd).
#
# Nothing downstream reads these; the "descriptive one-off" contract of
# scripts/summarise_ssb_employment.R applies, except these ARE targets because
# they read pipeline data and index.qmd is rendered through tar_quarto() (so a
# tar_read() in the notebook is the dependency edge -- CLAUDE.md 4.4.2).
#
# All functions take already-built data or plain counts, never the target
# store, so tests/testthat/test-fct_vm_summary.R exercises them on small
# synthetic frames.

#' Classify an eData Row's Matrix as Water / Sediment / Biota
#'
#' Drives the `matrix` column and row ordering in [summarise_vm_dataset()].
#' Sediment is split out of the Aquatic compartment by sub-compartment;
#' anything neither biota nor sediment is Water.
#'
#' @param compartment `ENVIRON_COMPARTMENT`.
#' @param subcompartment `ENVIRON_COMPARTMENT_SUB`.
#' @return Character vector, each element one of `"Water"`, `"Sediment"`,
#'   `"Biota"`.
#' @export
vm_matrix_class <- function(compartment, subcompartment) {
  dplyr::case_when(
    compartment == "Biota" ~ "Biota",
    subcompartment == "Aquatic Sediment" ~ "Sediment",
    TRUE ~ "Water"
  )
}

#' Summarise the Vannmiljø Dataset for the Manuscript
#'
#' One compact scale table plus a compartment x sub-compartment composition
#' table, both for direct rendering in index.qmd. Counts of measurements are
#' `sum(MEASURED_N)` (CLAUDE.md 4.4.-1); the row count is reported alongside and
#' labelled as rows.
#'
#' @param data The joined hub table (`load_literature_pqt` target), or any
#'   subset of it. Must carry `DATA_SOURCE`, `MEASURED_N`, `MEASURED_FLAG`,
#'   `SITE_CODE`, `CAMPAIGN_NAME`, `SAMPLING_DATE`, `ENVIRON_COMPARTMENT`,
#'   `ENVIRON_COMPARTMENT_SUB`, `SAMPLE_SPECIES`.
#' @param source_value The `DATA_SOURCE` value identifying Vannmiljø rows.
#'   Defaults to `"Vannmiljø"`.
#' @return A list of three tibbles:
#'   * `scale` -- `metric` / `value`, `value` pre-formatted for
#'     `knitr::kable()`.
#'   * `composition` -- one row per compartment x sub-compartment, with
#'     `matrix`, `measurements`, `rows`, `sites`, `n_species` (`NA` off biota),
#'     sorted by matrix then measurements.
#'   * `totals` -- one row: `measurements`, `rows`, `sites` (a distinct count,
#'     which is why it is not the column sum of `composition$sites`).
#' @export
summarise_vm_dataset <- function(data, source_value = "Vannmiljø") {
  vm <- dplyr::filter(data, .data$DATA_SOURCE == source_value)

  n_rows <- nrow(vm)
  n_meas <- sum(vm$MEASURED_N, na.rm = TRUE)
  # Weighted like everything else divided by n_meas (CLAUDE.md 4.4.-1). For
  # Vannmiljø every row is MEASURED_N == 1 so this equals the row count; a
  # literature slice makes the two diverge.
  is_cens <- vm$MEASURED_FLAG %in% c("< LOD", "< LOQ")
  n_cens <- sum(vm$MEASURED_N[is_cens], na.rm = TRUE)
  dates <- suppressWarnings(range(as.Date(vm$SAMPLING_DATE), na.rm = TRUE))

  big <- function(x) formatC(x, format = "d", big.mark = ",")
  pct <- function(num, den) if (den > 0) sprintf(" (%.1f%%)", 100 * num / den) else ""

  scale <- tibble::tibble(
    metric = c(
      "Monitoring campaigns",
      "Sampling sites",
      "Sampling period",
      "Measurements",
      "Rows",
      "Censored (< LOD / < LOQ)"
    ),
    value = c(
      big(dplyr::n_distinct(vm$CAMPAIGN_NAME)),
      big(dplyr::n_distinct(vm$SITE_CODE)),
      if (all(is.finite(dates))) {
        paste(format(dates, "%Y-%m-%d"), collapse = " to ")
      } else {
        NA_character_
      },
      big(n_meas),
      big(n_rows),
      paste0(big(n_cens), pct(n_cens, n_meas))
    )
  )

  composition <- vm |>
    dplyr::mutate(
      matrix = vm_matrix_class(
        .data$ENVIRON_COMPARTMENT, .data$ENVIRON_COMPARTMENT_SUB
      )
    ) |>
    dplyr::group_by(
      .data$matrix, .data$ENVIRON_COMPARTMENT, .data$ENVIRON_COMPARTMENT_SUB
    ) |>
    dplyr::summarise(
      measurements = sum(.data$MEASURED_N, na.rm = TRUE),
      rows = dplyr::n(),
      sites = dplyr::n_distinct(.data$SITE_CODE),
      n_species = dplyr::n_distinct(
        .data$SAMPLE_SPECIES[!is.na(.data$SAMPLE_SPECIES)]
      ),
      .groups = "drop"
    ) |>
    dplyr::rename(
      compartment = "ENVIRON_COMPARTMENT",
      subcompartment = "ENVIRON_COMPARTMENT_SUB"
    ) |>
    dplyr::mutate(
      matrix = factor(.data$matrix, levels = c("Water", "Sediment", "Biota")),
      n_species = dplyr::if_else(
        .data$compartment == "Biota", as.integer(.data$n_species), NA_integer_
      )
    ) |>
    dplyr::arrange(.data$matrix, dplyr::desc(.data$measurements))

  totals <- tibble::tibble(
    measurements = n_meas,
    rows = n_rows,
    sites = dplyr::n_distinct(vm$SITE_CODE)
  )

  list(scale = scale, composition = composition, totals = totals)
}

#' Build the Vannmiljø Cleaning-Funnel Table
#'
#' Turns the row counts captured at each pipeline filter step into a
#' step / rows / removed table for the SI. Kept separate from
#' [summarise_vm_dataset()] because it is fed the intermediate `vm_*` targets,
#' not the joined data.
#'
#' @param counts Named integer vector of row counts in pipeline order. First
#'   element is the raw export; each later element is the count *after* that
#'   step.
#' @param labels Named character vector remapping `names(counts)` to readable
#'   step descriptions. Names absent here fall back to the raw name.
#' @return A tibble: `step`, `rows`, `removed` (`NA` for the first row).
#' @export
vm_cleaning_funnel <- function(counts,
                               labels = c(
                                 raw = "Raw export",
                                 compartments = "Non-aquatic compartments removed",
                                 sites = "Svalbard / polygon-geometry sites removed",
                                 dates = "Outside 2010-2025 removed",
                                 compartment_conflicts = "Unresolved compartment conflicts removed",
                                 geographic_conflicts = "Unresolved geographic conflicts removed",
                                 analysis = "Analysis dataset"
                               )) {
  step_lab <- dplyr::coalesce(unname(labels[names(counts)]), names(counts))
  tibble::tibble(
    step = step_lab,
    rows = as.integer(unname(counts)),
    removed = as.integer(c(NA, diff(unname(counts))))
  )
}
