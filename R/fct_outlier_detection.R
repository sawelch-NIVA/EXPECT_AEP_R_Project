#' Flag Outliers via Tukey Fences and Robust Modified Z-Score
#'
#' Both criteria are computed on the **log10 scale**: Tukey fences (IQR x 1.5)
#' and the Robust Modified Z-Score (threshold 3.5). Below `min_n`, these
#' statistics are unreliable, so flag columns are returned as `NA` (and
#' `dot_fill` as `"not tested"`) rather than computed.
#'
#' **The RMZ moved from the raw scale to log10 on 2026-08-05**, and the reason
#' is worth keeping. Sam's observation was that outliers were "only highlighted
#' on the right", read as a missing `abs()`. The `abs()` was always there and the
#' Tukey fences were always two-sided; the asymmetry came from the *scale*. MAD
#' on raw lognormal data is set by the bulk near the median, so a value ten times
#' **below** the median sits well inside 3.5 MADs while a value ten times above
#' blows straight through. The criterion was in practice an upper-tail test.
#'
#' Measured over the 74 groups with at least 10 rows, double-flagged rows split
#' low/high as:
#'
#' | Scheme | low | high | % of rows |
#' |---|---|---|---|
#' | IQR log10, RMZ raw (old) | 5 | 2,525 | 2.8 |
#' | both raw | 5 | 8,876 | 9.9 |
#' | both log10 (this) | 359 | 1,637 | 2.2 |
#'
#' Moving the *fences* to raw instead was considered and rejected: it triples
#' right-tail flagging and leaves the left tail at the same 5 rows. Copper
#' concentrations here span roughly 12 orders of magnitude and are approximately
#' lognormal, so log space is where a symmetric fence means anything.
#'
#' `mad()` returns 0 where more than half the values are identical, which makes
#' every RMZ `Inf` or `NaN`. Guarded rather than left to propagate: `RMZ` comes
#' back all-`NA` and the criterion abstains, leaving the Tukey fences to decide.
#' Downstream code already treats an `NA` flag as "not an outlier"
#' (`add_triage_flags()` sums with `na.rm = TRUE`), which is the conservative
#' direction.
#'
#' @param x A numeric vector of measured values. Must be positive: non-positive
#'   values are `NaN` under `log10()` and are dropped upstream by
#'   [drop_nonpositive_measurements()].
#' @param min_n Minimum sample size required to compute flags.
#' @return A tibble with one row per element of `x`: `RMZ` (on the log10 scale),
#'   `outlier_RMZ`, `outlier_IQR`, and `dot_fill` (factor: "neither", "IQR",
#'   "RMZ", "both", "not tested").
flag_outliers <- function(x, min_n = 10) {
  dot_fill_levels <- c("neither", "IQR", "RMZ", "both", "not tested")
  n <- length(x)

  if (n < min_n) {
    return(tibble::tibble(
      RMZ = rep(NA_real_, n),
      outlier_RMZ = rep(NA, n),
      outlier_IQR = rep(NA, n),
      dot_fill = factor(rep("not tested", n), levels = dot_fill_levels)
    ))
  }

  log_val <- log10(x)
  Q1 <- stats::quantile(log_val, 0.25, na.rm = TRUE)
  Q3 <- stats::quantile(log_val, 0.75, na.rm = TRUE)
  fence <- stats::IQR(log_val, na.rm = TRUE) * 1.5
  # MAD of zero (more than half the values identical) makes every score Inf or
  # NaN, and abs(Inf) > 3.5 is TRUE, so the criterion would flag every value that
  # is not exactly the median. More likely on the log10 scale than it was on the
  # raw one, since rounding to a reporting precision collapses more ties. Abstain
  # instead and let the Tukey fences decide alone.
  RMZ <- if (isTRUE(stats::mad(log_val, na.rm = TRUE) > 0)) {
    robust_modified_z_score(log_val)
  } else {
    rep(NA_real_, n)
  }
  outlier_RMZ <- abs(RMZ) > 3.5
  outlier_IQR <- log_val < (Q1 - fence) | log_val > (Q3 + fence)

  tibble::tibble(
    RMZ = RMZ,
    outlier_RMZ = outlier_RMZ,
    outlier_IQR = outlier_IQR,
    dot_fill = factor(
      dplyr::case_when(
        outlier_RMZ & outlier_IQR ~ "both",
        outlier_RMZ ~ "RMZ",
        outlier_IQR ~ "IQR",
        .default = "neither"
      ),
      levels = dot_fill_levels
    )
  )
}

#' Hartigan's Dip Test for Unimodality, Gated by Minimum Sample Size
#'
#' Tests the null hypothesis that `x` is drawn from a unimodal distribution.
#' A significant result means unimodality is *rejected*; it does not identify
#' how many modes there are, and in particular it is not a test for
#' bimodality specifically. Hence `multimodal` rather than `bimodal`.
#'
#' Below `min_n`, the test is not run (it is unreliable, and errors outright
#' below n = 4). TODO: Add citation
#'
#' @param x A numeric vector of measured values.
#' @param min_n Minimum sample size required to run the test.
#' @return A list with `dip_p` (p-value, or `NA`) and `multimodal` (logical:
#'   `TRUE` where unimodality is rejected at `dip_p < 0.05`, or `NA` where the
#'   test was not run).
dip_test_safe <- function(x, min_n = 10) {
  if (length(x) < min_n) {
    return(list(dip_p = NA_real_, multimodal = NA))
  }
  result <- diptest::dip.test(x)
  list(dip_p = result$p.value, multimodal = result$p.value < 0.05)
}

#' Restrict Data to its Dominant Measured Unit
#'
#' Different measurement units (e.g. wet vs. dry weight) are not directly
#' comparable, so we restrict each group to whichever unit has the most
#' records and drop the rest, rather than silently pooling them.
#'
#' @param data A data frame with a `MEASURED_UNIT_STANDARD` column.
#' @return A filtered data frame.
restrict_to_dominant_unit <- function(data) {
  if (nrow(data) == 0) {
    return(data)
  }
  dominant_unit <- data |>
    dplyr::count(.data$MEASURED_UNIT_STANDARD, sort = TRUE) |>
    dplyr::slice(1) |>
    dplyr::pull(.data$MEASURED_UNIT_STANDARD)
  dplyr::filter(data, .data$MEASURED_UNIT_STANDARD == dominant_unit)
}

#' Run the Full Outlier Analysis for One Group
#'
#' Restricts `data` to its dominant measured unit, then computes outlier
#' flags, a dip test, and Winsorized value columns (all gated by `min_n`),
#' plus descriptive summary statistics (always computed, regardless of n).
#'
#' Intended to be called once per branch of a `tarchetypes::tar_map()`
#' factory, with `data` already pre-filtered to the group of interest (see
#' [prepare_compartment_group_data()] / [prepare_biota_group_data()]).
#'
#' @param data A pre-filtered data frame for a single group (compartment,
#'   species/tissue, etc.), with a `MEASURED_VALUE_STANDARD` column.
#' @param group_label A human-readable label for the group, used in plot
#'   titles etc.
#' @param min_n Minimum sample size required to compute outlier flags, dip
#'   test, and Winsorization bounds; below this, a distribution can still be
#'   plotted, but the statistics are not trustworthy enough to compute.
#' @return A list: `group_label`, `data` (row-level data with flag and
#'   Winsorized-value columns added), `n`, `n_dropped_unit` (rows excluded
#'   for using a non-dominant unit), `min_n`, `stats_computed` (logical),
#'   `dip_p`, `multimodal`, and `summary` (one-row tibble of descriptive stats).
#' @export
outlier_group_analysis <- function(
  data,
  group_label,
  min_n = 10
) {
  n_before_unit <- nrow(data)
  data <- restrict_to_dominant_unit(data)
  n <- nrow(data)
  x <- data$MEASURED_VALUE_STANDARD

  flags <- flag_outliers(x, min_n = min_n)
  dip <- dip_test_safe(x, min_n = min_n)
  stats_computed <- n >= min_n

  data_flagged <- dplyr::bind_cols(data, flags)

  if (stats_computed) {
    p01 <- stats::quantile(x, 0.01, na.rm = TRUE)
    p99 <- stats::quantile(x, 0.99, na.rm = TRUE)
    p05 <- stats::quantile(x, 0.05, na.rm = TRUE)
    p95 <- stats::quantile(x, 0.95, na.rm = TRUE)
    data_flagged <- data_flagged |>
      dplyr::mutate(
        value_winsor_98 = pmin(pmax(MEASURED_VALUE_STANDARD, p01), p99),
        value_winsor_90 = pmin(pmax(MEASURED_VALUE_STANDARD, p05), p95)
      )
  } else {
    data_flagged <- data_flagged |>
      dplyr::mutate(
        value_winsor_98 = MEASURED_VALUE_STANDARD,
        value_winsor_90 = MEASURED_VALUE_STANDARD
      )
  }

  summary_stats <- data_flagged |>
    dplyr::reframe(
      n = dplyr::n(),
      mean = mean(MEASURED_VALUE_STANDARD, na.rm = TRUE),
      median = stats::median(MEASURED_VALUE_STANDARD, na.rm = TRUE),
      sd = stats::sd(MEASURED_VALUE_STANDARD, na.rm = TRUE),
      unit = unique(MEASURED_UNIT_STANDARD)[1],
      date_min = suppressWarnings(min(SAMPLING_DATE, na.rm = TRUE)),
      date_max = suppressWarnings(max(SAMPLING_DATE, na.rm = TRUE)),
      n_campaigns = dplyr::n_distinct(CAMPAIGN_NAME_SHORT),
      n_references = dplyr::n_distinct(REFERENCE_ID)
    )

  list(
    group_label = group_label,
    data = data_flagged,
    n = n,
    n_dropped_unit = n_before_unit - n,
    min_n = min_n,
    stats_computed = stats_computed,
    dip_p = dip$dip_p,
    multimodal = dip$multimodal,
    summary = summary_stats
  )
}
