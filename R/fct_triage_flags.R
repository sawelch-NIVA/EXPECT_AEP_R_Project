# Triage ranking and flags (PLAN.md P1.4 / P1.5, 2026-07-30).
#
# Deliberately dumb, per the plan: rank on n descending and put the flags
# alongside for the eye. There is no composite score, and there should not be
# one. Weighting five heterogeneous warning signs into a single number would
# invent a precision the inputs do not support, and would hide which sign fired.

#' Flag Thresholds
#'
#' One place to tune what counts as worth noticing.
#'
#' **There are exactly two flags, and adding a third is Sam's call, not a
#' judgement to be made here.** Both predate this file: the 5% outlier fraction
#' and the dip test were already driving the summary table's highlighting. Two
#' further flags (drop proportion, multiple units) were added on 2026-07-30 and
#' removed the same day, because PLAN.md P1.4 asked for those as *columns* and
#' turning a column into a warning is a statistical judgement that has to be
#' justifiable later. A single-source flag was removed for the same reason plus a
#' 96% base rate. All three remain available as columns.
#'
#' @return A named list of numeric cutoffs.
#' @export
triage_flag_limits <- function() {
  list(
    outlier_fraction = 0.05
  )
}

#' Add Triage Ranking Columns and Flags to the Group Summary
#'
#' Derives everything the triage sheet ranks or warns on, so the summary target
#' stays a plain set of per-group statistics and the interpretation lives here
#' where it can be tested.
#'
#' `n_units` is the count of unit variants sharing a group's key **ignoring the
#' unit**. Within a group the unit is constant by construction (it is part of the
#' key), so a per-group count would always be 1; what matters for a grouping
#' decision is whether the same species, tissue and place also appears in another
#' basis of measurement.
#'
#' @param summary_data The per-group statistics, i.e. the `reframe()` output in
#'   the `summarise_literature_data` target.
#' @param dropped_report The `literature_dropped_report` target, or `NULL` to
#'   skip the drop columns.
#' @return `summary_data` with `cv`, `n_units`, `outlier_fraction`, the drop
#'   columns, and `flag_*` logicals, sorted by `n` descending.
#' @export
add_triage_flags <- function(summary_data, dropped_report = NULL) {
  limits <- triage_flag_limits()
  heading_cols <- intersect(triage_heading_cols(), names(summary_data))

  out <- summary_data |>
    dplyr::mutate(
      # NO coefficient of variation. It was added on 2026-07-30 and removed the
      # same day. Measured on this data, CV correlated 0.96 (Spearman) with
      # max/median across the 51 groups with n >= 20: it tracks the single largest
      # value rather than the spread of the group. Dropping one row of 4969 from
      # Marine/Salt Water moved CV from 40.7 to 4.7, while a log-scale spread
      # measure moved from 2.9 to 2.8. It was also redundant, being exactly
      # sd / mean where both are already reported.
      #
      # A log-scale replacement (geometric SD, interquartile ratio) was offered
      # and declined: a spread statistic nobody can justify later is worse than
      # no spread statistic. sd and mean remain reported as they always were.
      outlier_fraction = .data$n_double_outliers / .data$n
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(heading_cols))) |>
    dplyr::mutate(n_units = dplyr::n()) |>
    dplyr::ungroup()

  if (!is.null(dropped_report)) {
    join_cols <- intersect(analysis_group_cols(), names(summary_data))
    out <- out |>
      dplyr::left_join(
        dplyr::select(
          dropped_report,
          dplyr::all_of(join_cols),
          "n_dropped",
          "prop_dropped"
        ),
        by = join_cols
      )
  } else {
    out$n_dropped <- NA_integer_
    out$prop_dropped <- NA_real_
  }

  out |>
    dplyr::mutate(
      # `%in% TRUE` throughout, so a group whose test did not run (n below the
      # dip test's minimum, or a group absent from the drop report) reads as
      # unflagged rather than leaking NA into the highlight sets.
      #
      # NO single-source flag. It was listed in PLAN.md P1.4, but it fired on 234
      # of 245 groups (96%): Vannmiljo is one REFERENCE_ID covering environmental
      # monitoring for the whole of Norway, so a single source is the normal state
      # of this dataset, not an exception. Flagging the baseline made 236 of 245
      # groups flagged and buried the four signals that do discriminate. Removed
      # 2026-07-30 on Sam's call. `n_sources` is still reported per group.
      flag_outliers = (.data$outlier_fraction > limits$outlier_fraction) %in%
        TRUE,
      flag_multimodal = .data$multimodal %in% TRUE
      # n_units and prop_dropped are columns, not flags. See
      # triage_flag_limits().
    ) |>
    dplyr::arrange(dplyr::desc(.data$n))
}

#' Human-Readable Flag Text for One Group
#'
#' The same flags the summary table highlights, as prose. Used by the triage
#' notebook so that a reader looking at one group's panels sees the warnings
#' without having to find that group's row back in the table.
#'
#' Shared with the table on purpose: the highlighting and this text must not be
#' able to disagree about what is flagged.
#'
#' @param row A one-row data frame from [add_triage_flags()].
#' @return A character vector of flag phrases, empty where nothing is flagged.
#' @export
group_flag_text <- function(row) {
  # A small non-zero proportion must not round to a bare "0%", which reads as
  # "none" when it means "a few".
  pct <- function(x) {
    if (!is.finite(x)) {
      return("?")
    }
    if (x > 0 && x < 0.005) "<1%" else paste0(round(100 * x), "%")
  }
  flags <- character(0)

  if (isTRUE(row$flag_multimodal[1])) {
    # The dip test returns p values that underflow to 0 on the large groups.
    # Printing a bare "0" claims more than the test can support.
    p <- row$dip_p[1]
    p_text <- if (is.na(p)) {
      "not run"
    } else if (p < 0.001) {
      "p < 0.001"
    } else {
      paste0("p = ", formatC(p, digits = 3, format = "g"))
    }
    flags <- c(flags, paste0("**multimodal** (dip test ", p_text, ")"))
  }
  if (isTRUE(row$flag_outliers[1])) {
    flags <- c(
      flags,
      paste0("**", pct(row$outlier_fraction[1]), " outliers**")
    )
  }
  flags
}

#' One-Line Summary Sentence for a Triage Group
#'
#' Everything that goes under a group heading in the triage notebook: the counts,
#' the unit, and any flags. Built here rather than in the notebook so the wording
#' is testable and cannot drift from the table's highlighting.
#'
#' Reports counts, not statistics. Any summary statistic added here has to be one
#' Sam can defend in the methods section, which is why the CV that briefly lived
#' here is gone.
#'
#' @param row A one-row data frame carrying the columns from
#'   [add_triage_flags()] plus `n_rows`.
#' @return A single markdown string.
#' @export
group_summary_line <- function(row) {
  # Common name first, as an aide-memoire: the heading above carries the
  # scientific name, which says nothing about what the organism is or what
  # concentration would be plausible in it. Absent for non-biota groups and for
  # the species with no English vernacular (mostly copepods and amphipods).
  preamble <- if (
    !is.null(row$species_common_name) && !is.na(row$species_common_name[1])
  ) {
    paste0("**", row$species_common_name[1], "**. ")
  } else {
    ""
  }

  parts <- paste0(
    preamble,
    "`n` = ",
    format(row$n[1], big.mark = ","),
    " measurements across ",
    format(row$n_rows[1], big.mark = ","),
    " rows, from **",
    row$n_sources[1],
    "** source",
    if (isTRUE(row$n_sources[1] == 1)) "" else "s",
    " (distinct `REFERENCE_ID`). Unit: `",
    row$MEASURED_UNIT_STANDARD[1],
    "`."
  )

  flags <- group_flag_text(row)
  if (length(flags) > 0) {
    parts <- paste0(
      parts,
      "\n\n::: callout-warning\n## Flagged\n",
      paste0("- ", flags, collapse = "\n"),
      "\n:::"
    )
  }
  parts
}
