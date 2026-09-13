# Per-group summary statistics for the sample-groups table.
#
# Extracted from the `summarise_literature_data` target body (_targets.R) on
# 2026-08-28 so the Repparfjorden case-study notebook can run the same
# aggregation on rows trimmed to the A002 bounding box. The target now calls
# this function; the logic and its comments moved across unchanged, the only
# edit being `stats::` qualifiers on `sd()` / `median()` to match house style
# (see fct_outlier_detection.R) now that this runs in the package namespace.

#' eDataDRF's Fractionation-Protocol Vocabulary
#'
#' The controlled `Short_Name`/`Long_Name` pairs eDataDRF ships for the
#' "Fractionation Protocol" method category, e.g. `Short_Name = "Total"` /
#' `Long_Name = "Total fraction"`. Read fresh from the installed package
#' (`inst/extdata/*-methods.parquet`) rather than copied into this project, so
#' this always tracks whatever vocabulary eDataDRF currently ships. Matched by
#' pattern rather than the exact (dated) filename, since that file is the only
#' `*methods*.parquet` eDataDRF ships and the date in its name is expected to
#' change as the package is updated.
#'
#' Cached after the first call (a `local()` closure): this is read once per
#' group/node during a summary pass, which is cheap for a 19-row table but
#' pointless to repeat hundreds of times in the same session.
#'
#' @return A tibble with `Short_Name` and `Long_Name`, fractionation rows only.
fractionation_vocab <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      path <- list.files(
        system.file("extdata", package = "eDataDRF"),
        pattern = "methods.*\\.parquet$",
        full.names = TRUE
      )[1]
      vocab <- arrow::read_parquet(path)
      cached <<- vocab[vocab$Protocol_Type == "Fractionation Protocol", ]
    }
    cached
  }
})

#' Standardised Short Name for a Fractionation Protocol
#'
#' Maps free text onto eDataDRF's controlled vocabulary ([fractionation_vocab()])
#' so that, say, a study's own `PROTOCOL_NAME` of "Acid extractable fraction"
#' (matching the vocabulary's `Long_Name`) and a value already given as the
#' standardised `Short_Name` itself both resolve to the same `"Acid
#' extractable"` label, rather than two different-looking strings for a group
#' or node that lumps rows from both.
#'
#' Text that matches neither `Short_Name` nor `Long_Name` passes through
#' unchanged (visible-but-nonstandard is safer than silently blank), and `NA`
#' stays `NA`.
#'
#' @param x Character vector of raw fractionation text.
#' @return Character vector of standardised `Short_Name` values, or the
#'   original text/`NA` where nothing matched.
#' @export
fractionation_short_name <- function(x) {
  vocab <- fractionation_vocab()
  out <- x

  by_short <- match(x, vocab$Short_Name)
  out[!is.na(by_short)] <- vocab$Short_Name[by_short[!is.na(by_short)]]

  unresolved <- is.na(by_short)
  by_long <- match(x[unresolved], vocab$Long_Name)
  out_unresolved <- out[unresolved]
  out_unresolved[!is.na(by_long)] <- vocab$Short_Name[by_long[!is.na(by_long)]]
  out[unresolved] <- out_unresolved

  out
}

#' Distinct Fractionation Protocols Behind a Set of Rows
#'
#' Coalesces the literature side's `FRACTIONATION_PROTOCOL_CLASS` (joined from
#' each study's own Methods table -- e.g. "Acid extractable fraction", see
#' `fct_join_extraction_data.R`), run through [fractionation_short_name()] to
#' standardise it, with the Vannmiljø side's own `FRACTIONATION_PROTOCOL`
#' (already coded straight to the vocabulary's `Short_Name` values -- "Total" /
#' "Filtered 0.45um", see `fct_vm_eData.R`), so both sources report through one
#' standardised column. Sorted and comma-separated so more than one distinct
#' value is the visible signal that a group or node lumps rows measured under
#' different fractionation protocols (Total and Filtered, say), which is
#' exactly what CLAUDE.md 4.4.-1.5 says to check before trusting a mean.
#'
#' @param protocol_class `FRACTIONATION_PROTOCOL_CLASS` column (literature
#'   rows; `NA` for Vannmiljø).
#' @param protocol `FRACTIONATION_PROTOCOL` column, same length (already a
#'   standardised Short_Name for Vannmiljø rows; a bare protocol id for
#'   literature rows -- only used here where `protocol_class` is `NA`).
#' @return A single string: the sorted, comma-separated distinct labels, or
#'   `"Not reported"` where none of the rows carry either column.
#' @export
fractionation_summary <- function(protocol_class, protocol) {
  label <- dplyr::coalesce(fractionation_short_name(protocol_class), protocol)
  label <- unique(label[!is.na(label)])
  if (length(label) == 0) {
    return("Not reported")
  }
  paste(sort(label), collapse = ", ")
}

#' Summarise Analysis-Ready Literature Data Into Per-Group Statistics
#'
#' One row per sample group (the eight columns of [analysis_group_cols()]),
#' carrying `n`, source count, the source ids themselves (`references`), date
#' range, arithmetic and geometric mean/SD, median, unit, the distinct
#' fractionation protocols behind the group ([fractionation_summary()]), two
#' outlier counts, and Hartigan's dip test. Ranking and the triage flags are
#' appended by [add_triage_flags()].
#'
#' The grouping, the outlier logic and the dip-test gate are all as they were in
#' the `summarise_literature_data` target; the reasoning behind each choice
#' (weighted outlier counts, GSD in place of SD, the shared `min_n` gate) is in
#' the comments below and in `_targets.R`.
#'
#' @param data Analysis-ready literature data: the `literature_analysis_ready`
#'   target, or a subset of it (the Repparfjorden notebook passes the rows whose
#'   coordinates fall inside the A002 bounding box).
#' @param dropped_report The `literature_dropped_report` target, passed straight
#'   through to [add_triage_flags()].
#' @return A tibble with one row per group, ready for
#'   [build_sample_groups_table()].
#' @export
summarise_groups <- function(data, dropped_report) {
  data |>
    dplyr::group_by(
      ENVIRON_COMPARTMENT,
      ENVIRON_COMPARTMENT_SUB,
      SPECIES_GROUP,
      SAMPLE_SPECIES,
      SAMPLE_TISSUE,
      SITE_GEOGRAPHIC_FEATURE,
      SITE_GEOGRAPHIC_FEATURE_SUB,
      # we split by unit type for summary
      MEASURED_UNIT_STANDARD
    ) |>
    # NA/zero/negative measured values are dropped upstream by
    # literature_analysis_ready, so the filter that used to sit here is
    # redundant. Left as a comment because its removal is the reason this
    # summary's results may shift slightly on the next rebuild.
    # REPLACED an inline copy of the outlier logic with the shared function,
    # 2026-08-05. Two things were wrong with the copy beyond the duplication:
    #
    # 1. Its RMZ ran on the RAW scale while its Tukey fences ran on log10,
    #    which made the RMZ criterion an upper-tail test in practice. See
    #    flag_outliers() for the measurement. The plots called flag_outliers()
    #    and this target did not, so moving one without the other would have
    #    left the summary table disagreeing with the panels it ranks.
    # 2. It was UNGATED, computing flags for groups of any size, while the
    #    dip test below is gated at dip_test_safe()'s min_n. So a group of
    #    four could be flagged for outliers but never tested for modality.
    #    flag_outliers() applies the same min_n = 10, so the two flags now
    #    abstain together and "untested" means the same thing for both.
    dplyr::mutate(
      flag_outliers(MEASURED_VALUE_STANDARD)
    ) |>
    dplyr::reframe(
      n = sum(MEASURED_N),
      n_sources = length(unique(REFERENCE_ID)),
      # Author-Year text where the reference resolves in references.bib, the
      # raw REFERENCE_ID otherwise (see ?reference_citation_summary): sorted
      # and comma separated so the cell is stable between rebuilds. Shown by
      # build_sample_groups_table().
      references = reference_citation_summary(REFERENCE_ID, TITLE, YEAR),
      date_min = suppressWarnings(min(SAMPLING_DATE, na.rm = TRUE)),
      date_max = suppressWarnings(max(SAMPLING_DATE, na.rm = TRUE)),
      sd = stats::sd(MEASURED_VALUE_STANDARD, na.rm = TRUE),
      mean = mean(MEASURED_VALUE_STANDARD, na.rm = TRUE),
      # Geometric mean and geometric SD, added 2026-08-04 on Sam's call:
      # "GSD is a reversal, you're right. but it clearly makes more sense
      # than SD of non-normal data."
      #
      # These concentrations are log-normal over many orders of magnitude, so
      # the arithmetic mean sits above almost every observation and the
      # arithmetic sd is dominated by the largest value. GSD is a
      # MULTIPLICATIVE factor: 3 means roughly threefold either side of the
      # geometric mean, and that is the sentence the methods section needs.
      #
      # log10 throughout, matching every plot axis in the project.
      # literature_analysis_ready has already dropped zeros and negatives, so
      # the logs are all finite.
      geo_mean = 10^mean(log10(MEASURED_VALUE_STANDARD), na.rm = TRUE),
      gsd = 10^stats::sd(log10(MEASURED_VALUE_STANDARD), na.rm = TRUE),
      # FIXED 2026-07-30 (PLAN.md P1.5). This was sum(outlier_RMZ &
      # outlier_IQR), a count of *rows*, while n is sum(MEASURED_N), a count
      # of *measurements*. The ratio therefore divided a row count by a
      # measurement count and systematically under-fired wherever
      # MEASURED_N > 1. Sam's call: weight the outlier count by MEASURED_N so
      # numerator and denominator are the same quantity.
      #
      # na.rm because flag_outliers() returns NA flags where the group is
      # below min_n or the MAD is zero, and a single NA would otherwise blank
      # the whole group's count. Untested rows therefore count as
      # non-outliers, which is the conservative direction.
      n_double_outliers = sum(
        (outlier_RMZ & outlier_IQR) * MEASURED_N,
        na.rm = TRUE
      ),
      # The old row-count version, kept alongside so the two are comparable
      # and the change is auditable rather than silent.
      n_outlier_rows = sum(outlier_RMZ & outlier_IQR, na.rm = TRUE),
      median = stats::median(MEASURED_VALUE_STANDARD),
      unit = unique(MEASURED_UNIT_STANDARD),
      # So a reader can see at a glance whether this group lumps rows measured
      # under different fractionation protocols (Total vs Filtered, say)
      # before trusting its mean. CLAUDE.md 4.4.-1.5.
      fractionation = fractionation_summary(
        FRACTIONATION_PROTOCOL_CLASS, FRACTIONATION_PROTOCOL
      ),
      # Constant within a group by construction: the group key includes
      # SAMPLE_SPECIES and the common name is a function of the species.
      # Carried through so the triage notebook can print it as an
      # aide-memoire under each heading.
      species_common_name = SPECIES_COMMON_NAME[1],
      # Hartigan's dip test for unimodality (NA below dip_test_safe()'s min_n)
      dip_p = dip_test_safe(MEASURED_VALUE_STANDARD)$dip_p,
      multimodal = dip_test_safe(MEASURED_VALUE_STANDARD)$multimodal
    ) |>
    add_triage_flags(dropped_report)
}
