# Detecting unit errors in the SOURCE data (2026-08-05).
#
# Distinct from R/fct_units.R, which is about conversions this project performs.
# This is about errors that arrive already made, and it exists because three
# separate 1000x faults surfaced in one day from the same misconception:
#
#   ug/g IS mg/kg. Micro-over-gram cancels against milli-over-kilogram.
#
#   1. Sam's Coteur extraction recorded ug/kg where the paper said ug/g.
#   2. standardise_measured_units() divided every genuine ug/g row by 1000.
#   3. A Vannmiljo submitter multiplied ug/g values by 1000 to "convert" them,
#      and said so in the comment field.
#
# The third is the one this file is for: the error is in data we do not control,
# and no amount of care in our own conversion code will catch it. Two detectors,
# because the two failure modes leave different traces.

#' Comments in Which a Submitter Describes Their Own Unit Arithmetic
#'
#' The cheapest and most certain detector, because it is not an inference: the
#' row says what was done to it.
#'
#' The Vannmiljo Urban Fjord Contaminants rows carry *"Verdier oppgitt i ug/g
#' (w.w.) og multiplisert med 1000"* -- values given in ug/g wet weight and
#' multiplied by 1000. Since ug/g already **is** mg/kg, that multiplication is
#' the error, and it puts 33 rows across seven biota groups a thousandfold high.
#' Cod muscle at 3,670 mg/kg wet, krill at 18,600, polychaetes at 17,800; all
#' ordinary once divided back.
#'
#' Deliberately reports rather than corrects. Rewriting a measured value on the
#' strength of a free-text comment is a judgement, and it belongs to Sam.
#'
#' @param data A measurements table with `MEASUREMENT_COMMENT`.
#' @param pattern Regex of unit-arithmetic phrasings, Norwegian and English.
#' @return A tibble of matching rows summarised by comment and group.
#' @export
scan_comment_unit_flags <- function(
  data,
  pattern = paste(
    "multiplis", "dividert", "delt p", "omregn", "konvert",
    "multiplied", "divided", "converted from",
    "ug/g", "µg/g", "μg/g", "mg/g",
    sep = "|"
  )
) {
  if (!"MEASUREMENT_COMMENT" %in% names(data)) {
    return(empty_unit_anomalies())
  }

  hits <- data |>
    dplyr::filter(
      !is.na(.data$MEASUREMENT_COMMENT),
      grepl(pattern, .data$MEASUREMENT_COMMENT, ignore.case = TRUE)
    )

  if (nrow(hits) == 0) {
    return(empty_unit_anomalies())
  }

  hits |>
    dplyr::group_by(
      comment = .data$MEASUREMENT_COMMENT,
      campaign = .data$CAMPAIGN_NAME_SHORT,
      dplyr::across(dplyr::any_of(c(
        "ENVIRON_COMPARTMENT_SUB", "SAMPLE_SPECIES", "SAMPLE_TISSUE",
        "MEASURED_UNIT_STANDARD"
      )))
    ) |>
    dplyr::summarise(
      n_rows = dplyr::n(),
      geo_mean = 10^mean(log10(.data$MEASURED_VALUE_STANDARD), na.rm = TRUE),
      max = max(.data$MEASURED_VALUE_STANDARD, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$n_rows))
}

#' Zero-Row Anomaly Table
#' @return A zero-row tibble.
#' @export
empty_unit_anomalies <- function() {
  tibble::tibble(
    comment = character(0),
    campaign = character(0),
    n_rows = integer(0),
    geo_mean = numeric(0),
    max = numeric(0)
  )
}

#' Campaigns Sitting a Whole Number of Decades Off Their Peers
#'
#' The detector for errors that leave no comment. Within one sampling group,
#' every campaign is measuring the same quantity in the same matrix and unit, so
#' their geometric means should agree to within a factor of a few. A campaign
#' sitting three orders from the rest is not natural variation.
#'
#' **The reference is the MEDIAN of the other campaigns' geometric means, each
#' campaign counting once**, not the pooled mean of their rows. Two revisions to
#' get here, and the reasoning matters because it is the difference between a
#' diagnostic and a misleading one:
#'
#' * Comparing against the whole group lets a large faulty campaign drag the
#'   reference toward itself and hide.
#' * Comparing against the pooled *rows* of the other campaigns fixes that only
#'   when the faulty campaign is small. Measured on this data it was not: the 18
#'   bad *Gadus morhua* muscle rows outweighed the correct ones, so the four
#'   **correct** campaigns were each flagged as two orders low while the faulty
#'   one fell below the flag threshold. Exactly inverted.
#'
#' One campaign, one vote, and a median, so a single wrong campaign cannot move
#' the reference at all unless it is more than half of them.
#'
#' `near_decade` is the distance from the nearest whole power of ten. A ratio of
#' 10^2.98 is a unit error; one of 10^2.1 is more likely a genuinely contaminated
#' site, and the two want reading differently. Sam's own heuristic, 2026-08-05:
#' "if you see 1e3 foothills in your distributions, check the units".
#'
#' @param data The `literature_analysis_ready` target.
#' @param by Column separating the candidate from its peers.
#' @param min_rows Minimum rows in both the candidate and the comparison set.
#' @param min_orders Report only ratios at least this many orders from 1.
#' @return A tibble, worst first.
#' @export
scan_group_scale_offsets <- function(
  data,
  by = "CAMPAIGN_NAME_SHORT",
  min_rows = 3,
  min_orders = 1.5
) {
  key <- triage_group_cols()
  needed <- c(key, by, "MEASURED_VALUE_STANDARD")
  if (!all(needed %in% names(data))) {
    return(empty_scale_offsets())
  }

  d <- data |>
    dplyr::filter(
      !is.na(.data$MEASURED_VALUE_STANDARD),
      .data$MEASURED_VALUE_STANDARD > 0
    ) |>
    dplyr::mutate(.log = log10(.data$MEASURED_VALUE_STANDARD))

  per_campaign <- d |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(key, by)))) |>
    dplyr::summarise(
      .cand_mean = mean(.data$.log),
      n_rows = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$n_rows >= min_rows)

  # Leave-one-out median over the campaign means. Done by splitting per group
  # rather than by arithmetic, because a median has no subtractive shortcut.
  out <- per_campaign |>
    dplyr::group_by(dplyr::across(dplyr::all_of(key))) |>
    dplyr::mutate(
      n_peers = dplyr::n() - 1L,
      .peer_mean = vapply(
        seq_len(dplyr::n()),
        function(i) stats::median(.data$.cand_mean[-i]),
        numeric(1)
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(
      .data$n_peers >= 1,
      is.finite(.data$.peer_mean)
    ) |>
    dplyr::mutate(
      log10_ratio = .data$.cand_mean - .data$.peer_mean,
      geo_mean = 10^.data$.cand_mean,
      peer_geo_mean = 10^.data$.peer_mean,
      near_decade = abs(.data$log10_ratio - round(.data$log10_ratio)),
      # Magnitude alone, NOT "is it a clean decade".
      #
      # Gating on near_decade was tried and is wrong, because the case that
      # matters most fails it: where a campaign is only PARTLY affected, its
      # geometric mean sits between the good and bad clusters and lands nowhere
      # near a whole power of ten. Urban Fjord Gadus morhua muscle is 3 correct
      # rows and 15 wrong ones, giving 10^3.56, which a decade test rejects while
      # flagging nothing else. Two orders inside one sampling group is worth a
      # look whatever the fractional part.
      #
      # near_decade is kept as a column: close to zero strengthens the case,
      # since a clean factor of exactly 1000 has no environmental explanation.
      check_units = abs(.data$log10_ratio) >= 2
    ) |>
    dplyr::filter(abs(.data$log10_ratio) >= min_orders) |>
    dplyr::select(
      dplyr::all_of(c(key, by)),
      "n_rows", "n_peers", "geo_mean", "peer_geo_mean",
      "log10_ratio", "near_decade", "check_units"
    ) |>
    dplyr::arrange(dplyr::desc(abs(.data$log10_ratio)))

  out
}

#' Zero-Row Scale-Offset Table
#' @return A zero-row tibble.
#' @export
empty_scale_offsets <- function() {
  tibble::tibble(
    n_rows = integer(0),
    n_peers = integer(0),
    geo_mean = numeric(0),
    peer_geo_mean = numeric(0),
    log10_ratio = numeric(0),
    near_decade = numeric(0),
    check_units = logical(0)
  )
}

#' Warn About Source Unit Errors
#'
#' Called from the pipeline so the count is in the build log rather than
#' discovered by eye. Reports; never corrects.
#'
#' @param comments Output of [scan_comment_unit_flags()].
#' @param offsets Output of [scan_group_scale_offsets()].
#' @return `invisible(NULL)`.
#' @export
report_unit_anomalies <- function(comments, offsets) {
  msgs <- character(0)

  if (nrow(comments) > 0) {
    msgs <- c(msgs, paste0(
      sum(comments$n_rows), " row(s) carry a comment describing unit ",
      "arithmetic, across ", nrow(comments), " group/comment combination(s)"
    ))
  }
  flagged <- offsets[offsets$check_units %in% TRUE, , drop = FALSE]
  if (nrow(flagged) > 0) {
    msgs <- c(msgs, paste0(
      nrow(flagged), " campaign/group combination(s) sit a whole number of ",
      "decades from their peers (", sum(flagged$n_rows), " rows)"
    ))
  }

  if (length(msgs) > 0) {
    cli::cli_warn(c(
      "Possible unit errors in the SOURCE data:",
      stats::setNames(msgs, rep("*", length(msgs))),
      "i" = "Read {.code tar_read(unit_anomaly_report)}. Nothing is corrected automatically."
    ))
  }
  invisible(NULL)
}
