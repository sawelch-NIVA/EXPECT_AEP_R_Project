# Shared sample-groups summary table.
#
# Previously this dplyr logic was duplicated verbatim in index.qmd and
# docs/NBXX-Sample-Groups.qmd, which is how a rename broke one copy and not the
# other. The reshaping now lives in the `sample_groups_table` target and the
# presentation lives in sample_groups_flextable(); both documents read the
# target and call the formatter.

#' Build the Sample-Groups Display Table
#'
#' Reshapes `summarise_literature_data` into the columns actually shown to a
#' reader: compartment/taxonomy folded into one `group` label, geography folded
#' into `location`, and mean/SD/unit folded into a single cell.
#'
#' Column names here are plain and code-friendly. Display labels are applied
#' separately by [sample_groups_flextable()], so renaming a header never
#' requires touching this function (and vice versa).
#'
#' The two `.`-prefixed flag columns drive conditional formatting downstream.
#' They are carried through rather than recomputed so that any filtering a
#' document applies (e.g. the manuscript showing only large groups) stays
#' consistent with the highlighting.
#'
#' @param summary_data The `summarise_literature_data` target.
#' @return A tibble sorted by `group`, `location`, with columns `group`,
#'   `location`, `dates`, `n`, `mean_sd`, `median`, `n_outliers`,
#'   `dip_p_label`, `.is_multimodal`, `.is_outlier`.
#' @export
build_sample_groups_table <- function(summary_data) {
  summary_data |>
    dplyr::mutate(
      # Fold compartment + taxonomy into one column
      group = dplyr::if_else(
        .data$ENVIRON_COMPARTMENT == "Biota",
        paste(
          dplyr::coalesce(.data$SPECIES_GROUP, "Unknown"),
          dplyr::coalesce(.data$SAMPLE_SPECIES, "spp."),
          dplyr::coalesce(.data$SAMPLE_TISSUE, "—"),
          sep = " › "
        ),
        paste(
          .data$ENVIRON_COMPARTMENT,
          .data$ENVIRON_COMPARTMENT_SUB,
          sep = " › "
        )
      ),
      # Fold geographic feature columns
      location = paste(
        .data$SITE_GEOGRAPHIC_FEATURE,
        .data$SITE_GEOGRAPHIC_FEATURE_SUB,
        sep = " › "
      ),
      dates = paste0(
        format(.data$date_min, "%Y"),
        "–",
        format(.data$date_max, "%Y")
      ),
      n = as.integer(.data$n),
      # Fold mean, SD, and unit into one cell
      mean_sd = sprintf("%.2g ± %.2g %s", .data$mean, .data$sd, .data$unit),
      median = .data$median,
      n_outliers = .data$n_double_outliers,
      # Blank rather than NA where the dip test was not run (n below min_n)
      dip_p_label = dplyr::if_else(
        .data$multimodal %in% TRUE,
        formatC(.data$dip_p, digits = 2, format = "f"),
        ""
      ),
      # `%in% TRUE` keeps untested (NA) groups out of the highlight sets
      .is_multimodal = .data$multimodal %in% TRUE,
      # NOTE: n is sum(MEASURED_N) but n_double_outliers counts rows, so this
      # ratio is not apples-to-apples and under-fires where MEASURED_N > 1.
      # See PLAN.md P1.5 -- deliberately left as-is pending a decision.
      .is_outlier = (.data$n_double_outliers / .data$n) > 0.05 &
        !is.na(.data$n_double_outliers),
      .keep = "none"
    ) |>
    # `.keep = "none"` leaves columns that already existed in the input (n,
    # median) sitting in their original positions, so without this the table
    # renders as N, Median, Group, Location, ... Order the columns explicitly.
    dplyr::select(
      "group",
      "location",
      "dates",
      "n",
      "mean_sd",
      "median",
      "n_outliers",
      "dip_p_label",
      ".is_multimodal",
      ".is_outlier"
    ) |>
    dplyr::arrange(.data$group, .data$location)
}

#' Render the Sample-Groups Table as a flextable
#'
#' Applies display labels and conditional formatting. Row indices for the
#' highlighting are computed from whatever `tbl` is passed in, so filtering the
#' table before calling this (as the manuscript does, to show only large
#' groups) keeps the highlighting aligned.
#'
#' @param tbl Output of [build_sample_groups_table()], optionally filtered.
#' @return A `flextable` object.
#' @export
sample_groups_flextable <- function(tbl) {
  multimodal_idx <- which(tbl$.is_multimodal)
  outlier_idx <- which(tbl$.is_outlier)

  tbl |>
    dplyr::select(-".is_multimodal", -".is_outlier") |>
    flextable::flextable() |>
    flextable::set_header_labels(
      group = "Group",
      location = "Location",
      dates = "Dates",
      n = "N",
      mean_sd = "Mean ± SD",
      median = "Median",
      n_outliers = "Outliers",
      dip_p_label = "Multimodal (p)"
    ) |>
    flextable::theme_vanilla() |>
    flextable::bold(part = "header") |>
    flextable::colformat_double(j = "median", digits = 2) |>
    # Merge repeated values in grouping columns to reduce visual height
    flextable::merge_v(j = c("group", "location", "dates")) |>
    flextable::color(i = multimodal_idx, j = "dip_p_label", color = "red") |>
    flextable::bg(i = outlier_idx, bg = "#FFF3CD") |>
    flextable::fontsize(size = 9, part = "all") |>
    flextable::padding(padding = 2, part = "all") |>
    flextable::autofit()
}
