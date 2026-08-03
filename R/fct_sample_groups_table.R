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
#' `.anchor` is computed for every row, but nothing is linked unless a document
#' asks: see the `link_sections` argument of [sample_groups_flextable()]. The
#' anchors only resolve in the triage notebook, and the manuscript reads this
#' same target, so linking by default would put dead anchors into `index.qmd` and
#' from there into the docx.
#'
#' @param summary_data The `summarise_literature_data` target.
#' @return A tibble sorted by `group`, `location`, with columns `group`,
#'   `location`, `dates`, `n`, `mean_sd`, `median`, `n_outliers`,
#'   `dip_p_label`, `.is_multimodal`, `.is_outlier`, `.anchor`.
#' @export
build_sample_groups_table <- function(summary_data) {
  # Computed before the .keep = "none" mutate below, which discards the key
  # columns the anchor is derived from.
  anchors <- heading_anchor(summary_data)

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
      # A range of one year collapses to that year: "2005" not "2005–2005".
      # 165 of 245 groups are single-year, so this is the common case.
      dates = dplyr::if_else(
        format(.data$date_min, "%Y") == format(.data$date_max, "%Y"),
        format(.data$date_min, "%Y"),
        paste0(format(.data$date_min, "%Y"), "–", format(.data$date_max, "%Y"))
      ),
      n = as.integer(.data$n),
      # Fold mean, SD, and unit into one cell. The SD is dropped where it is NA,
      # which is any group with a single measurement: sd() of one value is NA, and
      # "3.2 ± NA mg/kg (dry)" claims a failed calculation rather than an absent
      # one. 67 of 245 groups are in this position.
      mean_sd = dplyr::if_else(
        is.na(.data$sd),
        sprintf("%.2g %s", .data$mean, .data$unit),
        sprintf("%.2g ± %.2g %s", .data$mean, .data$sd, .data$unit)
      ),
      median = .data$median,
      n_outliers = .data$n_double_outliers,
      # Blank rather than NA where the dip test was not run (n below min_n)
      dip_p_label = dplyr::if_else(
        .data$multimodal %in% TRUE,
        formatC(.data$dip_p, digits = 2, format = "f"),
        ""
      ),
      n_units = .data$n_units,
      # "<1%" rather than a rounded "0%", which reads as "none dropped" when it
      # means "a few dropped". Blank only where nothing was dropped at all.
      dropped_label = dplyr::case_when(
        is.na(.data$prop_dropped) | .data$prop_dropped == 0 ~ "",
        .data$prop_dropped < 0.005 ~ "<1%",
        .default = paste0(round(100 * .data$prop_dropped), "%")
      ),
      # Flags come from add_triage_flags(), the same source the notebook's
      # per-group warning text reads, so the highlighting here and the prose
      # there cannot disagree about what is flagged.
      .is_multimodal = .data$flag_multimodal,
      .is_outlier = .data$flag_outliers,
      .anchor = anchors,
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
      "n_units",
      "dropped_label",
      ".is_multimodal",
      ".is_outlier",
      ".anchor"
    ) |>
    # Ranked by n descending (PLAN.md P1.4), not alphabetically: the point of the
    # table is to work down from the groups carrying the most data. Note this
    # scatters repeated group labels, so merge_v() in the formatter collapses
    # fewer runs than it did under the old group/location ordering.
    dplyr::arrange(dplyr::desc(.data$n))
}

#' Render the Sample-Groups Table as a flextable
#'
#' Applies display labels and conditional formatting. Row indices for the
#' highlighting are computed from whatever `tbl` is passed in, so filtering the
#' table before calling this (as the manuscript does, to show only large
#' groups) keeps the highlighting aligned.
#'
#' Linking is opt-in. Pass `link_sections` the heading anchors that actually
#' exist as sections in the calling document (from
#' `sample_triage_groups()$heading_slug`) and those rows get their `group` cell
#' rendered as an internal link; every other row stays plain text, so the
#' 220-odd untriaged groups do not advertise dead anchors. The default of `NULL`
#' links nothing, which is what `index.qmd` needs: it reads the same target but
#' contains none of the sections, and its docx output would carry the dead links
#' outward.
#'
#' @param tbl Output of [build_sample_groups_table()], optionally filtered.
#' @param link_sections Character vector of anchors present in this document, or
#'   `NULL` to link nothing.
#' @return A `flextable` object.
#' @export
sample_groups_flextable <- function(tbl, link_sections = NULL) {
  multimodal_idx <- which(tbl$.is_multimodal)
  outlier_idx <- which(tbl$.is_outlier)
  linked_idx <- which(!is.na(tbl$.anchor) & tbl$.anchor %in% link_sections)

  # One constant for both the table-wide fontsize() below and the composed link
  # chunks further down. They have to be set separately: compose() replaces a
  # cell's content wholesale, and fp_text_default() takes its size from
  # get_flextable_defaults() (11pt) rather than from the fontsize() already
  # applied to the table, so the linked cells rendered two points larger than
  # every other row.
  font_size <- 9

  ft <- tbl |>
    dplyr::select(-".is_multimodal", -".is_outlier", -".anchor") |>
    flextable::flextable() |>
    flextable::set_header_labels(
      group = "Group",
      location = "Location",
      dates = "Dates",
      n = "N",
      mean_sd = "Mean ± SD",
      median = "Median",
      n_outliers = "Outliers",
      dip_p_label = "Multimodal (p)",
      n_units = "Units",
      dropped_label = "Dropped"
    ) |>
    flextable::theme_vanilla() |>
    flextable::bold(part = "header") |>
    flextable::colformat_double(j = "median", digits = 2) |>
    # Merge repeated values in grouping columns to reduce visual height
    flextable::merge_v(j = c("group", "location", "dates")) |>
    # Exactly two highlights, matching the two flags. `Units` and `Dropped` are
    # informational columns and are deliberately not highlighted.
    flextable::color(i = multimodal_idx, j = "dip_p_label", color = "red") |>
    flextable::bg(i = outlier_idx, bg = "#FFF3CD") |>
    flextable::fontsize(size = font_size, part = "all") |>
    flextable::padding(padding = 2, part = "all")

  # Link the triaged rows only. compose() replaces the cell content wholesale,
  # so it has to be handed the label as well as the href.
  #
  # `props` is load-bearing, not decoration. flextable emits
  # `<a href><span class="cl-..."></span></a>`, putting its own inline colour on
  # the span INSIDE the anchor, so the span wins over any link styling the page
  # supplies. Without explicit props the links render as ordinary black text,
  # identical to the 200-odd unlinked rows: they work, but nothing tells you
  # which 27 cells are clickable. This sets the colour and underline on the span
  # itself, which is the only level that survives.
  if (length(linked_idx) > 0) {
    ft <- flextable::compose(
      ft,
      i = linked_idx,
      j = "group",
      value = flextable::as_paragraph(
        flextable::hyperlink_text(
          x = tbl$group[linked_idx],
          url = paste0("#", tbl$.anchor[linked_idx]),
          props = flextable::fp_text_default(
            color = "#1A6FA8",
            underlined = TRUE,
            font.size = font_size
          )
        )
      )
    )
  }

  flextable::autofit(ft)
}
