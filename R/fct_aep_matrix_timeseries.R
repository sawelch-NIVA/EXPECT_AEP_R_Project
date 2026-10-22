# Per-AEP matrix time series (added 2026-09-03, was scripts/explore_aep_matrix_
# timeseries.R). One panel per compartment -- cod liver, blue mussel, coastal
# water, sediment -- inside an AEP bounding box, measured copper over time, in
# native units with a free y-axis per panel (dry / wet / per-litre are not
# comparable). Water and sediment points carry their M-608 quality class,
# consistent with fig06-aep1-concentrations / fig07-aep2-concentrations (four
# classes: copper skips M-608 Class III). Biota carry no M-608 copper ladder, so
# cod and mussel are shown
# on a separate above/below-PROREF colour scale.

#' M-608 Classes Used on Copper Figures
#'
#' Four, not five: M-608 defines no Class III (Moderate) for copper (see
#' [generate_copper_thresholds()]), so there is no sense carrying a break that
#' can never be filled. Matches `class_cols` in `docs/NBXX-rfjord-2.qmd`.
#'
#' @return A named character vector, display label to hex colour.
#' @export
copper_m608_class_colours <- function() {
  c(
    "Background" = unname(threshold_class_colours()["I"]),
    "Good" = unname(threshold_class_colours()["II"]),
    "Poor" = unname(threshold_class_colours()["IV"]),
    "Very Poor" = unname(threshold_class_colours()["V"])
  )
}

#' Off-Palette Colours for the Biota PROREF Comparison
#'
#' Deliberately not blue/green/orange/red, so the PROREF scale does not read as
#' part of the M-608 scale on the same figure.
#'
#' @return A named character vector.
#' @export
proref_status_colours <- function() {
  c("at or below PROREF" = "grey65", "above PROREF" = "#762A83")
}

# compartment -> native unit shown in the strip. Coastal water is displayed in
# µg/L (see aep_matrix_timeseries_plot()'s `value` mutate below, which rescales
# the plotted points and M-608 reference lines to match); MEASURED_UNIT_STANDARD
# stays "mg/L" throughout, so classify_by_thresholds() runs before this rescale
# and is unaffected.
aep_ts_compartment_units <- function() {
  c(
    "Cod liver" = "mg/kg ww",
    "Blue mussel" = "mg/kg ww",
    "Coastal water" = "µg/L",
    "Sediment" = "mg/kg dw"
  )
}

# Row -> compartment. A broad match (species/tissue/compartment-sub) rather than
# a single group id, because a box can hold more than one group per compartment
# (e.g. Kogel's cod is its own group); the groups actually present are reported
# in the strip text.
aep_ts_tag_compartment <- function(x) {
  dplyr::case_when(
    x$SAMPLE_SPECIES == "Gadus morhua" &
      x$SAMPLE_TISSUE == "Liver" &
      x$MEASURED_UNIT_STANDARD == "mg/kg (wet)" ~ "Cod liver",
    x$SAMPLE_SPECIES == "Mytilus edulis" &
      x$SAMPLE_TISSUE == "Total soft tissues" &
      x$MEASURED_UNIT_STANDARD == "mg/kg (wet)" ~ "Blue mussel",
    x$ENVIRON_COMPARTMENT_SUB == "Marine/Salt Water" ~ "Coastal water",
    x$ENVIRON_COMPARTMENT_SUB == "Aquatic Sediment" &
      x$MEASURED_UNIT_STANDARD == "mg/kg (dry)" ~ "Sediment",
    TRUE ~ NA_character_
  )
}

#' Copper-in-an-AEP-box Time Series Plot
#'
#' @param aep_id `"A001"` / `"A002"`; picks the bounding box from `manifest`.
#' @param data The `literature_analysis_ready` target.
#' @param thresholds The `copper_toxicity_thresholds` target.
#' @param group_ids The `group_ids` target (to name the sample groups per panel).
#' @param manifest The `aep_manifest` target.
#' @param recent_from Year whose 1 January gets the dotted vertical marker, or
#'   `NULL` to omit the marker entirely (it only means something for A001,
#'   where it marks the end of the Hammerfest harbour cleanup project -- see
#'   the fig-a001-timeseries caption in `_03-results.qmd`).
#' @return A ggplot. If the box holds nothing in any compartment, a placeholder
#'   plot carrying that message (so a `format = "file"` target still writes).
#' @export
aep_matrix_timeseries_plot <- function(
  aep_id,
  data,
  thresholds,
  group_ids,
  manifest,
  recent_from = 2024
) {
  key <- triage_group_cols()
  m <- manifest[manifest$aep_id == aep_id, , drop = FALSE]
  if (nrow(m) != 1) {
    stop("No single manifest row for aep_id ", sQuote(aep_id), ".")
  }

  klass_cols <- copper_m608_class_colours()
  comp_units <- aep_ts_compartment_units()
  comp_levels <- names(comp_units)
  proref <- c(
    "Cod liver" = thresholds$THRESHOLD_VALUE[
      grepl("PROREF: Cod", thresholds$TITLE_SHORT)
    ][1],
    "Blue mussel" = thresholds$THRESHOLD_VALUE[
      grepl("PROREF: Mussel", thresholds$TITLE_SHORT)
    ][1]
  )
  # M-608 class boundaries in native (displayed) units, for faint anchor lines.
  # Coastal water is quoted in µg/L (its M-608 source unit) to match the
  # display rescale of `value` below; no /1000 needed since neither side of
  # this line is MEASURED_UNIT_STANDARD's internal "mg/L" any more.
  m608_lines <- list(
    "Coastal water" = c(0.3, 2.6, 5.2),
    "Sediment" = c(20, 84, 147)
  )

  box <- data |>
    dplyr::filter(
      !is.na(.data$LONGITUDE),
      !is.na(.data$LATITUDE),
      .data$LONGITUDE >= m$lon_min,
      .data$LONGITUDE <= m$lon_max,
      .data$LATITUDE >= m$lat_min,
      .data$LATITUDE <= m$lat_max,
      !is.na(.data$SAMPLING_DATE),
      .data$MEASURED_VALUE_STANDARD > 0
    ) |>
    dplyr::mutate(
      compartment = aep_ts_tag_compartment(dplyr::pick(dplyr::everything()))
    ) |>
    dplyr::filter(!is.na(.data$compartment))

  if (nrow(box) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate(
          "text",
          x = 0,
          y = 0,
          label = paste0(
            "No copper data in the ",
            aep_id,
            " box\n",
            "for cod / mussel / water / sediment"
          )
        ) +
        ggplot2::theme_void()
    )
  }

  box <- classify_by_thresholds(box, thresholds) |>
    dplyr::left_join(
      dplyr::select(group_ids, dplyr::all_of(key), "group_id"),
      by = key
    ) |>
    dplyr::mutate(
      # Rescaled to µg/L for display, Coastal water only: classify_by_thresholds()
      # above already ran on the untouched MEASURED_VALUE_STANDARD, so this does
      # not affect M-608 classification, only the point this compartment's panel
      # plots against its own free y-axis (scales = "free_y", so the other three
      # panels are unaffected).
      value = dplyr::if_else(
        .data$compartment == "Coastal water",
        .data$MEASURED_VALUE_STANDARD * 1000,
        .data$MEASURED_VALUE_STANDARD
      ),
      date = as.Date(.data$SAMPLING_DATE),
      # Full composite group_id (e.g. "G006-Bf-Cnr-G-mor-Liv-Mw"), not the bare
      # "G006" prefix: since 2026-08-08 that composite string IS the group
      # name everywhere else (tbl-groups-a001/a002, aep_nodes.csv, docs/groups
      # anchors -- see fct_sample_groups_table.R). A separate bare form here
      # made this figure's strip text disagree with the table right above it.
      grp = .data$group_id,
      m608 = dplyr::recode(
        as.character(.data$threshold_class),
        I = "Background",
        II = "Good",
        IV = "Poor",
        V = "Very Poor"
      ),
      m608 = factor(.data$m608, levels = names(klass_cols)),
      proref_status = dplyr::case_when(
        .data$compartment == "Cod liver" &
          .data$value <= proref["Cod liver"] ~ "at or below PROREF",
        .data$compartment == "Cod liver" ~ "above PROREF",
        .data$compartment == "Blue mussel" &
          .data$value <= proref["Blue mussel"] ~ "at or below PROREF",
        .data$compartment == "Blue mussel" ~ "above PROREF",
        TRUE ~ NA_character_
      )
    )

  # Strip text: compartment plus the bare sample-group number(s) present, e.g.
  # "Blue mussel (G005)". Units no longer belong here -- each panel carries its
  # own y-axis unit label instead (see the ggh4x scale below), which is also
  # what frees this bracket up for the group number.
  grp_by_comp <- box |>
    dplyr::filter(!is.na(.data$grp)) |>
    dplyr::mutate(grp_num = sub("^(G[0-9]+).*", "\\1", .data$grp)) |>
    dplyr::distinct(.data$compartment, .data$grp_num) |>
    dplyr::arrange(.data$compartment, .data$grp_num) |>
    dplyr::summarise(
      grps = paste(.data$grp_num, collapse = ", "),
      .by = "compartment"
    )
  panel_of <- function(cmp) {
    g <- grp_by_comp$grps[match(cmp, grp_by_comp$compartment)]
    ifelse(is.na(g), cmp, sprintf("%s (%s)", cmp, g))
  }
  present <- comp_levels[comp_levels %in% box$compartment]
  panel_levels <- panel_of(present)
  box$panel <- factor(panel_of(box$compartment), levels = panel_levels)

  # Unit label per panel, drawn inside the panel rather than as a true axis
  # title. ggh4x::facetted_pos_scales() was tried first (2026-09-22) to give
  # each panel its own y-axis title, but under both facet_wrap() and
  # ggh4x::facet_wrap2(axes = "all") it reuses one title per gtable COLUMN:
  # the bottom-left panel silently inherited the top-left panel's unit
  # ("mg/kg ww" on the Coastal water panel, which is µg/L) instead of its
  # own. Caught by cropping the rendered PNG and reading the text, not by
  # eye -- see CLAUDE.md 2.3.1. This annotation sidesteps that bug entirely.
  unit_labels <- tibble::tibble(
    panel = factor(panel_levels, levels = panel_levels),
    label = unname(comp_units[present])
  )

  reflines <- dplyr::bind_rows(
    tibble::tibble(compartment = names(proref), y = unname(proref)),
    purrr::imap_dfr(m608_lines, ~ tibble::tibble(compartment = .y, y = .x))
  ) |>
    dplyr::filter(.data$compartment %in% present) |>
    dplyr::mutate(
      panel = factor(panel_of(.data$compartment), levels = panel_levels)
    )

  abiotic <- dplyr::filter(
    box,
    .data$compartment %in% c("Coastal water", "Sediment")
  )
  biota <- dplyr::filter(
    box,
    .data$compartment %in% c("Cod liver", "Blue mussel")
  )

  marker_layer <- if (is.null(recent_from)) {
    NULL
  } else {
    ggplot2::geom_vline(
      xintercept = as.Date(sprintf("%d-01-01", recent_from)),
      linetype = "dotted",
      colour = "grey45"
    )
  }

  ggplot2::ggplot(box, ggplot2::aes(.data$date, .data$value)) +
    marker_layer +
    ggplot2::geom_hline(
      data = reflines,
      ggplot2::aes(yintercept = .data$y),
      linetype = "dashed",
      colour = "grey60",
      linewidth = 0.3
    ) +
    ggplot2::geom_point(
      data = abiotic,
      ggplot2::aes(fill = .data$m608),
      shape = 21,
      size = 2.1,
      stroke = 0.3,
      colour = "grey25"
    ) +
    ggplot2::scale_fill_manual(
      values = klass_cols,
      drop = FALSE,
      limits = names(klass_cols),
      name = "EQS class (water, sediment)"
    ) +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_point(
      data = biota,
      ggplot2::aes(fill = .data$proref_status),
      shape = 21,
      size = 2.1,
      stroke = 0.3,
      colour = "grey25"
    ) +
    ggplot2::scale_fill_manual(
      values = proref_status_colours(),
      name = "PROREF (G. morhua/M. edulis)",
      na.translate = FALSE
    ) +
    # Extra top expansion (default is 5% each side): the unit label sits in
    # that headroom at the top-left of each panel, so it needs enough
    # clearance that the highest point in a panel (which can land anywhere,
    # including right at the top -- e.g. Blue mussel's PROREF-exceeding
    # cluster) never sits underneath the text.
    ggplot2::scale_y_log10(
      expand = ggplot2::expansion(mult = c(0.05, 0.18))
    ) +
    # Sub-ticks for years: yearly gridlines between the labelled 2-year breaks,
    # so a point's year is readable without counting gridlines (2026-09-14).
    ggplot2::scale_x_date(
      date_breaks = "2 years",
      date_labels = "%Y",
      date_minor_breaks = "1 year"
    ) +
    ggplot2::facet_wrap(~panel, ncol = 2, scales = "free_y") +
    ggplot2::geom_text(
      data = unit_labels,
      # x as a Date (not bare -Inf) so scale_x_date doesn't warn about
      # coercing a <numeric>; still resolves to the panel's left edge.
      ggplot2::aes(
        x = structure(-Inf, class = "Date"),
        y = Inf,
        label = .data$label
      ),
      inherit.aes = FALSE,
      hjust = -0.08,
      vjust = 1.6,
      size = 3,
      colour = "grey35"
    ) +
    # No title / subtitle / caption baked into the image: all of that prose
    # lives in the Quarto figure caption in _03-results.qmd (Sam, 2026-09-03).
    ggplot2::labs(
      x = NULL,
      y = "Measured copper concentration"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "vertical",
      # Tight vertical gap between the stacked PROREF and EQS legends
      # (2026-09-22): theme_minimal's default legend.spacing left a wide band
      # of empty space between them at the bottom of the figure.
      legend.spacing.y = grid::unit(2, "pt"),
      plot.caption = ggplot2::element_text(
        size = 8,
        colour = "grey40",
        hjust = 0
      ),
      panel.grid.minor.y = ggplot2::element_blank()
    )
}

#' Draw an AEP Matrix Time Series and Write it to `figures/`
#'
#' @inheritParams aep_matrix_timeseries_plot
#' @param dir Output directory.
#' @param width,height,dpi Canvas.
#' @return The written path: `figures/fig09-aep1-timeseries.png` for `"A001"`,
#'   `figures/fig11-aep2-timeseries.png` for `"A002"`.
#' @export
write_aep_matrix_timeseries <- function(
  aep_id,
  data,
  thresholds,
  group_ids,
  manifest,
  dir = here_rel("figures"),
  width = 10,
  height = 8,
  dpi = 200,
  recent_from = 2024
) {
  p <- aep_matrix_timeseries_plot(
    aep_id,
    data,
    thresholds,
    group_ids,
    manifest,
    recent_from = recent_from
  )
  # Manuscript figure numbers (order of appearance in index.qmd's Results).
  file <- c(
    A001 = "fig09-aep1-timeseries.png",
    A002 = "fig11-aep2-timeseries.png"
  )[aep_id]
  if (is.na(file)) {
    stop("No figure filename mapped for aep_id ", sQuote(aep_id), ".")
  }
  path <- file.path(dir, file)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  ggplot2::ggsave(
    path,
    p,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white",
    device = ragg::agg_png
  )
  path
}
