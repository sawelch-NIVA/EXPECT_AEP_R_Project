# Per-AEP matrix time series (added 2026-09-03, was scripts/explore_aep_matrix_
# timeseries.R). One panel per compartment -- cod liver, blue mussel, coastal
# water, sediment -- inside an AEP bounding box, measured copper over time, in
# native units with a free y-axis per panel (dry / wet / per-litre are not
# comparable). Water and sediment points carry their M-608 quality class,
# consistent with fig05-repparfjorden-concentrations (four classes: copper skips M-608
# Class III). Biota carry no M-608 copper ladder, so cod and mussel are shown
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

# compartment -> native unit shown in the strip
aep_ts_compartment_units <- function() {
  c(
    "Cod liver" = "mg/kg ww", "Blue mussel" = "mg/kg ww",
    "Coastal water" = "mg/L", "Sediment" = "mg/kg dw"
  )
}

# Row -> compartment. A broad match (species/tissue/compartment-sub) rather than
# a single group id, because a box can hold more than one group per compartment
# (e.g. Kogel's cod is its own group); the groups actually present are reported
# in the strip text.
aep_ts_tag_compartment <- function(x) {
  dplyr::case_when(
    x$SAMPLE_SPECIES == "Gadus morhua" & x$SAMPLE_TISSUE == "Liver" &
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
#' @param recent_from Year whose 1 January gets the dotted vertical marker.
#' @return A ggplot. If the box holds nothing in any compartment, a placeholder
#'   plot carrying that message (so a `format = "file"` target still writes).
#' @export
aep_matrix_timeseries_plot <- function(
  aep_id, data, thresholds, group_ids, manifest, recent_from = 2024
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
  # M-608 class boundaries in native units, for faint anchor lines.
  m608_lines <- list(
    "Coastal water" = c(0.3, 2.6, 5.2) / 1000,
    "Sediment" = c(20, 84, 147)
  )

  box <- data |>
    dplyr::filter(
      !is.na(.data$LONGITUDE), !is.na(.data$LATITUDE),
      .data$LONGITUDE >= m$lon_min, .data$LONGITUDE <= m$lon_max,
      .data$LATITUDE >= m$lat_min, .data$LATITUDE <= m$lat_max,
      !is.na(.data$SAMPLING_DATE), .data$MEASURED_VALUE_STANDARD > 0
    ) |>
    dplyr::mutate(compartment = aep_ts_tag_compartment(dplyr::pick(dplyr::everything()))) |>
    dplyr::filter(!is.na(.data$compartment))

  if (nrow(box) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate(
          "text", x = 0, y = 0,
          label = paste0("No copper data in the ", aep_id, " box\n",
                         "for cod / mussel / water / sediment")
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
      value = .data$MEASURED_VALUE_STANDARD,
      date = as.Date(.data$SAMPLING_DATE),
      grp = sub("^(G[0-9]+).*$", "\\1", .data$group_id),
      m608 = dplyr::recode(as.character(.data$threshold_class),
        I = "Background", II = "Good", IV = "Poor", V = "Very Poor"
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

  # Strip text: compartment, units, and the sample-group number(s) present.
  grp_by_comp <- box |>
    dplyr::filter(!is.na(.data$grp)) |>
    dplyr::distinct(.data$compartment, .data$grp) |>
    dplyr::arrange(.data$compartment, .data$grp) |>
    dplyr::summarise(
      grps = paste(.data$grp, collapse = ", "), .by = "compartment"
    )
  panel_of <- function(cmp) {
    g <- grp_by_comp$grps[match(cmp, grp_by_comp$compartment)]
    ifelse(
      is.na(g),
      sprintf("%s  (%s)", cmp, comp_units[cmp]),
      sprintf("%s  (%s)  ·  %s", cmp, comp_units[cmp], g)
    )
  }
  present <- comp_levels[comp_levels %in% box$compartment]
  panel_levels <- panel_of(present)
  box$panel <- factor(panel_of(box$compartment), levels = panel_levels)

  reflines <- dplyr::bind_rows(
    tibble::tibble(compartment = names(proref), y = unname(proref)),
    purrr::imap_dfr(m608_lines, ~ tibble::tibble(compartment = .y, y = .x))
  ) |>
    dplyr::filter(.data$compartment %in% present) |>
    dplyr::mutate(panel = factor(panel_of(.data$compartment), levels = panel_levels))

  abiotic <- dplyr::filter(box, .data$compartment %in% c("Coastal water", "Sediment"))
  biota <- dplyr::filter(box, .data$compartment %in% c("Cod liver", "Blue mussel"))
  trend <- box |>
    dplyr::group_by(.data$panel) |>
    dplyr::filter(dplyr::n() >= 6) |>
    dplyr::ungroup()

  marker <- as.Date(sprintf("%d-01-01", recent_from))

  ggplot2::ggplot(box, ggplot2::aes(.data$date, .data$value)) +
    ggplot2::geom_vline(
      xintercept = marker, linetype = "dotted", colour = "grey45"
    ) +
    ggplot2::geom_hline(
      data = reflines, ggplot2::aes(yintercept = .data$y),
      linetype = "dashed", colour = "grey60", linewidth = 0.3
    ) +
    ggplot2::geom_smooth(
      data = trend, method = "lm", formula = y ~ x, se = FALSE,
      colour = "grey30", linewidth = 0.5, linetype = "22"
    ) +
    ggplot2::geom_point(
      data = abiotic, ggplot2::aes(fill = .data$m608),
      shape = 21, size = 2.1, stroke = 0.3, colour = "grey25"
    ) +
    ggplot2::scale_fill_manual(
      values = klass_cols, drop = FALSE, limits = names(klass_cols),
      name = "M-608 class (water, sediment)"
    ) +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_point(
      data = biota, ggplot2::aes(fill = .data$proref_status),
      shape = 21, size = 2.1, stroke = 0.3, colour = "grey25"
    ) +
    ggplot2::scale_fill_manual(
      values = proref_status_colours(), name = "cod / mussel vs PROREF",
      na.translate = FALSE
    ) +
    ggplot2::scale_y_log10() +
    ggplot2::facet_wrap(~panel, ncol = 2, scales = "free_y") +
    ggplot2::labs(
      x = NULL,
      y = "Measured copper concentration  (log scale, native units per panel)",
      title = paste0("Copper in the ", aep_id, " bounding box over time"),
      subtitle = paste(
        strwrap(sprintf(paste(
          "Raw concentrations, free y-axis per compartment. Dashed grey line",
          "is a linear fit (panels with >= 6 points) - a rough eyeball of",
          "trend, not a model. Faint lines: PROREF (biota) and M-608 class",
          "boundaries (water, sediment). Dotted vertical marks %d. Strip text:",
          "compartment (units) and the sample groups present."
        ), recent_from), width = 98),
        collapse = "\n"
      ),
      caption = paste(
        strwrap(sprintf(
          paste(
            "%s box: %g-%g N, %g-%g E. Cod = liver, wet weight. Coastal water",
            "= ENVIRON_COMPARTMENT_SUB 'Marine/Salt Water', total copper;",
            "M-608 water classes are for dissolved copper, so indicative only."
          ),
          aep_id, m$lat_min, m$lat_max, m$lon_min, m$lon_max
        ), width = 112),
        collapse = "\n"
      )
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "vertical",
      plot.caption = ggplot2::element_text(size = 8, colour = "grey40", hjust = 0),
      panel.grid.minor = ggplot2::element_blank()
    )
}

#' Draw an AEP Matrix Time Series and Write it to `figures/`
#'
#' @inheritParams aep_matrix_timeseries_plot
#' @param dir Output directory.
#' @param width,height,dpi Canvas.
#' @return The written path: `figures/fig07-aep1-timeseries.png` for `"A001"`,
#'   `figures/fig09-aep2-timeseries.png` for `"A002"`.
#' @export
write_aep_matrix_timeseries <- function(
  aep_id, data, thresholds, group_ids, manifest,
  dir = here_rel("figures"), width = 10, height = 8, dpi = 200,
  recent_from = 2024
) {
  p <- aep_matrix_timeseries_plot(
    aep_id, data, thresholds, group_ids, manifest, recent_from = recent_from
  )
  # Manuscript figure numbers (order of appearance in index.qmd's Results).
  file <- c(
    A001 = "fig07-aep1-timeseries.png",
    A002 = "fig09-aep2-timeseries.png"
  )[aep_id]
  if (is.na(file)) {
    stop("No figure filename mapped for aep_id ", sQuote(aep_id), ".")
  }
  path <- file.path(dir, file)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  ggplot2::ggsave(
    path, p,
    width = width, height = height, dpi = dpi, bg = "white",
    device = ragg::agg_png
  )
  path
}
