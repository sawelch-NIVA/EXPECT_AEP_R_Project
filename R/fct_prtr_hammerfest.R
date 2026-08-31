# Norwegian PRTR (norske utslipp) copper releases from licensed facilities in
# Hammerfest kommune, for index.qmd's "Norwegian PRTR and REACH Product
# Register" section.
#
# The quantities are tiny: air is Hammerfest LNG alone in two years, water is a
# handful of facilities almost all below 1 kg yr-1. The figure's job is to make
# that smallness legible, which is itself a finding for AEP-001 (weak measured
# point-source signal). Releases in kg; NOT comparable with the REACH commerce
# tonnages (see R/fct_reach_hammerfest.R).
#
# Kommune match, not a coordinate join: the PRTR files carry no coordinates.
# "Hammerfest|Kvalsund" because Hammerfest absorbed Kvalsund in the 2020
# municipal reform and the pre-2020 Ulveryggen (Nussir catchment) deponi rows
# would otherwise be lost. See filter_prtr_kommune() in R/fct_prtr_emissions.R.

#' Copper Releases from PRTR Facilities in Hammerfest Kommune, Per Year
#'
#' @param prtr Output of [read_prtr_long()].
#' @param kommune Regex passed to [filter_prtr_kommune()].
#' @return A tibble: `facility`, `source_category`, `medium`, `year`, `kg`
#'   (summed within facility x category x medium x year), sorted by medium then
#'   facility then year.
#' @export
prtr_hammerfest_series <- function(prtr, kommune = "Hammerfest|Kvalsund") {
  filter_prtr_kommune(prtr, kommune) |>
    dplyr::summarise(
      kg = sum(.data$value_kg, na.rm = TRUE),
      .by = c("facility", "source_category", "medium", "year")
    ) |>
    dplyr::arrange(.data$medium, .data$facility, .data$year)
}

#' Hammerfest PRTR Copper Releases (ggplot)
#'
#' Line and dot, `kg` on a log10 y-axis, x = year, colour = facility, shape =
#' medium, one line per facility x medium. Non-positive values (reported zeros)
#' cannot sit on a log axis and are dropped. Returns the plot object;
#' [write_hammerfest_emissions_panel()] composes it with the REACH panel and
#' writes the file.
#'
#' @param series Output of [prtr_hammerfest_series()].
#' @return A ggplot object.
#' @export
plot_prtr_hammerfest <- function(series) {
  d <- series[!is.na(series$kg) & series$kg > 0, ]
  # Drop the parenthetical operator company from the label; keep the rest of
  # the facility name as reported.
  d$facility_short <- sub(" \\(.*\\)$", "", d$facility)

  ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = .data$year, y = .data$kg,
      colour = .data$facility_short, shape = .data$medium,
      group = interaction(.data$facility_short, .data$medium)
    )
  ) +
    ggplot2::geom_line(linewidth = 0.4) +
    ggplot2::geom_point(size = 2.6) +
    ggplot2::scale_y_log10(labels = function(x) formatC(x, format = "fg")) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(2)) +
    ggplot2::scale_shape_manual(values = c(Air = 17, Water = 16), name = NULL) +
    ggplot2::scale_colour_brewer(palette = "Set2", name = "Facility") +
    ggplot2::labs(x = NULL, y = "Copper released (kg/yr, log scale)") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "right", legend.box = "vertical")
}
