# The Hammerfest emissions figure for index.qmd's "Norwegian PRTR and REACH
# Product Register" section.
#
# Previously two panels (REACH net commerce tonnes vs PRTR released kg -- see
# R/fct_reach_hammerfest.R); the REACH/Product Register panel was dropped
# 2026-09-14 (Sam's call) so this now writes the PRTR panel alone. The REACH
# weighting pipeline (reach_hammerfest_weighted, ssb_employment_sections*)
# still exists in _targets.R but no longer feeds this figure.

#' Hammerfest Emissions Figure (one PNG)
#'
#' Writes [plot_prtr_hammerfest()] to `path`.
#'
#' @param series Output of [prtr_hammerfest_series()].
#' @param path Output PNG path; written and returned, for a `format = "file"`
#'   target.
#' @return `path`.
#' @export
write_hammerfest_emissions_panel <- function(series, path) {
  panel <- plot_prtr_hammerfest(series)

  ggplot2::ggsave(path, panel, width = 7, height = 4, dpi = 300)
  path
}
