# The two-panel Hammerfest emissions figure for index.qmd's "Norwegian PRTR
# and REACH Product Register" section.
#
# Panel a and panel b are DIFFERENT QUANTITIES and share no axis: (a) is copper
# in commerce (REACH net quantity, tonnes) scaled to Hammerfest by employment
# share; (b) is copper released to the environment (PRTR, kg). They sit in one
# figure because they answer the same question -- what does the national
# product/release record imply for Hammerfest -- not because the numbers are
# comparable. See R/fct_reach_hammerfest.R and R/fct_prtr_hammerfest.R.
#
# Panel composition (patchwork) is deliberate here: CLAUDE.md 4.4 reserves it
# for manuscript figures, which this is.

#' Two-Panel Hammerfest Emissions Figure (one PNG)
#'
#' Stacks [plot_reach_hammerfest()] (a) over [plot_prtr_hammerfest()] (b),
#' tagged `a`/`b`, and writes the file.
#'
#' @param weighted Output of [weight_reach_to_hammerfest()].
#' @param series Output of [prtr_hammerfest_series()].
#' @param path Output PNG path; written and returned, for a `format = "file"`
#'   target.
#' @param min_kg Passed to [plot_reach_hammerfest()].
#' @return `path`.
#' @export
write_hammerfest_emissions_panel <- function(
  weighted,
  series,
  path,
  min_kg = 1
) {
  panel <- patchwork::wrap_plots(
    plot_reach_hammerfest(weighted, min_kg = min_kg),
    plot_prtr_hammerfest(series),
    ncol = 1
  ) +
    patchwork::plot_annotation(tag_levels = "a")

  ggplot2::ggsave(path, panel, width = 8, height = 5, dpi = 300)
  path
}
