# Weight the national REACH product-register copper series down to Hammerfest by
# each industry sector's share of national employment (place of residence, SSB
# 08536), for index.qmd's "Norwegian PRTR and REACH Product Register" section.
#
# This is deliberately crude, and the manuscript says so: a sector's copper
# throughput is not proportional to its head-count. It is a first-order scaling
# to bound the magnitude of copper embedded in products entering the Hammerfest
# economy, nothing more. REACH "net quantity" is (imported + produced) minus
# exported: copper in COMMERCE, tonnes, NOT an environmental release, and not
# comparable with anything from the PRTR (see R/fct_prtr_emissions.R).
#
# The employment shares come from data/clean/derived/
# ssb_employment_hammerfest_sections.csv, written by
# scripts/summarise_ssb_employment.R (not the pipeline). Re-run that script
# before tar_make if the SSB figures move.

#' Read the NACE-Section Hammerfest Employment Shares
#'
#' @param path CSV written by `scripts/summarise_ssb_employment.R`'s section
#'   rollup. One row per NACE 2007 section A-U plus a `TOTAL` row.
#' @return A tibble: `nace_section`, `hammerfest_share` (proportion, not
#'   percent), `norway`, `hammerfest`.
#' @export
read_ssb_section_shares <- function(path) {
  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::transmute(
      .data$nace_section,
      hammerfest_share = .data$hammerfest_share_of_national_pct / 100,
      .data$norway,
      .data$hammerfest
    )
}

#' Which REACH Reporting Years Look Complete
#'
#' Same rule as [reach_complete_years()] but computed from the sector-year
#' table (which carries `net_kg`) rather than the raw declarations: a year is
#' complete where its national net total is at least `frac` of the median
#' across all years. 2018-2021 run 56-65 kt; 2022-2023 run 8-10 kt and fail.
#'
#' @param sector_years Output of [read_reach_sector_years()].
#' @param frac Fraction of the median below which a year is suspect.
#' @return A tibble: `year`, `net_kg_total`, `complete`.
#' @export
reach_years_complete <- function(sector_years, frac = 0.5) {
  totals <- sector_years |>
    dplyr::summarise(net_kg_total = sum(.data$net_kg, na.rm = TRUE), .by = "year") |>
    dplyr::arrange(.data$year)
  totals$complete <- totals$net_kg_total >= frac * stats::median(totals$net_kg_total)
  totals
}

#' Weight the REACH Sector-Year Series to Hammerfest
#'
#' Joins each REACH sector to its NACE-section Hammerfest employment share and
#' multiplies. A sector with no section (the `"Unclassified"` REACH row, whose
#' `isic_nace_section` is `NA`) is weighted by the all-industry share (the
#' `TOTAL` row of `section_shares`).
#'
#' @param sector_years Output of [read_reach_sector_years()]: `sector_en`,
#'   `isic_nace_section`, `year`, `net_kg`.
#' @param section_shares Output of [read_ssb_section_shares()].
#' @return `sector_years` plus `hammerfest_share`, `hammerfest_net_kg`, and a
#'   `complete` flag (from [reach_years_complete()]). Sorted by year then
#'   descending national `net_kg`.
#' @export
weight_reach_to_hammerfest <- function(sector_years, section_shares) {
  fallback <- section_shares$hammerfest_share[section_shares$nace_section == "TOTAL"]
  if (length(fallback) != 1L || is.na(fallback)) {
    stop("section_shares needs exactly one non-NA TOTAL row")
  }
  years <- reach_years_complete(sector_years)

  sector_years |>
    dplyr::left_join(
      section_shares[c("nace_section", "hammerfest_share")],
      by = c("isic_nace_section" = "nace_section")
    ) |>
    dplyr::mutate(
      hammerfest_share = dplyr::coalesce(.data$hammerfest_share, fallback),
      hammerfest_net_kg = .data$net_kg * .data$hammerfest_share
    ) |>
    dplyr::left_join(years[c("year", "complete")], by = "year") |>
    dplyr::arrange(.data$year, dplyr::desc(.data$net_kg))
}

#' Trim the Weighted REACH Series to What is Worth Plotting
#'
#' Keeps complete reporting years and strictly positive weighted values (a
#' log axis takes neither incomplete years nor net-exporter negatives), then
#' drops any sector whose largest weighted value across those years is below
#' `min_kg`. Those sectors are genuinely negligible (a whole NACE section
#' implying under a kilogram of copper a year in Hammerfest); their names are
#' returned as an attribute so the caption can list them.
#'
#' @param weighted Output of [weight_reach_to_hammerfest()].
#' @param min_kg Sector-max threshold, kg/yr. Default 1.
#' @param complete_only Keep only rows flagged `complete`. Default `TRUE`.
#' @return The kept rows, with `attr(., "dropped_sectors")` a character vector.
#' @export
reach_hammerfest_plot_data <- function(weighted, min_kg = 1, complete_only = TRUE) {
  d <- weighted
  if (complete_only) d <- d[d$complete %in% TRUE, ]
  d <- d[!is.na(d$hammerfest_net_kg) & d$hammerfest_net_kg > 0, ]

  keep <- d |>
    dplyr::summarise(mx = max(.data$hammerfest_net_kg), .by = "sector_en") |>
    dplyr::filter(.data$mx >= min_kg)

  dropped <- setdiff(unique(d$sector_en), keep$sector_en)
  out <- d[d$sector_en %in% keep$sector_en, ]
  attr(out, "dropped_sectors") <- sort(dropped)
  out
}

#' Implied Hammerfest REACH Copper, Per Year, by Sector (ggplot)
#'
#' Line and dot, one series per REACH sector kept by
#' [reach_hammerfest_plot_data()], `hammerfest_net_kg` on a log10 y-axis.
#' Returns the plot object; [write_hammerfest_emissions_panel()] composes it
#' with the PRTR panel and writes the file.
#'
#' @param weighted Output of [weight_reach_to_hammerfest()].
#' @param min_kg Passed to [reach_hammerfest_plot_data()].
#' @return A ggplot object.
#' @export
plot_reach_hammerfest <- function(weighted, min_kg = 1) {
  d <- reach_hammerfest_plot_data(weighted, min_kg = min_kg)
  d$sector_en <- stats::reorder(d$sector_en, -d$hammerfest_net_kg)

  ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = .data$year, y = .data$hammerfest_net_kg,
      colour = .data$sector_en, group = .data$sector_en
    )
  ) +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_y_log10(
      labels = function(x) formatC(x, format = "fg", big.mark = ",")
    ) +
    ggplot2::scale_x_continuous(breaks = sort(unique(d$year))) +
    ggplot2::scale_colour_brewer(palette = "Dark2", name = NULL) +
    ggplot2::labs(x = NULL, y = "Implied Hammerfest copper (kg/yr, log scale)") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "right")
}
