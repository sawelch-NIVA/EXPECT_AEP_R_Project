# REACH sector data as AEP source nodes (2026-08-11).
#
# Pulled out of docs/NBXX-REACH.qmd into the package so the qmd's own tables
# and the pipeline's node cards read the SAME lumping, not two copies that can
# drift. The qmd built this inline first; _targets.R needed it too once the
# REACH source nodes' per-year series started feeding write_aep_node_cards()
# (Sam, 2026-08-11: "why create an entirely separate dir structure for source
# cards?" -- because this logic lived only in the qmd and the pipeline's own
# card target had no way to reach it).
#
# NOT A GROUP-ID SYSTEM. Sam considered and explicitly deferred giving REACH
# sectors their own ledger of IDs, analogous to but distinct from the G-codes
# in fct_group_ids.R (2026-08-11: "not now"). What follows is the minimum that
# unblocks the pipeline wiring: named-vector lookups, not a persisted,
# allocated id ledger. If that changes, the lump map below is the thing that
# becomes a hand-edited CSV.

#' Translate NACE Sector Names, Norwegian to English
#'
#' Norwegian `Beskrivelse` values, as they appear in the "Sum
#' HovedgruppeAndvendelse" sheet, to their NACE section letter and English
#' name. Based on
#' <https://ec.europa.eu/eurostat/documents/3859598/5902521/KS-RA-07-015-EN.PDF.pdf>
#' p143.
#'
#' @return A tibble: `sector_no` (Norwegian, `NA` for the unclassified row),
#'   `sector_en`, `isic_nace_section`.
#' @export
reach_nace_sectors <- function() {
  tibble::tribble(
    ~sector_no, ~sector_en, ~isic_nace_section,
    NA, "Unclassified", NA,
    "Jordbruk, skogbruk og fiske", "Agriculture, forestry and fishing", "A",
    "Bergverksdrift og utvinning", "Mining and quarrying", "B",
    "Industri", "Manufacturing", "C",
    "Elektrisitets-, gass-, damp- og varmtvannsforsyning",
    "Electricity, gas, steam and air conditioning supply", "D",
    "Vannforsyning, avløps- og renovasjonsvirksomhet",
    "Water supply, sewerage and waste management", "E",
    "Bygge- og anleggsvirksomhet", "Construction", "F",
    "Varehandel, reparasjon av motorvogner",
    "Wholesale and retail trade, repair of motor vehicles", "G",
    "Transport og lagring", "Transportation and storage", "H",
    "Omsetning og drift av fast eiendom", "Real estate activities", "L",
    "Faglig, vitenskapelig og teknisk tjenesteyting",
    "Professional, scientific and technical activities", "M",
    "Forretningsmessig tjenesteyting",
    "Administrative and support service activities", "N",
    "Kulturell virksomhet, underholdning og fritidsaktiviteter",
    "Arts, entertainment and recreation", "R",
    "Annen tjenesteyting", "Other service activities", "S"
  )
}

#' Read REACH Copper Net Quantities by Sector and Year
#'
#' One row per (sector, year) declaration. Net quantity is (imported +
#' produced) minus exported, per Miljødirektoratet's own description of the
#' column; see the header of `docs/NBXX-REACH.qmd` for the full provenance
#' note (personal communication, 2 May 2025).
#'
#' A raw `sector` cell reading literally `"Other"` is folded to `NA` before
#' translation, which then resolves to `"Unclassified"` via
#' [reach_nace_sectors()]'s `NA` row. This looks backwards and is kept
#' unchanged from where it was first written in the qmd: it is Sam's own
#' documented guess at how the source file represents a blank/unclassified
#' sector, flagged there as unconfirmed ("TODO: blank sector -> 'Other'"), not
#' a decision to revisit here.
#'
#' @param path Path to the REACH xlsx.
#' @return A tibble: `sector_en`, `isic_nace_section`, `year`, `net_kg`.
#'   `isic_nace_section` is carried through from [reach_nace_sectors()]
#'   rather than dropped, since `docs/NBXX-REACH.qmd`'s by-sector figure facets
#'   on the NACE primary/secondary/tertiary supercategory derived from it and
#'   would otherwise need its own separate join to the same lookup.
#' @export
read_reach_sector_years <- function(
  path = here_rel("inst/extdata/emissions/REACH_copper_prtd.xlsx")
) {
  raw <- readxl::read_excel(path, sheet = "Sum HovedgruppeAndvendelse") |>
    dplyr::rename(
      year = "AmountYear",
      netto_tonn = "Netto Mengde (tonn)",
      sector = "Beskrivelse"
    ) |>
    dplyr::mutate(
      year = as.integer(.data$year),
      netto_tonn = as.numeric(.data$netto_tonn),
      sector = dplyr::na_if(trimws(.data$sector), "Other")
    )

  raw |>
    dplyr::left_join(reach_nace_sectors(), by = c("sector" = "sector_no")) |>
    dplyr::transmute(
      .data$sector_en,
      .data$isic_nace_section,
      .data$year,
      net_kg = .data$netto_tonn * 1000
    )
}

#' Which Sectors Lump Into Which AEP Source Node
#'
#' Two lumps, decided 2026-08-11 on the grounds that eleven NACE sectors is
#' more resolution than `n_years_reported` as low as 3-6 can support:
#' Construction and Real estate share a node, and five service/administration
#' sectors plus Unclassified share another. The other six sectors stand alone.
#'
#' @return A named character vector, raw `sector_en` to the lumped
#'   `node_sector` label used by [reach_node_id_by_sector()].
#' @export
reach_sector_lump_map <- function() {
  c(
    "Construction" = "Construction and real estate",
    "Real estate activities" = "Construction and real estate",
    "Other service activities" = "Other services and administration",
    "Unclassified" = "Other services and administration",
    "Arts, entertainment and recreation" = "Other services and administration",
    "Professional, scientific and technical activities" =
      "Other services and administration",
    "Electricity, gas, steam and air conditioning supply" =
      "Other services and administration",
    "Administrative and support service activities" =
      "Other services and administration"
  )
}

#' The Eight REACH Source Nodes, `node_sector` to `node_id`
#'
#' Written out rather than derived from sort order (e.g. rank by mean
#' quantity): a future data refresh reordering the sectors by size would
#' silently repoint this at the wrong node otherwise. Matches
#' `data/clean/aep/aep_nodes.csv` `N004`-`N011`.
#'
#' @return A named character vector, `node_sector` to `node_id`.
#' @export
reach_node_id_by_sector <- function() {
  c(
    "Manufacturing" = "N004-manufacturing",
    "Agriculture, forestry and fishing" =
      "N005-agriculture-forestry-and-fishing",
    "Mining and quarrying" = "N006-mining-and-quarrying",
    "Other services and administration" =
      "N007-other-services-and-administration",
    "Wholesale and retail trade, repair of motor vehicles" =
      "N008-wholesale-and-retail-trade",
    "Construction and real estate" = "N009-construction-and-real-estate",
    "Transportation and storage" = "N010-transportation-and-storage",
    "Water supply, sewerage and waste management" =
      "N011-water-supply-and-waste-management"
  )
}

#' Attach `node_sector` and `node_id` to a REACH Sector-Year Table
#'
#' The lumping step shared by [reach_node_summary()] (which collapses it to
#' one row per node) and [reach_external_series()] (which keeps every year, for
#' the card bar chart). Kept as its own function rather than inlined into
#' either, so a table that stops at "which node does this row belong to" is
#' available on its own -- that is the table
#' `docs/NBXX-REACH.qmd`'s per-sector-by-year figure is drawn from.
#'
#' @param years Output of [read_reach_sector_years()].
#' @return `years` plus `node_sector` and `node_id`. A row whose sector maps to
#'   neither [reach_sector_lump_map()] nor [reach_node_id_by_sector()] directly
#'   keeps its own `sector_en` as `node_sector`; if that name is not one of the
#'   eight nodes either, `node_id` is `NA` and the row is not a member of any
#'   node (there is no ninth catch-all node).
#' @export
reach_node_sectors <- function(years = read_reach_sector_years()) {
  lump <- reach_sector_lump_map()
  ids <- reach_node_id_by_sector()
  years |>
    dplyr::mutate(
      node_sector = dplyr::if_else(
        .data$sector_en %in% names(lump),
        unname(lump[.data$sector_en]),
        .data$sector_en
      ),
      node_id = unname(ids[.data$node_sector])
    )
}

#' One Row per REACH Source Node: Mean, SD, Years Reported
#'
#' The table `data/clean/aep/aep_nodes.csv`'s `external_value`/`external_sd`/
#' `external_n` for `N004`-`N011` were typed in from. Averaged over every
#' (sector, year) row pooled into that node -- for a lumped node this means
#' several sectors' values in one year are separate rows going into the same
#' mean, not summed to a yearly total first. See [reach_external_series()] if
#' a per-year total is what's wanted instead.
#'
#' @param series Output of [reach_node_sectors()].
#' @return A tibble: `node_id`, `node_sector`, `mean_net_kg`, `sd_net_kg`,
#'   `n_years_reported`, arranged largest mean first.
#' @export
reach_node_summary <- function(series = reach_node_sectors()) {
  series |>
    dplyr::filter(!is.na(.data$node_id)) |>
    dplyr::reframe(
      mean_net_kg = mean(.data$net_kg),
      sd_net_kg = stats::sd(.data$net_kg),
      n_years_reported = dplyr::n(),
      .by = c("node_id", "node_sector")
    ) |>
    dplyr::arrange(dplyr::desc(.data$mean_net_kg))
}

#' The Per-Year Series Behind Each REACH Node's Headline, Keyed by `node_id`
#'
#' [write_node_cards()]'s `external_series` argument wants exactly this shape:
#' a named list of `(year, value)` tibbles, one element per node. Built here
#' rather than left for every caller to reshape by hand, since `_targets.R`
#' and `docs/NBXX-REACH.qmd` both need it in this form.
#'
#' @param series Output of [reach_node_sectors()].
#' @return A named list, `node_id` to a tibble with `year` and `value`
#'   columns.
#' @export
reach_external_series <- function(series = reach_node_sectors()) {
  d <- series |>
    dplyr::filter(!is.na(.data$node_id), !is.na(.data$net_kg)) |>
    dplyr::transmute(.data$node_id, .data$year, value = .data$net_kg)
  split(d[c("year", "value")], d$node_id)
}
