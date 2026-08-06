# Reading and aggregating the Norwegian PRTR (norske utslipp) emission files, and
# the REACH declaration file. Added 2026-08-06.
#
# NOT WIRED INTO THE PIPELINE, and that is PLAN.md section 10: integrating the
# emissions and REACH data is deferred until after submission. These functions
# exist in the package rather than inside a script because
# scripts/summarise_prtr_emissions.R and docs/NBXX-emissions-prtr.qmd both need
# them, and two copies of a spreadsheet reshaping are two chances to reshape it
# differently.
#
# TWO SOURCES, TWO KINDS OF NUMBER, NEVER TO BE ADDED TOGETHER.
#
#   * norske_utslipp_*.xlsx -- Norwegian PRTR. RELEASES in kg/yr, per facility,
#     to air / water / subsurface. Carries Fylke and Kommune, so it restricts to
#     an AEP region.
#   * REACH_copper_prtd.xlsx -- declared quantities, "Netto mengde" = imported +
#     produced - exported, in TONNES/yr. Copper in COMMERCE, not released. It is
#     upstream of any emission and belongs on a different node.

#' The PRTR Facility Files and What They Cover
#'
#' @return A named character vector, source category to filename.
#' @export
prtr_facility_files <- function() {
  c(
    "Land-based industry" = "norske_utslipp_copper_land_industries_individ.xlsx",
    "Landfills" = "norske_utslipp_copper_landfills_individ.xlsx",
    "Offshore oil and gas" = "norske_utslipp_oil_marine.xlsx",
    "Waste water treatment" = "norske_utslipp_water_treatment.xlsx"
  )
}

#' Release Media, Norwegian Column Name to English Label
#'
#' Not every file has every medium, so these are selected by name where present
#' rather than by position.
#'
#' @return A named character vector.
#' @export
prtr_media <- function() {
  c(
    "Air" = "Årlig utslipp til luft",
    "Water" = "Årlig utslipp til vann",
    "Subsurface" = "Årlig utslipp til undergrunn"
  )
}

#' Read One PRTR Facility File
#'
#' The files put a title in row 1, a blank in row 2, and the real header in
#' row 3, hence `skip = 2`. Getting this wrong does not error: it promotes the
#' first data row to a header and silently loses it, which is what happened on
#' the first attempt and showed up only as column names full of facility names.
#'
#' @param path Full path to the xlsx.
#' @param category Label for the `source_category` column.
#' @return A tibble, one row per facility-year, in the file's own wide shape.
#' @export
read_prtr_file <- function(path, category) {
  if (!file.exists(path)) {
    stop("Missing PRTR file: ", path)
  }
  x <- suppressMessages(readxl::read_excel(path, skip = 2))
  x$source_category <- category
  x
}

#' Every PRTR Facility File, in Long Format
#'
#' One row per facility per year per medium.
#'
#' **A blank cell is "not reported", not zero**, and is dropped rather than
#' averaged in as a zero. Facilities report only the media they are required to,
#' and treating an unreported medium as a measured zero would drag every sector
#' mean down by however many facilities happen not to report it.
#'
#' @param dir Directory holding the xlsx files.
#' @param files Named character vector, category to filename.
#' @return A long tibble: facility, fylke, kommune, year, unit, source_category,
#'   medium, value_kg.
#' @export
read_prtr_long <- function(
  dir = here_rel("inst/extdata/emissions"),
  files = prtr_facility_files()
) {
  media <- prtr_media()

  out <- purrr::list_rbind(purrr::imap(files, function(file, category) {
    x <- read_prtr_file(file.path(dir, file), category)
    have <- media[media %in% names(x)]
    if (length(have) == 0) {
      return(NULL)
    }

    d <- x |>
      dplyr::select(
        facility = "Anleggsnavn",
        fylke = "Fylke",
        kommune = dplyr::any_of("Kommune"),
        year = "År",
        unit = dplyr::any_of("Enhet"),
        dplyr::all_of(unname(have)),
        "source_category"
      ) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(unname(have)),
        names_to = "medium_no",
        values_to = "value"
      ) |>
      dplyr::mutate(
        medium = names(have)[match(.data$medium_no, have)],
        year = suppressWarnings(as.integer(.data$year)),
        value_kg = suppressWarnings(as.numeric(.data$value))
      ) |>
      dplyr::filter(!is.na(.data$value_kg), !is.na(.data$year)) |>
      dplyr::select(-"medium_no", -"value")

    if (!"kommune" %in% names(d)) {
      d$kommune <- NA_character_
    }
    d
  }))

  # A tonne read as a kilogram is the same 1000x class of fault that cost two
  # days in PLAN.md 9b, and it is invisible once aggregated. Stop, do not warn.
  units <- unique(stats::na.omit(out$unit))
  if (length(units) > 0 && !identical(sort(units), "kg")) {
    stop(
      "PRTR files report more than one unit: ", paste(units, collapse = ", "),
      ". Convert before aggregating."
    )
  }

  out
}

#' Aggregate PRTR Releases
#'
#' **Two different numbers, and confusing them is the easiest mistake here.**
#'
#' * `total_kg_yr` -- sum across facilities within a year, then averaged over
#'   years. This is the annual release of the source, and it is what an AEP
#'   source node's magnitude should be.
#' * `mean_kg_yr` -- mean of individual facility-year values. Describes a typical
#'   facility, not the sector.
#'
#' The first version of this reported only the second and labelled it the sector
#' release. For land-based industry to water that is 149 kg/yr against a true
#' national total of 12,848: a factor of 25, in the direction that makes a source
#' look negligible.
#'
#' `sd_total_kg_yr` is the spread of the ANNUAL TOTALS, so it describes year to
#' year variation in the sector, which is the uncertainty an external node wants
#' beside its magnitude. `sd_kg_yr` is the spread between facility-years and is
#' much larger, because facility sizes span orders of magnitude.
#'
#' @param d Output of [read_prtr_long()], optionally pre-filtered.
#' @param by Character vector of grouping columns.
#' @param drop_incomplete Exclude years flagged by [prtr_complete_years()]?
#'   `TRUE` by default, because including them is wrong rather than merely
#'   cautious. See that function.
#' @param frac Passed to [prtr_complete_years()].
#' @return A tibble, one row per group, ordered by `total_kg_yr`. `n_dropped` is
#'   how many years were excluded as incomplete.
#' @export
summarise_prtr_releases <- function(d, by, drop_incomplete = TRUE, frac = 0.25) {
  if (nrow(d) == 0) {
    return(tibble::tibble())
  }

  per_year <- d |>
    dplyr::summarise(
      year_total_kg = sum(.data$value_kg),
      facilities = dplyr::n_distinct(.data$facility),
      .by = dplyr::all_of(c(by, "year"))
    )

  dropped <- per_year[0, c(by, "year"), drop = FALSE]
  if (drop_incomplete) {
    flags <- prtr_complete_years(d, by = by, frac = frac)
    per_year <- per_year |>
      dplyr::left_join(
        flags[, c(by, "year", "complete")],
        by = c(by, "year")
      )
    dropped <- per_year[!per_year$complete, , drop = FALSE]
    per_year <- per_year[per_year$complete, , drop = FALSE]
    d <- d |>
      dplyr::anti_join(dropped[, c(by, "year")], by = c(by, "year"))
  }

  n_dropped <- dropped |>
    dplyr::summarise(n_dropped = dplyr::n(), .by = dplyr::all_of(by))

  totals <- per_year |>
    dplyr::summarise(
      total_kg_yr = mean(.data$year_total_kg),
      sd_total_kg_yr = stats::sd(.data$year_total_kg),
      n_years = dplyr::n_distinct(.data$year),
      n_facilities = max(.data$facilities),
      year_min = min(.data$year),
      year_max = max(.data$year),
      .by = dplyr::all_of(by)
    )

  per_facility <- d |>
    dplyr::summarise(
      mean_kg_yr = mean(.data$value_kg),
      sd_kg_yr = stats::sd(.data$value_kg),
      n_rows = dplyr::n(),
      .by = dplyr::all_of(by)
    )

  totals |>
    dplyr::left_join(per_facility, by = by) |>
    dplyr::left_join(n_dropped, by = by) |>
    dplyr::mutate(n_dropped = dplyr::coalesce(.data$n_dropped, 0L)) |>
    dplyr::arrange(dplyr::desc(.data$total_kg_yr))
}

#' Years Where PRTR Reporting Looks Complete
#'
#' **The most recent year is routinely partial**, because facilities report on a
#' lag. In the 2026-08-06 extract, land-based industry falls from roughly 7,000
#' kg to water in 2023 to 70 kg in 2024, and from 2,000 kg to air to 10 kg. That
#' is a hundredfold drop in a single year across a hundred-odd facilities, which
#' is a submission deadline rather than an environmental event. The same thing
#' happens at the START of a series, where a reporting obligation is phasing in:
#' landfills report 0.5 kg nationally in 2009 and 0.2 kg in 2010, then 200 kg
#' from 2011.
#'
#' **Including those years is wrong, not merely cautious.** They enter the mean
#' as if they were low-emission years, so every source node magnitude comes out
#' understated by however many partial years the series happens to have at its
#' ends.
#'
#' Detected rather than hard-coded, on the same principle as
#' [reach_complete_years()]: a year is suspect where its group total falls below
#' `frac` of that group's median year. A hard-coded cut-off year would be wrong
#' the moment the files are refreshed, and silently so.
#'
#' `frac` is deliberately low. The failures this catches are one to three orders
#' of magnitude, so 0.25 separates them from ordinary year-to-year variation
#' without needing to be tuned.
#'
#' **THREE CONDITIONS, ALL REQUIRED**, and the first two exist because the naive
#' version was actively harmful on small groups. A median-of-totals rule applied
#' to the four Hammerfest facilities dropped 6 of 13 years: with that few
#' reporters, individual facility variation *is* the series, so a low year is a
#' low year and not a missing report. The rule now:
#'
#' 1. the group must typically have at least `min_facilities` reporters, or
#'    completeness is not inferable and every year is kept;
#' 2. this year's reporter count must have collapsed relative to the group's
#'    median count;
#' 3. this year's total must have collapsed relative to the group's median total.
#'
#' Requiring both a participation collapse and a magnitude collapse is what
#' distinguishes a submission deadline from a genuinely clean year.
#'
#' @param d Output of [read_prtr_long()].
#' @param by Grouping columns within which completeness is judged.
#' @param frac Fraction of the group's median total below which a year is
#'   suspect.
#' @param facility_frac Fraction of the group's median reporter count below
#'   which participation counts as collapsed.
#' @param min_facilities Below this typical reporter count, completeness is not
#'   inferable and no year is flagged.
#' @return A tibble of group, year, total_kg, n_facilities and `complete`.
#' @export
prtr_complete_years <- function(
  d,
  by = c("source_category", "medium"),
  frac = 0.25,
  facility_frac = 0.5,
  min_facilities = 5
) {
  totals <- d |>
    dplyr::summarise(
      total_kg = sum(.data$value_kg),
      n_facilities = dplyr::n_distinct(.data$facility),
      .by = dplyr::all_of(c(by, "year"))
    )

  totals |>
    dplyr::mutate(
      .med_total = stats::median(.data$total_kg),
      .med_fac = stats::median(.data$n_facilities),
      .by = dplyr::all_of(by)
    ) |>
    dplyr::mutate(
      complete = !(
        .data$.med_fac >= min_facilities &
          .data$n_facilities < facility_frac * .data$.med_fac &
          .data$total_kg < frac * .data$.med_total
      )
    ) |>
    dplyr::select(-".med_total", -".med_fac") |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(by, "year"))))
}

#' Annual National Totals, for a Time Series
#'
#' Kept separate from [summarise_prtr_releases()] because a trend needs the
#' per-year values rather than their average. **Reporting completeness changes
#' over time**, so a fall in a sector's total is at least as likely to be fewer
#' facilities reporting as less copper released; `n_facilities` is returned
#' alongside so the two can be told apart.
#'
#' @param d Output of [read_prtr_long()].
#' @param by Extra grouping columns beyond `year`.
#' @return A tibble, one row per year per group.
#' @export
prtr_annual_totals <- function(d, by = c("source_category", "medium")) {
  d |>
    dplyr::summarise(
      total_kg = sum(.data$value_kg),
      n_facilities = dplyr::n_distinct(.data$facility),
      .by = dplyr::all_of(c("year", by))
    ) |>
    dplyr::arrange(.data$year)
}

#' Restrict PRTR Rows to a Region
#'
#' Kommune rather than a coordinate join, because the PRTR files carry no
#' coordinates and the kommune-to-point join is unbuilt (PLAN.md section 10).
#' The A002 bounding box sits inside Hammerfest kommune, which absorbed Kvalsund
#' in the 2020 municipal reform, so both names must match or the pre-2020 rows
#' are silently lost.
#'
#' @param d Output of [read_prtr_long()].
#' @param kommune Regex matched case-insensitively against `kommune`.
#' @return The matching rows.
#' @export
filter_prtr_kommune <- function(d, kommune = "Hammerfest|Kvalsund") {
  d[!is.na(d$kommune) & grepl(kommune, d$kommune, ignore.case = TRUE), ,
    drop = FALSE
  ]
}

#' Read the REACH Declaration File
#'
#' Copper **in commerce**, tonnes per year: imported + produced - exported for
#' declared chemicals. Not a release, and not comparable with anything from
#' [read_prtr_long()].
#'
#' @param path Full path to the xlsx.
#' @return A tibble of year, sector, netto_tonn.
#' @export
read_reach_declarations <- function(
  path = here_rel("inst/extdata/emissions/REACH_copper_prtd.xlsx")
) {
  if (!file.exists(path)) {
    stop("Missing REACH file: ", path)
  }
  suppressMessages(
    readxl::read_excel(path, sheet = "Sum HovedgruppeAndvendelse")
  ) |>
    dplyr::rename(
      year = "AmountYear",
      netto_tonn = "Netto Mengde (tonn)",
      sector = "Beskrivelse"
    ) |>
    dplyr::mutate(
      year = suppressWarnings(as.integer(.data$year)),
      netto_tonn = suppressWarnings(as.numeric(.data$netto_tonn)),
      sector = dplyr::na_if(trimws(.data$sector), "Other")
    ) |>
    dplyr::filter(!is.na(.data$netto_tonn), !is.na(.data$year))
}

#' Years Where REACH Reporting Looks Complete
#'
#' **2018-2021 run 56-65 kt/yr and 2022-2023 run 8-10 kt.** That is a sixfold
#' step rather than a trend, and it is far more likely to be incomplete reporting
#' for recent years than a collapse in Norwegian copper imports. A mean across
#' both regimes describes neither.
#'
#' Detected rather than hard-coded, so the cut moves if the file is refreshed:
#' a year is flagged incomplete where its total is below `frac` of the median of
#' all years.
#'
#' @param reach Output of [read_reach_declarations()].
#' @param frac Fraction of the median below which a year is suspect.
#' @return A tibble of year, tonnes, and a `complete` flag.
#' @export
reach_complete_years <- function(reach, frac = 0.5) {
  totals <- reach |>
    dplyr::summarise(tonnes = sum(.data$netto_tonn), .by = "year") |>
    dplyr::arrange(.data$year)
  med <- stats::median(totals$tonnes)
  totals$complete <- totals$tonnes >= frac * med
  totals
}
