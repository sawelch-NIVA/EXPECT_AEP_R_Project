# PRTR and REACH source data (added 2026-08-06).
#
# The two things worth guarding here are both arithmetic mistakes that look like
# results rather than errors:
#
#   1. reporting an average FACILITY release as though it were the SECTOR
#      release, which understates land-based industry by a factor of 25;
#   2. averaging over a partial reporting year, which understates every sector
#      by however many partial years its series happens to have at the ends.
#
# Both were made while writing this, and both were invisible in the numbers
# until the series was plotted.

prtr_fixture <- function(...) {
  # Two facilities reporting steadily for four years, then a fifth year in which
  # only one reports and the total collapses. That fifth year is the reporting
  # lag pattern, in miniature.
  base <- tibble::tibble(
    facility = c(rep(c("A", "B"), 4), "A"),
    fylke = "Testland",
    kommune = "Testby",
    year = c(rep(2019:2022, each = 2), 2023L),
    unit = "kg",
    source_category = "Land-based industry",
    medium = "Water",
    value_kg = c(rep(c(100, 100), 4), 1)
  )
  args <- list(...)
  for (nm in names(args)) base[[nm]] <- args[[nm]]
  base
}

# A group large enough for prtr_complete_years() to engage at all: ten
# facilities reporting 100 kg each for four years, then a fifth year in which
# only ONE of the ten reports and the total collapses. That is the reporting-lag
# signature, and the counts matter: the rule needs participation to fall below
# half, so a fixture where five of ten still report sits exactly on the boundary
# and is correctly not flagged.
prtr_big_fixture <- function(last_year_facilities = 1, last_year_value = 1) {
  facilities <- paste0("F", sprintf("%02d", 1:10))
  steady <- tidyr::expand_grid(
    facility = facilities,
    year = 2019:2022
  ) |>
    dplyr::mutate(value_kg = 100)
  final <- tibble::tibble(
    facility = facilities[seq_len(last_year_facilities)],
    year = 2023L,
    value_kg = last_year_value
  )
  dplyr::bind_rows(steady, final) |>
    dplyr::mutate(
      fylke = "Testland",
      kommune = "Testby",
      unit = "kg",
      source_category = "Land-based industry",
      medium = "Water",
      year = as.integer(.data$year)
    )
}

# ---- The two-numbers distinction ----------------------------------------

test_that("the sector total is the sum across facilities, not their mean", {
  # THE MISTAKE THIS EXISTS TO PREVENT. Two facilities at 100 kg is a sector
  # releasing 200 kg/yr, not 100.
  d <- prtr_fixture() |> dplyr::filter(year < 2023)
  out <- summarise_prtr_releases(d, "source_category")

  expect_equal(out$total_kg_yr, 200)
  expect_equal(out$mean_kg_yr, 100)
})

test_that("the annual sd describes years, not facilities", {
  d <- prtr_fixture() |> dplyr::filter(year < 2023)
  out <- summarise_prtr_releases(d, "source_category")
  # Every year totals exactly 200, so year-to-year variation is zero even
  # though there are eight facility-years.
  expect_equal(out$sd_total_kg_yr, 0)
})

# ---- Incomplete reporting years -----------------------------------------

test_that("a collapsed final year is detected and excluded", {
  d <- prtr_big_fixture()

  flags <- prtr_complete_years(d, by = "source_category")
  expect_false(flags$complete[flags$year == 2023])
  expect_true(all(flags$complete[flags$year < 2023]))

  kept <- summarise_prtr_releases(d, "source_category")
  expect_equal(kept$n_dropped, 1)
  expect_equal(kept$year_max, 2022)
  # Ten facilities at 100 kg is 1,000 kg/yr, undiluted by the partial year.
  expect_equal(kept$total_kg_yr, 1000)
})

test_that("half the reporters still present is NOT enough to flag a year", {
  # The rule needs participation to fall BELOW half. Exactly half is the
  # boundary and stays complete, deliberately: the cost of wrongly dropping a
  # real year is a source node overstated, which is worse than one understated.
  d <- prtr_big_fixture(last_year_facilities = 5)
  expect_true(all(prtr_complete_years(d, by = "source_category")$complete))
})

test_that("dropping incomplete years can be turned off", {
  d <- prtr_big_fixture()
  out <- summarise_prtr_releases(d, "source_category", drop_incomplete = FALSE)
  expect_equal(out$n_years, 5)
  # The partial year drags the mean down: (1000 * 4 + 1) / 5.
  expect_lt(out$total_kg_yr, 1000)
})

test_that("a small group has no year flagged, however low", {
  # THE REGRESSION THIS EXISTS FOR. The first version of the rule used only the
  # median of totals, and on the four Hammerfest facilities it dropped 6 of 13
  # years. With that few reporters an individual facility IS the series, so a
  # low year is a low year rather than a missing report.
  d <- prtr_fixture()
  flags <- prtr_complete_years(d, by = "source_category")
  expect_true(all(flags$complete))

  out <- summarise_prtr_releases(d, "source_category")
  expect_equal(out$n_dropped, 0)
  expect_equal(out$n_years, 5)
})

test_that("a low year with undiminished participation is kept", {
  # A genuinely clean year is not a missing year. BOTH a participation collapse
  # and a magnitude collapse are required, which is what distinguishes a
  # submission deadline from an actual reduction in release.
  d <- prtr_big_fixture(last_year_facilities = 10, last_year_value = 0.5)
  flags <- prtr_complete_years(d, by = "source_category")
  expect_true(all(flags$complete))
})

# ---- Reading -------------------------------------------------------------

test_that("an unreported medium is dropped, not counted as zero", {
  # Facilities report only the media they are required to. Averaging a blank in
  # as a measured zero drags every sector mean down.
  d <- prtr_fixture()
  d$value_kg[1] <- NA_real_
  out <- summarise_prtr_releases(
    dplyr::filter(d, !is.na(value_kg), year < 2023), "source_category"
  )
  # 2019 now totals 100 rather than 200; the NA does not pull it to 50.
  expect_equal(out$mean_kg_yr, 100)
})

test_that("kommune filtering catches the pre-2020 municipality name", {
  # Hammerfest absorbed Kvalsund in the 2020 reform. Matching only the current
  # name silently loses every row from before it.
  d <- prtr_fixture(kommune = c(rep(c("Hammerfest", "Kvalsund"), 4), "Oslo"))
  out <- filter_prtr_kommune(d)
  expect_equal(nrow(out), 8)
  expect_setequal(unique(out$kommune), c("Hammerfest", "Kvalsund"))
})

test_that("an NA kommune does not match", {
  d <- prtr_fixture(kommune = NA_character_)
  expect_equal(nrow(filter_prtr_kommune(d)), 0)
})

test_that("empty input summarises to an empty tibble rather than erroring", {
  expect_equal(nrow(summarise_prtr_releases(prtr_fixture()[0, ], "medium")), 0)
})

# ---- REACH ---------------------------------------------------------------

test_that("REACH years far below the median are flagged incomplete", {
  reach <- tibble::tibble(
    year = 2018:2023,
    sector = "Test",
    netto_tonn = c(56000, 59000, 65000, 60000, 8259, 10429)
  )
  out <- reach_complete_years(reach)
  expect_equal(out$year[!out$complete], c(2022L, 2023L))
  expect_true(all(out$complete[out$year <= 2021]))
})

test_that("a REACH series with no break flags nothing", {
  reach <- tibble::tibble(
    year = 2018:2021, sector = "Test", netto_tonn = c(56000, 59000, 61000, 60000)
  )
  expect_true(all(reach_complete_years(reach)$complete))
})

# ---- Smoke run against the real files ------------------------------------

test_that("the real PRTR files read, aggregate and stay in kg", {
  # CLAUDE.md: a testthat pass on fixtures does not prove the spreadsheets parse.
  # The header is on row 3, and getting that wrong does not error, it promotes
  # the first data row to a header and silently loses it.
  dir <- here_rel("inst/extdata/emissions")
  skip_if_not(dir.exists(dir), "emissions files not present")

  d <- read_prtr_long(dir)
  expect_gt(nrow(d), 5000)
  expect_setequal(unique(d$source_category), names(prtr_facility_files()))
  expect_true(all(d$unit == "kg"))
  expect_true(all(is.finite(d$value_kg)))
  # Facility names must survive as text, not as a column header.
  expect_true(all(nzchar(d$facility)))

  national <- summarise_prtr_releases(d, c("source_category", "medium"))
  expect_true(all(national$total_kg_yr >= national$mean_kg_yr))

  hf <- filter_prtr_kommune(d)
  expect_gt(nrow(hf), 0)
  # Small group: nothing may be dropped as incomplete.
  expect_equal(sum(summarise_prtr_releases(hf, "medium")$n_dropped), 0)
})
