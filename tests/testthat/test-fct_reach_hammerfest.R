# Weighting the national REACH product-register series down to Hammerfest by
# NACE-section employment share (R/fct_reach_hammerfest.R). Small synthetic
# fixtures; the real data is only 6 years and one dominant sector.

shares_fixture <- function() {
  tibble::tibble(
    nace_section = c("A", "C", "E", "TOTAL"),
    hammerfest_share = c(0.004, 0.0012, 0.003, 0.0021),
    norway = c(65000, 220000, 19000, 2870000),
    hammerfest = c(260, 264, 57, 6000)
  )
}

sector_years_fixture <- function() {
  tibble::tibble(
    sector_en = rep(
      c("Manufacturing", "Agriculture, forestry and fishing", "Unclassified"),
      times = 4
    ),
    isic_nace_section = rep(c("C", "A", NA_character_), times = 4),
    year = rep(c(2019L, 2020L, 2021L, 2022L), each = 3),
    # 2019-2021 ~ 50k tonnes; 2022 collapses to ~5k -> flagged incomplete
    net_kg = c(
      50e6, 1.5e6, 3e4, # 2019
      52e6, 1.4e6, 4e4, # 2020
      51e6, 1.6e6, 2e4, # 2021
      4.5e6, 0.2e6, 1e4 # 2022
    )
  )
}

test_that("read_ssb_section_shares converts percent to proportion", {
  path <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(
    nace_section = c("A", "TOTAL"),
    section_label = c("Agriculture", "All"),
    divisions = c("01, 02, 03", "00-99"),
    norway = c(65000, 2870000),
    hammerfest = c(260, 6000),
    hammerfest_share_of_national_pct = c(0.4, 0.214)
  ), path)

  ss <- read_ssb_section_shares(path)
  expect_equal(ss$hammerfest_share[ss$nace_section == "A"], 0.004)
  expect_equal(ss$hammerfest_share[ss$nace_section == "TOTAL"], 0.00214)
})

test_that("reach_years_complete flags the collapsed years", {
  y <- reach_years_complete(sector_years_fixture())
  expect_equal(y$year, 2019:2022)
  expect_equal(y$complete, c(TRUE, TRUE, TRUE, FALSE))
})

test_that("weight_reach_to_hammerfest multiplies by the section share", {
  w <- weight_reach_to_hammerfest(sector_years_fixture(), shares_fixture())

  mf <- w[w$sector_en == "Manufacturing" & w$year == 2019, ]
  expect_equal(mf$hammerfest_share, 0.0012)
  expect_equal(mf$hammerfest_net_kg, 50e6 * 0.0012)

  ag <- w[w$sector_en == "Agriculture, forestry and fishing" & w$year == 2020, ]
  expect_equal(ag$hammerfest_net_kg, 1.4e6 * 0.004)
})

test_that("weight_reach_to_hammerfest falls back to TOTAL for a sectorless row", {
  w <- weight_reach_to_hammerfest(sector_years_fixture(), shares_fixture())
  un <- w[w$sector_en == "Unclassified" & w$year == 2019, ]
  expect_equal(un$hammerfest_share, 0.0021) # the TOTAL row
  expect_equal(un$hammerfest_net_kg, 3e4 * 0.0021)
})

test_that("weight_reach_to_hammerfest errors without a usable TOTAL row", {
  bad <- shares_fixture()
  bad$hammerfest_share[bad$nace_section == "TOTAL"] <- NA
  expect_error(
    weight_reach_to_hammerfest(sector_years_fixture(), bad),
    "TOTAL"
  )
})

test_that("reach_hammerfest_plot_data drops incomplete years and tiny sectors", {
  w <- weight_reach_to_hammerfest(sector_years_fixture(), shares_fixture())
  pd <- reach_hammerfest_plot_data(w, min_kg = 1)

  expect_false(2022L %in% pd$year) # incomplete
  expect_true(all(pd$hammerfest_net_kg > 0))
  # Unclassified maxes at 4e4 * 0.0021 = 84 kg -> kept at min_kg = 1
  expect_true("Unclassified" %in% pd$sector_en)

  pd2 <- reach_hammerfest_plot_data(w, min_kg = 200)
  expect_false("Unclassified" %in% pd2$sector_en)
  expect_true("Unclassified" %in% attr(pd2, "dropped_sectors"))
})

test_that("plot_reach_hammerfest returns a ggplot over the kept sectors", {
  w <- weight_reach_to_hammerfest(sector_years_fixture(), shares_fixture())
  p <- plot_reach_hammerfest(w, min_kg = 1)
  expect_s3_class(p, "ggplot")
  expect_true(all(p$data$hammerfest_net_kg > 0))
  expect_false(2022L %in% p$data$year)
})
