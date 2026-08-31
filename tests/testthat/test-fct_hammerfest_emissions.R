# The two-panel Hammerfest emissions figure (R/fct_hammerfest_emissions.R).

test_that("write_hammerfest_emissions_panel writes a non-empty PNG", {
  skip_if_not_installed("patchwork")

  prtr_series <- tibble::tibble(
    facility = c("Hammerfest LNG", "Hammerfest LNG", "SAR AS avd. Hammerfest"),
    source_category = "Land-based industry",
    medium = c("Air", "Air", "Water"),
    year = c(2022L, 2023L, 2014L),
    kg = c(2.47, 19.1, 4.58)
  )
  weighted <- tibble::tibble(
    sector_en = "Manufacturing",
    isic_nace_section = "C",
    year = 2018:2021,
    net_kg = 5e7,
    hammerfest_share = 0.0012,
    hammerfest_net_kg = 5e7 * 0.0012,
    complete = TRUE
  )

  path <- withr::local_tempfile(fileext = ".png")
  out <- write_hammerfest_emissions_panel(weighted, prtr_series, path)
  expect_equal(out, path)
  expect_true(file.exists(path))
  expect_gt(file.info(path)$size, 1000)
})
