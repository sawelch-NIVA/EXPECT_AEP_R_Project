# PRTR copper releases from Hammerfest-kommune facilities
# (R/fct_prtr_hammerfest.R). Synthetic fixture shaped like read_prtr_long()
# output.

prtr_fixture <- function() {
  tibble::tibble(
    facility = c(
      "Hammerfest LNG", "Hammerfest LNG",
      "SAR AS avd. Hammerfest",
      "Grotnes deponi (Finnmark ressursselskap as)",
      "Elsewhere plant"
    ),
    fylke = c("Finnmark", "Finnmark", "Finnmark", "Finnmark", "Vestland"),
    kommune = c("Hammerfest", "Hammerfest", "Kvalsund", "Hammerfest", "Bergen"),
    year = c(2022L, 2023L, 2014L, 2020L, 2020L),
    unit = "kg",
    source_category = c(
      "Land-based industry", "Land-based industry", "Land-based industry",
      "Landfills", "Land-based industry"
    ),
    medium = c("Air", "Air", "Water", "Water", "Water"),
    value_kg = c(2.47, 19.1, 4.58, 1.2, 999)
  )
}

test_that("prtr_hammerfest_series keeps Hammerfest + Kvalsund and drops the rest", {
  s <- prtr_hammerfest_series(prtr_fixture())
  expect_false("Elsewhere plant" %in% s$facility)
  expect_true("SAR AS avd. Hammerfest" %in% s$facility) # Kvalsund, pre-2020
  expect_equal(nrow(s), 4L)
  expect_equal(sum(s$kg), 2.47 + 19.1 + 4.58 + 1.2)
})

test_that("prtr_hammerfest_series sums within facility x category x medium x year", {
  d <- dplyr::bind_rows(
    prtr_fixture(),
    tibble::tibble(
      facility = "Hammerfest LNG", fylke = "Finnmark", kommune = "Hammerfest",
      year = 2022L, unit = "kg", source_category = "Land-based industry",
      medium = "Air", value_kg = 0.53
    )
  )
  s <- prtr_hammerfest_series(d)
  air22 <- s$kg[s$facility == "Hammerfest LNG" & s$year == 2022 & s$medium == "Air"]
  expect_equal(air22, 2.47 + 0.53)
})

test_that("plot_prtr_hammerfest returns a ggplot and drops reported zeros", {
  d <- dplyr::bind_rows(
    prtr_fixture(),
    tibble::tibble(
      facility = "SAR AS avd. Hammerfest", fylke = "Finnmark",
      kommune = "Kvalsund", year = 2019L, unit = "kg",
      source_category = "Land-based industry", medium = "Water", value_kg = 0
    )
  )
  s <- prtr_hammerfest_series(d)
  p <- plot_prtr_hammerfest(s)
  expect_s3_class(p, "ggplot")
  expect_true(all(p$data$kg > 0))
  expect_false(any(p$data$year == 2019 & p$data$facility_short == "SAR AS avd. Hammerfest"))
})
