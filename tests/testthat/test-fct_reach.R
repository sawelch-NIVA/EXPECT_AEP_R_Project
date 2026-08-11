# REACH sector data as AEP source nodes (2026-08-11). See the header of
# R/fct_reach.R for why this exists as package functions rather than qmd-local
# code: docs/NBXX-REACH.qmd and _targets.R both need the same lumping, and two
# copies is how they drift.

test_that("reach_nace_sectors() has one row per sector, no duplicate names", {
  d <- reach_nace_sectors()
  expect_false(any(duplicated(d$sector_en)))
  expect_true("Unclassified" %in% d$sector_en)
  # The Unclassified row has no Norwegian name to translate FROM.
  expect_true(is.na(d$sector_no[d$sector_en == "Unclassified"]))
})

test_that("read_reach_sector_years() reads the real bundled REACH file", {
  # No synthetic fixture here: the real file already ships with the package
  # (inst/extdata/emissions/REACH_copper_prtd.xlsx) and is small, so reading
  # it directly is fast and does not depend on the target store -- unlike
  # most of this project's tests, which build synthetic fixtures specifically
  # to avoid that dependency, this one has nothing to be dependent ON.
  d <- read_reach_sector_years()
  expect_true(all(c("sector_en", "year", "net_kg") %in% names(d)))
  expect_gt(nrow(d), 0)
  expect_false(any(is.na(d$sector_en)))
  expect_true("Manufacturing" %in% d$sector_en)
})

test_that("reach_node_sectors() lumps a sector into its node, standalone sectors pass through", {
  years <- tibble::tibble(
    sector_en = c("Construction", "Real estate activities", "Manufacturing"),
    year = c(2020L, 2020L, 2020L),
    net_kg = c(10, 20, 1000)
  )
  out <- reach_node_sectors(years)
  expect_equal(
    out$node_sector[out$sector_en %in% c("Construction", "Real estate activities")],
    rep("Construction and real estate", 2)
  )
  expect_equal(
    out$node_id[out$sector_en %in% c("Construction", "Real estate activities")],
    rep("N009-construction-and-real-estate", 2)
  )
  expect_equal(out$node_sector[out$sector_en == "Manufacturing"], "Manufacturing")
  expect_equal(out$node_id[out$sector_en == "Manufacturing"], "N004-manufacturing")
})

test_that("reach_node_sectors() leaves node_id NA for a sector that maps to no node", {
  years <- tibble::tibble(
    sector_en = "Not a real NACE sector",
    year = 2020L,
    net_kg = 5
  )
  out <- reach_node_sectors(years)
  expect_true(is.na(out$node_id))
  # node_sector still falls back to the sector's own name, not NA, so it can
  # still be inspected/plotted even though it belongs to no node.
  expect_equal(out$node_sector, "Not a real NACE sector")
})

test_that("every lumped sector name resolves to a real node_id", {
  # Consistency guard between reach_sector_lump_map() and
  # reach_node_id_by_sector(): editing one without the other should fail a
  # test, not silently drop a lumped sector's rows out of every node.
  lumped_into <- unique(unname(reach_sector_lump_map()))
  expect_true(all(lumped_into %in% names(reach_node_id_by_sector())))
})

test_that("there are exactly eight REACH source nodes", {
  expect_length(reach_node_id_by_sector(), 8)
  expect_false(any(duplicated(reach_node_id_by_sector())))
})

test_that("reach_node_summary() computes mean, sd and n per node", {
  years <- tibble::tibble(
    sector_en = c("Manufacturing", "Manufacturing", "Manufacturing"),
    year = c(2019L, 2020L, 2021L),
    net_kg = c(10, 20, 30)
  )
  out <- reach_node_summary(reach_node_sectors(years))
  expect_equal(nrow(out), 1)
  expect_equal(out$node_id, "N004-manufacturing")
  expect_equal(out$mean_net_kg, 20)
  expect_equal(out$sd_net_kg, stats::sd(c(10, 20, 30)))
  expect_equal(out$n_years_reported, 3)
})

test_that("reach_node_summary() pools a lumped node's sectors into one row", {
  years <- tibble::tibble(
    sector_en = c("Construction", "Real estate activities"),
    year = c(2020L, 2020L),
    net_kg = c(10, 30)
  )
  out <- reach_node_summary(reach_node_sectors(years))
  expect_equal(nrow(out), 1)
  expect_equal(out$node_id, "N009-construction-and-real-estate")
  expect_equal(out$mean_net_kg, 20)
  expect_equal(out$n_years_reported, 2)
})

test_that("reach_node_summary() drops rows with no node_id", {
  years <- tibble::tibble(
    sector_en = c("Manufacturing", "Not a real sector"),
    year = c(2020L, 2020L),
    net_kg = c(10, 999999)
  )
  out <- reach_node_summary(reach_node_sectors(years))
  expect_equal(nrow(out), 1)
  expect_equal(out$node_id, "N004-manufacturing")
})

test_that("reach_external_series() returns one (year, value) tibble per node_id", {
  years <- tibble::tibble(
    sector_en = c("Manufacturing", "Manufacturing", "Mining and quarrying"),
    year = c(2019L, 2020L, 2019L),
    net_kg = c(10, 20, 5)
  )
  out <- reach_external_series(reach_node_sectors(years))
  expect_setequal(names(out), c("N004-manufacturing", "N006-mining-and-quarrying"))
  expect_equal(nrow(out[["N004-manufacturing"]]), 2)
  expect_setequal(names(out[["N004-manufacturing"]]), c("year", "value"))
  expect_equal(out[["N004-manufacturing"]]$value, c(10, 20))
})

test_that("reach_external_series() drops rows with a missing value", {
  years <- tibble::tibble(
    sector_en = c("Manufacturing", "Manufacturing"),
    year = c(2019L, 2020L),
    net_kg = c(10, NA_real_)
  )
  out <- reach_external_series(reach_node_sectors(years))
  expect_equal(nrow(out[["N004-manufacturing"]]), 1)
  expect_equal(out[["N004-manufacturing"]]$year, 2019)
})
