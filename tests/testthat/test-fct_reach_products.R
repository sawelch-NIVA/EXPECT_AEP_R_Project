# REACH copper by product-use category (R/fct_reach_products.R, added 2026-09-03).

test_that("the translation table has no duplicate keys and matches the schema", {
  tr <- reach_product_category_translations()
  expect_setequal(names(tr), c("category_no", "category_en", "confidence", "note"))
  expect_equal(anyDuplicated(tr$category_no), 0L)
  expect_true(all(tr$confidence %in% c("high", "medium", "low")))
})

test_that("reach_product_summary lumps to n_lump + 'Other' and reports distinct years", {
  py <- tibble::tibble(
    category_en = c(rep("A", 3), rep("B", 3), rep("C", 2), rep("D", 2)),
    year = c(2018:2020, 2018:2020, 2018:2019, 2018:2019),
    netto_tonn = c(100, 100, 100, 10, 10, 10, 1, 1, 0.5, 0.5),
    net_kg = c(100, 100, 100, 10, 10, 10, 1, 1, 0.5, 0.5) * 1000
  )
  s <- reach_product_summary(py, n_lump = 2L)
  expect_setequal(as.character(s$category_en), c("A", "B", "Other"))
  # C + D fold into Other; two distinct years across them
  expect_equal(s$n_years_reported[s$category_en == "Other"], 2L)
  expect_equal(s$n_years_reported[s$category_en == "A"], 3L)
  # ordered by mean_net_kg desc
  expect_equal(as.character(s$category_en[1]), "A")
})

test_that("a single-year category gets NA sd (no whisker)", {
  py <- tibble::tibble(
    category_en = c("A", "A", "Solo"),
    year = c(2018, 2019, 2018),
    netto_tonn = c(5, 7, 3),
    net_kg = c(5, 7, 3) * 1000
  )
  s <- reach_product_summary(py, n_lump = 5L)
  expect_true(is.na(s$sd_net_kg[s$category_en == "Solo"]))
  expect_false(is.na(s$sd_net_kg[s$category_en == "A"]))
})

test_that("reach_product_year_series sums per category-year and keeps the lump", {
  py <- tibble::tibble(
    category_en = c("A", "A", "B", "C", "C", "D"),
    year = c(2018, 2019, 2018, 2018, 2018, 2019),
    netto_tonn = c(10, 10, 1, 0.2, 0.3, 0.1),
    net_kg = c(10, 10, 1, 0.2, 0.3, 0.1) * 1000
  )
  ys <- reach_product_year_series(py, n_lump = 2L)
  expect_setequal(as.character(unique(ys$category_en)), c("A", "B", "Other"))
  # C + D -> Other; the two C rows in 2018 are summed
  expect_equal(ys$net_kg[ys$category_en == "Other" & ys$year == 2018], 500)
})

test_that("reach_product_palette gives Other grey and last, rest Dark2", {
  pal <- reach_product_palette(c("Big", "Mid", "Other", "Small"))
  expect_equal(names(pal)[length(pal)], "Other")
  expect_equal(unname(pal["Other"]), "grey75")
  expect_false("grey75" %in% unname(pal[c("Big", "Mid", "Small")]))
})

test_that("reach_product_plot and the two-panel figure build", {
  s <- tibble::tibble(
    category_en = factor(c("A", "B", "Other")),
    mean_net_kg = c(1000, 100, 10),
    sd_net_kg = c(200, NA, 5),
    n_years_reported = c(6L, 1L, 6L)
  )
  p <- reach_product_plot(s)
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true(all(c("GeomCol", "GeomErrorbar") %in% geoms))

  py <- tibble::tibble(
    category_en = rep(c("A", "B", "C"), each = 3),
    year = rep(2018:2020, 3),
    netto_tonn = c(100, 90, 110, 5, 6, 7, 1, 1, 1),
    net_kg = c(100, 90, 110, 5, 6, 7, 1, 1, 1) * 1000
  )
  fig <- reach_product_figure(py, n_lump = 2L)
  expect_s3_class(fig, "patchwork")
})

test_that("the real REACH sheet reads, translates fully, and writes a figure", {
  path <- here::here("inst/extdata/emissions/REACH_copper_prtd.xlsx")
  skip_if_not(file.exists(path))
  py <- read_reach_product_years(path)
  expect_gt(nrow(py), 0)
  # every raw product_type resolved to an English name
  expect_equal(sum(is.na(py$category_en)), 0L)

  s <- reach_product_summary(py)
  expect_true("Other" %in% as.character(s$category_en))

  out <- withr::local_tempdir()
  f <- write_reach_product_figure(py, path = file.path(out, "x.png"),
                                  width = 8, height = 7, dpi = 72)
  expect_true(file.exists(f))
})
