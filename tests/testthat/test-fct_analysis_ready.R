# Tests ----

test_that("drop_nonpositive_measurements removes NA, zero and negative values", {
  df <- data.frame(
    MEASURED_VALUE_STANDARD = c(5, NA, 0, -2, 0.001),
    other = letters[1:5]
  )

  result <- drop_nonpositive_measurements(df)

  expect_equal(result$MEASURED_VALUE_STANDARD, c(5, 0.001))
  expect_equal(result$other, c("a", "e"))
})

test_that("drop_nonpositive_measurements only looks at the value column", {
  # The whole point of this filter is that it is NOT a whole-row drop_na():
  # many eData columns are legitimately sparse, so a row with a good measured
  # value must survive even when everything else is missing.
  df <- data.frame(
    MEASURED_VALUE_STANDARD = c(5, 7),
    SAMPLE_TISSUE = c(NA, "Liver"),
    LOD_VALUE_STANDARD = c(NA, NA)
  )

  expect_equal(nrow(drop_nonpositive_measurements(df)), 2)
})

test_that("drop_nonpositive_measurements honours a custom value column", {
  df <- data.frame(a = c(1, -1), b = c(-1, 1))

  expect_equal(drop_nonpositive_measurements(df, "a")$a, 1)
  expect_equal(drop_nonpositive_measurements(df, "b")$b, 1)
})

test_that("drop_nonpositive_measurements handles an all-bad input", {
  df <- data.frame(MEASURED_VALUE_STANDARD = c(NA, 0, -1))

  expect_equal(nrow(drop_nonpositive_measurements(df)), 0)
})

test_that("report_dropped_measurements counts each reason separately", {
  df <- data.frame(
    ENVIRON_COMPARTMENT = rep("Aquatic", 6),
    MEASURED_VALUE_STANDARD = c(1, 2, NA, 0, -3, 4)
  )

  result <- report_dropped_measurements(
    df,
    group_cols = "ENVIRON_COMPARTMENT"
  )

  expect_equal(result$n_input, 6)
  expect_equal(result$n_na, 1)
  expect_equal(result$n_zero, 1)
  expect_equal(result$n_negative, 1)
  expect_equal(result$n_dropped, 3)
  expect_equal(result$n_retained, 3)
  expect_equal(result$prop_dropped, 0.5)
})

test_that("report_dropped_measurements retained count matches the filter", {
  df <- data.frame(
    ENVIRON_COMPARTMENT = c("A", "A", "B", "B"),
    MEASURED_VALUE_STANDARD = c(1, 0, NA, 2)
  )

  report <- report_dropped_measurements(df, group_cols = "ENVIRON_COMPARTMENT")
  kept <- drop_nonpositive_measurements(df)

  expect_equal(sum(report$n_retained), nrow(kept))
})

test_that("report_dropped_measurements ignores group columns absent from data", {
  df <- data.frame(MEASURED_VALUE_STANDARD = c(1, 0))

  expect_no_error(
    report_dropped_measurements(df, group_cols = c("NOT_A_COLUMN"))
  )
})
