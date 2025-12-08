# Tests ----

test_that("impute_below_limits uses correct priority order", {
  df <- data.frame(
    measured = c(5, NA, NA, NA),
    lod = c(NA, 2, NA, 4),
    loq = c(NA, NA, 3, 5)
  )

  result <- impute_below_limits(df, "measured", "lod", "loq")

  # Measured value used when available
  expect_equal(result$MEASURED_OR_IMPUTED_VALUE[1], 5)
  # LOD imputed when no measured value
  expect_equal(result$MEASURED_OR_IMPUTED_VALUE[2], 2 / sqrt(2))
  # LOQ imputed when no measured or LOD
  expect_equal(result$MEASURED_OR_IMPUTED_VALUE[3], 3 / sqrt(2))
  # LOD takes priority over LOQ
  expect_equal(result$MEASURED_OR_IMPUTED_VALUE[4], 4 / sqrt(2))
})

test_that("impute_below_limits accepts custom imputation function", {
  df <- data.frame(
    measured = c(NA, NA),
    lod = c(10, NA),
    loq = c(NA, 20)
  )

  # Use half instead of sqrt(2)
  result <- impute_below_limits(
    df,
    "measured",
    "lod",
    "loq",
    impute_fn = function(x) x / 2
  )

  expect_equal(result$MEASURED_OR_IMPUTED_VALUE[1], 5)
  expect_equal(result$MEASURED_OR_IMPUTED_VALUE[2], 10)
})

test_that("impute_below_limits returns NA when all values missing", {
  df <- data.frame(
    measured = c(NA, NA),
    lod = c(NA, NA),
    loq = c(NA, NA)
  )

  result <- impute_below_limits(df, "measured", "lod", "loq")

  expect_true(all(is.na(result$MEASURED_OR_IMPUTED_VALUE)))
})
