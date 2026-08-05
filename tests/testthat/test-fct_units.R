# Unit parsing and standardisation.
#
# The regression these exist for: the old implementation read only the numerator
# prefix and assumed the denominator was kg or L, so `ug/g` (which IS mg/kg) was
# divided by 1000. 93 rows across 11 references came out a thousandfold low, and
# nothing in the pipeline noticed because the standard unit it wrote alongside
# the wrong value was correct.

test_that("ug/g is mg/kg, a 1:1 conversion", {
  # THE BUG. Micro over gram cancels against milli over kilogram exactly.
  for (u in c("μg/g (dry)", "µg/g (dry)", "ug/g (dry)")) {
    p <- parse_measured_unit(u)
    expect_equal(p$factor, 1, info = u)
    expect_equal(p$standard, "mg/kg (dry)", info = u)
  }
})

test_that("ug/kg is still a thousandth, which is what made the bug invisible", {
  p <- parse_measured_unit("μg/kg (dry)")
  expect_equal(p$factor, 1e-3)
  expect_equal(p$standard, "mg/kg (dry)")
})

test_that("numerator and denominator are read separately", {
  # A table rather than one case at a time, because the failure mode was a rule
  # that happened to be right for the common units and wrong for the rest.
  cases <- list(
    list("mg/kg (wet)", 1, "mg/kg (wet)"),
    list("g/kg (dry)", 1e3, "mg/kg (dry)"),
    list("ng/g (wet)", 1e-3, "mg/kg (wet)"),
    list("ng/kg (dry)", 1e-6, "mg/kg (dry)"),
    list("mg/g (dry)", 1e3, "mg/kg (dry)"),
    list("mg/L", 1, "mg/L"),
    list("µg/L", 1e-3, "mg/L"),
    list("ng/L", 1e-6, "mg/L"),
    list("mg/mL", 1e3, "mg/L"),
    list("g/L", 1e3, "mg/L")
  )
  for (case in cases) {
    p <- parse_measured_unit(case[[1]])
    expect_equal(p$factor, case[[2]], info = case[[1]])
    expect_equal(p$standard, case[[3]], info = case[[1]])
  }
})

test_that("a mass ratio with no basis is refused rather than guessed", {
  # Dry and wet weight differ by a factor of four or five in biota and are not
  # interconvertible without a moisture content this project does not hold.
  p <- parse_measured_unit("mg/kg")
  expect_true(is.na(p$standard))
  expect_true(is.na(p$factor))
  expect_match(p$reason, "no \\(dry\\) or \\(wet\\)")
})

test_that("unparseable units come back with a reason, not a bare NA", {
  p <- parse_measured_unit(c("%", "µM", "", NA, "Other", "furlongs/fortnight"))
  expect_true(all(is.na(p$factor)))
  expect_equal(p$reason[1], "not a ratio of two units")
  expect_equal(p$reason[2], "not a ratio of two units")
  expect_equal(p$reason[3], "empty or missing")
  expect_equal(p$reason[4], "empty or missing")
  expect_equal(p$reason[5], "marked Other")
  expect_match(p$reason[6], "unrecognised numerator")
})

test_that("all three micro codepoints parse identically", {
  units <- c("μg/L", "µg/L", "ug/L")
  p <- parse_measured_unit(units)
  expect_equal(p$factor, rep(1e-3, 3))
  expect_equal(p$standard, rep("mg/L", 3))
})

test_that("a corrupted micro sign is repaired rather than dropped", {
  # 18 rows of 2000JulshamnTraceElementLevels, seal tissue, values 0.8 to 18.
  # As ug/g those are ordinary; as ng/g or mg/g they are absurd. The old code
  # returned NA and the reference was lost in silence.
  p <- parse_measured_unit("�g/g (wet)")
  expect_equal(p$standard, "mg/kg (wet)")
  expect_equal(p$factor, 1)
})

test_that("a replacement character NOT in prefix position still fails", {
  # The repair is deliberately narrow: it only fires where the character sits
  # immediately before a "g". Anything else stays unparseable and gets reported.
  p <- parse_measured_unit("mg/k�")
  expect_true(is.na(p$factor))
  expect_match(p$reason, "unrecognised denominator")
})

# ---- The wrapper --------------------------------------------------------

unit_fixture <- function() {
  data.frame(
    MEASURED_UNIT = c(
      "mg/kg (dry)", "μg/g (dry)", "µg/L",
      "μg/kg (dry)", "%", "Other"
    ),
    MEASURED_VALUE = c(10, 10, 10, 10, 10, 10),
    stringsAsFactors = FALSE
  )
}

test_that("standardise_measured_units converts on the parsed factor", {
  out <- suppressWarnings(standardise_measured_units(
    unit_fixture(),
    value_columns = "MEASURED_VALUE",
    unit_column = "MEASURED_UNIT"
  ))
  expect_equal(
    out$MEASURED_VALUE_STANDARD,
    c(10, 10, 0.01, 0.01, NA, NA)
  )
  expect_equal(
    out$MEASURED_UNIT_STANDARD,
    c("mg/kg (dry)", "mg/kg (dry)", "mg/L", "mg/kg (dry)", NA, NA)
  )
})

test_that("the standard unit and the factor cannot disagree", {
  # The structural property, asserted directly. Previously they came from two
  # independent case_when()s over the same string.
  out <- suppressWarnings(standardise_measured_units(
    unit_fixture(),
    value_columns = "MEASURED_VALUE",
    unit_column = "MEASURED_UNIT"
  ))
  has_unit <- !is.na(out$MEASURED_UNIT_STANDARD)
  has_value <- !is.na(out$MEASURED_VALUE_STANDARD)
  expect_equal(has_unit, has_value)
})

test_that("an unusable unit is warned about, with its name and row count", {
  # Silence is how the 1000x error survived. `%` is genuinely unconvertible, so
  # it must be reported rather than quietly dropped.
  expect_warning(
    standardise_measured_units(
      unit_fixture(),
      value_columns = "MEASURED_VALUE",
      unit_column = "MEASURED_UNIT"
    ),
    "could not be converted"
  )
})

test_that("Other and empty units are dropped without a warning", {
  # Both are deliberate markers, not surprises.
  d <- data.frame(
    MEASURED_UNIT = c("mg/L", "Other", "", NA),
    MEASURED_VALUE = c(1, 2, 3, 4)
  )
  expect_no_warning(
    standardise_measured_units(
      d,
      value_columns = "MEASURED_VALUE",
      unit_column = "MEASURED_UNIT"
    )
  )
})

test_that("a repaired micro sign warns separately from an unusable unit", {
  d <- data.frame(
    MEASURED_UNIT = c("�g/g (wet)", "mg/L"),
    MEASURED_VALUE = c(1, 2)
  )
  expect_warning(
    standardise_measured_units(
      d,
      value_columns = "MEASURED_VALUE",
      unit_column = "MEASURED_UNIT"
    ),
    "corrupted micro sign"
  )
})

test_that("remove_other still drops Other rows", {
  out <- suppressWarnings(standardise_measured_units(
    unit_fixture(),
    value_columns = "MEASURED_VALUE",
    unit_column = "MEASURED_UNIT",
    remove_other = TRUE
  ))
  expect_false("Other" %in% out$MEASURED_UNIT)
  expect_equal(nrow(out), nrow(unit_fixture()) - 1)
})

test_that("multiple value columns share one parse", {
  d <- data.frame(
    MEASURED_UNIT = c("μg/g (dry)", "µg/L"),
    MEASURED_VALUE = c(10, 10),
    UNCERTAINTY_UPPER = c(20, 20),
    UNCERTAINTY_LOWER = c(5, 5)
  )
  out <- standardise_measured_units(
    d,
    value_columns = c("MEASURED_VALUE", "UNCERTAINTY_UPPER", "UNCERTAINTY_LOWER"),
    unit_column = "MEASURED_UNIT"
  )
  expect_equal(out$MEASURED_VALUE_STANDARD, c(10, 0.01))
  expect_equal(out$UNCERTAINTY_UPPER_STANDARD, c(20, 0.02))
  expect_equal(out$UNCERTAINTY_LOWER_STANDARD, c(5, 0.005))
})

test_that("the units this project already held are unchanged", {
  # Guards the rewrite: 90,053 of 90,164 rows must convert exactly as before, or
  # this was not a bug fix but a data change.
  unchanged <- c("mg/L", "mg/kg (dry)", "mg/kg (wet)", "µg/L", "μg/kg (dry)")
  expected_factor <- c(1, 1, 1, 1e-3, 1e-3)
  p <- parse_measured_unit(unchanged)
  expect_equal(p$factor, expected_factor)
})
