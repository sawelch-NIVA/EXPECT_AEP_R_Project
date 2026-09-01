# Tests ----
#
# PLAN.md P1.4 (ranking and flags) and P1.5 (the outlier denominator).

flag_summary <- function(...) {
  base <- data.frame(
    ENVIRON_COMPARTMENT = "Biota",
    ENVIRON_COMPARTMENT_SUB = "Biota, Aquatic",
    SPECIES_GROUP = "Fish",
    SAMPLE_SPECIES = "Gadus morhua",
    SAMPLE_TISSUE = "Liver",
    SITE_GEOGRAPHIC_FEATURE = "Coastal, fjord",
    SITE_GEOGRAPHIC_FEATURE_SUB = "Not reported",
    MEASURED_UNIT_STANDARD = "mg/kg (wet)",
    n = 1000L,
    n_sources = 4L,
    references = "RefA, RefB, RefC, RefD",
    date_min = as.Date("2001-01-01"),
    date_max = as.Date("2019-01-01"),
    sd = 5,
    mean = 10,
    n_double_outliers = 10L,
    n_outlier_rows = 2L,
    median = 9,
    unit = "mg/kg (wet)",
    dip_p = 0.4,
    multimodal = FALSE
  )
  modifyList(base, list(...)) |> as.data.frame()
}

# ---- Ranking -----------------------------------------------------------

test_that("add_triage_flags ranks by n descending", {
  out <- add_triage_flags(rbind(
    flag_summary(n = 100L, SAMPLE_SPECIES = "A"),
    flag_summary(n = 9000L, SAMPLE_SPECIES = "B"),
    flag_summary(n = 500L, SAMPLE_SPECIES = "C")
  ))
  expect_equal(out$n, c(9000L, 500L, 100L))
})

test_that("there are exactly two flags, and no CV", {
  # Guards against flags and summary statistics accreting without being asked
  # for. Both surviving flags predate this file; the 5% outlier fraction and the
  # dip test were already driving the summary table's highlighting.
  #
  # Removed 2026-07-30: a single-source flag (96% base rate), a drop-proportion
  # flag and a multi-unit flag (both asked for as columns, not warnings), and CV
  # (correlated 0.96 with max/median on this data, so it tracked the largest
  # single value rather than the spread, and was redundant with the reported
  # mean and sd). All remain available as columns except CV, which is gone.
  out <- add_triage_flags(flag_summary())
  expect_equal(
    sort(names(out)[startsWith(names(out), "flag_")]),
    c("flag_multimodal", "flag_outliers")
  )
  expect_false("cv" %in% names(out))
  # The inputs CV was derived from are still reported.
  expect_true(all(c("sd", "mean", "median") %in% names(out)))
})

test_that("units and drop proportion are columns, not flags", {
  dropped <- data.frame(
    ENVIRON_COMPARTMENT = "Biota",
    ENVIRON_COMPARTMENT_SUB = "Biota, Aquatic",
    SPECIES_GROUP = "Fish",
    SAMPLE_SPECIES = "Gadus morhua",
    SAMPLE_TISSUE = "Liver",
    SITE_GEOGRAPHIC_FEATURE = "Coastal, fjord",
    SITE_GEOGRAPHIC_FEATURE_SUB = "Not reported",
    MEASURED_UNIT_STANDARD = "mg/kg (wet)",
    n_dropped = 90L,
    prop_dropped = 0.9
  )
  out <- add_triage_flags(flag_summary(), dropped)
  # A 90% drop rate is reported and not flagged, because a drop-rate flag was
  # never asked for.
  expect_equal(out$prop_dropped, 0.9)
  expect_length(group_flag_text(out), 0)
})

# ---- P1.5: the outlier denominator -------------------------------------

test_that("outlier_fraction divides measurements by measurements", {
  # P1.5. The old table computed n_double_outliers (a count of ROWS) over n (a
  # count of MEASUREMENTS), so it systematically under-fired wherever
  # MEASURED_N > 1. n_double_outliers is now measurement-weighted upstream, so
  # this ratio is apples-to-apples.
  out <- add_triage_flags(flag_summary(n = 1000L, n_double_outliers = 80L))
  expect_equal(out$outlier_fraction, 0.08)
  expect_true(out$flag_outliers)
})

test_that("the outlier flag fires at just over five percent", {
  expect_false(
    add_triage_flags(flag_summary(n = 1000L, n_double_outliers = 50L))$flag_outliers
  )
  expect_true(
    add_triage_flags(flag_summary(n = 1000L, n_double_outliers = 51L))$flag_outliers
  )
})

test_that("the row count is retained alongside the weighted count", {
  # Kept so the P1.5 change is auditable rather than silent.
  out <- add_triage_flags(flag_summary(n_double_outliers = 10L, n_outlier_rows = 2L))
  expect_equal(out$n_double_outliers, 10L)
  expect_equal(out$n_outlier_rows, 2L)
})

# ---- Individual flags --------------------------------------------------

test_that("there is no single-source flag", {
  # Removed 2026-07-30. It was listed in PLAN.md P1.4, but Vannmiljo is one
  # REFERENCE_ID covering monitoring for the whole of Norway, so a single source
  # is the normal state of this dataset: the flag fired on 234 of 245 groups and
  # buried the four signals that do discriminate. n_sources is still reported.
  out <- add_triage_flags(flag_summary(n_sources = 1L))
  expect_false("flag_single_source" %in% names(out))
  expect_equal(out$n_sources, 1L)
  expect_length(group_flag_text(out), 0)
})

test_that("multimodal uses %in% TRUE so untested groups stay unflagged", {
  # A group below the dip test's minimum n gets NA, not FALSE, and must not leak
  # into the highlight set.
  expect_true(add_triage_flags(flag_summary(multimodal = TRUE))$flag_multimodal)
  expect_false(add_triage_flags(flag_summary(multimodal = NA))$flag_multimodal)
  expect_false(add_triage_flags(flag_summary(multimodal = FALSE))$flag_multimodal)
})

test_that("n_units counts unit variants of the same group, not units per group", {
  # Unit is part of the group key, so a per-group count is always 1. What matters
  # is whether the same species, tissue and place appears in another basis.
  both <- add_triage_flags(rbind(
    flag_summary(MEASURED_UNIT_STANDARD = "mg/kg (wet)"),
    flag_summary(MEASURED_UNIT_STANDARD = "mg/kg (dry)")
  ))
  expect_equal(unique(both$n_units), 2L)

  one <- add_triage_flags(flag_summary())
  expect_equal(one$n_units, 1L)
})

test_that("drop columns are NA without a drop report", {
  out <- add_triage_flags(flag_summary())
  expect_true(is.na(out$prop_dropped))
})

test_that("the drop report joins on the full group key", {
  dropped <- data.frame(
    ENVIRON_COMPARTMENT = "Biota",
    ENVIRON_COMPARTMENT_SUB = "Biota, Aquatic",
    SPECIES_GROUP = "Fish",
    SAMPLE_SPECIES = "Gadus morhua",
    SAMPLE_TISSUE = "Liver",
    SITE_GEOGRAPHIC_FEATURE = "Coastal, fjord",
    SITE_GEOGRAPHIC_FEATURE_SUB = "Not reported",
    MEASURED_UNIT_STANDARD = "mg/kg (wet)",
    n_dropped = 30L,
    prop_dropped = 0.3
  )
  out <- add_triage_flags(flag_summary(), dropped)
  expect_equal(out$prop_dropped, 0.3)

  # A group absent from the report gets NA rather than erroring or silently
  # reading as zero-dropped.
  other <- add_triage_flags(flag_summary(SAMPLE_SPECIES = "Clupea harengus"), dropped)
  expect_true(is.na(other$prop_dropped))
})

# ---- Flag text ---------------------------------------------------------

test_that("group_flag_text is empty when nothing is flagged", {
  expect_length(group_flag_text(add_triage_flags(flag_summary())), 0)
})

test_that("group_flag_text names every flag that fired, and nothing else", {
  row <- add_triage_flags(
    flag_summary(
      n_sources = 1L, multimodal = TRUE, dip_p = 0.002,
      n = 1000L, n_double_outliers = 120L
    ),
    data.frame(
      ENVIRON_COMPARTMENT = "Biota",
      ENVIRON_COMPARTMENT_SUB = "Biota, Aquatic",
      SPECIES_GROUP = "Fish",
      SAMPLE_SPECIES = "Gadus morhua",
      SAMPLE_TISSUE = "Liver",
      SITE_GEOGRAPHIC_FEATURE = "Coastal, fjord",
      SITE_GEOGRAPHIC_FEATURE_SUB = "Not reported",
      MEASURED_UNIT_STANDARD = "mg/kg (wet)",
      n_dropped = 20L,
      prop_dropped = 0.2
    )
  )
  txt <- group_flag_text(row)
  # Exactly two: multimodal and outliers. This row also has a single source and a
  # 20% drop rate, and neither is a flag.
  expect_length(txt, 2)
  expect_true(any(grepl("multimodal", txt)))
  expect_true(any(grepl("12% outliers", txt)))
  expect_false(any(grepl("single source", txt)))
  expect_false(any(grepl("dropped", txt)))
})

test_that("a dip p that underflows prints as a bound, not as zero", {
  # The test underflows to 0 on the large groups; a bare "p = 0" claims more than
  # the test can support.
  row <- add_triage_flags(flag_summary(multimodal = TRUE, dip_p = 0))
  expect_match(group_flag_text(row), "p < 0.001")
  row2 <- add_triage_flags(flag_summary(multimodal = TRUE, dip_p = 0.0087))
  expect_match(group_flag_text(row2), "p = 0.0087")
})

test_that("a small drop proportion does not round to a bare zero percent", {
  # "0%" reads as "none dropped" when it means "a few dropped".
  dropped <- data.frame(
    ENVIRON_COMPARTMENT = "Biota",
    ENVIRON_COMPARTMENT_SUB = "Biota, Aquatic",
    SPECIES_GROUP = "Fish",
    SAMPLE_SPECIES = "Gadus morhua",
    SAMPLE_TISSUE = "Liver",
    SITE_GEOGRAPHIC_FEATURE = "Coastal, fjord",
    SITE_GEOGRAPHIC_FEATURE_SUB = "Not reported",
    MEASURED_UNIT_STANDARD = "mg/kg (wet)",
    n_dropped = 1L,
    prop_dropped = 0.0002
  )
  # Below the flag threshold, so no text; the table label is where it shows.
  tbl <- build_sample_groups_table(add_triage_flags(flag_summary(), dropped))
  expect_equal(tbl$dropped_label, "<1%")
  # Exactly zero stays blank.
  dropped$prop_dropped <- 0
  tbl0 <- build_sample_groups_table(add_triage_flags(flag_summary(), dropped))
  expect_equal(tbl0$dropped_label, "")
})

test_that("group_summary_line reports the counts and the unit", {
  line <- group_summary_line(
    cbind(add_triage_flags(flag_summary(n = 2500L, n_sources = 3L)), n_rows = 400L)
  )
  expect_match(line, "2,500 measurements")
  expect_match(line, "400 rows")
  expect_match(line, "\\*\\*3\\*\\* sources")
  expect_match(line, "mg/kg \\(wet\\)")
  # Counts only. No summary statistic that would need justifying in the methods.
  expect_false(grepl("CV", line, fixed = TRUE))
})

test_that("group_summary_line singularises a lone source", {
  line <- group_summary_line(
    cbind(add_triage_flags(flag_summary(n_sources = 1L)), n_rows = 10L)
  )
  expect_match(line, "\\*\\*1\\*\\* source \\(")
  expect_false(grepl("1\\*\\* sources", line))
})

test_that("group_summary_line names the references when it has them", {
  # Whether a group is two Vannmiljø campaigns or two independent papers is what
  # a lump/split judgement turns on, and the old "(distinct REFERENCE_ID)"
  # wording could not say.
  row <- cbind(
    add_triage_flags(flag_summary(n_sources = 2L)),
    n_rows = 4L,
    reference_ids = "2003ZaukeHeavyMetalsOf, VannmiljoCopper2010-2025"
  )
  line <- group_summary_line(row)
  expect_match(line, "2003ZaukeHeavyMetalsOf, VannmiljoCopper2010-2025", fixed = TRUE)
  expect_false(grepl("distinct", line, fixed = TRUE))
})

test_that("group_summary_line falls back when reference_ids is missing", {
  # A caller holding a bare summary row, with no join against the measurements,
  # must still get a sentence rather than an empty "()".
  bare <- cbind(add_triage_flags(flag_summary(n_sources = 2L)), n_rows = 4L)
  expect_match(group_summary_line(bare), "distinct")

  empty <- cbind(bare, reference_ids = "")
  expect_match(group_summary_line(empty), "distinct")

  na_row <- cbind(bare, reference_ids = NA_character_)
  expect_match(group_summary_line(na_row), "distinct")
})

test_that("group_summary_line emits a callout only when something is flagged", {
  clean <- group_summary_line(
    cbind(add_triage_flags(flag_summary()), n_rows = 10L)
  )
  expect_false(grepl("callout-warning", clean, fixed = TRUE))

  flagged <- group_summary_line(
    cbind(add_triage_flags(flag_summary(multimodal = TRUE)), n_rows = 10L)
  )
  expect_match(flagged, "callout-warning")
  expect_match(flagged, "multimodal")
})

# ---- The table and the notebook must agree -----------------------------

test_that("the table highlights exactly what group_flag_text names", {
  # The whole reason both read add_triage_flags(): the yellow row fill and the
  # per-group prose must not be able to disagree about what is flagged.
  data <- rbind(
    flag_summary(SAMPLE_SPECIES = "A", multimodal = TRUE, dip_p = 0.01),
    flag_summary(SAMPLE_SPECIES = "B", n = 1000L, n_double_outliers = 200L),
    flag_summary(SAMPLE_SPECIES = "C")
  )
  flagged <- add_triage_flags(data)
  tbl <- build_sample_groups_table(flagged)

  for (i in seq_len(nrow(flagged))) {
    row <- flagged[i, , drop = FALSE]
    txt <- group_flag_text(row)
    match <- tbl[tbl$group == paste0("Fish › ", row$SAMPLE_SPECIES, " › Liver"), ]
    expect_equal(
      match$.is_multimodal,
      any(grepl("multimodal", txt)),
      info = row$SAMPLE_SPECIES
    )
    expect_equal(
      match$.is_outlier,
      any(grepl("outliers", txt)),
      info = row$SAMPLE_SPECIES
    )
  }
})

test_that("group_summary_line leads with the common name when there is one", {
  row <- cbind(add_triage_flags(flag_summary()), n_rows = 100L)
  row$species_common_name <- "Atlantic cod"
  expect_match(group_summary_line(row), "^\\*\\*Atlantic cod\\*\\*\\. `n` =")
})

test_that("group_summary_line omits the preamble when there is no common name", {
  # Non-biota groups, and species with no English vernacular.
  row <- cbind(add_triage_flags(flag_summary()), n_rows = 100L)
  row$species_common_name <- NA_character_
  expect_match(group_summary_line(row), "^`n` =")

  # Column absent entirely, e.g. an older summary table.
  row$species_common_name <- NULL
  expect_match(group_summary_line(row), "^`n` =")
})

test_that("flag text degrades gracefully when optional columns are absent", {
  # Callers carry different subsets. The group decisions table has the flags but
  # not dip_p, outlier_fraction or n_rows, and `row$missing[1]` is length zero,
  # which used to error inside is.na() and is.finite() rather than degrade.
  row <- data.frame(
    group_id = "G001",
    n = 100L,
    n_sources = 2L,
    MEASURED_UNIT_STANDARD = "mg/kg (wet)",
    flag_multimodal = TRUE,
    flag_outliers = TRUE
  )
  expect_no_error(txt <- group_flag_text(row))
  expect_length(txt, 2)
  expect_match(txt[1], "not run")
  expect_match(txt[2], "?", fixed = TRUE)

  expect_no_error(line <- group_summary_line(row))
  # The rows clause is dropped rather than printing "character(0) rows".
  expect_false(grepl("character", line, fixed = TRUE))
  expect_match(line, "100 measurements")
})
