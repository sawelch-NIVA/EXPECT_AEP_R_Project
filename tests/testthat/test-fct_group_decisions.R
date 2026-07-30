# Tests ----
#
# PLAN.md P2.2. The property that matters most is that scaffolding never destroys
# a decision, because that is what makes the file safe to regenerate when new data
# arrives.

decision_summary <- function(n = c(900, 60, 30, 10), species = LETTERS[1:4]) {
  data.frame(
    ENVIRON_COMPARTMENT = "Biota",
    ENVIRON_COMPARTMENT_SUB = "Biota, Aquatic",
    SPECIES_GROUP = "Fish",
    SAMPLE_SPECIES = species,
    SAMPLE_TISSUE = "Liver",
    SITE_GEOGRAPHIC_FEATURE = "Coastal, fjord",
    SITE_GEOGRAPHIC_FEATURE_SUB = "Not reported",
    MEASURED_UNIT_STANDARD = "mg/kg (wet)",
    n = n,
    n_sources = 2L,
    species_common_name = "Cod",
    flag_multimodal = FALSE,
    flag_outliers = FALSE
  )
}

# ---- Coverage tiers ----------------------------------------------------

test_that("each tier is the minimal set reaching its threshold", {
  # Tiering on cum_pct itself excludes whichever group crosses the line, which put
  # top90 at 6 groups covering 88% and disagreed with PLAN.md P2.2's wording.
  # 900 / 60 / 30 / 10 out of 1000: cumulative 90%, 96%, 99%, 100%.
  out <- add_coverage_columns(decision_summary())
  expect_equal(out$rank, 1:4)
  expect_equal(round(out$cum_pct, 3), c(0.900, 0.960, 0.990, 1.000))
  # The first group alone reaches 90%, so it is the whole top90 tier.
  expect_equal(out$tier, c("top90", "top95", "top99", "tail"))
})

test_that("coverage columns sort by n descending regardless of input order", {
  out <- add_coverage_columns(decision_summary(n = c(10, 900, 30, 60)))
  expect_equal(out$n, c(900, 60, 30, 10))
  expect_true(all(diff(out$cum_pct) > 0))
})

# ---- Scaffolding -------------------------------------------------------

test_that("scaffolding a fresh file leaves every group undecided", {
  path <- tempfile(fileext = ".csv")
  out <- scaffold_group_decisions(decision_summary(), path, verbose = FALSE)

  expect_true(file.exists(path))
  expect_equal(nrow(out), 4)
  expect_true(all(out$decision == ""))
  expect_true(all(group_decision_human_cols() %in% names(out)))
})

test_that("re-scaffolding never destroys a decision", {
  # THE property. A scaffold that clobbered decisions would make the file unsafe
  # to regenerate, which makes it unsafe to add new data.
  path <- tempfile(fileext = ".csv")
  scaffold_group_decisions(decision_summary(), path, verbose = FALSE)

  edited <- readr::read_csv(path, show_col_types = FALSE)
  edited$decision[edited$SAMPLE_SPECIES == "A"] <- "own_notebook"
  edited$notes[edited$SAMPLE_SPECIES == "A"] <- "carries most of the data"
  edited$decision[edited$SAMPLE_SPECIES == "B"] <- "lump"
  edited$lump_into[edited$SAMPLE_SPECIES == "B"] <- "A"
  readr::write_csv(edited, path, na = "")

  again <- scaffold_group_decisions(decision_summary(), path, verbose = FALSE)
  expect_equal(again$decision[again$SAMPLE_SPECIES == "A"], "own_notebook")
  expect_equal(again$notes[again$SAMPLE_SPECIES == "A"], "carries most of the data")
  expect_equal(again$lump_into[again$SAMPLE_SPECIES == "B"], "A")
})

test_that("machine context refreshes while decisions persist", {
  # n changes as data is added; that must flow through. The decision must not.
  path <- tempfile(fileext = ".csv")
  scaffold_group_decisions(decision_summary(), path, verbose = FALSE)
  edited <- readr::read_csv(path, show_col_types = FALSE)
  edited$decision[edited$SAMPLE_SPECIES == "A"] <- "own_notebook"
  readr::write_csv(edited, path, na = "")

  grown <- decision_summary(n = c(5000, 60, 30, 10))
  again <- scaffold_group_decisions(grown, path, verbose = FALSE)
  expect_equal(again$n[again$SAMPLE_SPECIES == "A"], 5000)
  expect_equal(again$decision[again$SAMPLE_SPECIES == "A"], "own_notebook")
})

test_that("new groups are appended as undecided", {
  path <- tempfile(fileext = ".csv")
  scaffold_group_decisions(decision_summary(), path, verbose = FALSE)
  edited <- readr::read_csv(path, show_col_types = FALSE)
  edited$decision <- "drop"
  readr::write_csv(edited, path, na = "")

  wider <- decision_summary(
    n = c(900, 60, 30, 10, 500),
    species = c(LETTERS[1:4], "E")
  )
  again <- scaffold_group_decisions(wider, path, verbose = FALSE)
  expect_equal(nrow(again), 5)
  expect_equal(again$decision[again$SAMPLE_SPECIES == "E"], "")
  expect_true(all(again$decision[again$SAMPLE_SPECIES != "E"] == "drop"))
})

test_that("losing a decided group warns rather than passing silently", {
  # A group vanishing usually means an upstream key changed: a species rename or a
  # unit fix. The decision attached to it is still worth re-reading.
  path <- tempfile(fileext = ".csv")
  scaffold_group_decisions(decision_summary(), path, verbose = FALSE)
  edited <- readr::read_csv(path, show_col_types = FALSE)
  edited$decision[edited$SAMPLE_SPECIES == "D"] <- "own_notebook"
  readr::write_csv(edited, path, na = "")

  shrunk <- decision_summary(n = c(900, 60, 30), species = LETTERS[1:3])
  expect_warning(
    scaffold_group_decisions(shrunk, path, verbose = FALSE),
    "no longer exist"
  )
})

test_that("losing an UNdecided group does not warn", {
  # Groups come and go as filters change; only lost judgement is worth a warning.
  path <- tempfile(fileext = ".csv")
  scaffold_group_decisions(decision_summary(), path, verbose = FALSE)
  shrunk <- decision_summary(n = c(900, 60, 30), species = LETTERS[1:3])
  expect_no_warning(scaffold_group_decisions(shrunk, path, verbose = FALSE))
})

# ---- Reading and validating --------------------------------------------

test_that("read_group_decisions rejects an unrecognised decision", {
  # The file is hand-edited, so a typo must fail here rather than silently produce
  # an empty group downstream.
  path <- tempfile(fileext = ".csv")
  scaffold_group_decisions(decision_summary(), path, verbose = FALSE)
  edited <- readr::read_csv(path, show_col_types = FALSE)
  edited$decision[1] <- "own_notebok"
  readr::write_csv(edited, path, na = "")

  expect_error(read_group_decisions(path), "Unrecognised decision")
})

test_that("read_group_decisions accepts every permitted value plus blank", {
  path <- tempfile(fileext = ".csv")
  scaffold_group_decisions(decision_summary(), path, verbose = FALSE)
  edited <- readr::read_csv(path, show_col_types = FALSE)
  edited$decision <- group_decision_levels()
  edited$lump_into[edited$decision == "lump"] <- "A"
  readr::write_csv(edited, path, na = "")

  expect_no_error(read_group_decisions(path))
})

test_that("a lump with no target warns", {
  path <- tempfile(fileext = ".csv")
  scaffold_group_decisions(decision_summary(), path, verbose = FALSE)
  edited <- readr::read_csv(path, show_col_types = FALSE)
  edited$decision[1] <- "lump"
  readr::write_csv(edited, path, na = "")

  expect_warning(read_group_decisions(path), "lump")
})

test_that("read_group_decisions reports groups missing from the file", {
  # This is how a stale decisions file is caught after new data arrives.
  path <- tempfile(fileext = ".csv")
  scaffold_group_decisions(decision_summary(), path, verbose = FALSE)
  wider <- decision_summary(
    n = c(900, 60, 30, 10, 500),
    species = c(LETTERS[1:4], "E")
  )
  expect_warning(read_group_decisions(path, wider), "absent from")
})

test_that("a missing file errors with the fix in the message", {
  expect_error(
    read_group_decisions(tempfile(fileext = ".csv")),
    "scaffold_group_decisions"
  )
})

test_that("a decisions file missing a human column errors", {
  path <- tempfile(fileext = ".csv")
  scaffold_group_decisions(decision_summary(), path, verbose = FALSE)
  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::select(-"notes") |>
    readr::write_csv(path, na = "")
  expect_error(read_group_decisions(path), "missing column")
})

# ---- Progress ----------------------------------------------------------

test_that("progress counts decided and undecided per tier", {
  path <- tempfile(fileext = ".csv")
  scaffold_group_decisions(decision_summary(), path, verbose = FALSE)
  edited <- readr::read_csv(path, show_col_types = FALSE)
  edited$decision[edited$tier == "top90"] <- "own_notebook"
  readr::write_csv(edited, path, na = "")

  progress <- group_decision_progress(read_group_decisions(path))
  top90 <- progress[progress$tier == "top90", ]
  expect_equal(top90$decided, top90$groups)
  expect_equal(top90$undecided, 0L)
  expect_equal(sum(progress$groups), 4)
})
