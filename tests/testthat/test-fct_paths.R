# here_rel() exists because here::here() is absolute and targets records paths
# verbatim (r-lib/here#36). The properties worth pinning are: the result is
# relative, it is a plain character vector, and it still points at the same file.

test_that("here_rel() returns a relative path", {
  expect_false(fs::is_absolute_path(here_rel("data/clean/group_decisions.csv")))
  expect_false(fs::is_absolute_path(here_rel("R")))
})

test_that("here::here() is absolute, which is the whole reason for this function", {
  # Guard against here growing a relative mode and this helper going stale.
  expect_true(fs::is_absolute_path(here::here("data/clean/group_decisions.csv")))
})

test_that("here_rel() returns plain character, not fs_path", {
  # fs_path inherits from character so targets accepts it, but hashes it
  # differently. A bare character keeps invalidation predictable.
  out <- here_rel("data")
  expect_type(out, "character")
  expect_false(inherits(out, "fs_path"))
  expect_length(out, 1L)
})

test_that("here_rel() and here::here() resolve to the same location", {
  rel <- here_rel("data/clean/group_decisions.csv")
  abs <- here::here("data/clean/group_decisions.csv")
  expect_identical(
    normalizePath(rel, winslash = "/", mustWork = FALSE),
    normalizePath(abs, winslash = "/", mustWork = FALSE)
  )
})

test_that("here_rel() accepts multiple path components like here()", {
  expect_identical(
    here_rel("data", "clean", "group_decisions.csv"),
    here_rel("data/clean/group_decisions.csv")
  )
})

test_that("here_rel() with no arguments gives the project root, relatively", {
  # Not necessarily ".": testthat runs with tests/testthat/ as the working
  # directory, so the correct answer there is "../..". Assert the property, not
  # a literal, or this passes interactively and fails under test_dir().
  out <- here_rel()
  expect_false(fs::is_absolute_path(out))
  expect_identical(
    normalizePath(out, winslash = "/", mustWork = FALSE),
    normalizePath(here::here(), winslash = "/", mustWork = FALSE)
  )
})

test_that("here_rel() is relative to the working directory, not the root", {
  # The case that matters for Quarto: notebooks render with docs/ as the working
  # directory, and a path relative to docs/ is the correct answer there.
  skip_if_not(dir.exists(here::here("R")), "no R/ directory to descend into")

  withr::with_dir(here::here("R"), {
    out <- here_rel("data/clean/group_decisions.csv")
    expect_false(fs::is_absolute_path(out))
    expect_match(out, "^\\.\\./")
    expect_identical(
      normalizePath(out, winslash = "/", mustWork = FALSE),
      normalizePath(
        here::here("data/clean/group_decisions.csv"),
        winslash = "/",
        mustWork = FALSE
      )
    )
  })
})

test_that("a here_rel() path is usable by the functions that take one", {
  # The point of the change: read_group_decisions() must work off the relative
  # default. Smoke test against the real file, since that is what the pipeline
  # passes.
  path <- here_rel("data/clean/group_decisions.csv")
  skip_if_not(file.exists(path), "no decisions file checked out")

  decisions <- read_group_decisions(path = path)
  expect_s3_class(decisions, "data.frame")
  expect_true(all(group_decision_human_cols() %in% names(decisions)))
})
