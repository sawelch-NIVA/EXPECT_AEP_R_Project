# Tests ----
#
# One property dominates: the generator must never destroy hand-written prose.
# Its predecessor was deleted in PLAN.md P0.2 along with fourteen notebooks, and
# the lesson in CLAUDE.md is that prose in generated files is reproducible from
# nowhere else.

nb_decisions <- function(notebooks = c("Fish", "Fish", "Molluscs"),
                         ids = c("G001", "G002", "G003"),
                         n = c(900, 60, 30)) {
  data.frame(
    group_id = ids,
    ENVIRON_COMPARTMENT = "Biota",
    ENVIRON_COMPARTMENT_SUB = "Biota, Aquatic",
    SPECIES_GROUP = "Fish",
    SAMPLE_SPECIES = paste("Species", seq_along(ids)),
    SAMPLE_TISSUE = "Liver",
    SITE_GEOGRAPHIC_FEATURE = "Coastal, fjord",
    SITE_GEOGRAPHIC_FEATURE_SUB = "Not reported",
    MEASURED_UNIT_STANDARD = "mg/kg (wet)",
    n = n,
    n_rows = n,
    n_sources = 2L,
    species_common_name = NA_character_,
    flag_multimodal = FALSE,
    flag_outliers = FALSE,
    notebook = notebooks
  )
}

nb_groups <- function(ids = "G001", slugs = "some_slug") {
  data.frame(group_id = ids, group_slug = slugs)
}

test_that("slugs are filesystem-safe and lower case", {
  expect_equal(notebook_slug("Crustaceans and Invertebrates"), "crustaceans-and-invertebrates")
  expect_equal(notebook_slug("Marine and Brackish Water"), "marine-and-brackish-water")
  expect_false(grepl("[^a-z0-9-]", notebook_slug("Algae and Plants")))
})

test_that("one file is created per notebook", {
  dir <- withr::local_tempdir()
  generate_group_notebooks(nb_decisions(), nb_groups(), dir = dir, verbose = FALSE)

  expect_true(file.exists(file.path(dir, "fish.qmd")))
  expect_true(file.exists(file.path(dir, "molluscs.qmd")))
  expect_length(list.files(dir, pattern = "\\.qmd$"), 2)
})

test_that("every group gets a section, anchored on its id", {
  dir <- withr::local_tempdir()
  generate_group_notebooks(nb_decisions(), nb_groups(), dir = dir, verbose = FALSE)

  expect_setequal(existing_group_sections(file.path(dir, "fish.qmd")), c("G001", "G002"))
  expect_equal(existing_group_sections(file.path(dir, "molluscs.qmd")), "G003")
})

test_that("re-running changes nothing at all", {
  dir <- withr::local_tempdir()
  generate_group_notebooks(nb_decisions(), nb_groups(), dir = dir, verbose = FALSE)
  before <- readLines(file.path(dir, "fish.qmd"), warn = FALSE)

  again <- generate_group_notebooks(nb_decisions(), nb_groups(), dir = dir, verbose = FALSE)
  after <- readLines(file.path(dir, "fish.qmd"), warn = FALSE)

  expect_identical(before, after)
  expect_equal(sum(again$appended), 0)
  expect_false(any(again$created))
})

test_that("hand-written prose survives regeneration", {
  # THE property. If this fails, an afternoon of judgement is gone and there is
  # nowhere to recover it from.
  dir <- withr::local_tempdir()
  generate_group_notebooks(nb_decisions(), nb_groups(), dir = dir, verbose = FALSE)
  path <- file.path(dir, "fish.qmd")

  edited <- readLines(path, warn = FALSE)
  edited <- sub(
    "\\*\\*Verdict:\\*\\* \\*\\(unwritten\\)\\*",
    "**Verdict:** lump with G002, same fishery, different reporting year.",
    edited
  )
  edited <- c(edited, "", "A trailing thought I added by hand.")
  writeLines(edited, path)

  # A new group appears, forcing an append.
  grown <- nb_decisions(
    notebooks = c("Fish", "Fish", "Molluscs", "Fish"),
    ids = c("G001", "G002", "G003", "G004"),
    n = c(900, 60, 30, 15)
  )
  generate_group_notebooks(grown, nb_groups(), dir = dir, verbose = FALSE)
  after <- readLines(path, warn = FALSE)

  expect_true(any(grepl("same fishery, different reporting year", after)))
  expect_true(any(grepl("A trailing thought I added by hand", after)))
  expect_true("G004" %in% existing_group_sections(path))
  # Nothing removed: every original line is still present.
  expect_true(all(edited %in% after))
})

test_that("a renamed heading does not cause a duplicate section", {
  # Sections are detected by anchor, not by heading text, so Sam can retitle a
  # heading freely.
  dir <- withr::local_tempdir()
  generate_group_notebooks(nb_decisions(), nb_groups(), dir = dir, verbose = FALSE)
  path <- file.path(dir, "fish.qmd")

  edited <- readLines(path, warn = FALSE)
  edited <- sub("^## G001 .*\\{#grp-G001\\}$", "## My own title {#grp-G001}", edited)
  writeLines(edited, path)

  generate_group_notebooks(nb_decisions(), nb_groups(), dir = dir, verbose = FALSE)
  after <- readLines(path, warn = FALSE)
  expect_equal(sum(grepl("\\{#grp-G001\\}", after)), 1)
  expect_true(any(grepl("My own title", after)))
})

test_that("groups with panels link them and groups without say so", {
  dir <- withr::local_tempdir()
  # G001 has a slug, G002 does not.
  generate_group_notebooks(nb_decisions(), nb_groups("G001", "my_slug"), dir = dir, verbose = FALSE)
  txt <- paste(readLines(file.path(dir, "fish.qmd"), warn = FALSE), collapse = "\n")

  expect_true(grepl("my_slug_a_density.png", txt, fixed = TRUE))
  expect_true(grepl("No triage panels", txt, fixed = TRUE))
  # The remedy is named, with the id to add.
  expect_true(grepl("must_include", txt, fixed = TRUE))
})

test_that("the glance table lists every group and links to its section", {
  dir <- withr::local_tempdir()
  generate_group_notebooks(nb_decisions(), nb_groups(), dir = dir, verbose = FALSE)
  txt <- paste(readLines(file.path(dir, "fish.qmd"), warn = FALSE), collapse = "\n")

  expect_true(grepl("[G001](#grp-G001)", txt, fixed = TRUE))
  expect_true(grepl("[G002](#grp-G002)", txt, fixed = TRUE))
  expect_true(grepl("# Comparison", txt, fixed = TRUE))
})

test_that("groups are ordered by n descending within a notebook", {
  dir <- withr::local_tempdir()
  generate_group_notebooks(
    nb_decisions(notebooks = rep("Fish", 3), n = c(30, 900, 60)),
    nb_groups(), dir = dir, verbose = FALSE
  )
  expect_equal(
    existing_group_sections(file.path(dir, "fish.qmd")),
    c("G002", "G003", "G001")
  )
})

test_that("unassigned groups are skipped rather than making a blank notebook", {
  dir <- withr::local_tempdir()
  d <- nb_decisions()
  d$notebook[3] <- ""
  generate_group_notebooks(d, nb_groups(), dir = dir, verbose = FALSE)
  expect_length(list.files(dir, pattern = "\\.qmd$"), 1)
})

test_that("existing_group_sections copes with an absent file", {
  expect_length(existing_group_sections(tempfile(fileext = ".qmd")), 0)
})

# ---- Figure-reference bullets (2026-08-05) ------------------------------

test_that("a section with panels gets one empty bullet per figure", {
  # Sam was typing "- @fig-g013-a:" by hand for five panels of every group before
  # writing anything, and a mistyped id fails silently as an unresolved
  # reference.
  md <- group_section_markdown(
    nb_decisions(notebooks = "Fish", ids = "G013", n = 314)[1, , drop = FALSE],
    plot_slug = "some_slug"
  )

  bullets <- grep("^- @fig-", md, value = TRUE)
  expect_equal(
    bullets,
    c(
      "- @fig-g013-a:",
      "- @fig-g013-b:",
      "- @fig-g013-c:",
      "- @fig-g013-d:",
      "- @fig-g013-e:"
    )
  )
})

test_that("the bullets sit above the figure div, where hand-written ones do", {
  md <- group_section_markdown(
    nb_decisions(notebooks = "Fish", ids = "G013", n = 314)[1, , drop = FALSE],
    plot_slug = "some_slug"
  )
  first_bullet <- min(grep("^- @fig-", md))
  div <- min(grep("^::: \\{#fig-", md))
  expect_lt(first_bullet, div)
})

test_that("a section without panels gets no bullets", {
  md <- group_section_markdown(
    nb_decisions(notebooks = "Fish", ids = "G013", n = 12)[1, , drop = FALSE],
    plot_slug = NA_character_
  )
  expect_length(grep("^- @fig-", md), 0)
  expect_true(any(grepl("No triage panels", md)))
})

test_that("bullet ids match the subfigure ids actually emitted", {
  # The bullets and the images derive their letters from the same `captions`
  # vector, so a caption rename cannot leave the two pointing at different
  # things. Asserted rather than assumed, because an unresolved cross-reference
  # renders as literal text and is easy to miss on a 245-section page.
  md <- group_section_markdown(
    nb_decisions(notebooks = "Fish", ids = "G008", n = 500)[1, , drop = FALSE],
    plot_slug = "some_slug"
  )
  bullet_ids <- sub("^- @([a-z0-9-]+):.*$", "\\1", grep("^- @fig-", md, value = TRUE))
  image_ids <- sub("^.*\\{#(fig-[a-z0-9-]+) .*$", "\\1", grep("^!\\[", md, value = TRUE))
  expect_equal(sort(bullet_ids), sort(image_ids))
})

test_that("adding bullets does not disturb the append-only guarantee", {
  # The bullets are emitted inside a section, and sections are only emitted for
  # absent anchors, so a group that already carries hand-written bullets is never
  # revisited. This is the property that matters most in this file.
  dir <- withr::local_tempdir()
  generate_group_notebooks(nb_decisions(), nb_groups(), dir = dir, verbose = FALSE)
  path <- file.path(dir, "fish.qmd")

  written <- readLines(path, warn = FALSE)
  written[grep("^- @fig-g001-a:", written)] <- "- @fig-g001-a: clear bimodality, two campaigns"
  writeLines(written, path)
  before <- readLines(path, warn = FALSE)

  generate_group_notebooks(nb_decisions(), nb_groups(), dir = dir, verbose = FALSE)
  after <- readLines(path, warn = FALSE)

  expect_identical(before, after)
  expect_true(any(grepl("clear bimodality, two campaigns", after, fixed = TRUE)))
})

# ---- Panels appearing after a section already exists (2026-08-05) -------

test_that("a section gains panels once its group has them", {
  # THE GAP. Adding a group to must_include and re-running the pipeline writes
  # its PNGs, but the section keeps saying "No triage panels" because
  # append-only skips any anchor already in the file. Sam hit this with G047
  # after correctly editing the target and re-running twice.
  dir <- withr::local_tempdir()
  generate_group_notebooks(
    nb_decisions(), nb_groups(ids = "G999", slugs = "unused"),
    dir = dir, verbose = FALSE
  )
  path <- file.path(dir, "fish.qmd")
  before <- readLines(path, warn = FALSE)
  expect_true(any(startsWith(before, "*No triage panels:")))

  out <- refresh_group_panels(
    nb_groups(ids = "G001", slugs = "some_slug"), dir = dir, verbose = FALSE
  )
  after <- readLines(path, warn = FALSE)

  expect_equal(out$group_id, "G001")
  expect_true(any(grepl("^::: \\{#fig-g001", after)))
  expect_true(any(grepl("^- @fig-g001-a:", after)))
  expect_false(any(grepl("G001` to `must_include`", after, fixed = TRUE)))
})

test_that("the repair is idempotent", {
  dir <- withr::local_tempdir()
  generate_group_notebooks(
    nb_decisions(), nb_groups(ids = "G999", slugs = "unused"),
    dir = dir, verbose = FALSE
  )
  groups <- nb_groups(ids = "G001", slugs = "some_slug")
  refresh_group_panels(groups, dir = dir, verbose = FALSE)
  once <- readLines(file.path(dir, "fish.qmd"), warn = FALSE)

  out <- refresh_group_panels(groups, dir = dir, verbose = FALSE)
  twice <- readLines(file.path(dir, "fish.qmd"), warn = FALSE)

  expect_equal(nrow(out), 0)
  expect_identical(once, twice)
})

test_that("the repair leaves a hand-edited placeholder alone", {
  # THE SAFETY PROPERTY. The match is byte-for-byte against the machine-written
  # boilerplate. One character of Sam's own and the section is untouched, which
  # is what keeps append-only intact.
  dir <- withr::local_tempdir()
  generate_group_notebooks(
    nb_decisions(), nb_groups(ids = "G999", slugs = "unused"),
    dir = dir, verbose = FALSE
  )
  path <- file.path(dir, "fish.qmd")
  lines <- readLines(path, warn = FALSE)
  i <- grep("G001` to `must_include`", lines, fixed = TRUE)[1]
  lines[i] <- paste(lines[i], "I have decided this one does not need panels.")
  writeLines(lines, path)
  before <- readLines(path, warn = FALSE)

  out <- refresh_group_panels(
    nb_groups(ids = "G001", slugs = "some_slug"), dir = dir, verbose = FALSE
  )
  expect_equal(nrow(out), 0)
  expect_identical(before, readLines(path, warn = FALSE))
})

test_that("the repair never touches prose or a verdict", {
  dir <- withr::local_tempdir()
  generate_group_notebooks(
    nb_decisions(), nb_groups(ids = "G999", slugs = "unused"),
    dir = dir, verbose = FALSE
  )
  path <- file.path(dir, "fish.qmd")
  lines <- readLines(path, warn = FALSE)
  v <- grep("^\\*\\*Verdict:\\*\\*", lines)[1]
  lines[v] <- "**Verdict:** keep, clearly bimodal, see the 2019 campaign"
  writeLines(lines, path)

  refresh_group_panels(
    nb_groups(ids = "G001", slugs = "some_slug"), dir = dir, verbose = FALSE
  )
  after <- readLines(path, warn = FALSE)
  expect_true(any(grepl("clearly bimodal, see the 2019 campaign", after, fixed = TRUE)))
})

test_that("a repaired section matches one written fresh", {
  # panel_block_markdown() is shared by both paths precisely so this holds.
  fresh <- group_section_markdown(
    nb_decisions(notebooks = "Fish", ids = "G001", n = 900)[1, , drop = FALSE],
    plot_slug = "some_slug"
  )
  block <- panel_block_markdown(
    "G001",
    triage_group_label(nb_decisions(notebooks = "Fish", ids = "G001", n = 900)[1, ]),
    "some_slug"
  )
  expect_true(all(block %in% fresh))
})
