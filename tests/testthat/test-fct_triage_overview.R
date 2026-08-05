# Tests ----
#
# The parent-level overview panels (a, b at the sub-compartment). The property that matters most is that
# the two levels to compare are resolved from the data rather than fixed, because
# the taxonomy columns are entirely NA for abiotic sub-compartments and a fixed
# rule would hand them two empty panels.

overview_rows <- function(
  n = 60,
  compartment = "Aquatic",
  sub = "Aquatic Sediment",
  unit = "mg/kg (dry)",
  species_group = NA_character_,
  species = NA_character_,
  tissue = NA_character_,
  feature = c("Coastal, fjord", "River, stream, canal"),
  feature_sub = c("Water benthos", "Water column, pelagic zone")
) {
  set.seed(42)
  data.frame(
    ENVIRON_COMPARTMENT = compartment,
    ENVIRON_COMPARTMENT_SUB = sub,
    SPECIES_GROUP = rep_len(species_group, n),
    SAMPLE_SPECIES = rep_len(species, n),
    SAMPLE_TISSUE = rep_len(tissue, n),
    SITE_GEOGRAPHIC_FEATURE = rep_len(feature, n),
    SITE_GEOGRAPHIC_FEATURE_SUB = rep_len(feature_sub, n),
    MEASURED_UNIT_STANDARD = unit,
    MEASURED_VALUE_STANDARD = 10^stats::runif(n, -2, 2),
    # One measurement per row, so `n` and `n_rows` coincide in the fixtures and a
    # threshold expressed in either reads the same way.
    MEASURED_N = 1L,
    SAMPLING_DATE = as.Date("2015-01-01") + seq_len(n),
    CAMPAIGN_NAME_SHORT = "Vm_2010_2025 (Polluted Seabed)",
    stringsAsFactors = FALSE
  )
}

# ---- Choosing the levels -----------------------------------------------

test_that("all-NA taxonomy columns are skipped for an abiotic node", {
  # The whole reason the levels are derived rather than fixed: SPECIES_GROUP,
  # SAMPLE_SPECIES and SAMPLE_TISSUE are NA throughout Aquatic Sediment.
  expect_equal(
    triage_overview_levels(overview_rows()),
    c("SITE_GEOGRAPHIC_FEATURE", "SITE_GEOGRAPHIC_FEATURE_SUB")
  )
})

test_that("a biota node lands on the taxonomy levels instead", {
  # Only SPECIES_GROUP: it is a stop column, because species detail is covered
  # by triage_species_nodes() one level down.
  expect_equal(
    triage_overview_levels(overview_rows(
      compartment = "Biota",
      sub = "Biota, Aquatic",
      species_group = c("Fish", "Molluscs"),
      species = c("Gadus morhua", "Mytilus edulis"),
      tissue = "Liver"
    )),
    "SPECIES_GROUP"
  )
})

test_that("descent stops at SPECIES_GROUP, which has its own tier", {
  # Before triage_species_nodes() existed, this returned SPECIES_GROUP plus
  # SAMPLE_SPECIES, and the species panel spanned all 76 species of
  # Biota, Aquatic truncated to the largest 25. Species detail now lives one
  # level down, where it is complete.
  expect_equal(
    triage_overview_levels(overview_rows(
      compartment = "Biota",
      sub = "Biota, Aquatic",
      species_group = c("Fish", "Molluscs"),
      species = c("Gadus morhua", "Mytilus edulis"),
      tissue = c("Liver", "Soft tissue")
    )),
    "SPECIES_GROUP"
  )
})

test_that("abiotic nodes still get both geography levels", {
  # The stop rule must not touch them: no candidate is a stop column, so
  # descent runs to the usual two.
  expect_equal(
    triage_overview_levels(overview_rows()),
    c("SITE_GEOGRAPHIC_FEATURE", "SITE_GEOGRAPHIC_FEATURE_SUB")
  )
})

test_that("an empty stop_cols restores the old two-level behaviour", {
  expect_equal(
    triage_overview_levels(
      overview_rows(
        compartment = "Biota",
        sub = "Biota, Aquatic",
        species_group = c("Fish", "Molluscs"),
        species = c("Gadus morhua", "Mytilus edulis"),
        tissue = c("Liver", "Soft tissue")
      ),
      stop_cols = character(0)
    ),
    c("SPECIES_GROUP", "SAMPLE_SPECIES")
  )
})

test_that("a column with one distinct value does not count as a level", {
  # SAMPLE_TISSUE is constant here, so it is skipped in favour of geography.
  out <- triage_overview_levels(overview_rows(
    compartment = "Biota",
    sub = "Biota, Aquatic",
    species_group = "Fish",
    species = "Gadus morhua",
    tissue = "Liver"
  ))
  expect_equal(out, c("SITE_GEOGRAPHIC_FEATURE", "SITE_GEOGRAPHIC_FEATURE_SUB"))
})

test_that("empty strings count as missing alongside NA", {
  # The decisions CSV round-trips NA to "", so anything derived from it can
  # carry either and both must be treated the same way.
  d <- overview_rows()
  d$SPECIES_GROUP <- rep_len(c("", ""), nrow(d))
  expect_false("SPECIES_GROUP" %in% triage_overview_levels(d))
})

test_that("no varying column yields no levels", {
  d <- overview_rows(
    feature = "Coastal, fjord",
    feature_sub = "Water benthos"
  )
  expect_length(triage_overview_levels(d), 0)
})

test_that("only one varying column yields one level", {
  d <- overview_rows(feature_sub = "Water benthos")
  expect_equal(triage_overview_levels(d), "SITE_GEOGRAPHIC_FEATURE")
})

# ---- Node selection ----------------------------------------------------

test_that("nodes below min_n are dropped", {
  d <- rbind(
    overview_rows(n = 60),
    overview_rows(n = 4, unit = "mg/kg (wet)")
  )
  nodes <- triage_overview_nodes(d, min_n = 30)
  expect_equal(nodes$MEASURED_UNIT_STANDARD, "mg/kg (dry)")
})

test_that("min_n counts measurements, not rows", {
  # The threshold has to be in the same currency as sample_triage_groups()'s
  # min_n, or the two disagree and a node gets written that the notebook never
  # displays. 40 rows carrying 5 measurements each clears a bar of 100 that a
  # row count would fail.
  d <- overview_rows(n = 40)
  d$MEASURED_N <- 5L
  nodes <- triage_overview_nodes(d, min_n = 100)
  expect_equal(nrow(nodes), 1)
  expect_equal(nodes$n[1], 200)
  expect_equal(nodes$n_rows[1], 40)
})

test_that("min_n defaults to 100, matching the triage cutoff", {
  d <- overview_rows(n = 60)
  expect_equal(nrow(triage_overview_nodes(d)), 0)
  expect_equal(nrow(triage_overview_nodes(overview_rows(n = 120))), 1)
})

test_that("n_groups counts the group keys under the node", {
  # Two site features x two site subtypes, but paired rather than crossed by
  # rep_len(), so two group keys, not four.
  nodes <- triage_overview_nodes(overview_rows(n = 120))
  expect_equal(nodes$n_groups[1], 2)
})

test_that("the unit is part of the node, not lumped across", {
  # Biota, Aquatic really does split 129 groups of wet against 66 of dry, and
  # overlaying them would read as a biological split rather than a units one.
  d <- rbind(
    overview_rows(n = 60),
    overview_rows(n = 60, unit = "mg/kg (wet)")
  )
  nodes <- triage_overview_nodes(d, min_n = 30)
  expect_equal(nrow(nodes), 2)
  expect_setequal(
    nodes$MEASURED_UNIT_STANDARD,
    c("mg/kg (dry)", "mg/kg (wet)")
  )
})

test_that("a node with nothing varying below it produces no row", {
  d <- overview_rows(feature = "Coastal, fjord", feature_sub = "Water benthos")
  nodes <- triage_overview_nodes(d, min_n = 30)
  expect_equal(nrow(nodes), 0)
  # Still shaped like a node table, so callers need no NULL branch.
  expect_true(all(
    c("level_1", "level_2", "node_slug") %in% names(nodes)
  ))
})

test_that("empty input returns an empty node table rather than erroring", {
  nodes <- triage_overview_nodes(overview_rows()[0, ], min_n = 30)
  expect_equal(nrow(nodes), 0)
})

test_that("nodes carry every group-key column so threshold matching is safe", {
  # thresholds_for_group() reads grp$SPECIES_GROUP[1] unconditionally for Biota.
  # On a tibble without that column it gets NULL, and `if (is.na(NULL))` fails
  # with a zero-length condition.
  nodes <- triage_overview_nodes(overview_rows(), min_n = 30)
  expect_true(all(triage_group_cols() %in% names(nodes)))
  expect_true(is.na(nodes$SPECIES_GROUP[1]))
})

test_that("node slugs are unique without make.unique disambiguation", {
  d <- rbind(
    overview_rows(n = 60),
    overview_rows(n = 60, unit = "mg/kg (wet)")
  )
  nodes <- triage_overview_nodes(d, min_n = 30)
  expect_equal(anyDuplicated(nodes$node_slug), 0L)
  expect_false(any(grepl("_[0-9]+$", nodes$node_slug)))
})

# ---- Category truncation ------------------------------------------------

test_that("truncation keeps the largest categories and says so", {
  d <- overview_rows(n = 100, feature = paste("Site", 1:40))
  out <- truncate_categories(d, "SITE_GEOGRAPHIC_FEATURE", max_categories = 10)
  expect_equal(length(unique(out$data$SITE_GEOGRAPHIC_FEATURE)), 10)
  expect_match(out$note, "10 largest of 40")
})

test_that("no truncation note when everything fits", {
  out <- truncate_categories(
    overview_rows(),
    "SITE_GEOGRAPHIC_FEATURE",
    max_categories = 25
  )
  expect_null(out$note)
})

# ---- Drawing ------------------------------------------------------------

test_that("overview panels build and write for an abiotic node", {
  # A ggplot object constructs fine and only fails when rendered, so force both
  # a build and a real device write.
  d <- overview_rows(n = 60)
  nodes <- triage_overview_nodes(d, min_n = 30)
  dir <- withr::local_tempdir()

  paths <- write_triage_overview_for_node(d, nodes[1, ], dir = dir)

  expect_length(paths, 2)
  expect_true(all(file.exists(paths)))
  expect_true(all(file.size(paths) > 0))
  expect_match(paths[1], "_a_overview_site_geographic_feature\\.png$")
  expect_match(paths[2], "_b_overview_site_geographic_feature_sub\\.png$")
})

test_that("a node with one level writes one panel", {
  d <- overview_rows(n = 60, feature_sub = "Water benthos")
  nodes <- triage_overview_nodes(d, min_n = 30)
  dir <- withr::local_tempdir()

  paths <- write_triage_overview_for_node(d, nodes[1, ], dir = dir)

  expect_length(paths, 1)
  expect_true(file.exists(paths))
})

test_that("every decade on the concentration axis is LABELLED", {
  # Changed 2026-08-04 on Sam's instruction: "I can't reliably read the
  # concentration from a)". Powers of ten used to be unlabelled MINOR breaks, so
  # the axis had a gridline per decade and a number against almost none of them,
  # because ggplot2's default log breaks label only two or three points across a
  # span this wide. They are now major breaks, and therefore labelled.
  p <- triage_plot_by_category(
    overview_rows(n = 60),
    "SITE_GEOGRAPHIC_FEATURE",
    "test",
    limits = c(1e-5, 1e5)
  )
  params <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
  labels <- params$x$get_labels()
  # Empty strings as well as NA, since 2026-08-05. The panel reserves a strip
  # beyond limits[2] for its count labels, and breaks falling in that strip are
  # blanked rather than dropped: an NA label at a real break renders as the
  # literal text "NA", so "" is what suppression looks like here.
  labels <- labels[!is.na(labels) & nzchar(labels)]

  # 1e-05 through 1e+05 inclusive.
  expect_equal(length(labels), 11)
  expect_true(all(grepl("^1e[+-][0-9]+$", labels)))
  expect_true("1e+00" %in% labels)
  expect_true("1e-05" %in% labels)
  # And nothing is labelled inside the reserved strip.
  expect_true(all(as.numeric(labels) <= 1e5))
})

test_that("the concentration axis has no minor breaks left to draw", {
  # The 1:9-per-decade grid is ~96 lines across a 12-decade axis and fights the
  # threshold lines and the class axis. Promoting the decades to major breaks is
  # what made the axis readable; adding a second tier back would undo it.
  p <- triage_plot_by_category(
    overview_rows(n = 60),
    "SITE_GEOGRAPHIC_FEATURE",
    "test",
    limits = c(1e-5, 1e5)
  )
  params <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
  expect_length(params$x$break_positions_minor(), 0)
})

test_that("no minor grid is drawn on the category axis", {
  # A minor grid on a discrete axis is clutter, and triage_theme() blanks both
  # directions precisely so any exception stays deliberate.
  p <- triage_plot_by_category(
    overview_rows(n = 60),
    "SITE_GEOGRAPHIC_FEATURE",
    "test"
  )
  expect_true(
    inherits(ggplot2::calc_element("panel.grid.minor.y", p$theme), "element_blank")
  )
})
