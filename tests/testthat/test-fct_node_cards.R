# Node report cards (PLAN.md 4.3, P3.2, added 2026-08-05).
#
# Two properties matter most. The card must draw CONSTITUENT GROUPS rather than
# the pooled distribution, because that is the only thing that can tell you a
# lumping was wrong. And it must degrade gracefully, because half-built nodes
# are the normal state while an assessment is in progress.

# card_nodes() is node_fixture() from helper-fixtures.R.
card_nodes <- node_fixture

# ---- EPEQ badges --------------------------------------------------------

test_that("the four criteria have distinct labels", {
  # Essentiality and Evidence both start with E, so single letters would put two
  # different criteria under one badge.
  labs <- epeq_badge_labels()
  expect_length(labs, 4)
  expect_equal(length(unique(labs)), 4)
  expect_setequal(names(labs), epeq_cols()[c(TRUE, FALSE)])
})

test_that("badges build, including for an unscored criterion", {
  # Explicitly unscored: the shared fixture scores all four.
  p <- node_epeq_badges(card_nodes(quantification_score = NA_real_))
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  drawn <- unlist(lapply(built$data, function(d) d$label))
  expect_true(all(c("Es", "Pl", "Ev", "Qn") %in% drawn))
  # The unscored one shows a dash, not a zero and not "NA".
  expect_true("-" %in% drawn)
  expect_false("NA" %in% drawn)
})

test_that("an unscored badge is grey, not the lowest-score colour", {
  # Grey must not read as a score of 1: "not assessed" and "assessed as weak"
  # are different claims.
  cols <- epeq_score_colours()
  expect_false(identical(cols[["NA"]], cols[["1"]]))
  expect_equal(length(unique(cols)), 4)
})

test_that("the score palette is not the threshold palette", {
  # A threshold class means red is bad; an EPEQ score means high is good.
  # Sharing a palette would invert the meaning of red halfway across a figure.
  expect_false(any(epeq_score_colours() %in% threshold_class_colours()[c("IV", "V")]))
})

# ---- Group strips -------------------------------------------------------

test_that("a strip is drawn per constituent group, not one for the pool", {
  # THE POINT OF THE CARD. A pooled distribution cannot reveal a bad lumping.
  d <- data_fixture()
  p <- node_group_strips(
    card_nodes(), members_fixture(c("G001", "G002")), d, ids_fixture(),
    limits = c(0.1, 100)
  )
  built <- ggplot2::ggplot_build(p)
  expect_s3_class(p, "ggplot")
  # Two discrete y positions, one per group.
  expect_equal(
    length(levels(ggplot2::ggplot_build(p)$plot$data$.facet)), 2
  )
})

test_that("strips are capped and the omission is named", {
  # G003 is a different unit and resolve_node_data() would refuse to pool it, so
  # the cap is exercised with two same-unit groups and max_groups = 1.
  d <- data_fixture()
  p <- node_group_strips(
    card_nodes(), members_fixture(c("G001", "G002")), d, ids_fixture(),
    limits = c(0.1, 100), max_groups = 1
  )
  expect_length(levels(ggplot2::ggplot_build(p)$plot$data$.facet), 1)
  # The dropped group is named rather than silently missing.
  expect_match(p$labels$caption, "1 smaller group")
})

test_that("groups are kept by measurement count, largest first", {
  d <- data_fixture()
  d$MEASURED_N <- ifelse(
    d$SITE_GEOGRAPHIC_FEATURE == "Lake, pond, pool, reservoir", 100L, 1L
  )
  p <- node_group_strips(
    card_nodes(), members_fixture(c("G001", "G002")), d, ids_fixture(),
    limits = c(0.1, 100), max_groups = 1
  )
  kept <- levels(ggplot2::ggplot_build(p)$plot$data$.facet)
  expect_equal(kept, "G002")
})

test_that("an external node gets a labelled blank, not an error", {
  d <- data_fixture()
  p <- node_group_strips(
    card_nodes(node_type = "external"), members_fixture("G001"), d,
    ids_fixture(), limits = c(0.1, 100)
  )
  expect_s3_class(p, "ggplot")
})

test_that("a node whose restrictions exclude everything gets a blank", {
  d <- data_fixture()
  p <- node_group_strips(
    card_nodes(lat_min = 89), members_fixture("G001"), d, ids_fixture(),
    limits = c(0.1, 100)
  )
  expect_s3_class(p, "ggplot")
})

# ---- Limits -------------------------------------------------------------

test_that("limits are shared per unit, not globally", {
  # An axis spanning mg/L water and mg/kg sediment compares incommensurable
  # things, so the sharing stops at the unit.
  d <- data_fixture()
  nodes <- dplyr::bind_rows(
    card_nodes(node_id = "N001"),
    card_nodes(node_id = "N002")
  )
  members <- dplyr::bind_rows(
    members_fixture("G001", "N001"),
    members_fixture("G003", "N002")
  )
  lims <- node_card_limits(nodes, members, d, ids_fixture())

  expect_setequal(names(lims), c("mg/L", "mg/kg (wet)"))
  expect_false(identical(lims[["mg/L"]], lims[["mg/kg (wet)"]]))
})

# ---- The whole card -----------------------------------------------------

test_that("a card assembles and writes", {
  d <- data_fixture()
  nodes <- card_nodes()
  members <- members_fixture("G001")
  ids <- ids_fixture()
  cards <- aep_node_report_cards(nodes, members, d, ids)

  dir <- withr::local_tempdir()
  paths <- suppressWarnings(
    write_node_cards(nodes, cards, members, d, ids, dir = dir)
  )
  expect_length(paths, 1)
  expect_true(file.exists(paths))
  # An actual draw, not just a constructed object: ggsave is where a malformed
  # patchwork fails.
  expect_gt(file.size(paths), 5000)
})

test_that("a card with no unit does not print the string NA", {
  # An external node has no measured unit, and "geo. mean - NA" reads as a value.
  d <- data_fixture()
  nodes <- card_nodes(node_type = "external")
  cards <- aep_node_report_cards(nodes, members_fixture("G001")[0, ], d, ids_fixture())
  p <- node_card_header(nodes, cards)
  built <- ggplot2::ggplot_build(p)
  drawn <- unlist(lapply(built$data, function(x) x$label))
  expect_false(any(grepl("NA", drawn)))
})

test_that("the header reports all four aggregation levels", {
  d <- data_fixture()
  nodes <- card_nodes()
  members <- members_fixture(c("G001", "G002"))
  cards <- aep_node_report_cards(nodes, members, d, ids_fixture())
  p <- node_card_header(nodes, cards)
  drawn <- unlist(lapply(ggplot2::ggplot_build(p)$data, function(x) x$label))
  txt <- paste(drawn, collapse = " ")

  expect_match(txt, "measurements")
  expect_match(txt, "rows")
  expect_match(txt, "group")
  expect_match(txt, "ref")
})
