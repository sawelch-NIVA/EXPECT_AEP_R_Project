# The AEP node layer (PLAN.md P3.1-P3.4, added 2026-08-05).
#
# The property that matters most: a node is NOT a sampling group. It may pool
# several, and it may restrict them in ways the group key knows nothing about
# (latitude, date, source, outliers). These tests cover each of those and the
# ways the combination can go wrong.

node_fixture <- function(...) {
  base <- tibble::tibble(
    node_id = "N001",
    label = "Test node",
    level = "medium",
    node_type = "empirical",
    x = 0, y = 1,
    lat_min = NA_real_, lat_max = NA_real_,
    date_min = as.Date(NA), date_max = as.Date(NA),
    exclude_references = NA_character_,
    drop_outliers = FALSE,
    value = NA_real_, value_sd = NA_real_,
    value_n = NA_real_, value_unit = NA_character_,
    essentiality_score = 3, essentiality_justification = "x",
    plausibility_score = 3, plausibility_justification = "x",
    evidence_score = 2, evidence_justification = "x",
    quantification_score = 2, quantification_justification = "x",
    notes = NA_character_
  )
  args <- list(...)
  for (nm in names(args)) base[[nm]] <- args[[nm]]
  base
}

ids_fixture <- function() {
  tibble::tibble(
    ENVIRON_COMPARTMENT = c("Aquatic", "Aquatic", "Biota"),
    ENVIRON_COMPARTMENT_SUB = c("Freshwater", "Freshwater", "Biota, Aquatic"),
    SPECIES_GROUP = c(NA, NA, "Fish"),
    SAMPLE_SPECIES = c(NA, NA, "Gadus morhua"),
    SAMPLE_TISSUE = c(NA, NA, "Liver"),
    SITE_GEOGRAPHIC_FEATURE = c("River, stream, canal", "Lake, pond, pool, reservoir", "Coastal, fjord"),
    SITE_GEOGRAPHIC_FEATURE_SUB = c("Water column, pelagic zone", "Water column, pelagic zone", "Not reported"),
    MEASURED_UNIT_STANDARD = c("mg/L", "mg/L", "mg/kg (wet)"),
    group_id = c("G001", "G002", "G003")
  )
}

data_fixture <- function() {
  ids <- ids_fixture()
  # 10 rows per group, latitudes straddling the Arctic Circle, two references.
  purrr::list_rbind(purrr::map(seq_len(nrow(ids)), function(i) {
    row <- ids[i, ]
    tibble::tibble(
      ENVIRON_COMPARTMENT = row$ENVIRON_COMPARTMENT,
      ENVIRON_COMPARTMENT_SUB = row$ENVIRON_COMPARTMENT_SUB,
      SPECIES_GROUP = row$SPECIES_GROUP,
      SAMPLE_SPECIES = row$SAMPLE_SPECIES,
      SAMPLE_TISSUE = row$SAMPLE_TISSUE,
      SITE_GEOGRAPHIC_FEATURE = row$SITE_GEOGRAPHIC_FEATURE,
      SITE_GEOGRAPHIC_FEATURE_SUB = row$SITE_GEOGRAPHIC_FEATURE_SUB,
      MEASURED_UNIT_STANDARD = row$MEASURED_UNIT_STANDARD,
      MEASURED_VALUE_STANDARD = seq(1, 10) * i,
      MEASURED_N = 1L,
      LATITUDE = seq(60, 78, length.out = 10),
      LONGITUDE = 10,
      SAMPLING_DATE = seq(as.Date("2010-01-01"), by = "year", length.out = 10),
      REFERENCE_ID = rep(c("RefA", "RefB"), 5)
    )
  }))
}

members_fixture <- function(group_ids = "G001", node_id = "N001") {
  tibble::tibble(
    node_id = node_id,
    group_id = group_ids,
    notes = NA_character_
  )
}

# ---- Resolution ---------------------------------------------------------

test_that("a node resolves to exactly its member groups", {
  d <- data_fixture()
  out <- resolve_node_data(node_fixture(), members_fixture("G001"), d, ids_fixture())
  expect_equal(nrow(out), 10)
  expect_true(all(out$SITE_GEOGRAPHIC_FEATURE == "River, stream, canal"))
})

test_that("a node can pool several groups", {
  # The case group_decisions.csv cannot express: two sampling groups assessed as
  # one thing.
  d <- data_fixture()
  out <- resolve_node_data(
    node_fixture(), members_fixture(c("G001", "G002")), d, ids_fixture()
  )
  expect_equal(nrow(out), 20)
})

test_that("mixed units are refused rather than averaged", {
  # A mean across mg/L and mg/kg (wet) is meaningless, and the failure would be
  # invisible in a report card.
  d <- data_fixture()
  expect_error(
    resolve_node_data(
      node_fixture(), members_fixture(c("G001", "G003")), d, ids_fixture()
    ),
    "pools 2 units"
  )
})

test_that("a latitude restriction applies, and is not in the group key", {
  # THE CASE THAT MOTIVATED THE WHOLE DESIGN. docs/NBXX-algae.qmd restricts its
  # marine node with LATITUDE >= 66.5, which triage_group_cols() knows nothing
  # about.
  d <- data_fixture()
  out <- resolve_node_data(
    node_fixture(lat_min = 66.5), members_fixture("G001"), d, ids_fixture()
  )
  expect_lt(nrow(out), 10)
  expect_true(all(out$LATITUDE >= 66.5))
})

test_that("date restrictions apply on both ends", {
  d <- data_fixture()
  out <- resolve_node_data(
    node_fixture(date_min = as.Date("2013-01-01"), date_max = as.Date("2016-12-31")),
    members_fixture("G001"), d, ids_fixture()
  )
  expect_equal(nrow(out), 4)
})

test_that("references can be excluded, semicolon separated", {
  # Semicolons because a comma cannot survive a CSV cell unquoted.
  d <- data_fixture()
  out <- resolve_node_data(
    node_fixture(exclude_references = "RefA"), members_fixture("G001"), d, ids_fixture()
  )
  expect_equal(nrow(out), 5)
  expect_true(all(out$REFERENCE_ID == "RefB"))

  out2 <- resolve_node_data(
    node_fixture(exclude_references = "RefA; RefB"),
    members_fixture("G001"), d, ids_fixture()
  )
  expect_equal(nrow(out2), 0)
})

test_that("outliers are flagged WITHIN the node, not inherited", {
  # A value that is an outlier against its own small sampling group may be
  # unremarkable against the pooled node, and the node is the thing being
  # assessed.
  d <- data_fixture()
  d$MEASURED_VALUE_STANDARD[1] <- 1e6
  kept <- resolve_node_data(
    node_fixture(drop_outliers = TRUE), members_fixture("G001"), d, ids_fixture()
  )
  expect_false(1e6 %in% kept$MEASURED_VALUE_STANDARD)
  expect_lt(nrow(kept), 10)
})

test_that("an external node resolves to no rows and that is not an error", {
  d <- data_fixture()
  out <- resolve_node_data(
    node_fixture(node_type = "external"), members_fixture("G001"), d, ids_fixture()
  )
  expect_equal(nrow(out), 0)
})

# ---- Report cards -------------------------------------------------------

test_that("the report card reports measurements, and rows separately", {
  d <- data_fixture()
  d$MEASURED_N <- 5L
  card <- node_report_card(node_fixture(), members_fixture("G001"), d, ids_fixture())
  expect_equal(card$n, 50)
  expect_equal(card$n_rows, 10L)
})

test_that("Arctic coverage is reported, not filtered", {
  # Sam's decision 2026-08-05, over a global 66.5 cut that would have dropped
  # 81% of measurements. The node keeps all its data and states its Arctic share.
  d <- data_fixture()
  card <- node_report_card(node_fixture(), members_fixture("G001"), d, ids_fixture())

  expect_equal(card$n_rows, 10L)
  expect_gt(card$n_arctic, 0)
  expect_lt(card$n_arctic, card$n)
  expect_equal(card$pct_arctic, 100 * card$n_arctic / card$n)
})

test_that("an external node's card carries the hand-entered magnitude", {
  d <- data_fixture()
  node <- node_fixture(
    node_type = "external", value = 4200, value_unit = "kg/year", value_n = 1
  )
  card <- node_report_card(node, members_fixture("G001"), d, ids_fixture())
  expect_equal(card$mean, 4200)
  expect_equal(card$unit, "kg/year")
  expect_equal(card$n_rows, 0L)
})

test_that("geometric statistics are reported alongside arithmetic ones", {
  d <- data_fixture()
  card <- node_report_card(node_fixture(), members_fixture("G001"), d, ids_fixture())
  v <- 1:10
  expect_equal(card$geo_mean, 10^mean(log10(v)))
  expect_equal(card$gsd, 10^sd(log10(v)))
})

# ---- Validation ---------------------------------------------------------

test_that("validation warns about an empirical node with no members", {
  d <- data_fixture()
  nodes <- node_fixture()
  members <- members_fixture("G001")[0, ]
  cards <- aep_node_report_cards(nodes, members, d, ids_fixture())
  expect_warning(validate_aep_nodes(nodes, members, cards), "no members")
})

test_that("validation warns about a node whose restrictions exclude everything", {
  d <- data_fixture()
  nodes <- node_fixture(lat_min = 89)
  members <- members_fixture("G001")
  cards <- aep_node_report_cards(nodes, members, d, ids_fixture())
  expect_warning(validate_aep_nodes(nodes, members, cards), "resolve to no data")
})

test_that("validation warns about unscored and unplaced nodes", {
  d <- data_fixture()
  nodes <- node_fixture(evidence_score = NA_real_, x = NA_real_)
  members <- members_fixture("G001")
  cards <- aep_node_report_cards(nodes, members, d, ids_fixture())
  expect_warning(validate_aep_nodes(nodes, members, cards), "not fully EPEQ scored")
  expect_warning(validate_aep_nodes(nodes, members, cards), "no x/y placement")
})

test_that("a fully specified node layer validates silently", {
  d <- data_fixture()
  nodes <- node_fixture()
  members <- members_fixture("G001")
  cards <- aep_node_report_cards(nodes, members, d, ids_fixture())
  expect_no_warning(validate_aep_nodes(nodes, members, cards))
})

# ---- Coverage backlog ---------------------------------------------------

summary_fixture <- function() {
  ids <- ids_fixture()
  dplyr::bind_cols(
    ids |> dplyr::select(-"group_id"),
    tibble::tibble(
      n = c(1000, 500, 100),
      n_sources = 1L,
      species_common_name = NA_character_,
      flag_multimodal = FALSE,
      flag_outliers = FALSE
    )
  )
}

test_that("coverage names the node claiming each group, and flags the rest", {
  cov <- node_coverage(members_fixture("G001"), summary_fixture(), ids_fixture())
  expect_equal(nrow(cov), 3)
  expect_true(cov$claimed[cov$group_id == "G001"])
  expect_false(any(cov$claimed[cov$group_id != "G001"]))
  expect_equal(cov$node_id[cov$group_id == "G001"], "N001")
})

test_that("coverage is ranked by measurements, largest unclaimed first", {
  # The whole point of the backlog: the biggest thing you have not looked at is
  # always the first thing on screen.
  cov <- node_coverage(members_fixture("G003"), summary_fixture(), ids_fixture())
  unclaimed <- cov[!cov$claimed, ]
  expect_equal(unclaimed$group_id[1], "G001")
  expect_false(is.unsorted(rev(unclaimed$n)))
})

test_that("a group in two nodes is reported as being in both", {
  members <- dplyr::bind_rows(
    members_fixture("G001", "N001"),
    members_fixture("G001", "N002")
  )
  cov <- node_coverage(members, summary_fixture(), ids_fixture())
  expect_equal(cov$node_id[cov$group_id == "G001"], "N001; N002")
})

test_that("the coverage summary reports the share of measurements claimed", {
  cov <- node_coverage(members_fixture("G001"), summary_fixture(), ids_fixture())
  s <- node_coverage_summary(cov)
  expect_equal(s$groups, 3)
  expect_equal(s$groups_claimed, 1)
  expect_equal(s$measurements, 1600)
  expect_equal(s$measurements_claimed, 1000)
  expect_equal(s$pct_measurements_claimed, 62.5)
})

# ---- Readers ------------------------------------------------------------

test_that("the nodes reader rejects an out-of-range EPEQ score", {
  path <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(node_fixture(evidence_score = 4), path, na = "")
  expect_error(read_aep_nodes(path), "out-of-range evidence_score")
})

test_that("the nodes reader rejects an unknown level or node_type", {
  path <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(node_fixture(level = "compartment"), path, na = "")
  expect_error(read_aep_nodes(path), "Unrecognised level")

  path2 <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(node_fixture(node_type = "measured"), path2, na = "")
  expect_error(read_aep_nodes(path2), "Unrecognised node_type")
})

test_that("the nodes reader rejects duplicate node ids", {
  path <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(dplyr::bind_rows(node_fixture(), node_fixture()), path, na = "")
  expect_error(read_aep_nodes(path), "Duplicate node_id")
})

test_that("the membership reader rejects unknown ids", {
  path <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(members_fixture("G999"), path, na = "")
  expect_error(
    read_aep_node_members(path, ids = ids_fixture()),
    "unknown group_id"
  )
  expect_error(
    read_aep_node_members(path, nodes = node_fixture(node_id = "N002")),
    "unknown node_id"
  )
})
