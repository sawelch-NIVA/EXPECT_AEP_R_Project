# The AEP node layer (PLAN.md P3.1-P3.4, added 2026-08-05).
#
# The property that matters most: a node is NOT a sampling group. It may pool
# several, and it may restrict them in ways the group key knows nothing about
# (latitude, date, source, outliers). These tests cover each of those and the
# ways the combination can go wrong.

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

test_that("campaigns can be excluded, semicolon separated", {
  d <- data_fixture()
  out <- resolve_node_data(
    node_fixture(exclude_campaigns = "Camp Y (b)"),
    members_fixture("G001"), d, ids_fixture()
  )
  expect_equal(nrow(out), 7)
  expect_true(all(out$CAMPAIGN_NAME_SHORT == "Camp X (a)"))

  out2 <- resolve_node_data(
    node_fixture(exclude_campaigns = "Camp X (a); Camp Y (b)"),
    members_fixture("G001"), d, ids_fixture()
  )
  expect_equal(nrow(out2), 0)
})

test_that("a campaign name containing parentheses and spaces survives", {
  # Real names look like "Vm_2010_2025 (Urban Fjord Contaminants)". Matching is
  # exact and literal, never a pattern.
  d <- data_fixture()
  out <- resolve_node_data(
    node_fixture(exclude_campaigns = "Camp X (a)"),
    members_fixture("G001"), d, ids_fixture()
  )
  expect_equal(nrow(out), 3)
})

test_that("campaign and reference exclusions are independent", {
  # Crossed in the fixture, so excluding one must not silently do the other.
  d <- data_fixture()
  out <- resolve_node_data(
    node_fixture(exclude_campaigns = "Camp Y (b)"),
    members_fixture("G001"), d, ids_fixture()
  )
  expect_true(all(c("RefA", "RefB") %in% out$REFERENCE_ID))
})

test_that("an exclusion matching nothing warns rather than passing silently", {
  # The failure this project keeps rediscovering: the node still resolves, still
  # produces a mean, and the rows you thought you removed are still in it.
  d <- data_fixture()
  expect_warning(
    resolve_node_data(
      node_fixture(exclude_campaigns = "Camp Z (typo)"),
      members_fixture("G001"), d, ids_fixture()
    ),
    "matched no rows"
  )
  expect_warning(
    resolve_node_data(
      node_fixture(exclude_references = "RefC"),
      members_fixture("G001"), d, ids_fixture()
    ),
    "matched no rows"
  )
})

test_that("a partly-matching exclusion warns only about the missing part", {
  d <- data_fixture()
  expect_warning(
    out <- resolve_node_data(
      node_fixture(exclude_campaigns = "Camp Y (b); Camp Z (typo)"),
      members_fixture("G001"), d, ids_fixture()
    ),
    "Camp Z"
  )
  expect_equal(nrow(out), 7)
})

test_that("a blank or NA exclusion is silent and changes nothing", {
  d <- data_fixture()
  n <- nrow(resolve_node_data(
    node_fixture(), members_fixture("G001"), d, ids_fixture()
  ))
  for (v in list(NA_character_, "", "  ", ";;")) {
    expect_silent(
      out <- resolve_node_data(
        node_fixture(exclude_campaigns = v),
        members_fixture("G001"), d, ids_fixture()
      )
    )
    expect_equal(nrow(out), n)
  }
})

test_that("apply_node_exclusion degrades when the target column is absent", {
  d <- data_fixture()
  d$CAMPAIGN_NAME_SHORT <- NULL
  expect_warning(
    out <- apply_node_exclusion(
      d, node_fixture(exclude_campaigns = "Camp Y (b)"),
      "exclude_campaigns", "CAMPAIGN_NAME_SHORT"
    ),
    "no .*CAMPAIGN_NAME_SHORT.* column|did nothing"
  )
  expect_equal(nrow(out), nrow(d))
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
    node_type = "external", external_value = 4200,
    external_unit = "kg/year", external_n = 1
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

# ---- external_* belongs to external nodes only (2026-08-05) --------------

test_that("an empirical node with an external_* value is refused", {
  # Sam's question: "why are we specifying these manually rather than
  # calculating from constituent groups?" For empirical nodes we DO calculate
  # them, and these columns are never read. A number typed here would be
  # silently discarded, which is the same failure class as the untracked
  # decisions file. So it stops.
  for (col in external_value_cols()) {
    node <- node_fixture(node_type = "empirical")
    node[[col]] <- if (col == "external_unit") "kg/year" else 42
    path <- withr::local_tempfile(fileext = ".csv")
    readr::write_csv(node, path, na = "")
    expect_error(read_aep_nodes(path), "external_\\* values set", info = col)
  }
})

test_that("an external node may set them, and an empirical node left blank passes", {
  path <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(
    node_fixture(node_type = "external", external_value = 4200,
                 external_unit = "kg/year"),
    path, na = ""
  )
  expect_no_error(read_aep_nodes(path))

  path2 <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(node_fixture(), path2, na = "")
  expect_no_error(read_aep_nodes(path2))
})

test_that("the refusal message names the offending nodes", {
  nodes <- dplyr::bind_rows(
    node_fixture(node_id = "N001"),
    node_fixture(node_id = "N002", external_value = 10),
    node_fixture(node_id = "N003", external_value = 20)
  )
  path <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(nodes, path, na = "")
  expect_error(read_aep_nodes(path), "N002")
  expect_error(read_aep_nodes(path), "N003")
})

test_that("an external node with no magnitude is warned about", {
  # The converse half-finished state: nothing to compute from AND nothing typed
  # in, so the card reports NA and nothing else says so.
  d <- data_fixture()
  nodes <- node_fixture(node_type = "external")
  members <- members_fixture("G001")[0, ]
  cards <- aep_node_report_cards(nodes, members, d, ids_fixture())
  expect_warning(validate_aep_nodes(nodes, members, cards), "no external_value")
})

test_that("a single-row nodes table does not break the check", {
  # apply() over a one-row matrix is a classic drop-to-vector trap.
  path <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(node_fixture(), path, na = "")
  expect_no_error(read_aep_nodes(path))
  expect_equal(nrow(read_aep_nodes(path)), 1L)
})

# ---- Date bounds accept a bare year (2026-08-05) ------------------------

test_that("a bare year expands to the inclusive end of its interval", {
  # REGRESSION, and the third silent-emptying bug of the day. Sam typed
  # date_min = 1900, date_max = 2100, readr made them numbers, and comparing a
  # Date to 2100 reads it as days-since-1970: every node resolved to zero rows
  # with no error anywhere.
  expect_equal(parse_node_date("2010", "min"), as.Date("2010-01-01"))
  expect_equal(parse_node_date("2010", "max"), as.Date("2010-12-31"))
  expect_equal(parse_node_date(2100, "max"), as.Date("2100-12-31"))
  expect_equal(parse_node_date(1900, "min"), as.Date("1900-01-01"))
})

test_that("full dates and blanks pass through", {
  expect_equal(parse_node_date("2015-06-30", "min"), as.Date("2015-06-30"))
  expect_true(is.na(parse_node_date(NA, "min")))
  expect_true(is.na(parse_node_date("", "min")))
  expect_equal(parse_node_date(character(0), "min"), as.Date(character(0)))
})

test_that("an unparseable date errors rather than becoming no restriction", {
  # A restriction that quietly becomes "no restriction" changes what the node
  # means without saying so.
  expect_error(parse_node_date("30/06/2015", "min"), "Unparseable date_min")
  expect_error(parse_node_date("last tuesday", "max"), "Unparseable date_max")
})

test_that("year bounds actually keep the data they should", {
  d <- data_fixture()
  path <- withr::local_tempfile(fileext = ".csv")
  node <- node_fixture()
  node$date_min <- "1900"
  node$date_max <- "2100"
  readr::write_csv(node, path, na = "")

  nodes <- read_aep_nodes(path)
  expect_s3_class(nodes$date_min, "Date")
  out <- resolve_node_data(nodes, members_fixture("G001"), d, ids_fixture())
  expect_equal(nrow(out), 10)
})

test_that("inverted bounds are refused", {
  path <- withr::local_tempfile(fileext = ".csv")
  node <- node_fixture()
  node$date_min <- "2020"
  node$date_max <- "2010"
  readr::write_csv(node, path, na = "")
  expect_error(read_aep_nodes(path), "date_min after date_max")

  path2 <- withr::local_tempfile(fileext = ".csv")
  node2 <- node_fixture(lat_min = 80, lat_max = 60)
  readr::write_csv(node2, path2, na = "")
  expect_error(read_aep_nodes(path2), "lat_min above lat_max")
})

test_that("the resolver refuses a numeric date bound outright", {
  d <- data_fixture()
  node <- node_fixture()
  node$date_max <- 2100
  expect_error(
    resolve_node_data(node, members_fixture("G001"), d, ids_fixture()),
    "not a Date"
  )
})

test_that("empirical and external cards bind together despite IDate dates", {
  # REGRESSION, found by the pipeline and not by these tests, whose fixtures use
  # plain Dates. Real SAMPLING_DATE is an IDate via standardise_IDate_all(), and
  # vctrs refuses to combine IDate with the as.Date(NA) the external branch
  # returns: the whole node set failed to bind.
  d <- data_fixture()
  d$SAMPLING_DATE <- data.table::as.IDate(d$SAMPLING_DATE)
  nodes <- dplyr::bind_rows(
    node_fixture(node_id = "N001"),
    node_fixture(node_id = "N002", node_type = "external",
                 external_value = 1, external_unit = "kg/year")
  )
  members <- members_fixture("G001", "N001")

  cards <- aep_node_report_cards(nodes, members, d, ids_fixture())
  expect_equal(nrow(cards), 2)
  expect_s3_class(cards$date_min, "Date")
  expect_false(inherits(cards$date_min, "IDate"))
})

# ---- Weighting the centre but not the spread (2026-08-05) ---------------

test_that("weighted_median respects the weights", {
  expect_equal(weighted_median(c(1, 2, 3), c(1, 1, 1)), 2)
  # 100 copies of 3 drag the median to 3.
  expect_equal(weighted_median(c(1, 2, 3), c(1, 1, 100)), 3)
  expect_true(is.na(weighted_median(c(NA, NA), c(1, 1))))
  expect_true(is.na(weighted_median(numeric(0), numeric(0))))
  # Zero weights are ignored rather than counted.
  expect_equal(weighted_median(c(1, 999), c(1, 0)), 1)
})

test_that("the centre is weighted by MEASURED_N", {
  # THE FIX. A literature row summarising 50 samples must not be outvoted by a
  # single Vannmiljo observation, and the reported n must describe the same
  # population as the reported mean.
  d <- data_fixture()
  d <- d[d$SITE_GEOGRAPHIC_FEATURE == "River, stream, canal", ]
  d$MEASURED_VALUE_STANDARD <- c(rep(1, 9), 1000)
  d$MEASURED_N <- c(rep(1L, 9), 91L)

  card <- node_report_card(node_fixture(), members_fixture("G001"), d, ids_fixture())

  expect_equal(card$n, 100)
  expect_equal(card$n_rows, 10L)
  # Unweighted this would be 10^(9*0 + 3)/10 = 10^0.3 = 2.0. Weighted, the
  # 1000 carries 91 of the 100 measurements.
  expect_equal(card$geo_mean, 10^stats::weighted.mean(
    log10(d$MEASURED_VALUE_STANDARD), w = d$MEASURED_N
  ))
  expect_gt(card$geo_mean, 100)
  expect_equal(card$median, 1000)
})

test_that("the spread stays per row", {
  # Not weighted, and deliberately so: we hold study means, not study values, so
  # weighting would treat 91 samples as 91 copies of one number and report the
  # data as far tighter than it is.
  d <- data_fixture()
  d <- d[d$SITE_GEOGRAPHIC_FEATURE == "River, stream, canal", ]
  d$MEASURED_VALUE_STANDARD <- c(rep(1, 9), 1000)
  d$MEASURED_N <- c(rep(1L, 9), 91L)

  card <- node_report_card(node_fixture(), members_fixture("G001"), d, ids_fixture())
  expect_equal(card$gsd, 10^stats::sd(log10(d$MEASURED_VALUE_STANDARD)))
  expect_equal(card$sd, stats::sd(d$MEASURED_VALUE_STANDARD))
})

test_that("every level of aggregation is reported", {
  # Sam's requirement: a node spans groups, rows, measurements and references,
  # and the card must say how many of each.
  d <- data_fixture()
  card <- node_report_card(
    node_fixture(), members_fixture(c("G001", "G002")), d, ids_fixture()
  )
  for (col in c("n", "n_rows", "n_groups", "n_sources")) {
    expect_true(col %in% names(card), info = col)
    expect_false(is.na(card[[col]]), info = col)
  }
  expect_equal(card$n_groups, 2L)
  expect_equal(card$n_rows, 20L)
  expect_equal(card$n_sources, 2L)
})

test_that("weighting changes nothing where every row is one measurement", {
  # Which is 94% of this dataset, so the change must be a no-op there.
  d <- data_fixture()
  card <- node_report_card(node_fixture(), members_fixture("G001"), d, ids_fixture())
  v <- d$MEASURED_VALUE_STANDARD[d$SITE_GEOGRAPHIC_FEATURE == "River, stream, canal"]
  expect_equal(card$geo_mean, 10^mean(log10(v)))
  expect_equal(card$mean, mean(v))
})
