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
  # One label per badge, letter and score on the same line (Sam 2026-08-05):
  # stacked needed a badge twice as tall for no extra information.
  expect_true(all(grepl("^(Es|Pl|Ev|Qn) ", drawn)))
  expect_length(drawn, 4)
  # The unscored one shows a dash, not a zero and not "NA".
  expect_true(any(grepl("Qn -$", drawn)))
  expect_false(any(grepl("NA", drawn)))
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

test_that("strips are capped at max_groups", {
  # G003 is a different unit and resolve_node_data() would refuse to pool it, so
  # the cap is exercised with two same-unit groups and max_groups = 1.
  d <- data_fixture()
  p <- node_group_strips(
    card_nodes(), members_fixture(c("G001", "G002")), d, ids_fixture(),
    limits = c(0.1, 100), max_groups = 1
  )
  expect_length(levels(ggplot2::ggplot_build(p)$plot$data$.facet), 1)
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

# ---- External nodes with a time series (2026-08-11) ---------------------
#
# A source figure like REACH net copper by sector is natively (year, value)
# before it collapses to the single mean/sd typed into external_value. Where
# that series is available, it draws instead of the "no measured data" blank.

test_that("bars are drawn, not a scale, for a single value in isolation", {
  series <- tibble::tibble(year = 2020, value = 5)
  p <- node_external_series_bars(series)
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  expect_true(any(c("GeomLine","GeomPoint") %in% vapply(built$plot$layers, function(l) class(l$geom)[1], character(1))))
})

test_that("the mean reference line is omitted when mean_value is NA", {
  series <- tibble::tibble(year = c(2020, 2021), value = c(5, 7))
  p <- node_external_series_bars(series, mean_value = NA_real_)
  built <- ggplot2::ggplot_build(p)
  geoms <- vapply(built$plot$layers, function(l) class(l$geom)[1], character(1))
  # GeomSegment, not GeomHline, since 2026-08-13: the line is clipped to the
  # node's own years rather than spanning the shared panel.
  expect_false("GeomSegment" %in% geoms)
})

test_that("the mean reference line is drawn when mean_value is supplied", {
  series <- tibble::tibble(year = c(2020, 2021), value = c(5, 7))
  p <- node_external_series_bars(series, mean_value = 6)
  built <- ggplot2::ggplot_build(p)
  geoms <- vapply(built$plot$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomSegment" %in% geoms)
})

test_that("node_group_strips() draws bars for an external node with a series", {
  d <- data_fixture()
  series <- list(
    "N001" = tibble::tibble(year = c(2019, 2020, 2021), value = c(3, 5, 4))
  )
  p <- node_group_strips(
    card_nodes(node_type = "external", external_value = 4),
    members_fixture("G001"), d, ids_fixture(),
    limits = c(0.1, 100), external_series = series
  )
  built <- ggplot2::ggplot_build(p)
  expect_true("GeomPoint" %in% vapply(
    built$plot$layers, function(l) class(l$geom)[1], character(1)
  ))
})

test_that("node_group_strips() falls back to a blank when no series matches this node_id", {
  d <- data_fixture()
  series <- list("N999-somewhere-else" = tibble::tibble(year = 2020, value = 5))
  p <- node_group_strips(
    card_nodes(node_type = "external"), members_fixture("G001"), d,
    ids_fixture(), limits = c(0.1, 100), external_series = series
  )
  built <- ggplot2::ggplot_build(p)
  expect_false("GeomPoint" %in% vapply(
    built$plot$layers, function(l) class(l$geom)[1], character(1)
  ))
})

test_that("an empirical node ignores external_series even if one is passed", {
  # external_series only applies to node_type = "external"; a distribution
  # node draws its violin regardless of what is in the lookup.
  d <- data_fixture()
  series <- list("N001" = tibble::tibble(year = 2020, value = 5))
  p <- node_group_strips(
    card_nodes(), members_fixture(c("G001", "G002")), d, ids_fixture(),
    limits = c(0.1, 100), external_series = series
  )
  built <- ggplot2::ggplot_build(p)
  geoms <- vapply(built$plot$layers, function(l) class(l$geom)[1], character(1))
  # NOT asserted on the absence of a point. Since 2026-08-13 the series draws
  # points, and so does the strips path below its n-switch (this fixture is
  # small, so it draws points and a boxplot rather than a violin). The two
  # geoms that actually separate the paths are the boxplot, which only the
  # strips draw, and the connecting line, which only the series draws.
  expect_true("GeomBoxplot" %in% geoms)
  expect_false("GeomLine" %in% geoms)
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

test_that("the header reports sample size and source count", {
  d <- data_fixture()
  nodes <- card_nodes()
  members <- members_fixture(c("G001", "G002"))
  cards <- aep_node_report_cards(nodes, members, d, ids_fixture())
  p <- node_card_header(nodes, cards)
  drawn <- unlist(lapply(ggplot2::ggplot_build(p)$data, function(x) x$label))
  txt <- paste(drawn, collapse = " ")

  expect_match(txt, "n = ")
  expect_match(txt, "refs = ")
})

test_that("the geometric mean is the bold headline", {
  d <- data_fixture()
  nodes <- card_nodes()
  cards <- aep_node_report_cards(nodes, members_fixture("G001"), d, ids_fixture())
  p <- node_card_header(nodes, cards)
  built <- ggplot2::ggplot_build(p)

  faces <- unlist(lapply(built$data, function(x) x$fontface))
  labels <- unlist(lapply(built$data, function(x) x$label))
  headline <- labels[faces == "bold" | faces == 2]
  expect_true(any(grepl(formatC(cards$geo_mean, format = "g", digits = 3),
                        headline, fixed = TRUE)))
})

# ---- Labelling which statistic the headline is (2026-08-11) -------------
#
# The coalesce in node_card_header() that fixed the "external nodes show a
# blank headline" bug means the headline can now be EITHER a geometric mean
# (an empirical node with a real distribution) or an arithmetic mean (an
# external node's typed-in value, which has no distribution to take a
# geometric mean of). Nothing distinguished the two on the card itself, and
# they are not interchangeable numbers, so the headline is now prefixed
# "GM "/"AM " to say which one a reader is looking at.

test_that("an empirical node's headline is labelled GM", {
  d <- data_fixture()
  nodes <- card_nodes()
  cards <- aep_node_report_cards(nodes, members_fixture("G001"), d, ids_fixture())
  built <- ggplot2::ggplot_build(node_card_header(nodes, cards))

  faces <- unlist(lapply(built$data, function(x) x$fontface))
  labels <- unlist(lapply(built$data, function(x) x$label))
  headline <- labels[faces == "bold" | faces == 2]
  expect_true(any(grepl("^GM ", headline)))
  expect_false(any(grepl("^AM ", headline)))
})

test_that("an external node's headline is labelled AM, not GM", {
  # External nodes never get a geo_mean (see resolve_node_data()); the
  # headline falls back to the typed-in external_value, which is an
  # arithmetic figure, not a geometric one.
  nodes <- card_nodes(
    node_type = "external",
    external_value = 1000000, external_sd = 200000,
    external_n = 5, external_unit = "kg/y"
  )
  cards <- aep_node_report_cards(
    nodes, members_fixture("G001")[0, ], data_fixture(), ids_fixture()
  )
  built <- ggplot2::ggplot_build(node_card_header(nodes, cards))

  faces <- unlist(lapply(built$data, function(x) x$fontface))
  labels <- unlist(lapply(built$data, function(x) x$label))
  headline <- labels[faces == "bold" | faces == 2]
  expect_true(any(grepl("^AM ", headline)))
  expect_false(any(grepl("^GM ", headline)))
  expect_true(any(grepl(formatC(1000000, format = "g", digits = 3),
                        headline, fixed = TRUE)))
})

test_that("an external node with no value entered gets no GM/AM label", {
  # N001-natural-occurrence's real state: external, nothing typed in yet.
  # "AM -" or "GM -" would be a label on nothing.
  nodes <- card_nodes(node_type = "external")
  cards <- aep_node_report_cards(
    nodes, members_fixture("G001")[0, ], data_fixture(), ids_fixture()
  )
  built <- ggplot2::ggplot_build(node_card_header(nodes, cards))

  drawn <- unlist(lapply(built$data, function(x) x$label))
  expect_false(any(grepl("GM|AM", drawn)))
})

test_that("Arctic coverage is off the card but still computed", {
  # Sam: "remove the Arctic measure from the plot, but keep the code."
  d <- data_fixture()
  nodes <- card_nodes()
  cards <- aep_node_report_cards(nodes, members_fixture("G001"), d, ids_fixture())

  expect_true("pct_arctic" %in% names(cards))
  expect_false(is.na(cards$pct_arctic))

  drawn <- unlist(lapply(
    ggplot2::ggplot_build(node_card_header(nodes, cards))$data,
    function(x) x$label
  ))
  expect_false(any(grepl("Arctic", drawn)))
})

# ---- Marking an untrustworthy headline (2026-08-05) ---------------------

test_that("geometric mean and median agreeing is not suspect", {
  # They coincide exactly on a lognormal distribution, so agreement is the
  # normal state and must not be flagged.
  card <- tibble::tibble(geo_mean = 1.68, median = 1.3)
  expect_false(headline_is_suspect(card))
})

test_that("a large divergence marks the headline", {
  # N005: geo mean 8.0 against median 0.235, a thirtyfold gap that says the node
  # holds two populations rather than one.
  card <- tibble::tibble(geo_mean = 8.0, median = 0.235)
  expect_true(headline_is_suspect(card))
})

test_that("missing or non-positive statistics abstain rather than flag", {
  expect_true(is.na(headline_is_suspect(tibble::tibble(geo_mean = NA, median = 1))))
  expect_true(is.na(headline_is_suspect(tibble::tibble(geo_mean = 1, median = NA))))
  expect_true(is.na(headline_is_suspect(tibble::tibble(geo_mean = 0, median = 1))))
})

test_that("a suspect headline is marked on the card", {
  d <- data_fixture()
  d <- d[d$SITE_GEOGRAPHIC_FEATURE == "River, stream, canal", ]
  # Two populations four orders apart, which is the N005 shape.
  d$MEASURED_VALUE_STANDARD <- c(rep(0.2, 5), rep(2000, 5))

  cards <- aep_node_report_cards(
    card_nodes(), members_fixture("G001"), d, ids_fixture()
  )
  expect_true(headline_is_suspect(cards))

  drawn <- unlist(lapply(
    ggplot2::ggplot_build(node_card_header(card_nodes(), cards))$data,
    function(x) x$label
  ))
  expect_true(any(grepl("(!)", drawn, fixed = TRUE)))
})

# ---- Compact style ------------------------------------------------------

test_that("the compact strip keeps a decade axis and blanks the real y axis", {
  # UPDATED 2026-08-08 for the 2026-08-06 design this had fallen behind: group
  # ids moved OFF the real y axis and onto compact_group_labels(), a geom_text
  # drawn inside the panel (see node_group_strips()), so the real y axis text
  # is now redundant and blanked. compact_value_scale() added a real x axis
  # with decade breaks the same day ("violin plots need _some_ kind of x axis
  # or they're fairly meaningless"), so x is the one that stays.
  d <- data_fixture()
  p <- node_group_strips(
    card_nodes(), members_fixture("G001"), d, ids_fixture(),
    limits = c(0.1, 100)
  )
  built <- ggplot2::ggplot_build(p)
  expect_false(inherits(built$plot$theme$axis.text.x, "element_blank"))
  expect_s3_class(built$plot$theme$axis.text.y, "element_blank")
})

test_that("a card writes", {
  d <- data_fixture()
  nodes <- card_nodes()
  members <- members_fixture("G001")
  ids <- ids_fixture()
  cards <- aep_node_report_cards(nodes, members, d, ids)

  dir <- withr::local_tempdir()
  paths <- suppressWarnings(
    write_node_cards(nodes, cards, members, d, ids, dir = dir)
  )
  expect_true(file.exists(paths))
})

test_that("long labels wrap rather than running off the card", {
  # "Aquaculture copper application" ran off both ends of a 2.4in canvas.
  nodes <- card_nodes(label = "Aquaculture copper application")
  cards <- tibble::tibble(
    node_id = "N001", label = nodes$label, geo_mean = NA_real_, mean = NA_real_,
    median = NA_real_, gsd = NA_real_, unit = NA_character_, n = NA_real_,
    n_rows = 0L, n_groups = 0L, n_sources = NA_integer_, pct_arctic = NA_real_
  )
  p <- node_card_header(nodes, cards)
  drawn <- unlist(lapply(ggplot2::ggplot_build(p)$data, function(x) x$label))
  expect_true(any(grepl("\n", drawn, fixed = TRUE)))
})

# ---- Compact card content (2026-08-06) ---------------------------------

test_that("the compact header outranks the headline number", {
  # Sam 2026-08-06: the name is the header and should be larger than the
  # geometric mean below it.
  #
  # UPDATED 2026-08-08: the old selector found the title by grepping for the
  # node's own id (nodes$node_id[1]) in the drawn labels. That stopped working
  # when the id was moved 2026-08-06 into its own small standalone corner
  # marker (size 2.6), separate from the bold title text (size 4.5), which
  # contains only the wrapped label and never the id -- so the old selector
  # picked up the corner marker instead of the actual title, compared 2.6
  # against the headline's 3.7, and failed even though the real title (4.5)
  # genuinely is larger. The label itself (str_wrap()-ped, so match its first
  # word rather than the full string) is now what identifies the title layer.
  d <- data_fixture()
  nodes <- card_nodes()
  cards <- aep_node_report_cards(nodes, members_fixture("G001"), d, ids_fixture())
  built <- ggplot2::ggplot_build(node_card_header(nodes, cards))

  # The corner id marker is drawn via annotation_custom() (Sam 2026-08-07: pinned
  # to a fixed 18pt/18pt offset from the card corner so long composite ids like
  # "N003-mine-tailings" don't drift), so its layer has no label/size columns and
  # must be skipped rather than rbind-ed with the geom_text layers.
  text_layers <- Filter(function(x) all(c("label", "size") %in% names(x)), built$data)
  layers <- do.call(rbind, lapply(text_layers, function(x) {
    x[, c("label", "size"), drop = FALSE]
  }))
  label_word <- strsplit(nodes$label[1], " ")[[1]][1]
  title_size <- layers$size[grepl(label_word, layers$label, fixed = TRUE)]
  headline_size <- layers$size[grepl(cards$unit[1], layers$label, fixed = TRUE)]

  expect_length(title_size, 1)
  expect_gt(title_size, max(headline_size))
})

test_that("the compact card keeps sample size and source count", {
  # A concentration with no n and no source count behind it is not a finding.
  d <- data_fixture()
  nodes <- card_nodes()
  cards <- aep_node_report_cards(nodes, members_fixture("G001"), d, ids_fixture())
  drawn <- unlist(lapply(
    ggplot2::ggplot_build(node_card_header(nodes, cards))$data,
    function(x) x$label
  ))
  txt <- paste(drawn, collapse = " ")

  expect_match(txt, "n = ")
  expect_match(txt, "refs = ")
  # But not the fuller breakdown, which is what "compact" means.
  expect_false(grepl("rows = ", txt))
})

test_that("the compact strip keeps group ids inside the panel, not on the y axis", {
  # UPDATED 2026-08-08, same fix as "the compact strip keeps a decade axis and
  # blanks the real y axis" above: group ids moved OFF the real y axis onto
  # compact_group_labels() on 2026-08-06, so the real y axis text is blanked
  # and the decade x axis stays. This test checks the OTHER object plot_aep()
  # code paths use (the ggplot object directly, not a built one), which is why
  # it is a separate test from the ggplot_build() version above rather than a
  # duplicate.
  d <- data_fixture()
  p <- node_group_strips(
    card_nodes(), members_fixture("G001"), d, ids_fixture(),
    limits = c(0.1, 100)
  )
  expect_false(inherits(p$theme$axis.text.x, "element_blank"))
  expect_s3_class(p$theme$axis.text.y, "element_blank")
})

# ---- Shared axes for the external-node bar charts ------------------------
# Added 2026-08-12. Three separate requirements from Sam, all easy to break
# silently: one pair of axes across every source node, violin styling, and a
# panel that occupies exactly the space a violin panel would.

series_fixture <- function() {
  list(
    big   = tibble::tibble(year = 2018:2020, value = c(1e6, 2e6, 5.85e7)),
    small = tibble::tibble(year = 2019:2021, value = c(74, 120, 148)),
    neg   = tibble::tibble(year = 2020:2021, value = c(-5.45e5, 3e5))
  )
}

test_that("shared limits span every node's series, not just one", {
  lim <- external_series_limits(series_fixture())
  expect_equal(lim$x, c(2017.5, 2021.5))
  expect_gte(lim$y[2], 5.85e7)
})

test_that("the shared y range is strictly positive, because the axis is log", {
  # Reversed from "always include zero" on 2026-08-12 when the axis went log.
  # A single negative anywhere must not be able to make the shared range
  # infinite or NaN, because that would break EVERY source card rather than
  # the one node at fault.
  lim <- external_series_limits(series_fixture())
  expect_gt(lim$y[1], 0)
  expect_true(all(is.finite(lim$y)))

  only_neg <- external_series_limits(
    list(a = tibble::tibble(year = 2020, value = c(-5, -1)))
  )
  expect_null(only_neg)
})

test_that("empty, NULL and all-missing series give NULL rather than an error", {
  expect_null(external_series_limits(NULL))
  expect_null(external_series_limits(list()))
  expect_null(external_series_limits(list(a = tibble::tibble(
    year = numeric(0), value = numeric(0)
  ))))
  expect_null(external_series_limits(list(a = tibble::tibble(
    year = NA_real_, value = NA_real_
  ))))
})

test_that("bars are limited by coord_cartesian, never by scale limits", {
  # scale_*_continuous(limits=) CENSORS out-of-range rows and silently drops
  # the bar. That mistake already ate the triage panels' count labels once
  # (PLAN.md 9a), and here it would delete a whole node's data.
  lim <- external_series_limits(series_fixture())
  p <- node_external_series_bars(series_fixture()$small, limits = lim)
  expect_s3_class(p$coordinates, "CoordCartesian")
  built <- ggplot2::ggplot_build(p)
  expect_equal(nrow(built$data[[1]]), 3)
})

test_that("the bar y axis is log10", {
  p <- node_external_series_bars(series_fixture()$small)
  expect_equal(p$scales$get_scales("y")$trans$name, "log-10")
})

test_that("every source node ends up on the same y window", {
  # The whole point of sharing. Built rather than asserted on the arguments,
  # so a scale that quietly overrides the coord would be caught.
  lim <- external_series_limits(series_fixture())
  win <- lapply(c("big", "small"), function(nm) {
    b <- ggplot2::ggplot_build(
      node_external_series_bars(series_fixture()[[nm]], limits = lim)
    )
    b$layout$panel_params[[1]]$y.range
  })
  expect_equal(win[[1]], win[[2]])
})

test_that("non-positive years are dropped loudly, not silently", {
  # A log axis cannot show them, and ggplot2's own "removed n rows" says
  # nothing about which node or why.
  expect_warning(
    node_external_series_bars(series_fixture()$neg),
    "non-positive"
  )
  p <- suppressWarnings(node_external_series_bars(series_fixture()$neg))
  built <- suppressWarnings(ggplot2::ggplot_build(p))
  expect_equal(nrow(built$data[[1]]), 1)
})

test_that("the bar panel claims no horizontal space outside itself", {
  # The alignment fix. patchwork aligns a card's three panels on their panel
  # areas, so a y axis here pushed the header and badge strips in by 0.46in on
  # a 2.4in card. Measured before the fix; asserted here so it cannot return.
  p <- node_external_series_bars(series_fixture()$small)
  expect_s3_class(p$theme$axis.text.y, "element_blank")
  expect_s3_class(p$theme$axis.ticks.y, "element_blank")
})

test_that("the marks take the violins' colour but not their alpha", {
  # The colour is shared so a card reads as one design; the alpha is not.
  # A violin is an area whose overlaps must stay readable at 0.35, while these
  # are a few points and a thin line that wash out on a pastel background.
  p <- node_external_series_bars(series_fixture()$small)
  cols <- vapply(p$layers, function(l) l$aes_params$colour %||% NA_character_,
                 character(1))
  expect_true(all(cols[!is.na(cols)] == "grey35"))
  # Opaque: no alpha channel baked in and no alpha aesthetic set.
  expect_false(any(grepl("^#[0-9A-Fa-f]{8}$", stats::na.omit(cols))))
  expect_true(all(vapply(
    p$layers, function(l) is.null(l$aes_params$alpha), logical(1)
  )))
})

test_that("EPEQ 1 is red and 2 is yellow", {
  # Sam 2026-08-12: the old bronze/gold ramp read as a medal, not a warning.
  cols <- epeq_score_colours()
  to_rgb <- function(h) grDevices::col2rgb(h)[, 1]
  expect_gt(to_rgb(cols[["1"]])["red"], to_rgb(cols[["1"]])["green"])
  expect_gt(to_rgb(cols[["2"]])["green"], to_rgb(cols[["1"]])["green"])
  expect_gt(to_rgb(cols[["3"]])["green"], to_rgb(cols[["3"]])["red"])
  # Unscored must never read as a low score.
  expect_equal(cols[["NA"]], "#D9D9D9")
})

# ---- In-panel value labels for the bars ----------------------------------
# Added 2026-08-12. Removing the y axis fixed the card alignment and went too
# far: the bars had no scale at all. The labels come back INSIDE the panel,
# where they cost no horizontal width, exactly as compact_group_labels() does
# for the violins.

test_that("value labels are exponent-only and inside the panel", {
  lim <- external_series_limits(series_fixture())
  layers <- compact_bar_value_labels(lim)
  expect_gt(length(layers), 0)
  d <- layers[[1]]$data
  expect_true(all(grepl("^1e-?[0-9]+$", d$label)))
  # Anchored at the panel's own left edge, not outside it.
  expect_true(all(d$x == lim$x[1]))
  # And every label sits within the shared y window.
  expect_true(all(d$y >= lim$y[1] & d$y <= lim$y[2]))
})

test_that("value labels are identical across cards, since the scale is shared", {
  lim <- external_series_limits(series_fixture())
  a <- compact_bar_value_labels(lim)[[1]]$data
  b <- compact_bar_value_labels(lim)[[1]]$data
  expect_equal(a$label, b$label)
})

test_that("degenerate limits give no labels rather than an error", {
  expect_equal(length(compact_bar_value_labels(NULL)), 0)
  expect_equal(
    length(compact_bar_value_labels(list(x = c(1, 2), y = c(0, 10)))), 0
  )
  expect_equal(
    length(compact_bar_value_labels(list(x = c(1, 2), y = c(NA, 10)))), 0
  )
})

test_that("the bar panel still claims no horizontal space with labels on", {
  # The whole point of drawing them inside. Guard against someone "fixing"
  # this back into a real axis.
  lim <- external_series_limits(series_fixture())
  p <- node_external_series_bars(series_fixture()$small, limits = lim)
  expect_s3_class(p$theme$axis.text.y, "element_blank")
  expect_s3_class(p$theme$axis.ticks.y, "element_blank")
})

test_that("an external node reports its hand-entered reference count", {
  # "refs = -" read as missing information on the REACH cards, which all come
  # from one extract, so the count is known rather than unknown.
  node <- tibble::tibble(
    node_id = "N004-x", label = "X", level = "source", node_type = "external",
    external_value = 42, external_sd = NA_real_, external_n = 6,
    external_unit = "kg/y", external_refs = 1,
    lat_min = NA_real_, lat_max = NA_real_,
    date_min = NA, date_max = NA,
    exclude_references = NA_character_, exclude_campaigns = NA_character_,
    drop_outliers = FALSE
  )
  card <- node_report_card(node, members_fixture("G001"), data_fixture(),
                           ids_fixture())
  expect_equal(card$n_sources, 1L)

  node$external_refs <- NA_real_
  card2 <- node_report_card(node, members_fixture("G001"), data_fixture(),
                            ids_fixture())
  expect_true(is.na(card2$n_sources))
})

test_that("marks sit at their own value, with no baseline to lose", {
  # WHY POINTS. geom_col()'s ymin is 0, which scale_y_log10() sends to -Inf,
  # and ggplot2 resolved that by putting the base at 0 in TRANSFORMED space,
  # i.e. y = 1 in data units: a bar of height 100 built as
  # `ymin = 0, ymax = 2`. A point has no baseline, so it cannot acquire a
  # wrong one.
  lim <- external_series_limits(series_fixture())
  p <- node_external_series_bars(series_fixture()$small, limits = lim)
  b <- ggplot2::ggplot_build(p)
  pt <- b$data[[which(vapply(
    p$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1)
  ))]]
  expect_equal(sort(round(10^pt$y)), sort(series_fixture()$small$value))
  expect_false("ymin" %in% names(pt))
})

test_that("the shared range is floored a fixed number of orders below the top", {
  # Five sub-gram values in one REACH sector stretched the axis to 12.3 orders
  # and spent 40% of every panel below the smallest real number.
  s <- list(
    big = tibble::tibble(year = 2020, value = 1e6),
    dregs = tibble::tibble(year = 2021, value = 7.4e-5)
  )
  lim <- external_series_limits(s, max_orders = 6)
  expect_gt(lim$y[1], 1e-5)
  expect_lt(log10(lim$y[2]) - log10(lim$y[1]), 7.5)
  # Without the floor it would span eleven.
  wide <- external_series_limits(s, max_orders = 20)
  expect_gt(log10(wide$y[2]) - log10(wide$y[1]), 10)
})

test_that("years are summed before drawing, one bar per year", {
  # Stacking went with geom_col. A node lumping several sectors claims their
  # sum, so the sum is what is drawn.
  s <- tibble::tibble(year = c(2020, 2020, 2021), value = c(10, 30, 50))
  p <- node_external_series_bars(s)
  b <- ggplot2::ggplot_build(p)
  expect_equal(nrow(b$data[[1]]), 2)
  expect_equal(sort(round(10^b$data[[1]]$y)), c(40, 50))
})

# ---- Gridlines and labels must agree -------------------------------------
# Added 2026-08-13. They were computed independently (scale_y_log10()'s own
# defaults for the lines, scales::breaks_log() for the text) and on the real
# cards produced three gridlines against two labels, with no way to tell which
# line either label belonged to.

test_that("every gridline gets exactly one label", {
  lim <- external_series_limits(series_fixture())
  p <- node_external_series_bars(series_fixture()$small, limits = lim)
  b <- ggplot2::ggplot_build(p)

  drawn <- b$layout$panel_params[[1]]$y$breaks
  drawn <- drawn[is.finite(drawn)]

  txt <- b$data[[which(vapply(
    p$layers, function(l) inherits(l$geom, "GeomText"), logical(1)
  ))]]
  expect_equal(length(drawn), nrow(txt))
  expect_equal(sort(drawn), sort(txt$y))
})

test_that("breaks are whole powers of ten", {
  # Intermediate breaks like 2.5e6 are unreadable at card size and pointless on
  # a log axis, where the decades are the structure.
  b <- external_series_breaks(external_series_limits(series_fixture()))
  expect_true(all(abs(log10(b) - round(log10(b))) < 1e-9))
})

test_that("breaks stay inside the panel and are capped at n", {
  lim <- external_series_limits(series_fixture())
  b <- external_series_breaks(lim, n = 3)
  expect_lte(length(b), 3)
  expect_true(all(b >= lim$y[1] & b <= lim$y[2]))
})

test_that("degenerate limits give no breaks rather than an error", {
  expect_length(external_series_breaks(NULL), 0)
  expect_length(external_series_breaks(list(x = c(1, 2), y = c(0, 10))), 0)
  expect_length(external_series_breaks(list(x = c(1, 2), y = c(NA, 10))), 0)
  # A range too narrow to contain a whole power of ten.
  expect_length(external_series_breaks(list(x = c(1, 2), y = c(11, 12))), 0)
})

# ---- The mean line -------------------------------------------------------

test_that("the mean line spans the node's own years, not the whole panel", {
  # A shared x axis meant geom_hline ran past both ends of a short series and
  # read as a threshold for the panel rather than the mean of these points.
  lim <- external_series_limits(series_fixture())
  s <- series_fixture()$small
  p <- node_external_series_bars(s, mean_value = 100, limits = lim)
  seg <- p$layers[[which(vapply(
    p$layers, function(l) inherits(l$geom, "GeomSegment"), logical(1)
  ))[1]]]
  expect_equal(seg$data$x, min(s$year))
  expect_equal(seg$data$xend, max(s$year))
  # And it stops short of the panel edges, which the shared limits set wider.
  expect_gt(seg$data$x, lim$x[1])
  expect_lt(seg$data$xend, lim$x[2])
})

test_that("the mean line is annotated AM, inside the panel", {
  s <- series_fixture()$small
  p <- node_external_series_bars(s, mean_value = 100,
                                 limits = external_series_limits(series_fixture()))
  # annotate() with a scalar label puts it in aes_params, NOT in the layer's
  # data, unlike compact_bar_value_labels() which builds a data frame. Both are
  # GeomText, so a search has to look in both places.
  label_of <- function(l) l$aes_params$label %||% l$data$label
  txt <- Filter(function(l) inherits(l$geom, "GeomText"), p$layers)
  am <- Filter(function(l) any(label_of(l) == "AM"), txt)
  expect_length(am, 1)
  # Right-anchored at the line's end, so it can never overflow the panel.
  expect_equal(am[[1]]$aes_params$hjust, 1)
  expect_equal(am[[1]]$data$x, max(s$year))
})

test_that("no mean line is drawn for a non-positive or missing mean", {
  s <- series_fixture()$small
  for (mv in list(NA_real_, 0, -5)) {
    p <- node_external_series_bars(s, mean_value = mv)
    has_seg <- any(vapply(p$layers, function(l) inherits(l$geom, "GeomSegment"),
                          logical(1)))
    expect_false(has_seg)
  }
})

test_that("no node label is long enough to wrap onto the headline", {
  # REGRESSION, 2026-08-13. str_wrap(width = 18) split "Marine benthic
  # inverts" onto two lines and the second landed on "GM 18.2 mg/kg (dry)"
  # underneath. Measured with grid::stringWidth() at the title's own size and
  # face, that label is 1.887in wide against a 2.289in panel, so it never
  # needed wrapping. This asserts the property rather than the number: no
  # label in the real node set may wrap.
  skip_if_not(file.exists(here::here("data/clean/aep/aep_nodes.csv")))
  labs <- read_aep_nodes()$label
  labs <- labs[!is.na(labs)]
  wrapped <- vapply(labs, function(l) {
    grepl("\n", stringr::str_wrap(l, width = 24), fixed = TRUE)
  }, logical(1))
  expect_false(any(wrapped), info = paste(labs[wrapped], collapse = ", "))
})
