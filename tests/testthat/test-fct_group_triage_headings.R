# Tests ----
#
# The 2026-07-30 additions to R/fct_group_triage.R: the heading hierarchy, the
# sibling ordering, the anchor scheme, and the categorical-panel binning that
# replaced geom_bin2d(). Split from test-fct_group_triage.R to keep that file
# about the plot switching logic.

fake_heading_groups <- function() {
  # Two compartments of unequal weight, plus a group split only by unit, which is
  # the case the shared-heading logic exists for.
  tibble::tibble(
    ENVIRON_COMPARTMENT = c("Aquatic", "Aquatic", "Biota", "Biota"),
    ENVIRON_COMPARTMENT_SUB = c(
      "Freshwater", "Aquatic Sediment", "Biota, Aquatic", "Biota, Aquatic"
    ),
    SPECIES_GROUP = c(NA, NA, "Fish", "Fish"),
    SAMPLE_SPECIES = c(NA, NA, "Gadus morhua", "Gadus morhua"),
    SAMPLE_TISSUE = c(NA, NA, "Liver", "Liver"),
    SITE_GEOGRAPHIC_FEATURE = c(
      "River, stream, canal", "Coastal, fjord",
      "Coastal, fjord", "Coastal, fjord"
    ),
    SITE_GEOGRAPHIC_FEATURE_SUB = c(
      "Water column", "Water benthos", "Not reported", "Not reported"
    ),
    MEASURED_UNIT_STANDARD = c(
      "mg/L", "mg/kg (dry)", "mg/kg (wet)", "mg/kg (dry)"
    ),
    n = c(100L, 5000L, 300L, 50L)
  )
}

# Same shape as the fixture in test-fct_group_triage.R, repeated rather than
# shared so neither file breaks when the other changes.
fake_category_data <- function(n = 60, unit = "mg/kg (dry)") {
  data.frame(
    ENVIRON_COMPARTMENT = "Aquatic",
    ENVIRON_COMPARTMENT_SUB = "Aquatic Sediment",
    SPECIES_GROUP = NA_character_,
    SAMPLE_SPECIES = NA_character_,
    SAMPLE_TISSUE = NA_character_,
    SITE_GEOGRAPHIC_FEATURE = "Coastal, fjord",
    SITE_GEOGRAPHIC_FEATURE_SUB = "Other",
    MEASURED_UNIT_STANDARD = unit,
    MEASURED_VALUE_STANDARD = seq(1, 100, length.out = n),
    SAMPLING_DATE = seq(as.Date("2000-01-01"), by = "month", length.out = n),
    CAMPAIGN_NAME_SHORT = rep(c("Camp A", "Camp B", "Camp C"), length.out = n),
    LONGITUDE = seq(5, 30, length.out = n),
    LATITUDE = seq(60, 80, length.out = n)
  )
}

# ---- Heading hierarchy --------------------------------------------------

test_that("triage_heading_cols excludes the unit", {
  # Unit is not a heading level: at most two occur per group, so they sit as
  # separate plot rows under a shared heading rather than splitting the tree.
  expect_false("MEASURED_UNIT_STANDARD" %in% triage_heading_cols())
  expect_equal(length(triage_heading_cols()), length(triage_group_cols()) - 1)
})

test_that("the biota hierarchy fits inside the six markdown levels", {
  # Seven heading columns, six available levels, so the last two share the leaf.
  # If a column is ever added to triage_group_cols(), this is where it surfaces.
  expect_lte(length(triage_heading_cols()) - 1, 6)
})

# ---- Sibling ordering --------------------------------------------------

test_that("siblings are ordered by summed measurements, hierarchy preserved", {
  sorted <- sort_triage_groups(fake_heading_groups())

  # Aquatic (5100) outweighs Biota (350), so it leads. The tree also stays
  # contiguous: compartments must not interleave, or headings cannot nest.
  expect_equal(rle(sorted$ENVIRON_COMPARTMENT)$values, c("Aquatic", "Biota"))
  # Within Aquatic, sediment (5000) outweighs freshwater (100).
  aquatic <- sorted[sorted$ENVIRON_COMPARTMENT == "Aquatic", ]
  expect_equal(
    aquatic$ENVIRON_COMPARTMENT_SUB,
    c("Aquatic Sediment", "Freshwater")
  )
})

test_that("n_heading sums across unit variants but nothing else", {
  sorted <- sort_triage_groups(fake_heading_groups())
  gadus <- sorted[sorted$SAMPLE_SPECIES %in% "Gadus morhua", ]
  # 300 wet + 50 dry, reported identically on both rows.
  expect_equal(unique(gadus$n_heading), 350)
  # A group with no unit variant keeps its own n.
  fresh <- sorted[sorted$ENVIRON_COMPARTMENT_SUB == "Freshwater", ]
  expect_equal(fresh$n_heading, fresh$n)
})

test_that("sort_triage_groups survives an empty input", {
  expect_equal(nrow(sort_triage_groups(fake_heading_groups()[0, ])), 0)
})

test_that("sort_triage_groups is deterministic for equal weights", {
  # Ties break on the level name, so the order does not depend on what the
  # sample happened to produce.
  groups <- fake_heading_groups()
  groups$n <- 100L
  first <- sort_triage_groups(groups)
  second <- sort_triage_groups(groups[rev(seq_len(nrow(groups))), ])
  expect_equal(first$SITE_GEOGRAPHIC_FEATURE_SUB, second$SITE_GEOGRAPHIC_FEATURE_SUB)
})

# ---- Anchors -----------------------------------------------------------

test_that("unit variants share one heading anchor", {
  anchors <- heading_anchor(fake_heading_groups())
  expect_equal(anchors[3], anchors[4]) # Gadus wet and dry
  expect_equal(length(unique(anchors)), 3) # 4 rows, 3 distinct headings
  expect_true(all(grepl("^grp-[a-z0-9-]+$", anchors)))
})

test_that("heading_anchor carries no make.unique suffixes", {
  # slugify_name() ends in make.unique(), whose suffix depends on what else is in
  # the vector. That makes the anchor computed over 245 summary rows differ from
  # the one computed over 25 triaged groups, silently linking the summary table
  # to the wrong section. heading_anchor() must not inherit that.
  groups <- fake_heading_groups()
  from_all <- heading_anchor(groups)
  from_subset <- heading_anchor(groups[3, ])
  expect_equal(from_subset, from_all[3])
  expect_false(any(grepl("_[0-9]+$", from_all)))
})

test_that("heading_anchor errors rather than merging two sections", {
  # Two distinct keys slugging to one anchor would silently merge two sections.
  groups <- fake_heading_groups()[1:2, ]
  groups$ENVIRON_COMPARTMENT_SUB <- "Freshwater"
  groups$SITE_GEOGRAPHIC_FEATURE <- "River, stream, canal"
  groups$SITE_GEOGRAPHIC_FEATURE_SUB <- c("Water column", "Water/column")
  expect_error(heading_anchor(groups), "share one link target")
})

test_that("heading_anchor handles NA levels without collision", {
  groups <- fake_heading_groups()
  expect_no_error(heading_anchor(groups))
  # The non-biota rows have NA taxonomy and must still produce distinct anchors.
  expect_equal(length(unique(heading_anchor(groups[1:2, ]))), 2)
})

# ---- Categorical panel binning -----------------------------------------

test_that("category bands are exactly one category tall", {
  # The bug this replaces: geom_bin2d(bins = 40) binned the discrete y axis too,
  # giving bands (k-1)/40 tall inside a row pitch of 1. Measured at 0.179.
  p <- triage_plot_by_category(
    fake_category_data(),
    "CAMPAIGN_NAME_SHORT",
    "c) test",
    limits = c(0.5, 200)
  )
  tiles <- ggplot2::ggplot_build(p)$data[[1]]
  expect_gt(nrow(tiles), 0)
  expect_true(all(abs((tiles$ymax - tiles$ymin) - 1) < 1e-9))
})

test_that("thresholds do not break the categorical panel", {
  # Kept from the v1 regression: stat_bin2d() took its binning range from the
  # shared scale, so a threshold label at y = Inf pushed that range to infinity,
  # the stat asked for over a million bins and failed outright, drawing no
  # heatmap at all. v2 has no in-panel text, and the tiles are counted
  # explicitly, so neither half of that can recur.
  data <- fake_category_data()
  grp <- data[1, triage_group_cols()]

  p <- triage_plot_by_category(
    data,
    "CAMPAIGN_NAME_SHORT",
    "c) test",
    limits = c(0.5, 500),
    thresholds = generate_copper_thresholds(),
    grp = grp
  )
  built <- ggplot2::ggplot_build(p)
  expect_gt(nrow(built$data[[1]]), 0) # tiles survived

  # Asserted by geom rather than by layer count. The count was 2 until the
  # distribution overlay landed (2026-08-04) and is now tiles + two boxplot
  # layers + the count labels + the threshold line, with two more when any
  # outlier ticks are drawn. Pinning the number means every future overlay
  # tweak breaks this test for no reason; what actually matters is that the
  # heatmap is still there and the threshold line is still last.
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomTile" %in% geoms)
  expect_equal(unname(utils::tail(geoms, 1)), "GeomVline")
})

test_that("count_by_category_bin anchors bins at the axis origin", {
  # Anchoring at log10 = 0 let the outermost midpoint fall outside the drawn
  # limits, so ggplot2 dropped the tile and warned.
  data <- data.frame(
    MEASURED_VALUE_STANDARD = c(1, 10, 100),
    .facet = factor(c("a", "a", "b"))
  )
  binned <- count_by_category_bin(data, binwidth = 0.5, origin = 1)
  expect_true(all(binned$value_mid >= 1))
  expect_equal(sum(binned$count), 3)
})

test_that("count_by_category_bin drops non-positive and missing values", {
  # log10 of zero or a negative is not plottable. literature_analysis_ready has
  # already dropped these, so this is belt and braces.
  data <- data.frame(
    MEASURED_VALUE_STANDARD = c(1, 0, -5, NA, 10),
    .facet = factor(rep("a", 5))
  )
  expect_equal(sum(count_by_category_bin(data, binwidth = 0.5)$count), 2)
})

test_that("count_by_category_bin returns an empty frame for empty input", {
  data <- data.frame(
    MEASURED_VALUE_STANDARD = numeric(0),
    .facet = factor(character(0))
  )
  expect_equal(nrow(count_by_category_bin(data, binwidth = 0.5)), 0)
})

test_that("category_x_binwidth stays positive for degenerate inputs", {
  # A single-valued group gives a zero span; any positive width will do, since
  # every observation lands in one bin regardless.
  one <- data.frame(MEASURED_VALUE_STANDARD = 5)
  expect_gt(category_x_binwidth(one, limits = NULL), 0)
  expect_gt(category_x_binwidth(one, limits = c(5, 5)), 0)
  # Non-positive limits cannot be log-transformed.
  expect_gt(category_x_binwidth(one, limits = c(0, 100)), 0)
  expect_gt(category_x_binwidth(one, limits = c(NA, NA)), 0)
  # A real span divides into the requested number of bins.
  expect_equal(category_x_binwidth(one, limits = c(1, 100), bins = 40), 2 / 40)
})

# ---- Threshold layers --------------------------------------------------

sediment_grp <- function() {
  tibble::tibble(
    ENVIRON_COMPARTMENT = "Aquatic",
    ENVIRON_COMPARTMENT_SUB = "Aquatic Sediment",
    SPECIES_GROUP = NA_character_,
    SAMPLE_SPECIES = NA_character_,
    SAMPLE_TISSUE = NA_character_,
    SITE_GEOGRAPHIC_FEATURE = "Coastal, fjord",
    SITE_GEOGRAPHIC_FEATURE_SUB = "Other",
    MEASURED_UNIT_STANDARD = "mg/kg (dry)"
  )
}

test_that("triage_threshold_layers returns nothing for an empty match", {
  # A group with no applicable threshold adds no layers rather than erroring, so
  # the call sites need no branching.
  expect_length(triage_threshold_layers(empty_threshold_match()), 0)
  expect_length(triage_threshold_layers(NULL), 0)
})

test_that("triage_threshold_layers draws one line layer and no text", {
  # v2: labels live on the secondary axis, so the panel carries no text at all.
  # The first attempt stacked three rotated labels inside 7% of the panel width.
  thr <- thresholds_for_group(generate_copper_thresholds(), sediment_grp())
  expect_equal(nrow(thr), 3) # 20, 84, 147
  expect_length(triage_threshold_layers(thr, limits = c(1, 1000)), 1)
})

test_that("triage_threshold_layers drops lines outside the axis", {
  thr <- thresholds_for_group(generate_copper_thresholds(), sediment_grp())
  expect_length(triage_threshold_layers(thr, limits = c(1000, 2000)), 0)
})

test_that("the secondary axis carries the class numerals", {
  thr <- thresholds_for_group(generate_copper_thresholds(), sediment_grp())
  sec <- triage_threshold_sec_axis(thr, limits = c(1, 1000))
  expect_s3_class(sec, "AxisSecondary")
  expect_equal(sec$breaks, c(20, 84, 147))
  # Named for the class each boundary OPENS since 2026-08-05, so 147 reads V
  # ("above here is Very Poor") rather than IV. Class I never appears: its lower
  # bound is zero.
  expect_equal(sec$labels, c("II", "IV", "V"))
  # Source AND matrix since 2026-08-04. The bare reference did not say what the
  # boundaries were set for, and the compartment match is many-to-one so the
  # reader cannot infer it.
  expect_equal(sec$name, "M-608|2016 (Aquatic Sediment, total)")
})

test_that("the secondary axis is a waiver where nothing applies", {
  # sec.axis expects waiver() when there is no secondary axis, so the call sites
  # need no branching.
  expect_s3_class(triage_threshold_sec_axis(empty_threshold_match()), "waiver")
  expect_s3_class(triage_threshold_sec_axis(NULL), "waiver")
  thr <- thresholds_for_group(generate_copper_thresholds(), sediment_grp())
  expect_s3_class(
    triage_threshold_sec_axis(thr, limits = c(1000, 2000)),
    "waiver"
  )
})

test_that("a categorical panel builds with a secondary axis", {
  data <- fake_category_data()
  p <- triage_plot_by_category(
    data,
    "CAMPAIGN_NAME_SHORT",
    "c) test",
    limits = c(0.5, 500),
    thresholds = generate_copper_thresholds(),
    grp = data[1, triage_group_cols()]
  )
  built <- ggplot2::ggplot_build(p)
  # The secondary axis reached the built plot, not just the spec.
  labels <- built$layout$panel_params[[1]]$x.sec$get_labels()
  expect_true(all(c("II", "IV") %in% labels))
  # V sits at 147, outside this panel's limits of c(0.5, 500)? No: it is inside,
  # so it must be present. Asserted separately from the pair above so a
  # regression that drops the top class is not masked.
  expect_true("V" %in% labels)
  expect_false("I" %in% labels)
})

test_that("the date panel has a haloed trendline and no secondary axis", {
  # The halo is a second geom_smooth, so there are two smooth layers. No sec axis
  # on this panel: the classes would collide on a secondary y axis. PLAN.md P1.1g.
  data <- fake_category_data()
  p <- triage_plot_by_date(
    data,
    limits = c(1, 500),
    date_limits = c(as.Date("1988-07-01"), as.Date("2025-11-18")),
    thresholds = generate_copper_thresholds(),
    grp = data[1, triage_group_cols()]
  )
  smooths <- vapply(
    p$layers,
    function(l) inherits(l$stat, "StatSmooth"),
    logical(1)
  )
  expect_equal(sum(smooths), 2)
  halo <- p$layers[which(smooths)[1]][[1]]
  top <- p$layers[which(smooths)[2]][[1]]
  # RGBA rather than "white" plus an alpha argument: geom_smooth() passes alpha
  # to the ribbon, not the line, so with se = FALSE it has nothing to act on and
  # the halo stayed fully opaque. Baking the alpha into the colour is the only
  # thing that dims it.
  expect_equal(halo$aes_params$colour, "#ffffff48")
  expect_equal(top$aes_params$colour, "grey60")
  # The halo must be solid: R dash lengths scale with line width, so a matched
  # linetype on a wider line drifts out of phase with the dots on top.
  expect_null(halo$aes_params$linetype)
  expect_equal(top$aes_params$linetype, "dotted")
  expect_gt(halo$aes_params$linewidth, top$aes_params$linewidth)

  # CHANGED 2026-08-05: panel (b) now DOES carry the threshold class axis. It was
  # left off since P1.1g because the numerals collide on a vertical axis (II and
  # IV are 0.24 orders apart on an axis spanning up to 12.6). Sam's call after
  # reading the panels without it: "just print the numerals even if they collide
  # for now" - a collided pair still says a boundary is there, which is more than
  # an unlabelled line does.
  #
  # Asserted on the scale spec, not on panel_params$y.sec: ggplot2 always builds
  # a secondary ViewScale (a mirror of the primary) whether or not a sec.axis was
  # requested, so the built object is never a waiver either way.
  expect_s3_class(p$scales$get_scales("y")$secondary.axis, "AxisSecondary")
  expect_equal(
    p$scales$get_scales("y")$secondary.axis$breaks,
    c(20, 84, 147)
  )
  expect_equal(
    p$scales$get_scales("y")$secondary.axis$labels,
    c("II", "IV", "V")
  )
})

test_that("panel b has no secondary axis where no threshold applies", {
  # The waiver path still has to work: a group with no matching threshold must
  # not acquire an empty axis.
  data <- fake_category_data()
  grp <- data[1, triage_group_cols()]
  p <- triage_plot_by_date(data, thresholds = NULL, grp = grp)
  expect_s3_class(p$scales$get_scales("y")$secondary.axis, "waiver")
})

test_that("the date panel places labels on a real date, not -Inf", {
  # A numeric -Inf on a Date scale warns and is silently coerced.
  data <- fake_category_data()
  grp <- data[1, triage_group_cols()]
  expect_no_warning(
    ggplot2::ggplot_build(
      triage_plot_by_date(
        data,
        limits = c(1, 500),
        date_limits = c(as.Date("1988-07-01"), as.Date("2025-11-18")),
        thresholds = generate_copper_thresholds(),
        grp = grp
      )
    )
  )
})

# ---- must_include (2026-07-30) -----------------------------------------

must_include_summary <- function() {
  data.frame(
    ENVIRON_COMPARTMENT = "Biota",
    ENVIRON_COMPARTMENT_SUB = "Biota, Aquatic",
    SPECIES_GROUP = "Fish",
    SAMPLE_SPECIES = LETTERS[1:4],
    SAMPLE_TISSUE = "Liver",
    SITE_GEOGRAPHIC_FEATURE = "Coastal, fjord",
    SITE_GEOGRAPHIC_FEATURE_SUB = "Not reported",
    MEASURED_UNIT_STANDARD = "mg/kg (wet)",
    n = c(900L, 500L, 60L, 10L),
    n_sources = 2L
  )
}

must_include_data <- function() {
  summary <- must_include_summary()
  rows <- summary[rep(seq_len(nrow(summary)), times = 3), ]
  rows$REFERENCE_ID <- "REF1"
  rows
}

must_include_ledger <- function() {
  ledger <- must_include_summary()[triage_group_cols()]
  ledger$group_id <- format_group_id(seq_len(nrow(ledger)))
  ledger
}

test_that("a named group is included however small it is", {
  # The two real algae groups sit at n = 70 and 68 against a min_n of 100, and no
  # cutoff reaches them without admitting seven unrelated groups.
  out <- sample_triage_groups(
    must_include_summary(), must_include_data(),
    min_n = 100, n_sample = Inf,
    ids = must_include_ledger(), must_include = "G004"
  )
  expect_true("G004" %in% out$group_id)
  expect_equal(out$n[out$group_id == "G004"], 10L)
  # G001 and G002 clear min_n on their own; G003 does not and was not named.
  expect_setequal(out$group_id, c("G001", "G002", "G004"))
})

test_that("a named group survives sampling", {
  # Without this, must_include would be silently advisory whenever n_sample is
  # finite: a named group could simply lose the coin toss.
  out <- sample_triage_groups(
    must_include_summary(), must_include_data(),
    min_n = 0, n_sample = 1,
    ids = must_include_ledger(), must_include = "G004"
  )
  expect_true("G004" %in% out$group_id)
  expect_equal(nrow(out), 1)
})

test_that("an unknown must_include id is an error, not a silent omission", {
  expect_error(
    sample_triage_groups(
      must_include_summary(), must_include_data(),
      min_n = 100, n_sample = Inf,
      ids = must_include_ledger(), must_include = "G999"
    ),
    "unknown group id"
  )
})

test_that("must_include without ids errors rather than silently doing nothing", {
  expect_error(
    sample_triage_groups(
      must_include_summary(), must_include_data(),
      min_n = 100, n_sample = Inf, must_include = "G004"
    ),
    "needs group ids"
  )
})

test_that("group_slug aliases group_id when ids are supplied, and is NA otherwise", {
  # Sam 2026-08-08: "we already have another naming scheme that we use for
  # plots [group_slug]. I think it makes sense to replace that one with this
  # one [the composite group_id]." group_slug is no longer its own
  # slugify_name(label) derivation.
  with_ids <- sample_triage_groups(
    must_include_summary(), must_include_data(),
    min_n = 100, n_sample = Inf, ids = must_include_ledger()
  )
  expect_equal(with_ids$group_slug, with_ids$group_id)

  without_ids <- sample_triage_groups(
    must_include_summary(), must_include_data(),
    min_n = 100, n_sample = Inf
  )
  expect_true(all(is.na(without_ids$group_slug)))
})

test_that("an empty must_include changes nothing", {
  with_ids <- sample_triage_groups(
    must_include_summary(), must_include_data(),
    min_n = 100, n_sample = Inf, ids = must_include_ledger()
  )
  expect_setequal(with_ids$group_id, c("G001", "G002"))
})

# ---- Axis readability (2026-08-04) --------------------------------------

test_that("the date axis labels every 5 years and ticks every year", {
  # Sam: "I can't reliably read the relevant year from b)". Major break and label
  # every five years, minor break every year. Over 1988-2025 that is 8 labels and
  # ~38 minor divisions.
  data <- fake_category_data()
  p <- triage_plot_by_date(
    data,
    limits = c(1, 500),
    date_limits = c(as.Date("1988-07-01"), as.Date("2025-11-18"))
  )
  params <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
  labels <- params$x$get_labels()
  labels <- labels[!is.na(labels)]

  expect_true(all(grepl("^[0-9]{4}$", labels)))
  # Consecutive labels are five years apart.
  yrs <- as.integer(labels)
  expect_true(all(diff(yrs) == 5))
  # A minor break per year between the majors.
  expect_gt(length(params$x$break_positions_minor()), 30)
})

test_that("the date panel's minor grid is on the date axis, not the value axis", {
  # It used to be on "y". When the value axis lost its minor breaks, leaving it
  # there would have styled a grid with nothing to draw and silently dropped the
  # yearly lines.
  p <- triage_plot_by_date(
    fake_category_data(),
    limits = c(1, 500),
    date_limits = c(as.Date("1988-07-01"), as.Date("2025-11-18"))
  )
  expect_false(
    inherits(ggplot2::calc_element("panel.grid.minor.x", p$theme), "element_blank")
  )
  expect_true(
    inherits(ggplot2::calc_element("panel.grid.minor.y", p$theme), "element_blank")
  )
})

test_that("value axis labels use the 1e form at every power", {
  p <- triage_plot_by_date(
    fake_category_data(),
    limits = c(1e-3, 1e3),
    date_limits = c(as.Date("1988-07-01"), as.Date("2025-11-18"))
  )
  labels <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y$get_labels()
  labels <- labels[!is.na(labels)]
  expect_equal(length(labels), 7)
  expect_true(all(grepl("^1e[+-][0-9]+$", labels)))
})
