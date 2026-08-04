# Tests ----

fake_summary <- function(...) {
  base <- data.frame(
    ENVIRON_COMPARTMENT = "Biota",
    ENVIRON_COMPARTMENT_SUB = "Fish",
    SPECIES_GROUP = "Fish",
    SAMPLE_SPECIES = "Gadus morhua",
    SAMPLE_TISSUE = "Liver",
    SITE_GEOGRAPHIC_FEATURE = "Coastal, fjord",
    SITE_GEOGRAPHIC_FEATURE_SUB = "Fjord",
    MEASURED_UNIT_STANDARD = "mg/kg (dry)",
    n = 500L,
    n_sources = 3L,
    date_min = as.Date("2001-01-01"),
    date_max = as.Date("2019-01-01"),
    sd = 2,
    mean = 10,
    n_double_outliers = 1L,
    median = 9,
    unit = "mg/kg (dry)",
    dip_p = 0.01,
    multimodal = TRUE
  )
  # Piped through add_triage_flags() because that is what the pipeline does: the
  # summarise_literature_data target ends with it, and build_sample_groups_table()
  # reads the flag columns it derives.
  modifyList(base, list(...)) |>
    as.data.frame() |>
    add_triage_flags()
}

test_that("build_sample_groups_table produces the expected columns", {
  result <- build_sample_groups_table(fake_summary())

  expect_named(
    result,
    c(
      "group_id", "group", "location", "dates", "n", "mean_sd", "median",
      "n_outliers", "dip_p_label", "n_units", "dropped_label",
      ".is_multimodal", ".is_outlier", ".anchor"
    )
  )
})

test_that("anchors are computed for every row and ignore the unit", {
  # Unit variants of one group share a heading in the triage notebook, so they
  # must share an anchor.
  wet <- build_sample_groups_table(fake_summary(
    MEASURED_UNIT_STANDARD = "mg/kg (wet)"
  ))
  dry <- build_sample_groups_table(fake_summary(
    MEASURED_UNIT_STANDARD = "mg/kg (dry)"
  ))
  expect_false(is.na(wet$.anchor))
  expect_equal(wet$.anchor, dry$.anchor)
  expect_match(wet$.anchor, "^grp-")
})

test_that("nothing is linked unless the document asks", {
  # index.qmd reads the same target but holds none of the sections, and its docx
  # output would carry the dead links outward.
  tbl <- build_sample_groups_table(fake_summary())
  plain <- sample_groups_flextable(tbl)
  linked <- sample_groups_flextable(tbl, link_sections = tbl$.anchor)

  plain_html <- flextable::htmltools_value(plain) |> as.character()
  linked_html <- flextable::htmltools_value(linked) |> as.character()

  expect_false(grepl("<a href", plain_html, fixed = TRUE))
  expect_true(grepl("<a href", linked_html, fixed = TRUE))
  expect_true(grepl(paste0("#", tbl$.anchor), linked_html, fixed = TRUE))
})

test_that("linked cells are the same font size as the rest of the table", {
  # compose() replaces a cell's content wholesale, and fp_text_default() takes
  # its size from get_flextable_defaults() (11pt) rather than from the
  # fontsize() already applied, so the linked cells rendered two points larger
  # than every other row until font.size was passed explicitly.
  #
  # Checked on both code paths because they are genuinely different: uncomposed
  # cells carry their size in the styles slot, composed ones per chunk.
  tbl <- build_sample_groups_table(fake_summary())
  ft <- sample_groups_flextable(tbl, link_sections = tbl$.anchor)

  styled <- unique(as.vector(as.matrix(ft$body$styles$text$font.size$data)))
  # NA here means "inherit from the styles slot", which is what every
  # uncomposed cell does and is correct. Only an explicitly set chunk size can
  # disagree with the table, so those are what must match.
  composed <- unlist(lapply(ft$body$content$data, `[[`, "font.size"))
  composed <- unique(composed[!is.na(composed)])

  expect_length(styled, 1)
  expect_setequal(composed, styled)
  expect_equal(
    unique(as.vector(as.matrix(ft$header$styles$text$font.size$data))),
    styled
  )
})

test_that("a group with no matching section stays plain text", {
  tbl <- build_sample_groups_table(fake_summary())
  html <- sample_groups_flextable(tbl, link_sections = "grp-something-else") |>
    flextable::htmltools_value() |>
    as.character()
  expect_false(grepl("<a href", html, fixed = TRUE))
})

test_that("build_sample_groups_table labels biota by taxonomy", {
  result <- build_sample_groups_table(fake_summary())

  expect_match(result$group, "Gadus morhua")
  expect_match(result$group, "Liver")
})

test_that("build_sample_groups_table labels non-biota by compartment", {
  result <- build_sample_groups_table(fake_summary(
    ENVIRON_COMPARTMENT = "Aquatic",
    ENVIRON_COMPARTMENT_SUB = "Freshwater"
  ))

  expect_match(result$group, "Aquatic")
  expect_match(result$group, "Freshwater")
  expect_false(grepl("Gadus", result$group))
})

test_that("build_sample_groups_table folds dates into a year range", {
  expect_equal(build_sample_groups_table(fake_summary())$dates, "2001–2019")
})

test_that("merge_v can swallow a link, and only the run head keeps one", {
  # merge_v(j = "group") renders only the first row of a run of identical labels,
  # so a link applied to a later row of that run never appears. Under the n
  # descending ordering this bites exactly once in the real 245-row table, but it
  # is worth pinning: if the ordering ever changes back to alphabetical, runs get
  # long again and most links would vanish.
  data <- rbind(
    fake_summary(SITE_GEOGRAPHIC_FEATURE_SUB = "Fjord", n = 500L),
    fake_summary(SITE_GEOGRAPHIC_FEATURE_SUB = "Open coast", n = 400L)
  )
  tbl <- build_sample_groups_table(data)
  # Same group label on both rows, different locations, so merge_v merges them.
  expect_equal(length(unique(tbl$group)), 1)
  html <- sample_groups_flextable(tbl, link_sections = tbl$.anchor) |>
    flextable::htmltools_value() |>
    as.character()
  # Two rows are eligible but only the run head renders its cell.
  expect_length(regmatches(html, gregexpr("<a href", html))[[1]], 1)
})

test_that("untested groups are not flagged as multimodal", {
  # multimodal is NA where n fell below dip_test_safe()'s min_n. NA must not
  # leak into the highlight set, or flextable colours arbitrary rows.
  result <- build_sample_groups_table(fake_summary(multimodal = NA))

  expect_false(result$.is_multimodal)
  expect_equal(result$dip_p_label, "")
})

test_that("multimodal groups carry a formatted p-value", {
  result <- build_sample_groups_table(fake_summary(dip_p = 0.0123))

  expect_true(result$.is_multimodal)
  expect_equal(result$dip_p_label, "0.01")
})

test_that("the outlier flag trips only above the 5% threshold", {
  under <- build_sample_groups_table(fake_summary(n = 1000L, n_double_outliers = 10L))
  over <- build_sample_groups_table(fake_summary(n = 100L, n_double_outliers = 10L))

  expect_false(under$.is_outlier)
  expect_true(over$.is_outlier)
})

test_that("build_sample_groups_table ranks by n descending", {
  # Changed 2026-07-30 (PLAN.md P1.4). Was alphabetical by group then location.
  # The table exists to be worked down from the groups carrying the most data.
  data <- rbind(
    fake_summary(SAMPLE_SPECIES = "Zoarces viviparus", n = 50L),
    fake_summary(SAMPLE_SPECIES = "Anarhichas lupus", n = 5000L),
    fake_summary(SAMPLE_SPECIES = "Gadus morhua", n = 900L)
  )

  result <- build_sample_groups_table(data)

  expect_equal(result$n, c(5000L, 900L, 50L))
  expect_equal(result$group[1], "Fish › Anarhichas lupus › Liver")
})

test_that("sample_groups_flextable builds, and survives filtering", {
  data <- rbind(
    fake_summary(SAMPLE_SPECIES = "Anarhichas lupus", n = 50L),
    fake_summary(SAMPLE_SPECIES = "Zoarces viviparus", n = 5000L)
  )
  tbl <- build_sample_groups_table(data)

  expect_s3_class(sample_groups_flextable(tbl), "flextable")
  # index.qmd filters before formatting; highlight indices must be recomputed
  # from the filtered table, not inherited from the full one.
  expect_s3_class(
    sample_groups_flextable(tbl[tbl$n >= 100, , drop = FALSE]),
    "flextable"
  )
})

test_that("sample_groups_flextable copes with no rows flagged", {
  tbl <- build_sample_groups_table(fake_summary(
    multimodal = FALSE,
    n_double_outliers = 0L
  ))

  expect_s3_class(sample_groups_flextable(tbl), "flextable")
})

# ---- Cell formatting (2026-07-30) --------------------------------------

test_that("a missing SD is omitted rather than printed as NA", {
  # sd() of a single measurement is NA, and "3.2 ± NA mg/kg (dry)" reads as a
  # failed calculation rather than an absent one. 67 of 245 real groups hit this.
  one <- build_sample_groups_table(fake_summary(mean = 3.2, sd = NA_real_))
  expect_equal(one$mean_sd, "3.2 mg/kg (dry)")
  expect_false(grepl("NA", one$mean_sd))
  expect_false(grepl("±", one$mean_sd))

  both <- build_sample_groups_table(fake_summary(mean = 3.2, sd = 1.1))
  expect_equal(both$mean_sd, "3.2 ± 1.1 mg/kg (dry)")
})

test_that("a single-year date range collapses to the year", {
  same <- build_sample_groups_table(fake_summary(
    date_min = as.Date("2005-02-01"),
    date_max = as.Date("2005-11-30")
  ))
  expect_equal(same$dates, "2005")

  spanning <- build_sample_groups_table(fake_summary(
    date_min = as.Date("2001-01-01"),
    date_max = as.Date("2019-01-01")
  ))
  expect_equal(spanning$dates, "2001–2019")
})

test_that("linked cells carry their own colour and underline", {
  # flextable emits <a href><span class="cl-..."></span></a>, putting its inline
  # colour on the span INSIDE the anchor, so the span overrides any link styling
  # the page supplies. Without explicit props the links render as ordinary black
  # text and nothing shows which cells are clickable.
  tbl <- build_sample_groups_table(fake_summary())
  html <- sample_groups_flextable(tbl, link_sections = tbl$.anchor) |>
    flextable::htmltools_value() |>
    as.character()

  anchor <- regmatches(html, regexpr("<a [^>]*>.*?</a>", html))
  expect_length(anchor, 1)
  span_class <- sub('.*<span class="([^"]+)".*', "\\1", anchor)
  css <- regmatches(
    html,
    regexpr(paste0("\\.", span_class, "\\{[^}]*\\}"), html)
  )
  expect_match(css, "text-decoration:underline")
  expect_match(css, "color:rgba\\(26, 111, 168")
})
