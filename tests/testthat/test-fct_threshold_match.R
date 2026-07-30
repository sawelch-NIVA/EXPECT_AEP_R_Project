# Tests ----
#
# Synthetic fixtures throughout, so these run in seconds and survive a store
# rebuild. The degenerate cases are the point: a heterogeneous dataset supplies
# unmapped compartments, NA species groups, and units nothing converts to.

fake_grp <- function(
  compartment = "Aquatic",
  sub = "Freshwater",
  species_group = NA_character_,
  species = NA_character_,
  tissue = NA_character_,
  unit = "mg/L"
) {
  tibble::tibble(
    ENVIRON_COMPARTMENT = compartment,
    ENVIRON_COMPARTMENT_SUB = sub,
    SPECIES_GROUP = species_group,
    SAMPLE_SPECIES = species,
    SAMPLE_TISSUE = tissue,
    SITE_GEOGRAPHIC_FEATURE = "Coastal, fjord",
    SITE_GEOGRAPHIC_FEATURE_SUB = "Other",
    MEASURED_UNIT_STANDARD = unit
  )
}

# ---- The corrected boundaries -------------------------------------------

test_that("freshwater and sediment classes skip Class III", {
  # The 2026-07-30 correction. M-608 defines no Class III for copper, and the
  # previous coding was misaligned by one row against its own comments: two
  # sediment classes shared an upper boundary of 20.
  thr <- generate_copper_thresholds()

  fw <- thr[thr$ENVIRON_COMPARTMENT_SUB == "Freshwater" &
    thr$THRESHOLD_TYPE == "Classification boundary", ]
  expect_equal(
    fw$THRESHOLD_CLASS,
    c("Background (I)", "Good (II)", "Poor (IV)", "Very Poor (V)")
  )
  expect_equal(fw$THRESHOLD_VALUE, c(0.3, 7.8, 15.6, NA_real_))
  expect_false(any(grepl("Moderate", fw$THRESHOLD_CLASS)))

  sed <- thr[thr$ENVIRON_COMPARTMENT_SUB == "Aquatic Sediment", ]
  expect_equal(sed$THRESHOLD_VALUE, c(20, 84, 147, NA_real_))
  # The specific fault that started this: no duplicated boundary.
  expect_equal(anyDuplicated(stats::na.omit(sed$THRESHOLD_VALUE)), 0L)
})

test_that("boundaries increase within every classification set", {
  # A monotonicity check catches a future off-by-one of the kind just fixed.
  thr <- generate_copper_thresholds()
  sets <- split(
    thr[thr$THRESHOLD_TYPE == "Classification boundary", ],
    ~ENVIRON_COMPARTMENT_SUB
  )
  for (s in sets) {
    vals <- stats::na.omit(s$THRESHOLD_VALUE)
    expect_true(all(diff(vals) > 0), info = s$ENVIRON_COMPARTMENT_SUB[1])
  }
})

# ---- Unit standardisation ----------------------------------------------

test_that("standardise_threshold_units rescales and keeps only convertibles", {
  thr <- generate_copper_thresholds()
  std <- standardise_threshold_units(thr)

  expect_true(all(
    std$MEASURED_UNIT_STANDARD %in%
      c("mg/L", "mg/kg (dry)", "mg/kg (wet)")
  ))
  # 0.3 ug/L is 0.0003 mg/L. `%in%` rather than `==`, or the NA class on the EQS
  # row selects an NA row alongside the intended one.
  fw <- std[std$ENVIRON_COMPARTMENT_SUB %in% "Freshwater" &
    std$THRESHOLD_CLASS %in% "Background (I)", ]
  expect_equal(fw$THRESHOLD_VALUE_STANDARD, 0.0003)
  # 6000 ug/kg dry is 6 mg/kg dry.
  bac <- std[std$THRESHOLD_TYPE == "BAC", ]
  expect_equal(unique(bac$THRESHOLD_VALUE_STANDARD), 6)
})

test_that("an unconvertible unit is dropped rather than mis-scaled", {
  # Silently plotting a nmol/L threshold as mg/L would put the line six orders
  # of magnitude out, which is worse than having no line.
  thr <- generate_copper_thresholds()[1, ]
  thr$MEASURED_UNIT <- "nmol/L"
  expect_equal(nrow(standardise_threshold_units(thr)), 0)
})

# ---- Matching ----------------------------------------------------------

test_that("freshwater classes reach the other non-marine waters", {
  thr <- generate_copper_thresholds()
  for (sub in c("Freshwater", "Stormwater", "Wastewater", "Groundwater")) {
    m <- thresholds_for_group(thr, fake_grp(sub = sub, unit = "mg/L"))
    expect_equal(nrow(m), 3, info = sub)
    expect_equal(unique(m$ENVIRON_COMPARTMENT_SUB), "Freshwater", info = sub)
  }
})

test_that("brackish water routes to the coastal classes", {
  thr <- generate_copper_thresholds()
  m <- thresholds_for_group(
    thr,
    fake_grp(sub = "Brackish/Transitional Water", unit = "mg/L")
  )
  expect_equal(unique(m$ENVIRON_COMPARTMENT_SUB), "Marine/Salt Water")
})

test_that("an unmapped compartment matches nothing", {
  # Terrestrial soil has no threshold and must not borrow one.
  thr <- generate_copper_thresholds()
  m <- thresholds_for_group(
    thr,
    fake_grp(
      compartment = "Terrestrial",
      sub = "Soil O Horizon (Organic)",
      unit = "mg/kg (dry)"
    )
  )
  expect_equal(nrow(m), 0)
})

test_that("open-ended classes are dropped", {
  # Very Poor (V) has THRESHOLD_VALUE NA: there is no line to draw for
  # "everything above the last boundary".
  thr <- generate_copper_thresholds()
  m <- thresholds_for_group(thr, fake_grp(sub = "Freshwater", unit = "mg/L"))
  expect_false(any(grepl("Very Poor", m$THRESHOLD_CLASS)))
  expect_false(any(is.na(m$THRESHOLD_VALUE_STANDARD)))
})

test_that("the EU EQS is excluded by default but reachable explicitly", {
  thr <- generate_copper_thresholds()
  grp <- fake_grp(sub = "Freshwater", unit = "mg/L")
  expect_false("EQS" %in% thresholds_for_group(thr, grp)$THRESHOLD_TYPE)
  expect_true(
    "EQS" %in% thresholds_for_group(thr, grp, types = "EQS")$THRESHOLD_TYPE
  )
})

test_that("vertebrates get Gadus and invertebrates get Mytilus", {
  thr <- generate_copper_thresholds()
  vert <- thresholds_for_group(
    thr,
    fake_grp("Biota", "Biota, Aquatic", "Fish", "Gadus morhua", "Liver",
      unit = "mg/kg (wet)")
  )
  expect_equal(vert$SAMPLE_SPECIES, "Gadus morhua")
  expect_equal(vert$THRESHOLD_VALUE_STANDARD, 14)

  invert <- thresholds_for_group(
    thr,
    fake_grp("Biota", "Biota, Aquatic", "Crustaceans", "Cancer pagurus",
      "Muscle", unit = "mg/kg (wet)")
  )
  expect_equal(invert$SAMPLE_SPECIES, "Mytilus edulis")
  expect_equal(invert$THRESHOLD_VALUE_STANDARD, 1.4)
})

test_that("dry and wet weight never cross", {
  # The rule Sam set on 2026-07-30: match like-for-like on basis, never convert.
  # PROREF is wet only, so a dry-weight vertebrate gets nothing, while a
  # dry-weight invertebrate falls through to the ICES BAC.
  thr <- generate_copper_thresholds()
  dry_vert <- thresholds_for_group(
    thr,
    fake_grp("Biota", "Biota, Aquatic", "Fish", "Gadus morhua", "Liver",
      unit = "mg/kg (dry)")
  )
  expect_equal(nrow(dry_vert), 0)

  dry_invert <- thresholds_for_group(
    thr,
    fake_grp("Biota", "Biota, Aquatic", "Molluscs", "Mytilus edulis",
      "Total soft tissues", unit = "mg/kg (dry)")
  )
  expect_equal(nrow(dry_invert), 1)
  expect_equal(dry_invert$THRESHOLD_TYPE, "BAC")
  expect_equal(dry_invert$MEASURED_UNIT_STANDARD, "mg/kg (dry)")
})

test_that("algae and the catch-all species groups get nothing", {
  thr <- generate_copper_thresholds()
  for (sg in c("Algae", "Moss/Hornworts", "Ecosystem", "Other", NA)) {
    m <- thresholds_for_group(
      thr,
      fake_grp("Biota", "Biota, Aquatic", sg, "Fucus vesiculosus", "Whole",
        unit = "mg/kg (wet)")
    )
    expect_equal(nrow(m), 0, info = paste("species group:", sg))
  }
})

test_that("NULL thresholds or NULL group return an empty match, not an error", {
  # The plot functions take both as optional; a caller that omits one omits both.
  expect_equal(nrow(thresholds_for_group(NULL, fake_grp())), 0)
  expect_equal(nrow(thresholds_for_group(generate_copper_thresholds(), NULL)), 0)
  expect_equal(nrow(thresholds_for_group(NULL, NULL)), 0)
  expect_equal(nrow(thresholds_for_group(generate_copper_thresholds()[0, ], fake_grp())), 0)
})

test_that("an NA unit returns an empty match", {
  thr <- generate_copper_thresholds()
  expect_equal(nrow(thresholds_for_group(thr, fake_grp(unit = NA))), 0)
})

# ---- Labels and bands --------------------------------------------------

test_that("labels quote the source unit, not the standardised one", {
  # 0.0003 mg/L is three leading zeros of noise in a sideways panel label.
  thr <- generate_copper_thresholds()
  m <- thresholds_for_group(thr, fake_grp(sub = "Freshwater", unit = "mg/L"))
  expect_true(any(grepl("0.3 μg/L", m$threshold_label, fixed = TRUE)))
  expect_false(any(grepl("0.0003", m$threshold_label, fixed = TRUE)))
})

test_that("biota labels name the borrowed species and tissue", {
  # The whole mitigation for comparing crab muscle to whole mussel: the caveat
  # travels with the line rather than sitting in prose.
  thr <- generate_copper_thresholds()
  m <- thresholds_for_group(
    thr,
    fake_grp("Biota", "Biota, Aquatic", "Crustaceans", "Cancer pagurus",
      "Muscle", unit = "mg/kg (wet)")
  )
  expect_match(m$threshold_label, "Mytilus edulis")
  expect_match(m$threshold_label, "Total soft tissues")
})

# ---- Class numbering, colours and linetypes ----------------------------

test_that("threshold_class_number parses IV rather than matching a bare I", {
  # THE trap. Regex alternation is ordered, so "(I{1,3}|IV|V)" matches the bare
  # "I" inside "(IV)" and never tries IV, which styled Poor as Background: blue
  # and dotted instead of orange and near-solid. Caught by eye on a prototype.
  expect_equal(as.character(threshold_class_number("Poor (IV)")), "IV")
  expect_equal(as.character(threshold_class_number("Background (I)")), "I")
  expect_equal(as.character(threshold_class_number("Good (II)")), "II")
  expect_equal(as.character(threshold_class_number("Moderate (III)")), "III")
  expect_equal(as.character(threshold_class_number("Very Poor (V)")), "V")
  # The coastal rows merge two classes; the first is the one that matters.
  expect_equal(
    as.character(threshold_class_number("Good - Moderate (II-III)")),
    "II"
  )
  # PROREF and BAC carry no class and are background values.
  expect_equal(as.character(threshold_class_number(NA_character_)), "I")
})

test_that("class number is a full I-V factor regardless of what is present", {
  # Copper has no Class III, but Poor must still key to the fourth style. If the
  # factor dropped absent levels, Poor would take the third colour.
  cls <- threshold_class_number(c("Background (I)", "Good (II)", "Poor (IV)"))
  expect_equal(levels(cls), c("I", "II", "III", "IV", "V"))
})

test_that("every class number has a colour and a linetype", {
  cols <- threshold_class_colours()
  ltys <- threshold_class_linetypes()
  expect_named(cols, c("I", "II", "III", "IV", "V"))
  expect_named(ltys, c("I", "II", "III", "IV", "V"))
  # No NA lookups for any class the parser can produce.
  all_cls <- levels(threshold_class_number("Background (I)"))
  expect_false(any(is.na(cols[all_cls])))
  expect_false(any(is.na(ltys[all_cls])))
})

test_that("linetypes grow monotonically more solid", {
  # Hex dash patterns rather than named linetypes, because dotted/dashed/twodash
  # are not monotone in dash length and the intended progression would not read.
  ltys <- threshold_class_linetypes()
  dashed <- ltys[names(ltys) != "V"]
  on_lengths <- as.integer(substr(dashed, 1, 1))
  expect_true(all(diff(on_lengths) > 0))
  expect_equal(unname(ltys["V"]), "solid")
})

test_that("the copper thresholds map onto the intended colours", {
  # The regression this locks in: sediment Poor must be orange, not blue.
  thr <- standardise_threshold_units(generate_copper_thresholds())
  sed <- thr[thr$ENVIRON_COMPARTMENT_SUB %in% "Aquatic Sediment", ]
  cls <- as.character(threshold_class_number(sed$THRESHOLD_CLASS))
  expect_equal(cls, c("I", "II", "IV", "V"))
  expect_equal(
    unname(threshold_class_colours()[cls]),
    c("#4A7FA5", "#5A9367", "#C67B3E", "#B04A4A")
  )
})

# ---- Secondary axis labels ---------------------------------------------

test_that("axis labels use the numeral, or the type where there is no class", {
  # PROREF and BAC are styled as class I but must not be labelled "I": neither is
  # a Norwegian classification class.
  thr <- standardise_threshold_units(generate_copper_thresholds())
  labels <- threshold_axis_label(thr)
  proref <- labels[thr$THRESHOLD_TYPE == "PROREF"]
  expect_true(all(proref == "PROREF"))
  expect_true(all(labels[thr$THRESHOLD_TYPE == "BAC"] == "BAC"))
  sed <- thr$ENVIRON_COMPARTMENT_SUB %in% "Aquatic Sediment"
  expect_equal(labels[sed], c("I", "II", "IV", "V"))
})
