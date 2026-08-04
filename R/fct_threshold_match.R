# Matching copper thresholds to sample groups (PLAN.md P1.1, 2026-07-30).
#
# READ THIS BEFORE TRUSTING A THRESHOLD LINE ON A TRIAGE PLOT.
#
# These are *sanity check* lines for data exploration, not a risk assessment.
# We hold thresholds for four situations (Norwegian freshwater, coastal water,
# marine sediment, and two biota species) and we are applying them to every
# compartment, species and tissue that shares a unit. That is deliberate and it
# is wrong in detail:
#
#   - The M-608 water classes are for *dissolved* copper; our measurements are
#     mostly total. Fraction is not checked. Sam's call: the M-608 numbers are
#     what users of Vannmiljo actually apply, and filtering on fraction adds
#     complication without adding insight at this stage.
#   - The sediment classes are marine. They get applied to lake and river
#     sediment too, for want of anything better.
#   - The vertebrate comparator is *Gadus morhua* LIVER, which concentrates
#     copper well above muscle. Muscle groups compared against it will look
#     clean. The invertebrate comparator is whole *Mytilus edulis* soft tissue,
#     applied to crustacean and worm tissues alike.
#
# The justification for accepting all of that: the errors we are hunting are
# orders of magnitude out (the 270,000 mg/kg dry sediment value is 27% copper by
# mass), whereas the species-and-tissue mismatch above is well under one order.
# A line in roughly the right place still catches those. It will not catch a
# wrong value that happens to look plausible, and nothing here pretends
# otherwise.
#
# Units are matched, never converted: comparing a wet-weight measurement to a
# dry-weight threshold would need a moisture content we do not have.

# ---- Compartment and taxon mapping -------------------------------------

#' Map Data Sub-Compartments onto Threshold Sub-Compartments
#'
#' The threshold set covers three abiotic situations. Everything else in the
#' data is routed to the nearest one, or to `NA` where there is no defensible
#' nearest one.
#'
#' Terrestrial compartments are deliberately unmapped: we hold no soil
#' thresholds, and the data volume (a few rows) does not justify finding any.
#'
#' @return A named character vector, data value -> threshold value.
#' @export
#'
# TODO: Document this properly in methods
threshold_compartment_map <- function() {
  c(
    # Fresh and waste waters all take the freshwater classes.
    "Freshwater" = "Freshwater",
    "Stormwater" = "Freshwater",
    "Wastewater" = "Freshwater",
    "Groundwater" = "Freshwater",
    "Porewater" = "Freshwater",
    "Sludge" = "Freshwater",
    # Saline and transitional waters take the coastal classes.
    "Marine/Salt Water" = "Marine/Salt Water",
    "Brackish/Transitional Water" = "Marine/Salt Water",
    # Sediment takes the (marine) sediment classes.
    "Aquatic Sediment" = "Aquatic Sediment"
  )
}

#' Map Species Groups onto a Biota Threshold Comparator
#'
#' Vertebrates are compared against *Gadus morhua*, invertebrates against
#' *Mytilus edulis* / *Mytilus* spp. Primary producers and the catch-all groups
#' get nothing: Sam expects to source indicative algal values separately, and
#' inventing a comparator for `Ecosystem` or `Other` would be worse than a gap.
#'
#' @return A named character vector, SPECIES_GROUP -> `"vertebrate"` /
#'   `"invertebrate"`.
#' @export
# TODO: Document this properly in methods
threshold_taxon_map <- function() {
  c(
    "Fish" = "vertebrate",
    "Birds" = "vertebrate",
    "Mammals" = "vertebrate",
    "Molluscs" = "invertebrate",
    "Crustaceans" = "invertebrate",
    "Invertebrates" = "invertebrate",
    "Worms" = "invertebrate"
    # Deliberately absent: Algae, Moss/Hornworts, Ecosystem, Other, NA.
  )
}

# ---- Unit handling -----------------------------------------------------

#' Convert Threshold Values to the Project's Standard Units
#'
#' `MEASURED_UNIT_STANDARD` in the data takes only three values (`mg/L`,
#' `mg/kg (dry)`, `mg/kg (wet)`), while the threshold sources are quoted in
#' whatever their documents used. This rescales to the standard unit and drops
#' any row whose unit has no standard equivalent, so an unconvertible threshold
#' vanishes rather than plotting in the wrong place by three orders of
#' magnitude.
#'
#' @param thresholds The `copper_toxicity_thresholds` target.
#' @return The same tibble with `MEASURED_UNIT_STANDARD` and a rescaled
#'   `THRESHOLD_VALUE_STANDARD`, filtered to convertible rows.
#' @export
standardise_threshold_units <- function(thresholds) {
  factors <- c(
    "μg/L" = 1e-3,
    "mg/L" = 1,
    "μg/kg (dry)" = 1e-3,
    "mg/kg (dry)" = 1,
    "μg/kg (wet)" = 1e-3,
    "mg/kg (wet)" = 1
  )
  standard <- c(
    "μg/L" = "mg/L",
    "mg/L" = "mg/L",
    "μg/kg (dry)" = "mg/kg (dry)",
    "mg/kg (dry)" = "mg/kg (dry)",
    "μg/kg (wet)" = "mg/kg (wet)",
    "mg/kg (wet)" = "mg/kg (wet)"
  )

  thresholds |>
    dplyr::mutate(
      MEASURED_UNIT_STANDARD = unname(standard[.data$MEASURED_UNIT]),
      THRESHOLD_VALUE_STANDARD = .data$THRESHOLD_VALUE *
        unname(factors[.data$MEASURED_UNIT])
    ) |>
    dplyr::filter(!is.na(.data$MEASURED_UNIT_STANDARD))
}

# ---- Matching ----------------------------------------------------------

#' Thresholds Applicable to One Sample Group
#'
#' Open-ended classes (`THRESHOLD_VALUE` of `NA`, i.e. Very Poor) are dropped:
#' there is no line to draw for "everything above the last boundary".
#'
#' @param thresholds The `copper_toxicity_thresholds` target.
#' @param grp A one-row tibble of group-defining columns, from
#'   [sample_triage_groups()].
#' @param types Threshold types to consider. The EU bioavailable EQS is excluded
#'   by default: it is a bioavailable number and mixing it with the M-608 total /
#'   dissolved classes on one axis invites exactly the fraction confusion this
#'   layer is trying to avoid. It stays in the dataset, just unplotted.
#' @param unit The unit to match. Defaults to the group's own unit; pass it
#'   explicitly for the unit-agnostic overall-distribution panel, which needs
#'   one call per unit present.
#' @return A tibble of matching threshold rows with `THRESHOLD_VALUE_STANDARD`
#'   and a `threshold_label` for annotation. Zero rows where nothing applies,
#'   which is a normal and expected outcome.
#' @export
thresholds_for_group <- function(
  thresholds,
  grp,
  types = c("Classification boundary", "PROREF", "BAC"),
  unit = NULL
) {
  # `grp = NULL` is a legitimate call: the plot functions take thresholds as
  # optional, and a caller that omits one omits both.
  if (is.null(thresholds) || nrow(thresholds) == 0 || is.null(grp)) {
    return(empty_threshold_match())
  }

  want_unit <- unit %||% grp$MEASURED_UNIT_STANDARD[1]
  if (length(want_unit) != 1 || is.na(want_unit)) {
    return(empty_threshold_match())
  }
  std <- standardise_threshold_units(thresholds) |>
    dplyr::filter(
      .data$THRESHOLD_TYPE %in% types,
      !is.na(.data$THRESHOLD_VALUE_STANDARD),
      .data$MEASURED_UNIT_STANDARD == want_unit
    )

  if (nrow(std) == 0) {
    return(empty_threshold_match())
  }

  matched <- if (grp$ENVIRON_COMPARTMENT[1] == "Biota") {
    taxon <- unname(threshold_taxon_map()[grp$SPECIES_GROUP[1]])
    if (is.na(taxon)) {
      return(empty_threshold_match())
    }
    # Genus-level match, so the PROREF "Mytilus edulis" and the ICES BAC
    # "Mytilus spp." rows both reach a Mytilus group, and neither is missed by
    # an exact-string comparison.
    genus <- if (identical(taxon, "vertebrate")) "Gadus" else "Mytilus"
    std |>
      dplyr::filter(
        .data$ENVIRON_COMPARTMENT == "Biota",
        stringr::str_starts(.data$SAMPLE_SPECIES, genus)
      )
  } else {
    sub <- unname(
      threshold_compartment_map()[grp$ENVIRON_COMPARTMENT_SUB[1]]
    )
    if (is.na(sub)) {
      return(empty_threshold_match())
    }
    std |>
      dplyr::filter(
        .data$ENVIRON_COMPARTMENT != "Biota",
        .data$ENVIRON_COMPARTMENT_SUB == sub
      )
  }

  if (nrow(matched) == 0) {
    return(empty_threshold_match())
  }

  matched |>
    dplyr::mutate(threshold_label = threshold_label(matched)) |>
    dplyr::arrange(.data$THRESHOLD_VALUE_STANDARD)
}

#' Zero-Row Threshold Match
#'
#' Returned wherever no threshold applies, so callers can bind or iterate
#' without special-casing `NULL`.
#' @return A zero-row tibble with the columns the plot layers read.
#' @export
empty_threshold_match <- function() {
  tibble::tibble(
    THRESHOLD_VALUE_STANDARD = numeric(0),
    THRESHOLD_CLASS = character(0),
    THRESHOLD_TYPE = character(0),
    SAMPLE_SPECIES = character(0),
    SAMPLE_TISSUE = character(0),
    REFERENCE_ID = character(0),
    threshold_label = character(0)
  )
}

#' Annotation Label for a Threshold Line
#'
#' Names the source explicitly, including the species and tissue for biota, so
#' that a reader can see the comparator is borrowed rather than exact. This is
#' the whole mitigation for the mismatch documented at the top of this file: the
#' caveat travels with the line instead of sitting in prose the reader may not
#' reach.
#'
#' The value is quoted in the **source's own unit**, not the standardised one.
#' Standardising freshwater to mg/L turns a legible 0.3 ug/L into 0.0003, which
#' is three leading zeros of noise in a label that has to fit sideways in a
#' panel. The line is still *positioned* on the standardised value; only the text
#' uses the original.
#'
#' @param thresholds Rows from a standardised threshold table.
#' @return A character vector of labels.
#' @export
threshold_label <- function(thresholds) {
  biota_part <- paste0(
    thresholds$SAMPLE_SPECIES,
    dplyr::if_else(
      is.na(thresholds$SAMPLE_TISSUE),
      "",
      paste0(", ", thresholds$SAMPLE_TISSUE)
    )
  )
  what <- dplyr::if_else(
    is.na(thresholds$THRESHOLD_CLASS),
    paste0(thresholds$THRESHOLD_TYPE, " (", biota_part, ")"),
    thresholds$THRESHOLD_CLASS
  )
  paste0(
    what,
    " ",
    format(thresholds$THRESHOLD_VALUE, trim = TRUE, drop0trailing = TRUE),
    " ",
    thresholds$MEASURED_UNIT
  )
}

#' Classification Class Number of a Threshold
#'
#' Extracts the roman numeral from a `THRESHOLD_CLASS` such as `Background (I)`
#' or the coastal `Good - Moderate (II-III)` (which yields `II`). PROREF and BAC
#' carry no class; both are background values, so they take `I`.
#'
#' The alternation below is ordered **longest first, and that is load-bearing.**
#' Regex alternation is ordered, so `(I{1,3}|IV|V)` matches the bare `I` inside
#' `(IV)` and never tries `IV`. That silently styled Poor as Background: blue and
#' dotted rather than orange and near-solid. Caught by eye on a prototype, not by
#' any numeric check.
#'
#' Keying style on the class *number* rather than on position within the classes
#' present matters because copper has no Class III (see
#' `generate_copper_thresholds()`). Poor must still read as the fourth class, and
#' a dataset that does have a Class III must pick up the third style without
#' anything being reordered.
#'
#' @param cls A character vector of `THRESHOLD_CLASS` values.
#' @return A factor with levels `I` to `V`.
#' @export
threshold_class_number <- function(cls) {
  found <- stringr::str_match(cls, "\\((IV|V|I{1,3})")[, 2]
  factor(
    dplyr::coalesce(found, "I"),
    levels = c("I", "II", "III", "IV", "V")
  )
}

#' Colour per Classification Class
#'
#' A muted reading of the Miljodirektoratet scheme: blue, green, yellow, orange,
#' red for classes I to V. Class III is an ochre rather than a true yellow, which
#' is invisible against a white panel.
#'
#' @return A named character vector of hex colours, keyed `I` to `V`.
#' @export
threshold_class_colours <- function() {
  c(
    "I" = "#4A7FA5",
    "II" = "#5A9367",
    "III" = "#C7A83E",
    "IV" = "#C67B3E",
    "V" = "#B04A4A"
  )
}

#' Linetype per Classification Class
#'
#' Progressively more solid from I to V, so severity reads off the line as well
#' as the colour.
#'
#' Hex dash patterns rather than the named linetypes, because `dotted`, `dashed`
#' and `twodash` are **not** monotone in dash length: the intended progression
#' would not have read as one. Each digit is a run length (on, off) in multiples
#' of the line width.
#'
#' @return A named character vector of linetypes, keyed `I` to `V`.
#' @export
threshold_class_linetypes <- function() {
  c("I" = "12", "II" = "42", "III" = "62", "IV" = "82", "V" = "solid")
}

#' Short Axis Label for a Threshold
#'
#' What goes on the secondary axis: the class numeral where there is one, and the
#' threshold type otherwise. PROREF and BAC are *styled* as class I but must not
#' be *labelled* `I`, since neither is a Norwegian classification class.
#'
#' @param thresholds Rows from a standardised threshold table.
#' @return A character vector of short labels.
#' @export
threshold_axis_label <- function(thresholds) {
  numeral <- stringr::str_match(
    thresholds$THRESHOLD_CLASS,
    "\\((IV|V|I{1,3})"
  )[, 2]
  dplyr::coalesce(numeral, thresholds$THRESHOLD_TYPE)
}

#' Matrix a Threshold Was Set For
#'
#' Names the matrix each threshold applies to, for the secondary axis title.
#'
#' The match is deliberately loose (see the notes at the top of this file):
#' `threshold_compartment_map()` sends several of our sub-compartments to one
#' threshold matrix, and biota match at genus. So a line drawn on a panel is
#' often a *borrowed* comparator, and which matrix it was actually set for is
#' not recoverable from the class numeral alone. Sam, 2026-08-04: "though in
#' most cases the matrix is obvious, it isn't always."
#'
#' The fraction is included for external thresholds because the Norwegian
#' classification boundaries are set on **dissolved** copper while much of the
#' measured data is total, which is a real comparability caveat and is otherwise
#' invisible on the panel. Biota thresholds carry species and tissue instead,
#' which is the same information in the form biota needs it.
#'
#' Columns are read defensively: [empty_threshold_match()] does not carry
#' `ENVIRON_COMPARTMENT_SUB` or `THRESHOLD_FRACTION`, and callers may hand over
#' a subset.
#'
#' @param thresholds Rows from a standardised threshold table.
#' @return A character vector of matrix names, one per row.
#' @export
threshold_matrix_label <- function(thresholds) {
  n <- nrow(thresholds)
  if (n == 0) {
    return(character(0))
  }
  col <- function(name) {
    if (name %in% names(thresholds)) {
      as.character(thresholds[[name]])
    } else {
      rep(NA_character_, n)
    }
  }

  compartment <- col("ENVIRON_COMPARTMENT")
  species <- col("SAMPLE_SPECIES")
  tissue <- col("SAMPLE_TISSUE")
  sub <- col("ENVIRON_COMPARTMENT_SUB")
  fraction <- col("THRESHOLD_FRACTION")

  biota <- paste0(
    species,
    dplyr::if_else(is.na(tissue), "", paste0(", ", tolower(tissue)))
  )
  external <- paste0(
    sub,
    dplyr::if_else(is.na(fraction), "", paste0(", ", tolower(fraction)))
  )

  out <- dplyr::if_else(
    !is.na(compartment) & compartment == "Biota",
    biota,
    external
  )
  # A row with neither a sub-compartment nor a species yields "NA" from paste0,
  # which would be drawn literally. Blank it instead so the caller can drop it.
  dplyr::if_else(is.na(species) & is.na(sub), NA_character_, out)
}

#' Secondary-Axis Title Naming Source and Matrix
#'
#' `"M-608|2016 (Freshwater, dissolved)"` rather than a bare `"M-608|2016"`.
#' Distinct source/matrix pairs are joined with `" / "`, because the
#' unit-agnostic overall-distribution panel can carry more than one source.
#'
#' @param thresholds Rows from a standardised threshold table.
#' @return A single string.
#' @export
threshold_source_title <- function(thresholds) {
  if (nrow(thresholds) == 0) {
    return("")
  }
  matrix_name <- threshold_matrix_label(thresholds)
  parts <- dplyr::if_else(
    is.na(matrix_name),
    as.character(thresholds$REFERENCE_ID),
    paste0(thresholds$REFERENCE_ID, " (", matrix_name, ")")
  )
  paste(unique(parts), collapse = " / ")
}
