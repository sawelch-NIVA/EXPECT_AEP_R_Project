# Copper Thresholds and Background Values ----

#' Generate Copper Threshold and Background Values
#'
#' Creates a tibble containing copper threshold and background values from multiple
#' regulatory and scientific sources. Includes PROREF values from Norwegian monitoring
#' and classification thresholds from Miljødirektoratet (M-608).
#'
#' @return A tibble with columns matching standard eData DRF formats:
#'   \itemize{
#'     \item REFERENCE_ID: Short identifier for the source
#'     \item REFERENCE_TYPE: Type of reference document
#'     \item TITLE: Full title of source document
#'     \item TITLE_SHORT: Short description of threshold (max 40 characters)
#'     \item DOCUMENT_NUMBER: Document identifier (e.g., M-608|2016)
#'     \item YEAR: Year of publication (integer)
#'     \item ACCESS_DATE: Date threshold was accessed/compiled
#'     \item URL: URL to source document
#'     \item THRESHOLD_TYPE: Type of threshold (PROREF, Classification boundary)
#'     \item PARAMETER_NAME: Parameter name (Copper)
#'     \item ENVIRON_COMPARTMENT: Environmental compartment (Aquatic, Terrestrial, Biota)
#'     \item ENVIRON_COMPARTMENT_SUB: Subcompartment specification
#'     \item MEASURED_CATEGORY: Measurement category (External, Internal, Surface)
#'     \item SAMPLE_SPECIES: Species name (for biota only)
#'     \item SAMPLE_TISSUE: Tissue type (for biota only)
#'     \item THRESHOLD_CLASS: Classification class (Background (I), Good - Moderate (II-III), Poor (IV), Very Poor (V))
#'     \item THRESHOLD_VALUE: Numeric **upper** boundary of the named class. `NA`
#'       for open-ended classes (Very Poor).
#'     \item MEASURED_UNIT: Unit of measurement with basis (e.g., mg/kg (wet), μg/L)
#'     \item THRESHOLD_FRACTION: Fraction measured (dissolved, bioavailable, total, etc.)
#'     \item THRESHOLD_COMMENT: Additional context or notes
#'   }
#'
#' @details
#' Sources include:
#' \itemize{
#'   \item Norwegian monitoring program (M-8022-2024) PROREF values
#'   \item Miljødirektoratet classification system (M-608|2016, revised 2020)
#' }
#'
#' Norwegian classification system uses five classes:
#' \itemize{
#'   \item Klasse I - Bakgrunn (Background)
#'   \item Klasse II - God (Good)
#'   \item Klasse III - Moderat (Moderate)
#'   \item Klasse IV - Dårlig (Poor)
#'   \item Klasse V - Svært dårlig (Very Poor)
#' }
#'
#' **Copper has no Klasse III** in either freshwater or sediment: Klasse II runs
#' straight into Klasse IV. M-608 does not explain the omission. The coded rows
#' therefore carry four boundaries, not five, and label the merged class
#' "Good - Moderate (II-III)" throughout (freshwater and sediment relabelled
#' 2026-09-11 to match coastal, which already used this style). See the inline
#' comments for the 2026-07-30 correction that established the boundaries and
#' the 2026-09-11 relabelling.
#'
#' Two threshold sources previously included here, an EU-wide bioavailable EQS
#' from Peters et al. (2023) and ICES BAC for marine biota, were removed
#' 2026-09-11: neither surfaced in any rendered output. See the "Combine all
#' sources" comment in the function body for detail.
#'
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
generate_copper_thresholds <- function() {
  # PROREF from M-8022-2024 ----
  proref <- tibble(
    REFERENCE_ID = "M-8022-2024",
    REFERENCE_TYPE = "Report",
    TITLE = "Contaminants in coastal waters 2023 / Miljøgifter i kystområdene 2023",
    TITLE_SHORT = c("PROREF: Mussel", "PROREF: Cod liver"),
    DOCUMENT_NUMBER = "M-8022-2024",
    YEAR = 2025L,
    ACCESS_DATE = as.Date("2025-11-19"),
    URL = "https://www.miljodirektoratet.no/publikasjoner/2025/januar-2025/contaminants-in-coastal-waters-2023/",
    THRESHOLD_TYPE = "PROREF",
    PARAMETER_NAME = "Copper",
    ENVIRON_COMPARTMENT = "Biota",
    ENVIRON_COMPARTMENT_SUB = c("Biota, Aquatic", "Biota, Aquatic"),
    MEASURED_CATEGORY = "Internal",
    SAMPLE_SPECIES = c("Mytilus edulis", "Gadus morhua"),
    SAMPLE_TISSUE = c("Total soft tissues", "Liver"),
    THRESHOLD_CLASS = NA_character_,
    THRESHOLD_VALUE = c(1.4, 14),
    MEASURED_UNIT = "mg/kg (wet)",
    THRESHOLD_FRACTION = "Total",
    THRESHOLD_COMMENT = c(
      "Background concentration (PROREF) for Norwegian coastal monitoring program. Blue mussel",
      "Background concentration (PROREF) for Norwegian coastal monitoring program. Cod"
    )
  )

  # M-608 Freshwater classifications ----
  #
  # CORRECTED 2026-07-30. The previous coding was misaligned by one row: it
  # listed five classes I-V against the boundaries c(0.15, 0.3, 7.8, 15.6, NA),
  # and its own comments contradicted themselves (the Class II row was labelled
  # "upper boundary" while quoting the range as 0.3-7.8, i.e. 0.3 as the lower).
  #
  # M-608 defines no Class III for copper in freshwater: Class II (Good) runs
  # 0.3-7.8 and the scale then skips to Class IV. The source document does not
  # explain why. Dropping Class III leaves four classes and three finite
  # boundaries, which is what the numbers 0.3 / 7.8 / 15.6 actually are. The
  # stray 0.15 has no place under that reading and is removed.
  #
  # THRESHOLD_VALUE is the UPPER boundary of the named class throughout. Class V
  # is open-ended, hence NA.
  #
  # RELABELLED 2026-09-11, at Sam's request: the Class II row is now labelled
  # "Good - Moderate (II-III)", matching the merged style already used for
  # coastal below, so @tbl-copper-thresholds no longer shows freshwater and
  # sediment jumping straight from II to IV while coastal alone shows II-III
  # for the same underlying fact (no Class III defined for copper).
  # THRESHOLD_VALUE is unchanged; only the label text.
  freshwater <- tibble(
    REFERENCE_ID = "M-608|2016",
    REFERENCE_TYPE = "Report",
    TITLE = "Grenseverdier for klassifisering av vann, sediment og biota",
    TITLE_SHORT = c(
      "M-608: Freshwater Background (I)",
      "M-608: Freshwater Good - Moderate (II-III)",
      "M-608: Freshwater Poor (IV)",
      "M-608: Freshwater Very Poor (V)"
    ),
    DOCUMENT_NUMBER = "M-608|2016",
    YEAR = 2016L,
    ACCESS_DATE = as.Date("2025-11-19"),
    URL = "https://www.miljodirektoratet.no/globalassets/publikasjoner/m608/m608.pdf",
    THRESHOLD_TYPE = "Classification boundary",
    PARAMETER_NAME = "Copper",
    ENVIRON_COMPARTMENT = "Aquatic",
    ENVIRON_COMPARTMENT_SUB = "Freshwater",
    MEASURED_CATEGORY = "External",
    SAMPLE_SPECIES = NA_character_,
    SAMPLE_TISSUE = NA_character_,
    THRESHOLD_CLASS = c(
      "Background (I)",
      "Good - Moderate (II-III)",
      "Poor (IV)",
      "Very Poor (V)"
    ),
    THRESHOLD_VALUE = c(0.3, 7.8, 15.6, NA_real_),
    MEASURED_UNIT = "μg/L",
    THRESHOLD_FRACTION = "Dissolved",
    THRESHOLD_COMMENT = c(
      "Norwegian water classification. Revised 30.10.2020. Upper boundary for Class I (Background: 0-0.3)",
      "Norwegian water classification. Revised 30.10.2020. Upper boundary for Class II (Good: 0.3-7.8). No Class III is defined for copper",
      "Norwegian water classification. Revised 30.10.2020. Upper boundary for Class IV (Poor: 7.8-15.6)",
      "Norwegian water classification. Revised 30.10.2020. Class V (Very Poor: >15.6). Open-ended"
    )
  )

  # M-608 Coastal water classifications ----
  #
  # Left as-is 2026-07-30, unlike freshwater and sediment above: these three
  # boundaries and their comments already agree with each other. This is also
  # the merged "Good - Moderate (II-III)" label that freshwater and sediment
  # were brought into line with on 2026-09-11 (see their comments above),
  # having originally been left with a bare "Good (II)" that jumped straight
  # to Class IV. Downstream plotting keys on a simplified band rather than on
  # THRESHOLD_CLASS verbatim, so the class label text does not itself reach
  # figures either way; it only reaches @tbl-copper-thresholds in the
  # manuscript.
  # Class V (>5.2) IS now coded, as an open-ended row with THRESHOLD_VALUE = NA,
  # matching freshwater and sediment above.
  #
  # CHANGED 2026-08-05, and the reason is presentational rather than scientific.
  # Since the boundary lines are labelled by the class they OPEN rather than the
  # class they close (see add_threshold_boundary_class()), the top line needs a
  # row above it to take its name from. Without a Class V row, the 5.2 line on
  # every coastal panel came out unlabelled, which is precisely the gap Sam
  # queried for sediment on 2026-08-05 ("the highest threshold is V according to
  # comments, why isn't it here").
  #
  # The row plots nothing on its own: thresholds_for_group() drops NA values,
  # because there is no line to draw for "everything above the last boundary".
  coastal <- tibble(
    REFERENCE_ID = "M-608|2016",
    REFERENCE_TYPE = "Report",
    TITLE = "Grenseverdier for klassifisering av vann, sediment og biota",
    TITLE_SHORT = c(
      "M-608: Coastal Background (I)",
      "M-608: Coastal Good - Moderate (II-III)",
      "M-608: Coastal Poor (IV)",
      "M-608: Coastal Very Poor (V)"
    ),
    DOCUMENT_NUMBER = "M-608|2016",
    YEAR = 2016L,
    ACCESS_DATE = as.Date("2025-11-19"),
    URL = "https://www.miljodirektoratet.no/globalassets/publikasjoner/m608/m608.pdf",
    THRESHOLD_TYPE = "Classification boundary",
    PARAMETER_NAME = "Copper",
    ENVIRON_COMPARTMENT = "Aquatic",
    ENVIRON_COMPARTMENT_SUB = "Marine/Salt Water",
    MEASURED_CATEGORY = "External",
    SAMPLE_SPECIES = NA_character_,
    SAMPLE_TISSUE = NA_character_,
    THRESHOLD_CLASS = c(
      "Background (I)",
      "Good - Moderate (II-III)",
      "Poor (IV)",
      "Very Poor (V)"
    ),
    THRESHOLD_VALUE = c(0.3, 2.6, 5.2, NA_real_),
    MEASURED_UNIT = "μg/L",
    THRESHOLD_FRACTION = "Dissolved",
    THRESHOLD_COMMENT = c(
      "Norwegian water classification. Revised 30.10.2020. Upper boundary for Class I (Background: 0-0.3)",
      "Norwegian water classification. Revised 30.10.2020. Upper boundary for Class II (Good: 0.3-2.6)",
      "Norwegian water classification. Revised 30.10.2020. Upper boundary for Class IV (Poor: 2.6 - 5.2)",
      "Norwegian water classification. Revised 30.10.2020. Class V (Very Poor: >5.2). Open-ended"
    )
  )

  # M-608 Sediment classifications ----
  #
  # CORRECTED 2026-07-30, same fault as freshwater above. The previous coding
  # was c(20, 20, 84, 147) against classes I-IV, which gave two different
  # classes the same upper boundary while its own comments described distinct
  # ranges (0-20, 20-84, 84-147, >147). As with freshwater, M-608 defines no
  # Class III for copper: Class II (Good) runs 20-84 and the scale skips to
  # Class IV. Three finite boundaries, four classes.
  #
  # THRESHOLD_VALUE is the UPPER boundary of the named class throughout.
  #
  # RELABELLED 2026-09-11, at Sam's request, same as freshwater above: "Good
  # (II)" becomes "Good - Moderate (II-III)", matching coastal's merged style.
  sediment <- tibble(
    REFERENCE_ID = "M-608|2016",
    REFERENCE_TYPE = "Report",
    TITLE = "Grenseverdier for klassifisering av vann, sediment og biota",
    TITLE_SHORT = c(
      "M-608: Sediment Background (I)",
      "M-608: Sediment Good - Moderate (II-III)",
      "M-608: Sediment Poor (IV)",
      "M-608: Sediment Very Poor (V)"
    ),
    DOCUMENT_NUMBER = "M-608|2016",
    YEAR = 2016L,
    ACCESS_DATE = as.Date("2025-11-19"),
    URL = "https://www.miljodirektoratet.no/globalassets/publikasjoner/m608/m608.pdf",
    THRESHOLD_TYPE = "Classification boundary",
    PARAMETER_NAME = "Copper",
    ENVIRON_COMPARTMENT = "Aquatic",
    ENVIRON_COMPARTMENT_SUB = "Aquatic Sediment",
    MEASURED_CATEGORY = "External",
    SAMPLE_SPECIES = NA_character_,
    SAMPLE_TISSUE = NA_character_,
    THRESHOLD_CLASS = c(
      "Background (I)",
      "Good - Moderate (II-III)",
      "Poor (IV)",
      "Very Poor (V)"
    ),
    THRESHOLD_VALUE = c(20, 84, 147, NA_real_),
    MEASURED_UNIT = "mg/kg (dry)",
    THRESHOLD_FRACTION = "Total",
    THRESHOLD_COMMENT = c(
      "Norwegian sediment classification. Revised 30.10.2020. Upper boundary for Class I (Background: 0-20)",
      "Norwegian sediment classification. Revised 30.10.2020. Upper boundary for Class II (Good: 20-84). No Class III is defined for copper",
      "Norwegian sediment classification. Revised 30.10.2020. Upper boundary for Class IV (Poor: 84-147)",
      "Norwegian sediment classification. Revised 30.10.2020. Class V (Very Poor: >147). Open-ended"
    )
  )

  # Combine all sources ----
  #
  # Peters et al. (2023) EU bioavailable EQS and ICES BAC (biota) were removed
  # 2026-09-11 at Sam's request: he had already filtered both out of
  # @tbl-copper-thresholds by hand, and neither actually surfaced in any
  # rendered figure (Peters2023's EQS type was excluded from
  # thresholds_for_group()'s default types already; ICES BAC's dry-weight unit
  # never matched a group's standardised unit in practice).
  all_thresholds <- bind_rows(
    proref,
    freshwater,
    coastal,
    sediment
  )

  return(all_thresholds)
}
