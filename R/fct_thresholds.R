# Copper Thresholds and Background Values ----

#' Generate Copper Threshold and Background Values
#'
#' Creates a tibble containing copper threshold and background values from multiple
#' regulatory and scientific sources. Includes PROREF values from Norwegian monitoring,
#' classification thresholds from Miljødirektoratet (M-608), EU bioavailable EQS,
#' and ICES BAC values for biota.
#'
#' @return A tibble with columns matching standard eData DRF formats:
#'   \itemize{
#'     \item REFERENCE_ID: Short identifier for the source
#'     \item REFERENCE_TYPE: Type of reference document
#'     \item TITLE: Full title of source document
#'     \item DOCUMENT_NUMBER: Document identifier (e.g., M-608|2016)
#'     \item YEAR: Year of publication (integer)
#'     \item ACCESS_DATE: Date threshold was accessed/compiled
#'     \item URL: URL to source document
#'     \item THRESHOLD_TYPE: Type of threshold (PROREF, BAC, EQS, Classification boundary)
#'     \item PARAMETER_NAME: Parameter name (Copper)
#'     \item ENVIRON_COMPARTMENT: Environmental compartment (Aquatic, Terrestrial, Biota)
#'     \item ENVIRON_COMPARTMENT_SUB: Subcompartment specification
#'     \item MEASURED_CATEGORY: Measurement category (External, Internal, Surface)
#'     \item SAMPLE_SPECIES: Species name (for biota only)
#'     \item SAMPLE_TISSUE: Tissue type (for biota only)
#'     \item THRESHOLD_CLASS: Classification class the threshold applies to
#'     \item THRESHOLD_VALUE: Numeric threshold value
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
#'   \item EU-wide bioavailable EQS from Peters et al. (2023)
#'   \item ICES BAC for marine biota
#' }
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
  freshwater <- tibble(
    REFERENCE_ID = "M-608|2016",
    REFERENCE_TYPE = "Report",
    TITLE = "Grenseverdier for klassifisering av vann, sediment og biota",
    DOCUMENT_NUMBER = "M-608|2016",
    YEAR = 2016L,
    ACCESS_DATE = as.Date("2025-11-19"),
    URL = "https://www.miljodirektoratet.no/globalassets/publikasjoner/m608/m608.pdf",
    THRESHOLD_TYPE = "Classification boundary",
    PARAMETER_NAME = "Copper",
    ENVIRON_COMPARTMENT = "Aquatic",
    ENVIRON_COMPARTMENT_SUB = "Water, Freshwater",
    MEASURED_CATEGORY = "External",
    SAMPLE_SPECIES = NA_character_,
    SAMPLE_TISSUE = NA_character_,
    THRESHOLD_CLASS = c("I/II", "II/III", "III/IV", "IV/V"),
    THRESHOLD_VALUE = c(0.3, 7.8, 15.6, NA_real_),
    MEASURED_UNIT = "μg/L",
    THRESHOLD_FRACTION = "Dissolved",
    THRESHOLD_COMMENT = c(
      "Norwegian water classification. Revised 30.10.2020. Boundary between Class I (Background: 0-0.3) and Class II (Good: 0.3-7.8)",
      "Norwegian water classification. Revised 30.10.2020. Boundary between Class II (Good: 0.3-7.8) and Class III (Moderate: 7.8-15.6)",
      "Norwegian water classification. Revised 30.10.2020. Boundary between Class III (Moderate: 7.8-15.6) and Class IV (Poor: >15.6)",
      "Norwegian water classification. Revised 30.10.2020. Boundary between Class IV (Poor: >15.6) and Class V (Very Poor). Open-ended"
    )
  )

  # M-608 Coastal water classifications ----
  coastal <- tibble(
    REFERENCE_ID = "M-608|2016",
    REFERENCE_TYPE = "Report",
    TITLE = "Grenseverdier for klassifisering av vann, sediment og biota",
    DOCUMENT_NUMBER = "M-608|2016",
    YEAR = 2016L,
    ACCESS_DATE = as.Date("2025-11-19"),
    URL = "https://www.miljodirektoratet.no/globalassets/publikasjoner/m608/m608.pdf",
    THRESHOLD_TYPE = "Classification boundary",
    PARAMETER_NAME = "Copper",
    ENVIRON_COMPARTMENT = "Aquatic",
    ENVIRON_COMPARTMENT_SUB = "Water, Coastal",
    MEASURED_CATEGORY = "External",
    SAMPLE_SPECIES = NA_character_,
    SAMPLE_TISSUE = NA_character_,
    THRESHOLD_CLASS = c("I/II", "II/III", "III/IV", "IV/V"),
    THRESHOLD_VALUE = c(0.3, 2.6, 5.2, NA_real_),
    MEASURED_UNIT = "μg/L",
    THRESHOLD_FRACTION = "Dissolved",
    THRESHOLD_COMMENT = c(
      "Norwegian water classification. Revised 30.10.2020. Boundary between Class I (Background: 0-0.3) and Class II (Good: 0.3-2.6)",
      "Norwegian water classification. Revised 30.10.2020. Boundary between Class II (Good: 0.3-2.6) and Class III (Moderate: 2.6-5.2)",
      "Norwegian water classification. Revised 30.10.2020. Boundary between Class III (Moderate: 2.6-5.2) and Class IV (Poor: >5.2)",
      "Norwegian water classification. Revised 30.10.2020. Boundary between Class IV (Poor: >5.2) and Class V (Very Poor). Open-ended"
    )
  )

  # M-608 Sediment classifications ----
  sediment <- tibble(
    REFERENCE_ID = "M-608|2016",
    REFERENCE_TYPE = "Report",
    TITLE = "Grenseverdier for klassifisering av vann, sediment og biota",
    DOCUMENT_NUMBER = "M-608|2016",
    YEAR = 2016L,
    ACCESS_DATE = as.Date("2025-11-19"),
    URL = "https://www.miljodirektoratet.no/globalassets/publikasjoner/m608/m608.pdf",
    THRESHOLD_TYPE = "Classification boundary",
    PARAMETER_NAME = "Copper",
    ENVIRON_COMPARTMENT = "Aquatic",
    ENVIRON_COMPARTMENT_SUB = "Sediment",
    MEASURED_CATEGORY = "External",
    SAMPLE_SPECIES = NA_character_,
    SAMPLE_TISSUE = NA_character_,
    THRESHOLD_CLASS = c("I/II", "II/III", "III/IV", "IV/V"),
    THRESHOLD_VALUE = c(20, 84, 147, NA_real_),
    MEASURED_UNIT = "mg/kg (dry)",
    THRESHOLD_FRACTION = "Total",
    THRESHOLD_COMMENT = c(
      "Norwegian sediment classification. Revised 30.10.2020. Boundary between Class I (Background: 0-20) and Class II (Good: 20-84)",
      "Norwegian sediment classification. Revised 30.10.2020. Boundary between Class II (Good: 20-84) and Class III (Moderate: 84-147)",
      "Norwegian sediment classification. Revised 30.10.2020. Boundary between Class III (Moderate: 84-147) and Class IV (Poor: >147)",
      "Norwegian sediment classification. Revised 30.10.2020. Boundary between Class IV (Poor: >147) and Class V (Very Poor). Open-ended"
    )
  )

  # EU bioavailable EQS from Peters et al. 2023 ----
  eu_eqs <- tibble(
    REFERENCE_ID = "Peters2023",
    REFERENCE_TYPE = "Journal Article",
    TITLE = "Following the evidence and using the appropriate regulatory tools: A European-wide risk assessment of copper in freshwaters",
    DOCUMENT_NUMBER = NA_character_,
    YEAR = 2023L,
    ACCESS_DATE = as.Date("2025-11-19"),
    URL = "https://academic.oup.com/ieam/article/19/6/1570/7725173",
    THRESHOLD_TYPE = "EQS",
    PARAMETER_NAME = "Copper",
    ENVIRON_COMPARTMENT = "Aquatic",
    ENVIRON_COMPARTMENT_SUB = "Water, Freshwater",
    MEASURED_CATEGORY = "External",
    SAMPLE_SPECIES = NA_character_,
    SAMPLE_TISSUE = NA_character_,
    THRESHOLD_CLASS = NA_character_,
    THRESHOLD_VALUE = 1.0,
    MEASURED_UNIT = "μg/L",
    THRESHOLD_FRACTION = "Bioavailable",
    THRESHOLD_COMMENT = "EU-wide EQS for bioavailable Cu. Based on 5th percentile HC5 from Austrian dataset (most Cu-sensitive EU region). Typical water conditions: pH 8.4, DOC 0.7 mg/L, Ca 60 mg/L"
  )

  # ICES BAC for biota ----
  ices_bac <- tibble(
    REFERENCE_ID = "ICES_BAC",
    REFERENCE_TYPE = "Database",
    TITLE = "ICES Assessment criteria for contaminants in biota",
    DOCUMENT_NUMBER = NA_character_,
    YEAR = 2019L,
    ACCESS_DATE = as.Date("2025-11-19"),
    URL = "https://dome.ices.dk/ohat/trDocuments/2019/help_ac_biota_metals.html",
    THRESHOLD_TYPE = "BAC",
    PARAMETER_NAME = "Copper",
    ENVIRON_COMPARTMENT = "Biota",
    ENVIRON_COMPARTMENT_SUB = c("Biota, Aquatic", "Biota, Aquatic"),
    MEASURED_CATEGORY = "Internal",
    SAMPLE_SPECIES = c("Mytilus spp.", "Crassostrea spp."),
    SAMPLE_TISSUE = c("Total soft tissues", "Total soft tissues"),
    THRESHOLD_CLASS = NA_character_,
    THRESHOLD_VALUE = c(6000, 6000),
    MEASURED_UNIT = "μg/kg (dry)",
    THRESHOLD_FRACTION = "Total",
    THRESHOLD_COMMENT = c(
      "Background Assessment Concentration (BAC). Mean concentrations significantly below BAC are near background. Developed within OSPAR framework. Mussels",
      "Background Assessment Concentration (BAC). Mean concentrations significantly below BAC are near background. Developed within OSPAR framework. Oysters"
    )
  )

  # Combine all sources ----
  all_thresholds <- bind_rows(
    proref,
    freshwater,
    coastal,
    sediment,
    eu_eqs,
    ices_bac
  )

  return(all_thresholds)
}
