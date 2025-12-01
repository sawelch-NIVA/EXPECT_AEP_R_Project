# fct_check_missing.R ----
# Functions for checking data quality and identifying missing values
# These functions are used by the data_quality_report target

# Helper functions ----

#' Check if a value is missing (NA or empty string)
#' @param x Vector to check
#' @return Logical vector indicating missing values
is_missing <- function(x) {
  is.na(x) | x == "" | is.null(x)
}


# Main quality check function ----

#' Generate a comprehensive data quality report
#'
#' @param data A tibble/data.frame of literature data (e.g., load_literature_pqt)
#' @return A named list containing summary statistics and tibbles of problem rows
#' @export
check_data_quality <- function(data) {
  # 1. Missing measurements ----
  # Only flag if ALL of measured value, LOQ, and LOD are missing
  missing_measurements <- data |>
    dplyr::filter(
      is_missing(MEASURED_VALUE) &
        is_missing(LOQ_VALUE) &
        is_missing(LOD_VALUE)
    ) |>
    dplyr::group_by(REFERENCE_ID) |>
    dplyr::summarise(
      n_rows = dplyr::n(),
      sample_ids = list(SAMPLE_ID),
      .groups = "drop"
    )

  # 2. Missing or zero sample size ----
  missing_n <- data |>
    dplyr::filter(is_missing(MEASURED_N) | MEASURED_N == 0) |>
    dplyr::group_by(REFERENCE_ID) |>
    dplyr::summarise(
      n_rows = dplyr::n(),
      sample_ids = list(SAMPLE_ID),
      .groups = "drop"
    )

  # 3. Missing analytical methods ----
  missing_methods <- data |>
    dplyr::filter(
      is_missing(ANALYTICAL_PROTOCOL) |
        is_missing(EXTRACTION_PROTOCOL) |
        is_missing(FRACTIONATION_PROTOCOL) |
        is_missing(SAMPLING_PROTOCOL)
    ) |>
    dplyr::group_by(REFERENCE_ID) |>
    dplyr::summarise(
      n_rows = dplyr::n(),
      missing_analytical = any(is_missing(ANALYTICAL_PROTOCOL)),
      missing_extraction = any(is_missing(EXTRACTION_PROTOCOL)),
      missing_fractionation = any(is_missing(FRACTIONATION_PROTOCOL)),
      missing_sampling = any(is_missing(SAMPLING_PROTOCOL)),
      sample_ids = list(SAMPLE_ID),
      .groups = "drop"
    )

  # 4. Missing uncertainty type (measurement-level categorical) ----
  missing_uncertainty <- data |>
    dplyr::filter(is.na(UNCERTAINTY_TYPE)) |>
    dplyr::group_by(REFERENCE_ID) |>
    dplyr::summarise(
      n_rows = dplyr::n(),
      sample_ids = list(SAMPLE_ID),
      .groups = "drop"
    )

  # 5. Missing site data ----
  # Categorical fields with NA (should be "Not reported" or "Not relevant")
  # Numeric coords that are NA (genuinely missing)
  missing_sites <- data |>
    dplyr::filter(
      is_missing(LATITUDE) |
        is_missing(LONGITUDE) |
        is_missing(SITE_NAME) |
        is_missing(SITE_GEOGRAPHIC_FEATURE) |
        is_missing(SITE_GEOGRAPHIC_FEATURE_SUB) |
        is_missing(COUNTRY_ISO) |
        is_missing(OCEAN_IHO)
    ) |>
    dplyr::group_by(REFERENCE_ID, SITE_CODE) |>
    dplyr::summarise(
      n_rows = dplyr::n(),
      missing_coords = any(is_missing(LATITUDE) | is_missing(LONGITUDE)),
      missing_name = any(is_missing(SITE_NAME)),
      missing_feature = any(is_missing(SITE_GEOGRAPHIC_FEATURE)),
      missing_feature_sub = any(is_missing(SITE_GEOGRAPHIC_FEATURE_SUB)),
      missing_country = any(is_missing(COUNTRY_ISO)),
      missing_ocean = any(is_missing(OCEAN_IHO)),
      .groups = "drop"
    )

  # 6. Missing biota data ----
  # Only check for rows where ENVIRON_COMPARTMENT == "Biota"
  missing_biota <- data |>
    dplyr::filter(ENVIRON_COMPARTMENT == "Biota") |>
    dplyr::filter(
      is.na(SAMPLE_SPECIES) |
        is.na(SAMPLE_TISSUE) |
        is.na(SAMPLE_SPECIES_LIFESTAGE) |
        is.na(SAMPLE_SPECIES_GENDER) |
        is.na(SPECIES_GROUP)
    ) |>
    dplyr::group_by(REFERENCE_ID) |>
    dplyr::summarise(
      n_rows = dplyr::n(),
      missing_species = any(is.na(SAMPLE_SPECIES)),
      missing_tissue = any(is.na(SAMPLE_TISSUE)),
      missing_lifestage = any(is.na(SAMPLE_SPECIES_LIFESTAGE)),
      missing_gender = any(is.na(SAMPLE_SPECIES_GENDER)),
      missing_group = any(is.na(SPECIES_GROUP)),
      sample_ids = list(SAMPLE_ID),
      .groups = "drop"
    )

  # Summary counts ----
  summary_stats <- list(
    total_rows = nrow(data),
    total_references = dplyr::n_distinct(data$REFERENCE_ID),
    total_sites = dplyr::n_distinct(data$SITE_CODE),

    # Measurements
    n_refs_missing_measurements = nrow(missing_measurements),
    n_rows_missing_measurements = sum(missing_measurements$n_rows),

    # Sample size
    n_refs_missing_n = nrow(missing_n),
    n_rows_missing_n = sum(missing_n$n_rows),

    # Methods
    n_refs_missing_methods = nrow(missing_methods),
    n_rows_missing_methods = sum(missing_methods$n_rows),

    # Uncertainty
    n_refs_missing_uncertainty = nrow(missing_uncertainty),
    n_rows_missing_uncertainty = sum(missing_uncertainty$n_rows),

    # Sites
    n_refs_missing_sites = dplyr::n_distinct(missing_sites$REFERENCE_ID),
    n_sites_missing_data = nrow(missing_sites),

    # Biota
    n_refs_missing_biota = nrow(missing_biota),
    n_rows_missing_biota = sum(missing_biota$n_rows)
  )

  # Return everything as a named list ----
  list(
    summary = summary_stats,
    missing_measurements = missing_measurements,
    missing_n = missing_n,
    missing_methods = missing_methods,
    missing_uncertainty = missing_uncertainty,
    missing_sites = missing_sites,
    missing_biota = missing_biota
  )
}


# Formatting helpers for Quarto output ----

#' Generate a text summary of data quality issues
#'
#' @param qc_report Output from check_data_quality()
#' @return A character string with markdown-formatted summary
format_quality_summary <- function(qc_report) {
  s <- qc_report$summary

  glue::glue(
    "
Dataset contains **{s$total_rows}** rows from **{s$total_references}** references across **{s$total_sites}** sites.

**Issues identified:**

- **Measurements**: {s$n_rows_missing_measurements} rows ({s$n_refs_missing_measurements} references) missing all measurement data (value, LOQ, and LOD)
- **Sample size**: {s$n_rows_missing_n} rows ({s$n_refs_missing_n} references) missing or zero sample size
- **Methods**: {s$n_rows_missing_methods} rows ({s$n_refs_missing_methods} references) missing method information
- **Uncertainty**: {s$n_rows_missing_uncertainty} rows ({s$n_refs_missing_uncertainty} references) missing uncertainty type
- **Site data**: {s$n_sites_missing_data} sites ({s$n_refs_missing_sites} references) missing location/geographic data
- **Biota data**: {s$n_rows_missing_biota} rows ({s$n_refs_missing_biota} references) missing species/tissue information
"
  )
}
