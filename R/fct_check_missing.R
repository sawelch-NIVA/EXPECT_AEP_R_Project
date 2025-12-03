# Helper functions ----

#' Check if a value is truly missing (NA, NULL, or empty string)
#' @param x Vector to check
#' @return Logical vector indicating missing values
is_missing <- function(x) {
  is.na(x) | x == "" | is.null(x)
}

#' Check if a value is not reported
#' @param x Vector to check
#' @return Logical vector indicating "Not reported" values
is_not_reported <- function(x) {
  x == "Not Reported" | x == "Not reported"
}

#' Check if a value is not relevant
#' @param x Vector to check
#' @return Logical vector indicating "Not relevant" values
is_not_relevant <- function(x) {
  x == "Not Relevant" | x == "Not relevant"
}

#' Check if a value has any data quality issue (missing, not reported, or not relevant)
#' @param x Vector to check
#' @return Logical vector indicating any problematic values
has_data_issue <- function(x) {
  is_missing(x) | is_not_reported(x) | is_not_relevant(x)
}

#' Classify the type of data issue for a value
#' @param x Vector to check
#' @return Character vector with "Missing", "Not reported", "Not relevant", or NA (for valid data)
classify_data_issue <- function(x) {
  dplyr::case_when(
    is_missing(x) ~ "Missing (NA, blank string, null)",
    is_not_reported(x) ~ "Not reported",
    is_not_relevant(x) ~ "Not relevant",
    .default = NA_character_
  )
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

  # 4. Missing or problematic uncertainty ----
  # Check for missing uncertainty type AND problematic uncertainty bounds
  # 4. Missing or problematic uncertainty ----
  # Check for missing uncertainty type AND problematic uncertainty bounds
  missing_uncertainty <- data |>
    dplyr::filter(
      has_data_issue(UNCERTAINTY_TYPE) |
        is.na(UNCERTAINTY_UPPER_STANDARD) |
        is.na(UNCERTAINTY_LOWER_STANDARD) |
        UNCERTAINTY_UPPER_STANDARD == 0 |
        UNCERTAINTY_LOWER_STANDARD == 0 |
        # Check if bounds are flipped relative to measured value
        (!is.na(MEASURED_VALUE_STANDARD) &
          !is.na(UNCERTAINTY_UPPER_STANDARD) &
          UNCERTAINTY_UPPER_STANDARD < MEASURED_VALUE_STANDARD) |
        (!is.na(MEASURED_VALUE_STANDARD) &
          !is.na(UNCERTAINTY_LOWER_STANDARD) &
          UNCERTAINTY_LOWER_STANDARD > MEASURED_VALUE_STANDARD)
    ) |>
    dplyr::group_by(REFERENCE_ID) |>
    dplyr::summarise(
      n_rows = dplyr::n(),
      # Uncertainty type issues
      type_missing = any(is_missing(UNCERTAINTY_TYPE)),
      type_not_reported = any(is_not_reported(UNCERTAINTY_TYPE)),
      type_not_relevant = any(is_not_relevant(UNCERTAINTY_TYPE)),
      # Upper bound issues
      upper_missing = any(is.na(UNCERTAINTY_UPPER_STANDARD)),
      upper_zero = any(UNCERTAINTY_UPPER_STANDARD == 0, na.rm = TRUE),
      upper_below_value = any(
        !is.na(MEASURED_VALUE_STANDARD) &
          !is.na(UNCERTAINTY_UPPER_STANDARD) &
          UNCERTAINTY_UPPER_STANDARD < MEASURED_VALUE_STANDARD
      ),
      # Lower bound issues
      lower_missing = any(is.na(UNCERTAINTY_LOWER_STANDARD)),
      lower_zero = any(UNCERTAINTY_LOWER_STANDARD == 0, na.rm = TRUE),
      lower_above_value = any(
        !is.na(MEASURED_VALUE_STANDARD) &
          !is.na(UNCERTAINTY_LOWER_STANDARD) &
          UNCERTAINTY_LOWER_STANDARD > MEASURED_VALUE_STANDARD
      ),
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

  # 7. Encoding issues ----
  # Check for replacement characters that indicate encoding problems
  encoding_issues <- data |>
    dplyr::mutate(
      row_id = dplyr::row_number(),
      # Check all character columns for encoding issues
      has_encoding_issue = dplyr::if_any(
        where(is.character),
        ~ stringr::str_detect(.x, "\uFFFD|�|\\?{2,}") # Unicode replacement char, �, or multiple ?
      )
    ) |>
    dplyr::filter(has_encoding_issue) |>
    dplyr::group_by(REFERENCE_ID) |>
    dplyr::summarise(
      n_rows = dplyr::n(),
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
    n_rows_missing_biota = sum(missing_biota$n_rows),

    # Encoding
    n_refs_encoding_issues = nrow(encoding_issues),
    n_rows_encoding_issues = sum(encoding_issues$n_rows)
  )

  # Return everything as a named list ----
  list(
    summary = summary_stats,
    missing_measurements = missing_measurements,
    missing_n = missing_n,
    missing_methods = missing_methods,
    missing_uncertainty = missing_uncertainty,
    missing_sites = missing_sites,
    missing_biota = missing_biota,
    encoding_issues = encoding_issues
  )
}
