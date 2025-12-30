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
#'
#' @importFrom dplyr filter group_by summarise n n_distinct if_any row_number
#' @importFrom dplyr mutate where select rowwise ungroup first any_of
#' @importFrom stringr str_detect
#'
#' @export
check_data_quality <- function(data) {
  # 1. Missing measurements ----
  # Only flag if ALL of measured value, LOQ, and LOD are missing

  missing_measurements <- data |>
    filter(
      is_missing(MEASURED_VALUE) &
        is_missing(LOQ_VALUE) &
        is_missing(LOD_VALUE)
    ) |>
    group_by(source_file_measurements) |>
    summarise(
      read_timestamp_measurements = first(read_timestamp_measurements),
      n_rows = n(),
      # sample_ids = list(SAMPLE_ID),
      .groups = "drop"
    )

  # 2. Missing or zero sample size ----

  missing_n <- data |>
    filter(is_missing(MEASURED_N) | MEASURED_N == 0) |>
    group_by(source_file_measurements) |>
    summarise(
      read_timestamp_measurements = first(read_timestamp_measurements),
      n_rows = n(),
      # sample_ids = list(SAMPLE_ID),
      .groups = "drop"
    )

  # 3. Missing analytical methods ----

  missing_methods <- data |>
    filter(
      is_missing(ANALYTICAL_PROTOCOL) |
        is_missing(EXTRACTION_PROTOCOL) |
        is_missing(FRACTIONATION_PROTOCOL) |
        is_missing(SAMPLING_PROTOCOL)
    ) |>
    group_by(source_file_measurements) |>
    summarise(
      read_timestamp_measurements = first(read_timestamp_measurements),
      n_rows = n(),
      missing_analytical = any(is_missing(ANALYTICAL_PROTOCOL)),
      missing_extraction = any(is_missing(EXTRACTION_PROTOCOL)),
      missing_fractionation = any(is_missing(FRACTIONATION_PROTOCOL)),
      missing_sampling = any(is_missing(SAMPLING_PROTOCOL)),
      # sample_ids = list(SAMPLE_ID),
      .groups = "drop"
    ) |>
    rowwise() |>
    mutate(
      missing_elements = paste(
        c(
          if (missing_analytical) "Analytical",
          if (missing_extraction) "Extraction",
          if (missing_fractionation) "Fractionation",
          if (missing_sampling) "Sampling"
        ),
        collapse = ", "
      )
    ) |>
    ungroup() |>
    select(
      source_file_measurements,
      read_timestamp_measurements,
      n_rows,
      missing_elements,
      # sample_ids
    )

  # 4. Missing or problematic uncertainty ----
  # Check for missing uncertainty type AND problematic uncertainty bounds

  missing_uncertainty <- data |>
    filter(
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
    group_by(source_file_measurements) |>
    summarise(
      read_timestamp_measurements = first(read_timestamp_measurements),
      n_rows = n(),
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
      # sample_ids = list(SAMPLE_ID),
      .groups = "drop"
    ) |>
    rowwise() |>
    mutate(
      missing_elements = paste(
        c(
          if (type_missing) "Type missing",
          if (type_not_reported) "Type not reported",
          if (type_not_relevant) "Type not relevant",
          if (upper_missing) "Upper missing",
          if (upper_zero) "Upper zero",
          if (upper_below_value) "Upper below value",
          if (lower_missing) "Lower missing",
          if (lower_zero) "Lower zero",
          if (lower_above_value) "Lower above value"
        ),
        collapse = ", "
      )
    ) |>
    ungroup() |>
    select(
      source_file_measurements,
      read_timestamp_measurements,
      n_rows,
      missing_elements,
      # sample_ids
    )

  # 5. Missing site data ----
  # Categorical fields with NA (should be "Not reported" or "Not relevant")
  # Numeric coords that are NA (genuinely missing)

  missing_sites <- data |>
    filter(
      is_missing(LATITUDE) |
        is_missing(LONGITUDE) |
        is_missing(SITE_NAME) |
        is_missing(SITE_GEOGRAPHIC_FEATURE) |
        is_missing(SITE_GEOGRAPHIC_FEATURE_SUB) |
        is_missing(COUNTRY_ISO) |
        is_missing(OCEAN_IHO)
    ) |>
    group_by(SITE_CODE, source_file_measurements) |>
    summarise(
      read_timestamp_measurements = first(read_timestamp_measurements),
      n_rows = n(),
      missing_coords = any(is_missing(LATITUDE) | is_missing(LONGITUDE)),
      missing_name = any(is_missing(SITE_NAME)),
      missing_feature = any(is_missing(SITE_GEOGRAPHIC_FEATURE)),
      missing_feature_sub = any(is_missing(SITE_GEOGRAPHIC_FEATURE_SUB)),
      missing_country = any(is_missing(COUNTRY_ISO)),
      missing_ocean = any(is_missing(OCEAN_IHO)),
      .groups = "drop"
    ) |>
    rowwise() |>
    mutate(
      missing_elements = paste(
        c(
          if (missing_coords) "Coordinates",
          if (missing_name) "Name",
          if (missing_feature) "Feature",
          if (missing_feature_sub) "Feature sub",
          if (missing_country) "Country",
          if (missing_ocean) "Ocean"
        ),
        collapse = ", "
      )
    ) |>
    ungroup() |>
    select(
      source_file_measurements,
      read_timestamp_measurements,
      n_rows,
      SITE_CODE,
      missing_elements
    )

  # 6. Missing biota data ----
  # Only check for rows where ENVIRON_COMPARTMENT == "Biota"

  missing_biota <- data |>
    filter(ENVIRON_COMPARTMENT == "Biota") |>
    filter(
      is.na(SAMPLE_SPECIES) |
        is.na(SAMPLE_TISSUE) |
        is.na(SAMPLE_SPECIES_LIFESTAGE) |
        is.na(SAMPLE_SPECIES_GENDER) |
        is.na(SPECIES_GROUP)
    ) |>
    group_by(source_file_measurements) |>
    summarise(
      read_timestamp_measurements = first(read_timestamp_measurements),
      n_rows = n(),
      missing_species = any(is.na(SAMPLE_SPECIES)),
      missing_tissue = any(is.na(SAMPLE_TISSUE)),
      missing_lifestage = any(is.na(SAMPLE_SPECIES_LIFESTAGE)),
      missing_gender = any(is.na(SAMPLE_SPECIES_GENDER)),
      missing_group = any(is.na(SPECIES_GROUP)),
      sample_ids = list(SAMPLE_ID),
      .groups = "drop"
    ) |>
    rowwise() |>
    mutate(
      missing_elements = paste(
        c(
          if (missing_species) "Species",
          if (missing_tissue) "Tissue",
          if (missing_lifestage) "Lifestage",
          if (missing_gender) "Gender",
          if (missing_group) "Group"
        ),
        collapse = ", "
      )
    ) |>
    ungroup() |>
    select(
      source_file_measurements,
      read_timestamp_measurements,
      n_rows,
      missing_elements,
      sample_ids
    )

  # 7. Encoding issues ----
  # Check for replacement characters that indicate encoding problems

  encoding_issues <- data |>
    mutate(
      row_id = row_number(),
      # Check all character columns for encoding issues
      has_encoding_issue = if_any(
        where(is.character),
        ~ str_detect(.x, "\uFFFD|�|\\?{2,}")
      )
    ) |>
    filter(has_encoding_issue) |>
    group_by(source_file_measurements) |>
    summarise(
      read_timestamp_measurements = first(read_timestamp_measurements),
      n_rows = n(),
      # sample_ids = list(SAMPLE_ID),
      .groups = "drop"
    )

  # Summary counts ----

  summary_stats <- list(
    total_rows = nrow(data),
    total_sites = n_distinct(data$SITE_CODE),
    total_source_files = n_distinct(data$source_file_measurements),

    # Measurements
    n_files_missing_measurements = n_distinct(
      missing_measurements$source_file_measurements
    ),
    n_rows_missing_measurements = sum(missing_measurements$n_rows),

    # Sample size
    n_files_missing_n = n_distinct(missing_n$source_file_measurements),
    n_rows_missing_n = sum(missing_n$n_rows),

    # Methods
    n_files_missing_methods = n_distinct(
      missing_methods$source_file_measurements
    ),
    n_rows_missing_methods = sum(missing_methods$n_rows),

    # Uncertainty
    n_files_missing_uncertainty = n_distinct(
      missing_uncertainty$source_file_measurements
    ),
    n_rows_missing_uncertainty = sum(missing_uncertainty$n_rows),

    # Sites
    n_files_missing_sites = n_distinct(missing_sites$source_file_measurements),
    n_sites_missing_data = n_distinct(missing_sites$SITE_CODE),

    # Biota
    n_files_missing_biota = n_distinct(missing_biota$source_file_measurements),
    n_rows_missing_biota = sum(missing_biota$n_rows),

    # Encoding
    n_files_encoding_issues = n_distinct(
      encoding_issues$source_file_measurements
    ),
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
