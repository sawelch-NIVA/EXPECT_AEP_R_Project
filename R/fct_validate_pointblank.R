# TODO: Round this stuff out so we cover everything important enough.

# Pointblank validation functions ----

#' Run pointblank validation on an eData table
#'
#' Applies pointblank validation rules to check data quality and schema
#' compliance for eData format tables. Can operate in two modes:
#' - Agent mode (agent = TRUE): Creates an agent for reporting and interrogation
#' - Pipeline mode (agent = FALSE): Returns validated data directly for use in pipelines
#'
#' @param data Data frame to validate
#' @param table_name Name of the table being validated (for reporting)
#' @param validation_steps Function that takes a data frame or agent and returns
#'   it with validation steps added. Should be a function like:
#'   `function(x) { x |> col_vals_not_null(...) |> ... }`
#' @param agent Logical. If TRUE (default), returns a pointblank agent object.
#'   If FALSE, returns the validated data with failed rows removed.
#' @param actions Action levels for pointblank agent (only used when agent = TRUE)
#'
#' @return If agent = TRUE, a pointblank agent object containing validation results.
#'   If agent = FALSE, the input data with validation failures removed.
#'
#' @details
#' In agent mode, the agent object can be used to:
#' - Extract validation results with `get_sundered_data()`
#' - Generate reports with `get_agent_report()`
#' - Check pass/fail status
#'
#' In pipeline mode, failed rows are automatically removed from the data.
#'
#' @importFrom pointblank create_agent interrogate action_levels
#' @importFrom glue glue
#' @export
pb_validate_edata_table <- function(
  data,
  table_name,
  validation_steps,
  agent = TRUE,
  actions = action_levels()
) {
  if (agent) {
    # Agent mode: create agent, apply validations, and interrogate ----
    data |>
      create_agent(
        label = glue("eData {table_name} Validation"),
        actions = actions
      ) |>
      validation_steps() |>
      interrogate()
  } else {
    # Pipeline mode: apply validation steps directly to data ----
    data |>
      validation_steps()
  }
}

#' Validate eData tables using pointblank
#'
#' @description
#' Comprehensive validation of eData format tables using pointblank.
#' Each validation function can operate in two modes:
#' - Agent mode (agent = TRUE): Creates an agent with validation rules for reporting
#' - Pipeline mode (agent = FALSE): Returns validated data with failures removed
#'
#' @param data Data frame to validate
#' @param actions Action levels for pointblank agent (default: action_levels()).
#'   Only used when agent = TRUE.
#' @param agent Logical. If TRUE (default), returns a pointblank agent object.
#'   If FALSE, returns the validated data with failed rows removed.
#'
#' @return If agent = TRUE, a pointblank agent object with interrogation results.
#'   If agent = FALSE, the input data with validation failures removed.
#'
#' @name validate_edata_tables
NULL

# ## Campaign validation ----
#' @rdname validate_edata_tables
#' @importFrom pointblank col_vals_not_null col_vals_gte col_vals_lte action_levels
#' @export
pb_validate_campaign <- function(
  data,
  actions = action_levels(),
  agent = TRUE
) {
  apply_validations <- function(x) {
    x |>
      # Core identifiers
      col_vals_not_null(columns = CAMPAIGN_NAME_SHORT) |>
      col_vals_not_null(columns = CAMPAIGN_NAME) |>

      # Date validation
      col_vals_not_null(columns = CAMPAIGN_START_DATE) |>
      col_vals_gte(
        columns = CAMPAIGN_START_DATE,
        value = as.Date("1900-01-01")
      ) |>
      col_vals_lte(columns = CAMPAIGN_START_DATE, value = Sys.Date()) |>
      # col_vals_gte(
      #   columns = CAMPAIGN_END_DATE,
      #   value = as.Date("1900-01-01"),
      #   na_pass = TRUE # not all papers report this
      # ) |>
      # col_vals_lte(
      #   columns = CAMPAIGN_END_DATE,
      #   value = Sys.Date(),
      #   na_pass = TRUE # not all papers report this
      # ) |>

      # Metadata
      col_vals_not_null(columns = ENTERED_BY) |>
      col_vals_not_null(columns = ENTERED_DATE) |>
      col_vals_lte(columns = ENTERED_DATE, value = Sys.Date())
  }

  pb_validate_edata_table(
    data = data,
    table_name = "Campaign",
    validation_steps = apply_validations,
    agent = agent,
    actions = actions
  )
}

# ## Reference validation ----
#' @rdname validate_edata_tables
#' @importFrom pointblank col_vals_not_null col_vals_not_equal col_vals_gte col_vals_lte action_levels
#' @export
pb_validate_reference <- function(
  data,
  actions = action_levels(),
  agent = TRUE
) {
  apply_validations <- function(x) {
    x |>
      # Core identifiers
      col_vals_not_null(columns = REFERENCE_ID) |>
      col_vals_not_equal(columns = REFERENCE_ID, value = "Unknown Reference") |>
      col_vals_not_null(columns = REFERENCE_TYPE) |>

      # Bibliographic fields
      col_vals_not_null(columns = AUTHOR) |>
      col_vals_not_null(columns = TITLE) |>
      col_vals_not_null(columns = YEAR) |>
      col_vals_gte(columns = YEAR, value = 1900) |>
      col_vals_lte(
        columns = YEAR,
        value = as.integer(format(Sys.Date(), "%Y"))
      ) |>

      # Access date
      col_vals_gte(columns = ACCESS_DATE, value = as.Date("2000-01-01")) |>
      col_vals_lte(columns = ACCESS_DATE, value = Sys.Date()) |>

      # Numeric fields
      col_vals_gte(columns = VOLUME, value = 1, na_pass = TRUE) |>
      col_vals_gte(columns = ISSUE, value = 1, na_pass = TRUE)
  }

  pb_validate_edata_table(
    data = data,
    table_name = "Reference",
    validation_steps = apply_validations,
    agent = agent,
    actions = actions
  )
}

# ## Parameters validation ----
#' @rdname validate_edata_tables
#' @importFrom pointblank col_vals_not_null col_vals_equal action_levels
#' @export
pb_validate_parameters <- function(
  data,
  actions = action_levels(),
  agent = TRUE
) {
  apply_validations <- function(x) {
    x |>
      # Core identifiers
      col_vals_not_null(columns = PARAMETER_TYPE) |>
      col_vals_not_null(columns = MEASURED_TYPE) |>
      col_vals_not_null(columns = PARAMETER_NAME) |>
      col_vals_equal(columns = PARAMETER_NAME, "Copper") |>

      # Metadata
      col_vals_not_null(columns = ENTERED_BY)
  }

  pb_validate_edata_table(
    data = data,
    table_name = "Parameters",
    validation_steps = apply_validations,
    agent = agent,
    actions = actions
  )
}

# ## Sites validation ----
#' @rdname validate_edata_tables
#' @importFrom pointblank col_vals_not_null col_vals_in_set col_vals_between rows_distinct action_levels
#' @export
pb_validate_sites <- function(
  data,
  actions = action_levels(),
  agent = TRUE
) {
  apply_validations <- function(x) {
    x |>
      # Core identifiers
      col_vals_not_null(columns = SITE_CODE) |>
      col_vals_not_null(columns = SITE_NAME) |>
      # No duplicate codes/names
      rows_distinct(columns = c(SITE_CODE, SITE_NAME)) |>

      # Geographic classifications
      col_vals_in_set(
        columns = SITE_GEOGRAPHIC_FEATURE,
        set = geographic_features_vocabulary()
      ) |>
      col_vals_in_set(
        columns = SITE_GEOGRAPHIC_FEATURE_SUB,
        set = geographic_features_sub_vocabulary()
      ) |>
      col_vals_in_set(
        columns = COUNTRY_ISO,
        set = c(countries_vocabulary(), "Not reported", "Not relevant")
      ) |>
      col_vals_in_set(
        columns = OCEAN_IHO,
        set = c(areas_vocabulary(), "Not reported", "Not relevant")
      ) |>

      # Coordinates
      col_vals_between(
        columns = LATITUDE,
        left = -90,
        right = 90
      ) |>
      col_vals_between(
        columns = LONGITUDE,
        left = -180,
        right = 180
      ) |>
      col_vals_in_set(
        columns = SITE_COORDINATE_SYSTEM,
        set = coordinate_systems_vocabulary()
      ) |>

      # Altitude
      col_vals_between(
        columns = ALTITUDE_VALUE,
        left = -11000,
        right = 9000
      ) |>
      col_vals_in_set(
        columns = ALTITUDE_UNIT,
        set = altitude_units_vocabulary()
      ) |>

      # Metadata
      col_vals_not_null(columns = ENTERED_BY) |>
      col_vals_not_null(columns = ENTERED_DATE)
  }

  pb_validate_edata_table(
    data = data,
    table_name = "Sites",
    validation_steps = apply_validations,
    agent = agent,
    actions = actions
  )
}

# ## Samples validation ----
#' @rdname validate_edata_tables
#' @importFrom pointblank col_vals_not_null col_vals_in_set action_levels
#' @importFrom purrr flatten
#' @export
pb_validate_samples <- function(
  data,
  actions = action_levels(),
  agent = TRUE
) {
  apply_validations <- function(x) {
    x |>
      # Core identifiers
      col_vals_not_null(columns = SAMPLE_ID) |>
      col_vals_not_null(columns = SITE_CODE) |>
      col_vals_not_null(columns = PARAMETER_NAME) |>

      # Environmental compartments
      col_vals_in_set(
        columns = ENVIRON_COMPARTMENT,
        set = environ_compartments_vocabulary()
      ) |>
      col_vals_in_set(
        columns = ENVIRON_COMPARTMENT_SUB,
        set = environ_compartments_sub_vocabulary() |> purrr::flatten()
      )
    # TODO: We never actually set this properly, not that it really matters.
    # Will always be internal for biota and otherwise external, I think.
    # col_vals_not_null(columns = MEASURED_CATEGORY)
  }

  pb_validate_edata_table(
    data = data,
    table_name = "Samples",
    validation_steps = apply_validations,
    agent = agent,
    actions = actions
  )
}

# ## Biota validation ----
#' @rdname validate_edata_tables
#' @importFrom pointblank col_vals_not_null col_vals_equal col_vals_in_set action_levels
#' @export
pb_validate_biota <- function(
  data,
  actions = action_levels(),
  agent = TRUE
) {
  apply_validations <- function(x) {
    x |>
      # Core identifiers
      col_vals_not_null(columns = SAMPLE_ID) |>
      col_vals_not_null(columns = SITE_CODE) |>
      col_vals_not_null(columns = PARAMETER_NAME) |>
      col_vals_equal(columns = PARAMETER_NAME, "Copper") |>

      # Biota-specific fields
      col_vals_not_null(columns = SPECIES_GROUP) |>
      col_vals_not_null(columns = SAMPLE_SPECIES) |>
      col_vals_not_null(columns = SAMPLE_TISSUE) |>

      # Environmental compartments
      col_vals_equal(
        columns = ENVIRON_COMPARTMENT,
        "Biota"
      ) |>
      col_vals_in_set(
        columns = ENVIRON_COMPARTMENT_SUB,
        set = environ_compartments_sub_vocabulary()$Biota
      ) |>

      # Biota-specific vocabularies
      col_vals_in_set(
        columns = SPECIES_GROUP,
        set = species_groups_vocabulary()
      ) |>
      col_vals_in_set(
        columns = SAMPLE_TISSUE,
        set = tissue_types_vocabulary() |>
          # TODO: tissue mapping - Fix me properly
          append(c(
            "Brown meat",
            "Shoot tips",
            "Disc skeleton",
            "Echinoid corona",
            "Bile",
            "Plant tissue",
            "Shoot tip",
            "Total soft tissues minus gonads"
          ))
      ) |>
      col_vals_in_set(
        columns = SAMPLE_SPECIES_LIFESTAGE,
        set = lifestage_vocabulary()
      ) |>
      col_vals_in_set(
        columns = SAMPLE_SPECIES_GENDER,
        set = gender_vocabulary()
      )
  }

  pb_validate_edata_table(
    data = data,
    table_name = "Biota",
    validation_steps = apply_validations,
    agent = agent,
    actions = actions
  )
}

# ## Measurements validation ----
#' @rdname validate_edata_tables
#' @importFrom pointblank col_vals_not_null col_vals_equal col_vals_gte col_vals_lte col_vals_in_set col_vals_not_equal action_levels
#' @importFrom purrr flatten
#' @export
pb_validate_measurements <- function(
  data,
  actions = action_levels(),
  agent = TRUE
) {
  apply_validations <- function(x) {
    x |>
      # Core identifiers
      col_vals_not_null(columns = SITE_CODE) |>
      col_vals_not_null(columns = PARAMETER_NAME) |>
      col_vals_equal(columns = PARAMETER_NAME, "Copper") |>
      col_vals_not_null(columns = SAMPLING_DATE) |>
      col_vals_gte(columns = SAMPLING_DATE, value = as.Date("1900-01-01")) |>
      col_vals_lte(columns = SAMPLING_DATE, value = Sys.Date()) |>

      # Environmental compartments
      col_vals_in_set(
        columns = ENVIRON_COMPARTMENT,
        set = environ_compartments_vocabulary()
      ) |>
      col_vals_in_set(
        columns = ENVIRON_COMPARTMENT_SUB,
        set = environ_compartments_sub_vocabulary() |> purrr::flatten()
      ) |>

      # Measurement values
      col_vals_gte(columns = MEASURED_VALUE, value = 0, na_pass = TRUE) |>
      col_vals_gte(columns = MEASURED_N, value = 1, na_pass = TRUE) |>
      col_vals_gte(
        columns = UNCERTAINTY_UPPER,
        value = 0,
        na_pass = TRUE
      ) |>
      # FIXME: Papers do report lower uncertainty bounds below 0 fairly often
      # col_vals_gte(columns = UNCERTAINTY_LOWER, value = 0, na_pass = TRUE) |>

      # LOQ/LOD values
      col_vals_gte(columns = LOQ_VALUE, value = 0, na_pass = TRUE) |>
      col_vals_gte(columns = LOD_VALUE, value = 0, na_pass = TRUE) |>

      # Units consistency
      col_vals_not_null(columns = MEASURED_UNIT) |>

      # Reference integrity
      col_vals_not_null(columns = REFERENCE_ID) |>
      col_vals_not_equal(columns = REFERENCE_ID, value = "Unknown Reference") |>
      col_vals_not_null(columns = SAMPLE_ID)
  }

  pb_validate_edata_table(
    data = data,
    table_name = "Measurements",
    validation_steps = apply_validations,
    agent = agent,
    actions = actions
  )
}

# ## Methods validation ----
#' @rdname validate_edata_tables
#' @importFrom pointblank col_vals_not_null col_vals_in_set action_levels
#' @importFrom dplyr pull
#' @export
pb_validate_methods <- function(
  data,
  actions = action_levels(),
  agent = TRUE
) {
  apply_validations <- function(x) {
    x |>
      col_vals_not_null(columns = c(PROTOCOL_ID, CAMPAIGN_NAME)) |>
      col_vals_in_set(
        PROTOCOL_CATEGORY,
        set = protocol_categories_vocabulary()
      ) |>
      col_vals_in_set(
        PROTOCOL_NAME,
        set = protocol_options_vocabulary() |> pull(Long_Name)
      )
  }

  pb_validate_edata_table(
    data = data,
    table_name = "Methods",
    validation_steps = apply_validations,
    agent = agent,
    actions = actions
  )
}

# ## CREED Scores validation ----
#' @rdname validate_edata_tables
#' @importFrom pointblank col_vals_not_null col_vals_not_equal col_vals_in_set action_levels
#' @export
pb_validate_creed_scores <- function(
  data,
  actions = action_levels(),
  agent = TRUE
) {
  CREED_classifications_rb <- c(
    "Not usable",
    "Reliable with restrictions",
    "Reliable without restrictions"
  )
  CREED_classifications_rv <- c(
    "Not usable",
    "Relevant with restrictions",
    "Relevant without restrictions"
  )

  apply_validations <- function(x) {
    x |>
      # Core identifiers
      col_vals_not_null(columns = REFERENCE_ID) |>
      col_vals_not_equal(columns = REFERENCE_ID, value = "Unknown Reference") |>

      # CREED fields
      col_vals_in_set(
        columns = c(SILVER_RELIABILITY, GOLD_RELIABILITY),
        set = CREED_classifications_rb
      ) |>
      col_vals_in_set(
        columns = c(SILVER_RELEVANCE, GOLD_RELEVANCE),
        set = CREED_classifications_rv
      )
  }

  pb_validate_edata_table(
    data = data,
    table_name = "CREED Scores",
    validation_steps = apply_validations,
    agent = agent,
    actions = actions
  )
}

# ## Validate all tables ----
#' Validate all eData tables at once
#'
#' @param campaign Campaign table
#' @param reference Reference table
#' @param parameters Parameters table
#' @param sites Sites table
#' @param samples Samples table (optional)
#' @param biota Biota table (optional)
#' @param measurements Measurements table
#' @param methods Methods table (optional)
#' @param creed_scores CREED Scores table (optional)
#' @param actions Action levels for pointblank agents (only used when agent = TRUE)
#' @param agent Logical. If TRUE (default), returns a list of pointblank agent objects.
#'   If FALSE, returns a list of validated data frames with failures removed.
#'
#' @return If agent = TRUE, a named list of pointblank agent objects.
#'   If agent = FALSE, a named list of validated data frames.
#' @export
pb_validate_all_edata_tables <- function(
  campaign,
  reference,
  parameters,
  sites,
  samples = NULL,
  biota = NULL,
  measurements,
  methods = NULL,
  creed_scores = NULL,
  actions = action_levels(),
  agent = TRUE
) {
  results <- list(
    campaign = pb_validate_campaign(campaign, actions, agent),
    reference = pb_validate_reference(reference, actions, agent),
    parameters = pb_validate_parameters(parameters, actions, agent),
    sites = pb_validate_sites(sites, actions, agent),
    measurements = pb_validate_measurements(measurements, actions, agent)
  )

  # Add optional tables if provided
  if (!is.null(samples)) {
    results$samples <- pb_validate_samples(samples, actions, agent)
  }

  if (!is.null(biota)) {
    results$biota <- pb_validate_biota(biota, actions, agent)
  }

  if (!is.null(methods)) {
    results$methods <- pb_validate_methods(methods, actions, agent)
  }

  if (!is.null(creed_scores)) {
    results$creed_scores <- pb_validate_creed_scores(
      creed_scores,
      actions,
      agent
    )
  }

  return(results)
}


#' Wrapper for col_vals_in_set with enhanced error reporting
#'
#' @param x A data table
#' @param columns Column(s) to validate
#' @param set Valid set of values
#' @param actions Action levels for pointblank
#' @param value_name Optional name to describe what the values represent (e.g., "Reference IDs")
#'
#' @return The validated data table
#' @export
col_vals_in_set_verbose <- function(
  x,
  columns,
  set,
  actions,
  value_name = NULL
) {
  # Capture the column name for reporting
  col_name <- rlang::as_name(rlang::enquo(columns))

  # Find missing values before validation
  missing_vals <- x |>
    filter({{ columns }} %notin% set) |>
    pull({{ columns }}) |>
    unique()

  # Run the validation
  result <- col_vals_in_set(
    x,
    columns = {{ columns }},
    set = set,
    actions = actions
  )

  # If there are missing values, issue a detailed warning
  if (length(missing_vals) > 0) {
    value_desc <- if (!is.null(value_name)) value_name else col_name
    warning(
      sprintf(
        "%s not found in reference set: %s",
        value_desc,
        paste(missing_vals, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  result
}
