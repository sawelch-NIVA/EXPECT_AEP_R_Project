# Functions for making processing Vm data so it can be handed off to fct_vm_eData.R

# # Filter Vannmiljø data ----

#' Filter data by environmental compartments
#'
#' Filters Vannmiljø data to include only specified environmental compartments
#' and subcompartments. Uses both vkat (parameter) and medium (matrix) lookups,
#' accepting rows where either source matches the allowed values.
#'
#' @param data Data frame with compartment columns (ENVIRON_COMPARTMENT_vkat,
#'   ENVIRON_COMPARTMENT_medium, ENVIRON_COMPARTMENT_SUB_vkat,
#'   ENVIRON_COMPARTMENT_SUB_medium)
#' @param compartments Character vector of allowed compartments. Default:
#'   c("Aquatic", "Biota", "*")
#' @param subcompartments Character vector of allowed subcompartments. Default:
#'   c("Freshwater", "Aquatic Sediment", "Marine/Salt Water",
#'   "Brackish/Transitional Water", "Biota, Aquatic", "*")
#'
#' @return Filtered data frame containing only rows matching the specified
#'   compartment and subcompartment criteria
#'
#' @export
vm_filter_compartments <- function(
  data,
  compartments = c("Aquatic", "Biota", "*"),
  subcompartments = c(
    "Freshwater",
    "Aquatic Sediment",
    "Marine/Salt Water",
    "Brackish/Transitional Water",
    "Biota, Aquatic",
    "*"
  )
) {
  data |>
    filter(
      (ENVIRON_COMPARTMENT_vkat %in%
        compartments |
        ENVIRON_COMPARTMENT_medium %in% compartments) &
        (ENVIRON_COMPARTMENT_SUB_vkat %in%
          subcompartments |
          ENVIRON_COMPARTMENT_SUB_medium %in% subcompartments)
    )
}


#' Filter data by site type and excluded sites
#'
#' Filters Vannmiljø data to include only point sites while excluding specific
#' named sites. Non-point sites (e.g., transects, areas) are removed.
#'
#' @param data Data frame with site information (Objekttype, Vannlokalitetsnavn)
#' @param exclude_sites Character vector of site names to exclude. Default:
#'   empty vector (no sites excluded)
#'
#' @return Filtered data frame containing only point sites that are not in the
#'   exclusion list
#'
#' @export
vm_filter_sites <- function(data, exclude_sites = character()) {
  data |>
    filter(
      Objekttype == "point",
      !Vannlokalitetsnavn %in% exclude_sites
    )
}


#' Filter data by date range
#'
#' Filters Vannmiljø data to include only samples within a specified date range
#' (inclusive).
#'
#' @param data Data frame with SAMPLING_DATE column
#' @param date_start Minimum date (Date object or character in YYYY-MM-DD format)
#' @param date_end Maximum date (Date object or character in YYYY-MM-DD format)
#'
#' @return Filtered data frame containing only rows with SAMPLING_DATE between
#'   date_start and date_end (inclusive)
#'
#' @export
vm_filter_dates <- function(data, date_start, date_end) {
  if (is.character(date_start)) {
    date_start <- as.Date(date_start)
  }
  if (is.character(date_end)) {
    date_end <- as.Date(date_end)
  }

  data |>
    filter(SAMPLING_DATE >= date_start, SAMPLING_DATE <= date_end)
}

# # Resolve lookup conflicts ----

#' Resolve environmental compartment conflicts between data sources
#'
#' Resolves conflicts when parameter (vkat) and medium lookups provide different
#' compartment or subcompartment values. Applies hierarchical rules to determine
#' the correct compartment classification. Rows that cannot be resolved are
#' flagged for removal.
#'
#' Resolution rules:
#' * Biota compartment always takes precedence
#' * Non-wildcard values preferred over wildcards (*)
#' * For subcompartments: Aquatic Sediment > water types
#' * Matching values between sources are accepted
#' * Unresolvable conflicts are flagged
#'
#' @param df Data frame with compartment columns from both vkat and medium
#'   lookups (ENVIRON_COMPARTMENT_vkat, ENVIRON_COMPARTMENT_medium,
#'   ENVIRON_COMPARTMENT_SUB_vkat, ENVIRON_COMPARTMENT_SUB_medium, and
#'   ENVIRON_COMPARTMENT_SUB_biota for biota-specific lookups)
#'
#' @return Data frame with two new columns:
#'   * ENVIRON_COMPARTMENT_resolved: Resolved compartment or "FLAG: Compartment conflict."
#'   * ENVIRON_COMPARTMENT_SUB_resolved: Resolved subcompartment or "FLAG: Compartment conflict."
#'
#' @details Prints messages showing number of conflicts resolved and flagged.
#'   Issues a warning if any conflicts remain unresolved.
#'
#' @export
resolve_compartment_conflicts <- function(df) {
  df <- df |>
    mutate(
      # SET COMPARTMENT
      ENVIRON_COMPARTMENT_resolved = case_when(
        # Explicit rule: Biota trumps all
        ENVIRON_COMPARTMENT_vkat == "Biota" |
          ENVIRON_COMPARTMENT_medium == "Biota" ~ "Biota",

        # if ENVIRON_COMPARTMENT_vkat is wildcard then use ENVIRON_COMPARTMENT_medium
        !has_data_issue(ENVIRON_COMPARTMENT_vkat) &
          !has_data_issue(ENVIRON_COMPARTMENT_medium) &
          ENVIRON_COMPARTMENT_vkat == "*" &
          ENVIRON_COMPARTMENT_medium != "*" ~ ENVIRON_COMPARTMENT_medium,

        # if ENVIRON_COMPARTMENT_medium is wildcard then use ENVIRON_COMPARTMENT_vkat
        !has_data_issue(ENVIRON_COMPARTMENT_vkat) &
          !has_data_issue(ENVIRON_COMPARTMENT_medium) &
          ENVIRON_COMPARTMENT_vkat != "*" &
          ENVIRON_COMPARTMENT_medium == "*" ~ ENVIRON_COMPARTMENT_vkat,

        # if there's no conflict, use the first value  - unless both are *
        ENVIRON_COMPARTMENT_vkat == ENVIRON_COMPARTMENT_medium &
          ENVIRON_COMPARTMENT_medium != "*" ~ ENVIRON_COMPARTMENT_medium,

        # If none of these rules resolve the conflict, flag the row
        .default = "FLAG: Compartment conflict."
      ),

      # SET SUBCOMPARTMENT
      ENVIRON_COMPARTMENT_SUB_resolved = case_when(
        # Handle Biota compartment first (all biota rules together)
        ENVIRON_COMPARTMENT_resolved == "Biota" ~ case_when(
          # Prefer explicit "Biota, Aquatic" from either source
          ENVIRON_COMPARTMENT_SUB_vkat == "Biota, Aquatic" |
            ENVIRON_COMPARTMENT_SUB_medium ==
              "Biota, Aquatic" ~ "Biota, Aquatic",

          # Use biota-specific lookup if available
          !is.na(ENVIRON_COMPARTMENT_SUB_biota) &
            ENVIRON_COMPARTMENT_SUB_biota !=
              "*" ~ ENVIRON_COMPARTMENT_SUB_biota,

          .default = "FLAG: Compartment conflict."
        ),

        # Non-biota compartments: simpler logic
        # Sediment trumps water
        ENVIRON_COMPARTMENT_SUB_medium ==
          "Aquatic Sediment" ~ "Aquatic Sediment",

        # Wildcard handling
        ENVIRON_COMPARTMENT_SUB_vkat == "*" &
          ENVIRON_COMPARTMENT_SUB_medium !=
            "*" ~ ENVIRON_COMPARTMENT_SUB_medium,
        ENVIRON_COMPARTMENT_SUB_medium == "*" &
          ENVIRON_COMPARTMENT_SUB_vkat != "*" ~ ENVIRON_COMPARTMENT_SUB_vkat,

        # No conflict
        ENVIRON_COMPARTMENT_SUB_vkat == ENVIRON_COMPARTMENT_SUB_medium &
          ENVIRON_COMPARTMENT_SUB_medium != "*" ~ ENVIRON_COMPARTMENT_SUB_vkat,

        .default = "FLAG: Compartment conflict."
      )
    )

  # Count potential conflicts (where both sources differ and neither is NA)
  # Why is this generating negative results?
  n_potential_compartment <- df |>
    filter(
      !has_data_issue(ENVIRON_COMPARTMENT_vkat),
      !has_data_issue(ENVIRON_COMPARTMENT_medium),
      ENVIRON_COMPARTMENT_vkat != ENVIRON_COMPARTMENT_medium
    ) |>
    nrow()

  n_potential_sub <- df |>
    filter(
      !has_data_issue(ENVIRON_COMPARTMENT_SUB_vkat),
      !has_data_issue(ENVIRON_COMPARTMENT_SUB_medium),
      ENVIRON_COMPARTMENT_SUB_vkat != ENVIRON_COMPARTMENT_SUB_medium
    ) |>
    nrow()

  # Count resolved
  n_resolved <- df |>
    tally(ENVIRON_COMPARTMENT_resolved != "FLAG: Compartment conflict.") -
    n_potential_compartment
  n_resolved_sub <- df |>
    tally(ENVIRON_COMPARTMENT_SUB_resolved != "FLAG: Compartment conflict.") -
    n_potential_sub

  # Count unresolved
  n_unresolved <- df |>
    tally(ENVIRON_COMPARTMENT_resolved == "FLAG: Compartment conflict.")
  n_unresolved_sub <- df |>
    tally(ENVIRON_COMPARTMENT_SUB_resolved == "FLAG: Compartment conflict.")

  # todo: this is a little confusing, because n_resolved currently includes all the problem-free rows too
  message(glue(
    "Compartments: {n_resolved} resolved, {n_unresolved} flagged for removal (of {n_potential_compartment} conflicts)"
  ))
  message(glue(
    "Subcompartments: {n_resolved_sub} resolved, {n_unresolved_sub} flagged for removal (of {n_potential_sub} conflicts)"
  ))

  if (n_unresolved + n_unresolved_sub > 0) {
    warning(
      "Some (sub)compartments still aren't resolved and have been flagged for removal."
    )
  }

  df
}


#' Resolve geographic feature conflicts between data sources
#'
#' Resolves conflicts when parameter (vkat) and medium lookups provide different
#' geographic feature values. Uses similar logic to compartment resolution.
#' Geographic sub-features that cannot be resolved are set to "Not reported"
#' rather than flagged for removal (as they are less critical).
#'
#' Resolution rules:
#' * Non-wildcard values preferred over wildcards (*)
#' * Matching values between sources are accepted
#' * Unresolvable main features are flagged
#' * Unresolvable sub-features default to "Not reported"
#'
#' @param df Data frame with geographic feature columns from both vkat and
#'   medium lookups (SITE_GEOGRAPHIC_FEATURE_vkat, SITE_GEOGRAPHIC_FEATURE_medium,
#'   SITE_GEOGRAPHIC_FEATURE_SUB_vkat, SITE_GEOGRAPHIC_FEATURE_SUB_medium)
#'
#' @return Data frame with two new columns:
#'   * SITE_GEOGRAPHIC_FEATURE_resolved: Resolved feature or "FLAG: Geographic conflict."
#'   * SITE_GEOGRAPHIC_FEATURE_SUB_resolved: Resolved sub-feature or "Not reported"
#'
#' @details Prints messages showing number of conflicts resolved and unresolved.
#'   Issues a warning if any main feature conflicts remain unresolved.
#'
#' @export
resolve_geographic_conflicts <- function(df) {
  df <- df |>
    mutate(
      # SET GEOGRAPHIC FEATURE
      SITE_GEOGRAPHIC_FEATURE_resolved = case_when(
        # if SITE_GEOGRAPHIC_FEATURE_vkat is wildcard then use SITE_GEOGRAPHIC_FEATURE_medium
        !has_data_issue(SITE_GEOGRAPHIC_FEATURE_vkat) &
          !has_data_issue(SITE_GEOGRAPHIC_FEATURE_medium) &
          SITE_GEOGRAPHIC_FEATURE_vkat == "*" &
          SITE_GEOGRAPHIC_FEATURE_medium !=
            "*" ~ SITE_GEOGRAPHIC_FEATURE_medium,

        # if SITE_GEOGRAPHIC_FEATURE_medium is wildcard then use SITE_GEOGRAPHIC_FEATURE_vkat
        !has_data_issue(SITE_GEOGRAPHIC_FEATURE_vkat) &
          !has_data_issue(SITE_GEOGRAPHIC_FEATURE_medium) &
          SITE_GEOGRAPHIC_FEATURE_vkat != "*" &
          SITE_GEOGRAPHIC_FEATURE_medium == "*" ~ SITE_GEOGRAPHIC_FEATURE_vkat,

        # if there's no conflict (unless both are *), use the first value
        SITE_GEOGRAPHIC_FEATURE_vkat == SITE_GEOGRAPHIC_FEATURE_medium &
          SITE_GEOGRAPHIC_FEATURE_medium !=
            "*" ~ SITE_GEOGRAPHIC_FEATURE_medium,

        # If none of these rules resolve the conflict, flag the row
        .default = "FLAG: Geographic conflict."
      ),

      # SET GEOGRAPHIC FEATURE SUB
      SITE_GEOGRAPHIC_FEATURE_SUB_resolved = case_when(
        # if SITE_GEOGRAPHIC_FEATURE_SUB_vkat is wildcard then use SITE_GEOGRAPHIC_FEATURE_SUB_medium
        !has_data_issue(SITE_GEOGRAPHIC_FEATURE_SUB_vkat) &
          !has_data_issue(SITE_GEOGRAPHIC_FEATURE_SUB_medium) &
          SITE_GEOGRAPHIC_FEATURE_SUB_vkat == "*" &
          SITE_GEOGRAPHIC_FEATURE_SUB_medium !=
            "*" ~ SITE_GEOGRAPHIC_FEATURE_SUB_medium,

        # if SITE_GEOGRAPHIC_FEATURE_SUB_medium is wildcard then use SITE_GEOGRAPHIC_FEATURE_SUB_vkat
        !has_data_issue(SITE_GEOGRAPHIC_FEATURE_SUB_vkat) &
          !has_data_issue(SITE_GEOGRAPHIC_FEATURE_SUB_medium) &
          SITE_GEOGRAPHIC_FEATURE_SUB_vkat != "*" &
          SITE_GEOGRAPHIC_FEATURE_SUB_medium ==
            "*" ~ SITE_GEOGRAPHIC_FEATURE_SUB_vkat,

        # if there's no conflict (unless both are *), use the first value
        SITE_GEOGRAPHIC_FEATURE_SUB_vkat == SITE_GEOGRAPHIC_FEATURE_SUB_medium &
          SITE_GEOGRAPHIC_FEATURE_SUB_medium !=
            "*" ~ SITE_GEOGRAPHIC_FEATURE_SUB_vkat,

        # Sub-features are less important and not that frequently reported anyway.
        .default = "Not reported"
      )
    )

  # Count potential conflicts
  n_potential_feature <- df |>
    filter(
      !has_data_issue(SITE_GEOGRAPHIC_FEATURE_vkat),
      !has_data_issue(SITE_GEOGRAPHIC_FEATURE_medium),
      SITE_GEOGRAPHIC_FEATURE_vkat != SITE_GEOGRAPHIC_FEATURE_medium
    ) |>
    nrow()

  n_potential_sub <- df |>
    filter(
      !has_data_issue(SITE_GEOGRAPHIC_FEATURE_SUB_vkat),
      !has_data_issue(SITE_GEOGRAPHIC_FEATURE_SUB_medium),
      SITE_GEOGRAPHIC_FEATURE_SUB_vkat != SITE_GEOGRAPHIC_FEATURE_SUB_medium
    ) |>
    nrow()

  # Count unresolved
  n_unresolved <- df |>
    tally(SITE_GEOGRAPHIC_FEATURE_resolved == "FLAG: Geographic conflict.")
  n_unresolved_sub <- df |>
    tally(SITE_GEOGRAPHIC_FEATURE_SUB_resolved == "FLAG: Geographic conflict.")

  message(glue(
    "Geographic features: {n_potential_feature - n_unresolved} resolved, {n_unresolved} unresolved (of {n_potential_feature} conflicts)"
  ))
  message(glue(
    "Geographic sub-features: {n_potential_sub - n_unresolved_sub} resolved, {n_unresolved_sub} unresolved (of {n_potential_sub} conflicts)"
  ))

  if (n_unresolved + n_unresolved_sub > 0) {
    warning("Some geographic features still aren't resolved.")
  }

  df
}

# # Split Vannmiljø sites

#' Split sites with multiple geographic feature combinations
#'
#' When a single Vannlok_kode (site code) has measurements associated with
#' multiple distinct geographic feature combinations, creates unique site
#' identifiers by appending numeric suffixes. This handles cases where the
#' same nominal site actually represents multiple distinct sampling locations.
#'
#' @param vm_compartment_geo_conflicts_resolved_removed Data frame with
#'   resolved geographic features (SITE_GEOGRAPHIC_FEATURE_resolved,
#'   SITE_GEOGRAPHIC_FEATURE_SUB_resolved) and site codes (Vannlok_kode)
#'
#' @return Data frame with additional columns:
#'   * n_geo_combos: Number of distinct geographic combinations per Vannlok_kode
#'   * geo_combo: Concatenated geographic feature and sub-feature
#'   * geo_suffix: Numeric suffix (1, 2, 3...) for sites with multiple combinations
#'   * Vannlok_kode_split: Modified site code with suffix (e.g., "ABC123-01")
#'     or original code if only one geographic combination exists
#'
#' @details Sites are only split when they have >1 distinct geographic feature
#'   combination. Suffixes are zero-padded to 2 digits.
#'
#' @export
vm_split_sites <- function(
  vm_compartment_geo_conflicts_resolved_removed
) {
  vm_compartment_geo_conflicts_resolved_removed |>
    group_by(Vannlok_kode) |>
    mutate(
      n_geo_combos = n_distinct(paste(
        SITE_GEOGRAPHIC_FEATURE_resolved,
        SITE_GEOGRAPHIC_FEATURE_SUB_resolved
      )),
      geo_combo = paste(
        SITE_GEOGRAPHIC_FEATURE_resolved,
        SITE_GEOGRAPHIC_FEATURE_SUB_resolved
      ),
      geo_suffix = if_else(
        n_geo_combos > 1,
        match(geo_combo, unique(geo_combo)),
        NA_integer_
      ),
      Vannlok_kode_split = case_when(
        !is.na(geo_suffix) ~ paste0(
          Vannlok_kode,
          "-",
          sprintf("%02d", geo_suffix)
        ),
        TRUE ~ Vannlok_kode
      )
    ) |>
    ungroup()
}

#' Generate sample ID with components
#'
#' Creates unique sample identifiers by concatenating site code, parameter,
#' compartment, date, and subsample information. This is a helper function
#' copied from STOPeData::mod_samples_fct.R.
#'
#' @param site_code Site code (vectorised)
#' @param parameter_name Parameter name (vectorised)
#' @param environ_compartment Environmental compartment (vectorised)
#' @param environ_compartment_sub Environmental sub-compartment (vectorised)
#' @param date Sampling date (vectorised)
#' @param subsample Subsample identifier (vectorised)
#'
#' @return Character vector of sample IDs in format:
#'   {site_code}-{param_abbrev}-{comp_abbrev}-{date}-R-{subsample}
#'
#' @details
#' - Parameter names are abbreviated to 8 characters (alphanumeric only)
#' - Compartments are abbreviated to 12 characters (alphanumeric only)
#' - Subsample values are truncated to 20 characters
#'
#' @keywords internal
generate_sample_id_with_components <- function(
  site_code,
  parameter_name,
  environ_compartment,
  environ_compartment_sub,
  date,
  subsample = 1
) {
  # Create abbreviated versions for ID (vectorised)
  param_abbrev <- substr(gsub("[^A-Za-z0-9]", "", parameter_name), 1, 8)
  comp_abbrev <- substr(
    gsub("[^A-Za-z0-9]", "", environ_compartment_sub),
    1,
    12
  )
  date_abbrev <- gsub("-", "-", date)

  base_id <- glue("{site_code}-{param_abbrev}-{comp_abbrev}-{date_abbrev}")

  # vectorised replicate
  # Subsamples will generally be text, so let's abbreviate them a bit
  subsample_suffix <- stringr::str_trunc(subsample, 20, "right", ellipsis = "")
  paste0(base_id, "-R-", subsample_suffix)
}


# # Unit and operator conversion functions ----

#' Convert Vannmiljø operator symbols to eData measurement flags
#'
#' Converts Norwegian operator symbols from Vannmiljø to standardised
#' measurement flag descriptions used in eData format. Stops with an error
#' if an unknown operator is encountered.
#'
#' @param col Character vector of operator symbols from Vannmiljø
#'
#' @return Character vector of standardised measurement flags:
#' - "=" → "" (empty string, value is exactly as measured)
#' - "<" → "< LOQ" (below limit of quantification)
#' - ">" → Stops with error (unexpected operator)
#' - "ND" → "< LOD" (below limit of detection, non-detect)
#'
#' @details The ">" operator is not expected in normal concentration data
#'   and triggers an error to prevent incorrect data interpretation.
#'
#' @examples
#' \dontrun{
#' vm_convert_operator(c("=", "<", "ND"))
#' # Returns: c("", "< LOQ", "< LOD")
#' }
#'
#' @export
vm_convert_operator <- function(col) {
  # Check for unexpected operators before conversion
  unexpected <- col[!col %in% c("=", "<", "ND", NA)]

  if (length(unexpected) > 0) {
    stop(glue(
      "Unexpected operator(s) found in Vannmiljø data: ",
      "{paste(unique(unexpected), collapse = ', ')}. ",
      "Expected operators are: '=', '<', 'ND'. ",
      "Please investigate why '>' or other operators are present."
    ))
  }

  case_match(
    col,
    "=" ~ "",
    "<" ~ "< LOQ",
    "ND" ~ "< LOD",
    .default = NA_character_
  )
}


#' Convert Vannmiljø unit names to standardised eData units
#'
#' Converts Norwegian unit names from Vannmiljø to standardised unit notation
#' used in eData format. Stops with an error if an unknown unit is encountered
#' to prevent silent conversion errors.
#'
#' @param col Character vector of unit names from Vannmiljø
#'
#' @return Character vector of standardised unit names:
#' - "µg/l" → "µg/L" (micrograms per liter)
#' - "mg/kg t.v." → "mg/kg (dry)" (milligrams per kilogram dry weight)
#' - "mg/kg v.v." → "mg/kg (wet)" (milligrams per kilogram wet weight)
#'
#' @details Known unit conversions:
#' - Norwegian "t.v." (tørrvekt) = dry weight
#' - Norwegian "v.v." (våtvekt) = wet weight
#' - Volume units standardised to capital L
#'
#' Any unit not in the conversion table will trigger an error with the
#' unknown unit name to facilitate investigation and table updates.
#'
#' @examples
#' \dontrun{
#' vm_convert_unit(c("µg/l", "mg/kg t.v.", "mg/kg v.v."))
#' # Returns: c("µg/L", "mg/kg (dry)", "mg/kg (wet)")
#' }
#'
#' @export
vm_convert_unit <- function(col) {
  # Get unique units for checking
  unique_units <- unique(col[!is.na(col)])
  known_units <- c("µg/l", "mg/kg t.v.", "mg/kg v.v.")
  unknown_units <- setdiff(unique_units, known_units)

  if (length(unknown_units) > 0) {
    stop(glue(
      "Unknown unit(s) found in Vannmiljø data: ",
      "{paste(unknown_units, collapse = ', ')}. ",
      "Known units are: {paste(known_units, collapse = ', ')}. ",
      "Please add conversion rule to vm_convert_unit() function."
    ))
  }

  case_match(
    col,
    "µg/l" ~ "µg/L",
    "mg/kg t.v." ~ "mg/kg (dry)",
    "mg/kg v.v." ~ "mg/kg (wet)",
    .default = NA_character_
  )
}


#' Generate sample ID with components
#'
#' Creates unique sample identifiers by concatenating site code, parameter,
#' copied from STOPeData::mod_samples_fct.R.
#'
#' @param site_code Site code (vectorised)
#' @param parameter_name Parameter name (vectorised)
#' @param environ_compartment Environmental compartment (vectorised)
#' @param environ_compartment_sub Environmental sub-compartment (vectorised)
#' @param date Sampling date (vectorised)
#' @param subsample Subsample identifier (vectorised)
#'
#' @return Character vector of sample IDs in format:
#'   {site_code}-{param_abbrev}-{comp_abbrev}-{date}-R-{subsample}
#'
#' @details
#' - Parameter names are abbreviated to 8 characters (alphanumeric only)
#' - Compartments are abbreviated to 12 characters (alphanumeric only)
#' - Subsample values are truncated to 20 characters
#'
#' @keywords internal
generate_sample_id_with_components <- function(
  site_code,
  parameter_name,
  environ_compartment,
  environ_compartment_sub,
  date,
  subsample = 1
) {
  # Create abbreviated versions for ID (vectorised)
  param_abbrev <- substr(gsub("[^A-Za-z0-9]", "", parameter_name), 1, 8)
  comp_abbrev <- substr(
    gsub("[^A-Za-z0-9]", "", environ_compartment_sub),
    1,
    12
  )
  date_abbrev <- gsub("-", "-", date)

  base_id <- glue("{site_code}-{param_abbrev}-{comp_abbrev}-{date_abbrev}")

  # vectorised replicate
  # Subsamples will generally be text, so let's abbreviate them a bit
  subsample_suffix <- stringr::str_trunc(subsample, 20, "right", ellipsis = "")
  paste0(base_id, "-R-", subsample_suffix)
}


#' Map Vannmiljø tissue types to eData tissue categories
#'
#' Converts Norwegian tissue type names from Vannmiljø MediumID to standardized
#' English tissue categories used in eData format.
#'
#' @param medium_id_name Character vector of Vannmiljø MediumID names
#'
#' @return Character vector of standardized tissue type names
#'
#' @details Tissue mappings:
#' - Biota bløtdeler → Total soft tissues
#' - Biota gjeller → Gills
#' - Biota helkropp → Whole body
#' - Biota lever → Liver
#' - Biota muskelvev → Muscle
#' - Biota plantevev → Plant tissue
#' - Biota egg → Egg
#' - Biota blod → Blood
#' - Biota skuddspiss → Shoot tip
#' - Biota fettvev → Fat/Adipose
#' - Biota galle → Bile
#' - Unknown values → "Unknown Tissue"
#'
#' @keywords internal
map_tissue_type <- function(medium_id_name) {
  case_match(
    # TODO: tissue mapping should be a table or something
    medium_id_name,
    "Biota bløtdeler" ~ "Total soft tissues",
    "Biota gjeller" ~ "Gill",
    "Biota helkropp" ~ "Whole body",
    "Biota lever" ~ "Liver",
    "Biota muskelvev" ~ "Muscle",
    "Biota plantevev" ~ "Plant tissue",
    "Biota egg" ~ "Egg",
    "Biota blod" ~ "Blood",
    "Biota skuddspiss" ~ "Shoot tip",
    "Biota fettvev" ~ "Fat/Adipose",
    "Biota galle" ~ "Bile",
    .default = "Unknown tissue"
  )
}
