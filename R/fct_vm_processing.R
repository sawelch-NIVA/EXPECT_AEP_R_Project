# Functions specifically related to getting Vannmiljo data ready for analysis.

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

# # Create eData tables ----

#' Create eData campaign table from Vannmiljø data
#'
#' Generates a standardised eData campaign table using processed Vannmiljø data.
#' Creates campaign metadata including date range, organization, and descriptive
#' comments about the data scope.
#'
#' @param vm_data Processed Vannmiljø data frame (e.g., vm_sites_split_clean)
#' @param campaign_name_short Short campaign identifier
#' @param campaign_name Full campaign name
#' @param date_start Campaign start date (Date object or character YYYY-MM-DD)
#' @param date_end Campaign end date (Date object or character YYYY-MM-DD)
#' @param organisation Organization name
#' @param entered_by Person/entity who entered the data
#'
#' @return A tibble conforming to eData campaign schema with one row containing
#'   campaign metadata
#'
#' @export
vm_create_edata_campaign_table <- function(
  vm_data,
  campaign_name_short,
  campaign_name,
  date_start,
  date_end,
  organisation,
  entered_by
) {
  edata_campaign <- initialise_campaign_tibble() |>
    add_row(
      CAMPAIGN_NAME_SHORT = campaign_name_short,
      CAMPAIGN_NAME = campaign_name,
      CAMPAIGN_START_DATE = as.Date(date_start),
      CAMPAIGN_END_DATE = as.Date(date_end),
      RELIABILITY_SCORE = NA_character_,
      RELIABILITY_EVAL_SYS = NA_character_,
      CONFIDENTIALITY_EXPIRY_DATE = as.Date(NA),
      ORGANISATION = organisation,
      ENTERED_BY = entered_by,
      ENTERED_DATE = as.Date(Sys.Date()),
      CAMPAIGN_COMMENT = glue(
        "Copper and copper pyrithione measurements from Vannmiljø database ",
        "covering all Norwegian municipalities and media types. ",
        "{nrow(vm_data)} measurements from {date_start} to {date_end}."
      )
    )

  message(glue("Created campaign table with {nrow(edata_campaign)} row"))
  edata_campaign
}


#' Create eData reference table for Vannmiljø data source
#'
#' Generates a standardised eData reference table documenting the Vannmiljø
#' database as the data source. Includes download date, access URL, and
#' search parameters used.
#'
#' @param vm_data Processed Vannmiljø data frame (e.g., vm_sites_split_clean)
#' @param reference_id Unique reference identifier
#' @param date_start Data collection start date
#' @param date_end Data collection/access end date
#' @param organisation Organization name
#' @param entered_by Person/entity who entered the data
#'
#' @return A tibble conforming to eData reference schema with one row containing
#'   bibliographic information for the Vannmiljø database
#'
#' @export
vm_create_edata_reference_table <- function(
  vm_data,
  reference_id,
  date_start,
  date_end,
  organisation,
  entered_by
) {
  edata_reference <- initialise_references_tibble() |>
    add_row(
      REFERENCE_ID = reference_id,
      REFERENCE_TYPE = "Database",
      DATA_SOURCE = "Vannmiljø",
      AUTHOR = organisation,
      TITLE = "Vannmiljø Database - Copper and Copper Pyrithione Data",
      YEAR = year(date_end),
      ACCESS_DATE = as.Date(date_end),
      PERIODICAL_JOURNAL = NA_character_,
      VOLUME = NA_integer_,
      ISSUE = NA_integer_,
      PUBLISHER = NA_character_,
      INSTITUTION = organisation,
      DOI = NA_character_,
      URL = "https://www.vannmiljo.no/",
      ISBN_ISSN = NA_character_,
      EDITION = NA_character_,
      DOCUMENT_NUMBER = NA_character_,
      REF_COMMENT = glue(
        "Downloaded {date_end}, data for {date_start} to {date_end}, ",
        "all kommune, all media, all campaigns. Search for Kobber & Kobberpyrition. ",
        "{nrow(vm_data)} measurements."
      )
    )

  message(glue("Created reference table with {nrow(edata_reference)} row"))
  edata_reference
}


#' Create eData parameters table for copper measurements
#'
#' Generates a standardised eData parameters table for copper based on the
#' parameters present in the Vannmiljø data. Currently extracts unique
#' parameter names from the data.
#'
#' @param vm_data Processed Vannmiljø data frame with parameter information
#' @param entered_by Person/entity who entered the data
#'
#' @return A tibble conforming to eData parameters schema with rows for each
#'   unique parameter found in vm_data
#'
#' @details Parameter metadata (CAS RN, InChIKey, PubChem CID) is currently
#'   set to NA and should be filled in separately if needed. Extracts unique
#'   values from the Parameter column in vm_data.
#'
#' @export
vm_create_edata_parameters_table <- function(vm_data, entered_by) {
  # Extract unique parameters from the data
  unique_params <- vm_data |>
    distinct(Parameter_id) |>
    pull(Parameter_id)

  stopifnot(
    "I haven't checked this function works properly for datasets of >1 parameter, which I guess you have. So time to do some work!" = length(
      unique_params
    ) ==
      1
  )

  edata_parameters <- initialise_parameters_tibble()

  for (param in unique_params) {
    edata_parameters <- edata_parameters |>
      add_row(
        PARAMETER_TYPE = NA_character_,
        PARAMETER_TYPE_SUB = NA_character_,
        MEASURED_TYPE = NA_character_,
        PARAMETER_NAME = "Copper",
        PARAMETER_NAME_SUB = NA_character_,
        INCHIKEY_SD = NA_character_,
        PUBCHEM_CID = NA_integer_,
        CAS_RN = NA_character_,
        ENTERED_BY = glue("{entered_by} from Vannmiljø"),
        PARAMETER_COMMENT = NA_character_
      )
  }

  message(glue(
    "Created parameters table with {nrow(edata_parameters)} parameters: ",
    "{paste(unique_params, collapse = ', ')}"
  ))
  edata_parameters
}


#' Extract and format unique Vannmiljø sites with geographic metadata
#'
#' Extracts unique monitoring sites from split Vannmiljø data, including
#' geographic features, coordinates, and emission sources. Coordinates are
#' reprojected from UTM33 to WGS84.
#'
#' @param vm_data Data frame with split sites containing columns:
#'   Vannlok_kode_split, Vannlokalitetsnavn, Beskrivelse, UTM33 coordinates,
#'   Knytt til påvirkning, and resolved geographic features
#' @param entered_by Person/entity who entered the data
#'
#' @return A tibble conforming to eData sites schema with columns:
#'   SITE_CODE, SITE_NAME, SITE_GEOGRAPHIC_FEATURE, SITE_GEOGRAPHIC_FEATURE_SUB,
#'   LATITUDE, LONGITUDE, SITE_COORDINATE_SYSTEM, COUNTRY_ISO, OCEAN_IHO,
#'   ENTERED_BY, ENTERED_DATE, SITE_COMMENT
#'
#' @details Emission sources are categorized from Norwegian text:
#'   - "Industri/INDUSTRI" → "Industrial"
#'   - "Akvakultur/AKVAKULTUR" → "Aquaculture"
#'
#'   SITE_CODE format: "Vannmiljø_{Vannlok_kode_split}"
#'   SITE_NAME format: "Vannmiljø Station {Vannlokalitetsnavn}"
#'
#'   Validates against duplicate SITE_CODEs and checks coordinate reprojection.
#'
#' @export
vm_create_edata_sites_table <- function(vm_data, entered_by) {
  # Extract unique sites with relevant metadata ----
  vm_sites_unique <- vm_data |>
    select(
      Vannlok_kode_split,
      Vannlokalitetsnavn,
      Beskrivelse,
      `UTM33 Ost (X)`,
      `UTM33 Nord (Y)`,
      `Knytt til påvirkning`,
      SITE_GEOGRAPHIC_FEATURE_resolved,
      SITE_GEOGRAPHIC_FEATURE_SUB_resolved
    ) |>
    distinct() |>
    # Clean up emission source
    mutate(
      Emission_Source = case_when(
        `Knytt til påvirkning` %in% c("Industri", "INDUSTRI") ~ "Industrial",
        `Knytt til påvirkning` %in%
          c("Akvakultur", "AKVAKULTUR") ~ "Aquaculture",
        TRUE ~ NA_character_
      )
    )

  message(glue("Extracted {nrow(vm_sites_unique)} unique sites"))

  # Format to eData structure ----
  edata_sites_temp <- vm_sites_unique |>
    mutate(
      SITE_CODE = glue("Vannmiljø_{Vannlok_kode_split}"),
      SITE_NAME = glue("Vannmiljø Station {Vannlokalitetsnavn}"),
      SITE_GEOGRAPHIC_FEATURE = SITE_GEOGRAPHIC_FEATURE_resolved,
      SITE_GEOGRAPHIC_FEATURE_SUB = SITE_GEOGRAPHIC_FEATURE_SUB_resolved,
      COUNTRY_ISO = "Norway",
      OCEAN_IHO = "Not relevant",
      ENTERED_BY = glue("{entered_by} (Vm Conversion)"),
      ENTERED_DATE = as.character(today()),
      ALTITUDE_VALUE = 0, # Altitude not relevant for water monitoring
      ALTITUDE_UNIT = "m",
      # Combine description and emission source where available
      SITE_COMMENT = case_when(
        !is.na(Beskrivelse) &
          Beskrivelse != "" &
          !is.na(Emission_Source) &
          Emission_Source != "" ~
          glue(
            "Vm Original Comment: {Beskrivelse}. Vm Emission Source: {Emission_Source}"
          ),
        !is.na(Beskrivelse) & Beskrivelse != "" ~
          glue("Vm Original Comment: {Beskrivelse}"),
        !is.na(Emission_Source) & Emission_Source != "" ~
          glue("Vm Emission Source: {Emission_Source}"),
        .default = NA_character_
      )
    )

  # Reproject coordinates from UTM33 to WGS84 ----
  tryCatch(
    {
      sites_sf <- edata_sites_temp |>
        st_as_sf(
          coords = c("UTM33 Ost (X)", "UTM33 Nord (Y)"),
          crs = 25833
        ) |>
        st_transform(4326) |>
        mutate(
          LATITUDE = st_coordinates(geometry)[, 2],
          LONGITUDE = st_coordinates(geometry)[, 1],
          SITE_COORDINATE_SYSTEM = "WGS84"
        )

      sites_coords <- sites_sf |>
        st_coordinates() |>
        as.data.frame() |>
        bind_cols(st_drop_geometry(sites_sf))

      message(glue(
        "Successfully reprojected {nrow(sites_sf)} sites from UTM33 to WGS84"
      ))
    },
    error = function(e) {
      stop(glue(
        "Failed to reproject coordinates from UTM33 to WGS84: {e$message}"
      ))
    }
  )

  # Finalise sites table ----
  tryCatch(
    {
      # Check for duplicates before joining
      sites_for_join <- sites_sf |>
        st_drop_geometry() |>
        select(SITE_CODE, LATITUDE, LONGITUDE, SITE_COORDINATE_SYSTEM) |>
        distinct()

      duplicate_sites <- sites_for_join |>
        group_by(SITE_CODE) |>
        filter(n() > 1) |>
        arrange(SITE_CODE)

      if (nrow(duplicate_sites) > 0) {
        warning(glue(
          "Found {nrow(duplicate_sites)} duplicate SITE_CODE entries in sites_sf. ",
          "Displaying first 10 conflicts:"
        ))
        print(head(duplicate_sites, 10))
      }

      edata_sites <- edata_sites_temp |>
        left_join(
          sites_for_join,
          by = "SITE_CODE",
          relationship = "many-to-many" # Temporarily allow to see what happens
        ) |>
        select(
          SITE_CODE,
          SITE_NAME,
          SITE_GEOGRAPHIC_FEATURE,
          SITE_GEOGRAPHIC_FEATURE_SUB,
          LATITUDE,
          LONGITUDE,
          SITE_COORDINATE_SYSTEM,
          COUNTRY_ISO,
          OCEAN_IHO,
          ENTERED_BY,
          ENTERED_DATE,
          SITE_COMMENT
        )

      # Check result for duplicates
      result_duplicates <- edata_sites |>
        distinct() |>
        group_by(SITE_CODE) |>
        filter(n() > 1)

      if (nrow(result_duplicates) > 0) {
        stop(glue(
          "Many-to-many join created {nrow(result_duplicates)} duplicate rows. ",
          "Check vm_sites_unique for duplicate coordinates per site."
        ))
      }
    },
    warning = function(w) {
      stop(glue(
        "Unexpected many-to-many relationship when joining sites table:\n{w}"
      ))
    }
  )

  # Validate against eData schema ----
  edata_sites <- initialise_sites_tibble() |>
    add_row(edata_sites)

  message(glue("Created sites table: {nrow(edata_sites)} sites"))

  edata_sites
}

# Additional eData conversion functions ----

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


#' Map Vannmiljø tissue types to eData tissue categories
#'
#' Converts Norwegian tissue type names from Vannmiljø MediumID to standardised
#' English tissue categories used in eData format.
#'
#' @param medium_id_name Character vector of Vannmiljø MediumID names
#'
#' @return Character vector of standardised tissue type names
#'
#' @details Tissue mappings:
#' - Biota bløtdeler → Total soft tissues
#' - Biota gjeller → Gills
#' - Biota helkropp → Whole body
#' - Biota lever → Liver
#' - Biota muskelvev → Muscle tissue
#' - Biota plantevev → Plant tissue
#' - Biota egg → Egg
#' - Biota blod → Blood
#' - Biota skuddspiss → Shoot tip
#' - Biota fettvev → Adipose tissue
#' - Biota galle → Bile
#' - Unknown values → "Unknown Tissue"
#'
#' @keywords internal
map_tissue_type <- function(medium_id_name) {
  case_match(
    medium_id_name,
    "Biota bløtdeler" ~ "Total soft tissues",
    "Biota gjeller" ~ "Gills",
    "Biota helkropp" ~ "Whole body",
    "Biota lever" ~ "Liver",
    "Biota muskelvev" ~ "Muscle tissue",
    "Biota plantevev" ~ "Plant tissue",
    "Biota egg" ~ "Egg",
    "Biota blod" ~ "Blood",
    "Biota skuddspiss" ~ "Shoot tip",
    "Biota fettvev" ~ "Adipose tissue",
    "Biota galle" ~ "Bile",
    .default = "Unknown Tissue"
  )
}


#' Create eData samples table from Vannmiljø data
#'
#' Generates a standardised eData samples table from processed Vannmiljø data.
#' Filters out unresolved compartment conflicts and creates unique sample IDs
#' for each measurement.
#'
#' @param vm_data Processed Vannmiljø data with resolved compartments
#'   (e.g., vm_sites_split_clean)
#'
#' @return A tibble conforming to eData samples schema containing:
#'   SAMPLE_ID, SITE_CODE, SITE_NAME, PARAMETER_NAME, PARAMETER_TYPE,
#'   ENVIRON_COMPARTMENT, ENVIRON_COMPARTMENT_SUB, SAMPLING_DATE, SUBSAMPLE,
#'   and other sample metadata
#'
#' @details
#' - Excludes samples with unresolved compartment conflicts (flagged rows)
#' - SITE_CODE format: "Vannmiljø_{Vannlok_kode_split}"
#' - SITE_NAME format: "Vannmiljø Station {Vannlokalitetsnavn}"
#' - Uses generate_sample_id_with_components() to create unique sample IDs
#'
#' @export
vm_create_edata_samples_table <- function(vm_data) {
  # Create samples table ----
  edata_samples_wide <- vm_data |>
    filter(
      ENVIRON_COMPARTMENT_resolved != "FLAG: Compartment conflict.",
      ENVIRON_COMPARTMENT_SUB_resolved != "FLAG: Compartment conflict."
    ) |>
    mutate(
      # Core identifiers
      SITE_CODE = glue("Vannmiljø_{Vannlok_kode_split}"),
      SITE_NAME = glue("Vannmiljø Station {Vannlokalitetsnavn}"),
      PARAMETER_NAME = "Copper",
      PARAMETER_TYPE = "Not reported",

      # Compartment information
      ENVIRON_COMPARTMENT = ENVIRON_COMPARTMENT_resolved,
      ENVIRON_COMPARTMENT_SUB = ENVIRON_COMPARTMENT_SUB_resolved,
      MEASURED_CATEGORY = NA_character_,

      SAMPLING_DATE = as.character(SAMPLING_DATE),

      # Sample information
      SUBSAMPLE = "NA",
      SAMPLE_ID = generate_sample_id_with_components(
        SITE_CODE,
        PARAMETER_NAME,
        ENVIRON_COMPARTMENT,
        ENVIRON_COMPARTMENT_SUB,
        SAMPLING_DATE,
        SUBSAMPLE
      )
    )

  # Validate against eData schema ----
  edata_samples <- initialise_samples_tibble() |>
    add_row(
      edata_samples_wide |>
        select(any_of(names(initialise_samples_tibble())))
    )

  message(glue("Created samples table: {nrow(edata_samples)} samples"))

  edata_samples
}


#' Create eData biota table from Vannmiljø data
#'
#' Generates a standardised eData biota table from processed Vannmiljø data.
#' Handles species name corrections, tissue type mapping, and compartment
#' inference for biota samples.
#'
#' @param vm_data Processed Vannmiljø data with resolved compartments and biota
#'   information (e.g., vm_sites_split_clean)
#'
#' @return A tibble conforming to eData biota schema containing:
#'   SAMPLE_ID, SPECIES_GROUP, SAMPLE_SPECIES, SAMPLE_TISSUE,
#'   SAMPLE_SPECIES_LIFESTAGE, SAMPLE_SPECIES_GENDER, BIOTA_COMMENT,
#'   and other biota-specific metadata
#'
#' @details
#' Species corrections:
#' - "Laksesmolt" → "Salmo salar" with lifestage "Juvenile"
#'
#' Compartment inference:
#' - Terrestrial species get ENVIRON_COMPARTMENT_SUB = "Biota, terrestrial"
#'
#' Tissue mapping:
#' - Uses map_tissue_type() to convert Norwegian tissue names to English
#'
#' Quality checks:
#' - Reports number of samples with missing species groups
#' - Reports number of samples with missing subcompartments
#' - Reports number of samples with unknown tissue types
#'
#' @export
vm_create_edata_biota_table <- function(vm_data) {
  # Filter to biota samples only ----
  edata_biota <- vm_data |>
    filter(ENVIRON_COMPARTMENT_resolved == "Biota")

  edata_species <- unique(edata_biota$VitenskapligNavn) |> length()
  message(glue(
    "{nrow(edata_biota)} biota samples found ({edata_species} unique species)"
  ))

  # Check data quality ----
  biota_frequency <- count(
    edata_biota,
    VitenskapligNavn,
    ENVIRON_COMPARTMENT_SUB_resolved
  ) |>
    arrange(desc(n)) |>
    mutate(
      SAMPLE_SPECIES = case_when(
        VitenskapligNavn == "Laksesmolt" ~ "Salmo salar",
        TRUE ~ VitenskapligNavn
      ),
      SAMPLE_SPECIES_LIFESTAGE = case_when(
        VitenskapligNavn == "Laksesmolt" ~ "Juvenile",
        TRUE ~ "Not reported"
      )
    )

  n_missing_species_group <- biota_frequency |>
    filter(is.na(SAMPLE_SPECIES)) |>
    pull(n) |>
    sum()

  n_missing_sub_compartment <- biota_frequency |>
    filter(ENVIRON_COMPARTMENT_SUB_resolved == "*") |>
    pull(n) |>
    sum()

  message(glue(
    "Biota data quality:\n",
    "  - {n_missing_species_group} samples with missing species group\n",
    "  - {n_missing_sub_compartment} samples with missing subcompartment"
  ))

  # Tissue type quality check ----
  tissue_frequency <- count(edata_biota, MediumID_Name) |>
    arrange(desc(n)) |>
    mutate(SAMPLE_TISSUE = map_tissue_type(MediumID_Name))

  n_unknown_tissue <- tissue_frequency |>
    filter(SAMPLE_TISSUE == "Unknown Tissue") |>
    pull(n) |>
    sum()

  message(glue(
    "  - {n_unknown_tissue} samples with unknown tissue type"
  ))

  # Merge species and tissue corrections ----
  edata_biota_merged <- edata_biota |>
    mutate(
      SAMPLE_SPECIES = case_when(
        VitenskapligNavn == "Laksesmolt" ~ "Salmo salar",
        TRUE ~ VitenskapligNavn
      ),
      SAMPLE_SPECIES_LIFESTAGE = case_when(
        VitenskapligNavn == "Laksesmolt" ~ "Juvenile",
        TRUE ~ "Not reported"
      ),
      # Guess compartment from species
      # All species not sampled from something with a Vannkategori are terrestrial
      ENVIRON_COMPARTMENT_SUB_resolved = case_when(
        ENVIRON_COMPARTMENT_SUB_resolved !=
          "*" ~ ENVIRON_COMPARTMENT_SUB_resolved,
        SAMPLE_SPECIES %in%
          c(
            "Capreolus capreolus",
            "Felis catus",
            "Hylocomium splendens",
            "Larus argentatus",
            "Lichen",
            "Phoca vitulina",
            "Rattus norvegicus",
            "Somateria mollissima",
            "Turdus pilaris",
            "Vulpes vulpes"
          ) ~ "Biota, terrestrial",
        .default = ENVIRON_COMPARTMENT_SUB_resolved
      ),
      SAMPLE_TISSUE = map_tissue_type(MediumID_Name)
    )

  # Create biota samples with full metadata ----
  vm_samples_biota_only <- edata_biota_merged |>
    mutate(
      # Core identifiers
      SITE_CODE = glue("Vannmiljø_{Vannlok_kode_split}"),
      PARAMETER_NAME = "Copper",

      # Compartment information
      ENVIRON_COMPARTMENT = ENVIRON_COMPARTMENT_resolved,
      ENVIRON_COMPARTMENT_SUB = ENVIRON_COMPARTMENT_SUB_resolved,
      MEASURED_CATEGORY = NA_character_,

      SAMPLING_DATE = as.character(SAMPLING_DATE),

      # Sample information
      SUBSAMPLE = "NA",
      SAMPLE_ID = generate_sample_id_with_components(
        SITE_CODE,
        PARAMETER_NAME,
        ENVIRON_COMPARTMENT,
        ENVIRON_COMPARTMENT_SUB,
        SAMPLING_DATE,
        SUBSAMPLE
      ),

      # Biota-specific fields
      SPECIES_GROUP = species_group,
      SAMPLE_SPECIES,
      SAMPLE_TISSUE,
      SAMPLE_SPECIES_LIFESTAGE,
      SAMPLE_SPECIES_GENDER = "Not reported",
      BIOTA_COMMENT = "Raw species names from Vannmiljø. May be erroneous."
    )

  # Validate against eData schema ----
  edata_biota <- initialise_biota_tibble() |>
    add_row(
      vm_samples_biota_only |>
        select(any_of(names(initialise_biota_tibble())))
    )

  message(glue("Created biota table: {nrow(edata_biota)} rows"))

  edata_biota
}


#' Create eData measurements table from Vannmiljø samples and biota
#'
#' Generates a standardised eData measurements table by combining non-biota
#' samples with biota samples and extracting measurement values, flags,
#' uncertainty, and detection limits.
#'
#' @param vm_samples_table eData samples table from vm_create_edata_samples_table()
#' @param vm_biota_table eData biota table from vm_create_edata_biota_table()
#' @param campaign_name_short Short campaign identifier
#' @param reference_id Reference ID for the data source
#'
#' @return A tibble conforming to eData measurements schema containing:
#'   SAMPLE_ID, SITE_CODE, PARAMETER_NAME, SAMPLING_DATE, CAMPAIGN_NAME_SHORT,
#'   REFERENCE_ID, MEASURED_VALUE, MEASURED_UNIT, MEASURED_FLAG, LOQ_VALUE,
#'   LOD_VALUE, protocol references, and other measurement metadata
#'
#' @details
#' Measurement fields:
#' - MEASURED_FLAG: Converted from Vannmiljø Operator using vm_convert_operator()
#' - MEASURED_VALUE: Direct from Vannmiljø Verdi
#' - MEASURED_UNIT: Converted from Vannmiljø Unit_Name using vm_convert_unit()
#' - MEASURED_N: Number of measurements (Ant_verdier)
#'
#' Detection limits:
#' - LOQ_VALUE: Quantification limit (Kvantifiseringsgrense)
#' - LOD_VALUE: Detection limit (Deteksjonsgrense)
#'
#' Protocols:
#' - Currently uses placeholder IDs ("1", "2", "3", "4")
#' - # TODO: Implement proper protocol ID mapping
#'
#' @export
vm_create_edata_measurements_table <- function(
  vm_samples_table,
  vm_biota_table,
  campaign_name_short,
  reference_id
) {
  # Merge samples and biota ----
  # This is needed because biota has additional columns that regular samples don't
  biota_samples_merged <- vm_samples_table |>
    bind_rows(vm_biota_table)

  # Create measurements table ----
  edata_measurements <- biota_samples_merged |>
    mutate(
      # Core identifiers
      SITE_CODE,
      PARAMETER_NAME,
      SAMPLING_DATE = as.character(SAMPLING_DATE),
      CAMPAIGN_NAME_SHORT = campaign_name_short,
      REFERENCE_ID = reference_id,

      # Compartment information
      ENVIRON_COMPARTMENT,
      ENVIRON_COMPARTMENT_SUB,

      # Parameter classification
      PARAMETER_TYPE = "Stressor",
      MEASURED_TYPE = "Concentration",

      # Sample information
      SUBSAMPLE,
      SAMPLE_ID,

      # Measurement values
      MEASURED_FLAG = vm_convert_operator(Operator),
      MEASURED_VALUE = Verdi,
      MEASURED_UNIT = vm_convert_unit(Unit_Name),
      MEASURED_N = Ant_verdier,

      # Uncertainty (not reported in Vannmiljø)
      UNCERTAINTY_TYPE = "Not reported",
      UNCERTAINTY_UPPER = NA_real_,
      UNCERTAINTY_LOWER = NA_real_,

      # Detection limits
      LOQ_VALUE = Kvantifiseringsgrense,
      LOQ_UNIT = vm_convert_unit(Unit_Name),
      LOD_VALUE = Deteksjonsgrense,
      LOD_UNIT = vm_convert_unit(Unit_Name),

      # TODO: Fix methods reference
      # Protocol references (FIXME: Need proper protocol ID mapping)
      SAMPLING_PROTOCOL = "1",
      EXTRACTION_PROTOCOL = "2",
      FRACTIONATION_PROTOCOL = "3",
      ANALYTICAL_PROTOCOL = "4",

      # Comments
      MEASUREMENT_COMMENT = Kommentar,

      .keep = "none"
    )

  # Validate against eData schema ----
  edata_measurements <- initialise_measurements_tibble() |>
    add_row(edata_measurements)

  message(glue(
    "Created measurements table: {nrow(edata_measurements)} measurements"
  ))

  edata_measurements
}
# Unit and operator conversion functions ----

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

# Additional eData conversion functions ----

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
#' - Biota muskelvev → Muscle tissue
#' - Biota plantevev → Plant tissue
#' - Biota egg → Egg
#' - Biota blod → Blood
#' - Biota skuddspiss → Shoot tip
#' - Biota fettvev → Adipose tissue
#' - Biota galle → Bile
#' - Unknown values → "Unknown Tissue"
#'
#' @keywords internal
map_tissue_type <- function(medium_id_name) {
  case_match(
    medium_id_name,
    "Biota bløtdeler" ~ "Total soft tissues",
    "Biota gjeller" ~ "Gills",
    "Biota helkropp" ~ "Whole body",
    "Biota lever" ~ "Liver",
    "Biota muskelvev" ~ "Muscle tissue",
    "Biota plantevev" ~ "Plant tissue",
    "Biota egg" ~ "Egg",
    "Biota blod" ~ "Blood",
    "Biota skuddspiss" ~ "Shoot tip",
    "Biota fettvev" ~ "Adipose tissue",
    "Biota galle" ~ "Bile",
    .default = "Unknown Tissue"
  )
}


#' Create intermediate samples-biota table for eData conversion
#'
#' Generates an intermediate table that combines non-biota samples and biota
#' samples with both eData standardized columns and original Vannmiljø columns.
#' This table is used as input for creating the final eData samples, biota,
#' and measurements tables.
#'
#' @param vm_data Processed Vannmiljø data with resolved compartments
#'   (e.g., vm_sites_split_clean)
#'
#' @return A wide-format tibble containing:
#' - All original Vannmiljø columns (for measurements extraction)
#' - Standardized eData columns (SITE_CODE, SAMPLE_ID, etc.)
#' - Biota-specific columns (SAMPLE_SPECIES, SAMPLE_TISSUE, etc.) where applicable
#'
#' @details
#' Processing steps:
#' 1. Creates base samples table with eData structure for all samples
#' 2. Identifies and processes biota samples separately with species/tissue info
#' 3. Merges biota samples back with base samples
#' 4. Retains all original Vannmiljø columns for downstream use
#'
#' Species corrections:
#' - "Laksesmolt" → "Salmo salar" with lifestage "Juvenile"
#'
#' Compartment inference for biota:
#' - Terrestrial species get ENVIRON_COMPARTMENT_SUB = "Biota, terrestrial"
#'
#' Quality checks:
#' - Reports number of samples with missing species groups
#' - Reports number of samples with missing subcompartments
#' - Reports number of samples with unknown tissue types
#'
#' @export
vm_create_intermediate_samples_biota_table <- function(vm_data) {
  # Create base samples table (all samples, non-biota structure) ----
  edata_samples_wide <- vm_data |>
    filter(
      ENVIRON_COMPARTMENT_resolved != "FLAG: Compartment conflict.",
      ENVIRON_COMPARTMENT_SUB_resolved != "FLAG: Compartment conflict."
    ) |>
    mutate(
      # Core identifiers
      SITE_CODE = glue("Vannmiljø_{Vannlok_kode_split}"),
      SITE_NAME = glue("Vannmiljø Station {Vannlokalitetsnavn}"),
      PARAMETER_NAME = "Copper",
      PARAMETER_TYPE = "Not reported",

      # Compartment information
      ENVIRON_COMPARTMENT = ENVIRON_COMPARTMENT_resolved,
      ENVIRON_COMPARTMENT_SUB = ENVIRON_COMPARTMENT_SUB_resolved,
      MEASURED_CATEGORY = NA_character_,

      SAMPLING_DATE = as.character(SAMPLING_DATE),

      # Sample information
      SUBSAMPLE = "NA",
      SAMPLE_ID = generate_sample_id_with_components(
        SITE_CODE,
        PARAMETER_NAME,
        ENVIRON_COMPARTMENT,
        ENVIRON_COMPARTMENT_SUB,
        SAMPLING_DATE,
        SUBSAMPLE
      )
    )

  message(glue(
    "Created base samples table: {nrow(edata_samples_wide)} samples"
  ))

  # Process biota samples separately ----
  edata_biota <- vm_data |>
    filter(ENVIRON_COMPARTMENT_resolved == "Biota")

  if (nrow(edata_biota) > 0) {
    edata_species <- unique(edata_biota$VitenskapligNavn) |> length()
    message(glue(
      "{nrow(edata_biota)} biota samples found ({edata_species} unique species)"
    ))

    # Check data quality ----
    biota_frequency <- count(
      edata_biota,
      VitenskapligNavn,
      ENVIRON_COMPARTMENT_SUB_resolved
    ) |>
      arrange(desc(n)) |>
      mutate(
        SAMPLE_SPECIES = case_when(
          VitenskapligNavn == "Laksesmolt" ~ "Salmo salar",
          TRUE ~ VitenskapligNavn
        ),
        SAMPLE_SPECIES_LIFESTAGE = case_when(
          VitenskapligNavn == "Laksesmolt" ~ "Juvenile",
          TRUE ~ "Not reported"
        )
      )

    n_missing_species_group <- biota_frequency |>
      filter(is.na(SAMPLE_SPECIES)) |>
      pull(n) |>
      sum()

    n_missing_sub_compartment <- biota_frequency |>
      filter(ENVIRON_COMPARTMENT_SUB_resolved == "*") |>
      pull(n) |>
      sum()

    message(glue(
      "Biota data quality:\n",
      "  - {n_missing_species_group} samples with missing species group\n",
      "  - {n_missing_sub_compartment} samples with missing subcompartment"
    ))

    # Tissue type quality check ----
    tissue_frequency <- count(edata_biota, MediumID_Name) |>
      arrange(desc(n)) |>
      mutate(SAMPLE_TISSUE = map_tissue_type(MediumID_Name))

    n_unknown_tissue <- tissue_frequency |>
      filter(SAMPLE_TISSUE == "Unknown Tissue") |>
      pull(n) |>
      sum()

    message(glue(
      "  - {n_unknown_tissue} samples with unknown tissue type"
    ))

    # Merge species and tissue corrections ----
    edata_biota_merged <- edata_biota |>
      mutate(
        SAMPLE_SPECIES = case_when(
          VitenskapligNavn == "Laksesmolt" ~ "Salmo salar",
          TRUE ~ VitenskapligNavn
        ),
        SAMPLE_SPECIES_LIFESTAGE = case_when(
          VitenskapligNavn == "Laksesmolt" ~ "Juvenile",
          TRUE ~ "Not reported"
        ),
        # Guess compartment from species
        # All species not sampled from something with a Vannkategori are terrestrial
        # TODO: Needs to be somewhere more transparent
        ENVIRON_COMPARTMENT_SUB_resolved = case_when(
          ENVIRON_COMPARTMENT_SUB_resolved !=
            "*" ~ ENVIRON_COMPARTMENT_SUB_resolved,
          SAMPLE_SPECIES %in%
            c(
              "Capreolus capreolus",
              "Felis catus",
              "Hylocomium splendens",
              "Larus argentatus",
              "Lichen",
              "Phoca vitulina",
              "Rattus norvegicus",
              "Somateria mollissima",
              "Turdus pilaris",
              "Vulpes vulpes"
            ) ~ "Biota, terrestrial",
          .default = ENVIRON_COMPARTMENT_SUB_resolved
        ),
        SAMPLE_TISSUE = map_tissue_type(MediumID_Name)
      )

    # Create biota samples with full metadata ----
    vm_samples_biota_only <- edata_biota_merged |>
      mutate(
        # Core identifiers
        SITE_CODE = glue("Vannmiljø_{Vannlok_kode_split}"),
        SITE_NAME = glue("Vannmiljø Station {Vannlokalitetsnavn}"),
        PARAMETER_NAME = "Copper",
        PARAMETER_TYPE = "Not reported",

        # Compartment information
        ENVIRON_COMPARTMENT = ENVIRON_COMPARTMENT_resolved,
        ENVIRON_COMPARTMENT_SUB = ENVIRON_COMPARTMENT_SUB_resolved,
        MEASURED_CATEGORY = NA_character_,

        SAMPLING_DATE = as.character(SAMPLING_DATE),

        # Sample information
        SUBSAMPLE = "NA",
        SAMPLE_ID = generate_sample_id_with_components(
          SITE_CODE,
          PARAMETER_NAME,
          ENVIRON_COMPARTMENT,
          ENVIRON_COMPARTMENT_SUB,
          SAMPLING_DATE,
          SUBSAMPLE
        ),

        # Biota-specific fields
        SPECIES_GROUP = species_group,
        SAMPLE_SPECIES,
        SAMPLE_TISSUE,
        SAMPLE_SPECIES_LIFESTAGE,
        SAMPLE_SPECIES_GENDER = "Not reported",
        BIOTA_COMMENT = "Raw species names from Vannmiljø. May be erroneous."
      )

    # Merge biota and non-biota samples ----
    biota_samples_merged <- edata_samples_wide |>
      bind_rows(vm_samples_biota_only)

    message(glue(
      "Created intermediate table: {nrow(biota_samples_merged)} total samples ",
      "({nrow(edata_samples_wide)} non-biota + {nrow(vm_samples_biota_only)} biota)"
    ))
  } else {
    # No biota samples found
    biota_samples_merged <- edata_samples_wide
    message("No biota samples found in dataset")
  }

  biota_samples_merged
}


#' Create eData samples table from intermediate samples-biota table
#'
#' Extracts and validates the samples portion of the intermediate table,
#' conforming to the eData samples schema.
#'
#' @param vm_intermediate Intermediate samples-biota table from
#'   vm_create_intermediate_samples_biota_table()
#'
#' @return A tibble conforming to eData samples schema
#'
#' @export
vm_create_edata_samples_table <- function(vm_intermediate) {
  edata_samples <- initialise_samples_tibble() |>
    add_row(
      vm_intermediate |>
        select(any_of(names(initialise_samples_tibble())))
    )

  message(glue("Created samples table: {nrow(edata_samples)} samples"))

  edata_samples
}


#' Create eData biota table from intermediate samples-biota table
#'
#' Extracts and validates the biota portion of the intermediate table,
#' conforming to the eData biota schema. Only includes rows where
#' ENVIRON_COMPARTMENT is "Biota".
#'
#' @param vm_intermediate Intermediate samples-biota table from
#'   vm_create_intermediate_samples_biota_table()
#'
#' @return A tibble conforming to eData biota schema
#'
#' @export
vm_create_edata_biota_table <- function(vm_intermediate) {
  edata_biota <- initialise_biota_tibble() |>
    add_row(
      vm_intermediate |>
        filter(ENVIRON_COMPARTMENT == "Biota") |>
        select(any_of(names(initialise_biota_tibble())))
    )

  message(glue("Created biota table: {nrow(edata_biota)} rows"))

  edata_biota
}


#' Create eData measurements table from intermediate samples-biota table
#'
#' Generates a standardized eData measurements table by extracting measurement
#' values, flags, uncertainty, and detection limits from the intermediate table
#' that contains both eData structure and original Vannmiljø columns.
#'
#' @param vm_intermediate Intermediate samples-biota table from
#'   vm_create_intermediate_samples_biota_table()
#' @param campaign_name_short Short campaign identifier
#' @param reference_id Reference ID for the data source
#'
#' @return A tibble conforming to eData measurements schema containing:
#'   SAMPLE_ID, SITE_CODE, PARAMETER_NAME, SAMPLING_DATE, CAMPAIGN_NAME_SHORT,
#'   REFERENCE_ID, MEASURED_VALUE, MEASURED_UNIT, MEASURED_FLAG, LOQ_VALUE,
#'   LOD_VALUE, protocol references, and other measurement metadata
#'
#' @details
#' Measurement fields:
#' - MEASURED_FLAG: Converted from Vannmiljø Operator using vm_convert_operator()
#' - MEASURED_VALUE: Direct from Vannmiljø Verdi
#' - MEASURED_UNIT: Converted from Vannmiljø Unit_Name using vm_convert_unit()
#' - MEASURED_N: Number of measurements (Ant_verdier)
#'
#' Detection limits:
#' - LOQ_VALUE: Quantification limit (Kvantifiseringsgrense)
#' - LOD_VALUE: Detection limit (Deteksjonsgrense)
#'
#' Protocols:
#' - Currently uses placeholder IDs ("1", "2", "3", "4")
#' - TODO: Implement proper protocol ID mapping
#'
#' @export
vm_create_edata_measurements_table <- function(
  vm_intermediate,
  campaign_name_short,
  reference_id
) {
  # Create measurements table ----
  edata_measurements <- vm_intermediate |>
    mutate(
      # Core identifiers
      SITE_CODE,
      PARAMETER_NAME,
      SAMPLING_DATE = as.character(SAMPLING_DATE),
      CAMPAIGN_NAME_SHORT = campaign_name_short,
      REFERENCE_ID = reference_id,

      # Compartment information
      ENVIRON_COMPARTMENT,
      ENVIRON_COMPARTMENT_SUB,

      # Parameter classification
      PARAMETER_TYPE = "Stressor",
      MEASURED_TYPE = "Concentration",

      # Sample information
      SUBSAMPLE,
      SAMPLE_ID,

      # Measurement values
      MEASURED_FLAG = vm_convert_operator(Operator),
      MEASURED_VALUE = Verdi,
      MEASURED_UNIT = vm_convert_unit(Unit_Name),
      MEASURED_N = Ant_verdier,

      # Uncertainty (not reported in Vannmiljø)
      UNCERTAINTY_TYPE = "Not reported",
      UNCERTAINTY_UPPER = NA_real_,
      UNCERTAINTY_LOWER = NA_real_,

      # Detection limits
      LOQ_VALUE = Kvantifiseringsgrense,
      LOQ_UNIT = vm_convert_unit(Unit_Name),
      LOD_VALUE = Deteksjonsgrense,
      LOD_UNIT = vm_convert_unit(Unit_Name),

      # TODO: Fix methods reference
      # Protocol references (FIXME: Need proper protocol ID mapping)
      SAMPLING_PROTOCOL = "1",
      EXTRACTION_PROTOCOL = "2",
      FRACTIONATION_PROTOCOL = "3",
      ANALYTICAL_PROTOCOL = "4",

      # Comments
      MEASUREMENT_COMMENT = Kommentar,

      .keep = "none"
    )

  # Validate against eData schema ----
  edata_measurements <- initialise_measurements_tibble() |>
    add_row(edata_measurements)

  message(glue(
    "Created measurements table: {nrow(edata_measurements)} measurements"
  ))

  edata_measurements
}
