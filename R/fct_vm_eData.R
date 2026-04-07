# Functions for making eData tables from processed Vm data.

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
#' @importFrom eDataDRF initialise_campaign_tibble
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
    mutate(across(.cols = contains("DATE"), .fns = as.IDate)) |>
    add_row(
      CAMPAIGN_NAME_SHORT = campaign_name_short,
      CAMPAIGN_NAME = campaign_name,
      CAMPAIGN_START_DATE = as.IDate(date_start),
      CAMPAIGN_END_DATE = as.IDate(date_end),
      ORGANISATION = organisation,
      ENTERED_BY = entered_by,
      ENTERED_DATE = as.IDate(Sys.Date()),
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
#' @importFrom eDataDRF initialise_references_tibble
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
    mutate(across(.cols = contains("DATE"), .fns = as.IDate)) |>

    add_row(
      REFERENCE_ID = reference_id,
      REFERENCE_TYPE = "Database",
      DATA_SOURCE = "Vannmiljø",
      AUTHOR = organisation,
      TITLE = "Vannmiljø Database - Copper and Copper Pyrithione Data",
      YEAR = year(date_end),
      ACCESS_DATE = as.IDate(date_end),
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
#' @importFrom eDataDRF initialise_sites_tibble
#' @export
vm_create_edata_sites_table <- function(vm_data, entered_by) {
  # Extract unique sites with relevant metadata
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

  # Format to eData structure
  edata_sites_temp <- vm_sites_unique |>
    mutate(
      SITE_CODE = glue("Vannmiljø_{Vannlok_kode_split}"),
      SITE_NAME = glue("Vannmiljø Station {Vannlokalitetsnavn}"),
      SITE_GEOGRAPHIC_FEATURE = SITE_GEOGRAPHIC_FEATURE_resolved,
      SITE_GEOGRAPHIC_FEATURE_SUB = SITE_GEOGRAPHIC_FEATURE_SUB_resolved,
      COUNTRY_ISO = "Norway",
      OCEAN_IHO = "Not relevant",
      ENTERED_BY = glue("{entered_by} (Vm Conversion)"),
      ENTERED_DATE = as.IDate(today()),
      ALTITUDE_VALUE = 0, # Not used in our analysis, so being ignored.
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

  # Reproject coordinates from UTM33 to WGS84
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
          SITE_COORDINATE_SYSTEM = "WGS 84"
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

  # Finalise sites table
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
          ALTITUDE_VALUE,
          ALTITUDE_UNIT,
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

  # Validate against eData schema
  edata_sites <- initialise_sites_tibble() |>
    mutate(across(.cols = contains("DATE"), .fns = as.IDate)) |>
    add_row(edata_sites)

  message(glue("Created sites table: {nrow(edata_sites)} sites"))

  edata_sites
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
#' @importFrom eDataDRF initialise_parameters_tibble
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
        PARAMETER_TYPE = "Stressor",
        PARAMETER_TYPE_SUB = "Homogeneous metal compounds",
        MEASURED_TYPE = "Concentration",
        PARAMETER_NAME = "Copper",
        PARAMETER_NAME_SUB = NA_character_,
        INCHIKEY_SD = "RYGMFSIKBFXOCR-UHFFFAOYSA-N",
        PUBCHEM_CID = NA_integer_,
        CAS_RN = "7440-50-8",
        ENTERED_BY = glue("{entered_by} from Vannmiljø"),
        PARAMETER_COMMENT = "NO: Kobber"
      )
  }

  message(glue(
    "Created parameters table with {nrow(edata_parameters)} parameters: ",
    "{paste(unique_params, collapse = ', ')}"
  ))
  edata_parameters
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
#' @importFrom eDataDRF initialise_samples_tibble
#' @export
vm_create_edata_samples_table <- function(vm_intermediate) {
  edata_samples <- initialise_samples_tibble() |>
    mutate(across(.cols = contains("DATE"), .fns = as.IDate)) |>

    add_row(
      vm_intermediate |>
        mutate(SUBSAMPLE = as.character(SUBSAMPLE)) |>
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
#' @importFrom eDataDRF initialise_biota_tibble
#' @export
vm_create_edata_biota_table <- function(vm_intermediate) {
  edata_biota <- initialise_biota_tibble() |>
    mutate(across(.cols = contains("DATE"), .fns = as.IDate)) |>
    add_row(
      vm_intermediate |>
        filter(ENVIRON_COMPARTMENT == "Biota") |>
        mutate(SUBSAMPLE = as.character(SUBSAMPLE)) |>
        select(any_of(names(initialise_biota_tibble())))
    )

  message(glue("Created biota table: {nrow(edata_biota)} rows"))

  edata_biota
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
  # Create base samples table (all samples, non-biota structure)
  edata_samples_wide <- vm_data |>
    filter(
      ENVIRON_COMPARTMENT_resolved != "FLAG: Compartment conflict.",
      ENVIRON_COMPARTMENT_SUB_resolved != "FLAG: Compartment conflict.",
      ENVIRON_COMPARTMENT_resolved != "Biota"
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

      SAMPLING_DATE = as.IDate(SAMPLING_DATE)
    ) |>
    # need to group and generate subsample numbers, or else we'll end up with collisions later
    group_by(
      SITE_CODE,
      PARAMETER_NAME,
      ENVIRON_COMPARTMENT,
      ENVIRON_COMPARTMENT_SUB,
      SAMPLING_DATE
    ) |>

    mutate(
      # Sample information
      SUBSAMPLE = row_number(),
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

  # Process biota samples separately
  edata_biota <- vm_data |>
    filter(ENVIRON_COMPARTMENT_resolved == "Biota")

  if (nrow(edata_biota) > 0) {
    edata_species <- unique(edata_biota$VitenskapligNavn) |> length()
    message(glue(
      "{nrow(edata_biota)} biota samples found ({edata_species} unique species)"
    ))

    # Check data quality
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

    # Tissue type quality check
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

    # Merge species and tissue corrections
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

    # Create biota samples with full metadata
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

        SAMPLING_DATE = as.IDate(SAMPLING_DATE)
      ) |>
      # need to group and generate subsample numbers, or else we'll end up with collisions later
      group_by(
        SITE_CODE,
        PARAMETER_NAME,
        ENVIRON_COMPARTMENT,
        ENVIRON_COMPARTMENT_SUB,
        SAMPLING_DATE
      ) |>

      mutate(
        # Sample information
        SUBSAMPLE = row_number(),
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

    # Merge biota and non-biota samples
    biota_samples_merged <- edata_samples_wide |>
      filter(ENVIRON_COMPARTMENT != "Biota") |>
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
#' @importFrom eDataDRF initialise_measurements_tibble
#' @export
vm_create_edata_measurements_table <- function(
  vm_edata_intermediate,
  vm_lookup_methods,
  campaign_name_short,
  reference_id
) {
  # join methods lookup for sampling and analytical protocols
  # TODO: this probably isn't the place for this methods transformation
  vm_edata_intermediate_methods <- vm_edata_intermediate |>
    left_join(
      vm_lookup_methods |> select(ISO_ID, PROTOCOL_ID),
      by = c("Provetakmetode_id" = "ISO_ID")
    ) |>
    rename(SAMPLING_PROTOCOL = PROTOCOL_ID) |>
    left_join(
      vm_lookup_methods |> select(ISO_ID, PROTOCOL_ID),
      by = c("Analysemetode_id" = "ISO_ID")
    ) |>
    rename(ANALYTICAL_PROTOCOL = PROTOCOL_ID)

  edata_measurements <- vm_edata_intermediate |>
    mutate(
      # Core identifiers
      SITE_CODE,
      PARAMETER_NAME,
      SAMPLING_DATE = as.IDate(SAMPLING_DATE),
      CAMPAIGN_NAME_SHORT = campaign_name_short,
      REFERENCE_ID = reference_id,

      # Compartment information
      ENVIRON_COMPARTMENT,
      ENVIRON_COMPARTMENT_SUB,

      # Parameter classification
      PARAMETER_TYPE = "Stressor",
      MEASURED_TYPE = "Concentration",

      # Sample information
      SUBSAMPLE = as.integer(SUBSAMPLE),
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
      # left join to vm_lookup_methods using Provetakmetode_id and Analysmethod
      SAMPLING_PROTOCOL = "1",
      EXTRACTION_PROTOCOL = "2",
      FRACTIONATION_PROTOCOL = "3",
      ANALYTICAL_PROTOCOL = "4",

      # Comments
      MEASUREMENT_COMMENT = Kommentar,

      .keep = "none"
    )

  # Validate against eData schema
  edata_measurements <- initialise_measurements_tibble() |>
    mutate(
      across(.cols = contains("DATE"), .fns = as.IDate)
    ) |>
    add_row(edata_measurements |> mutate(SUBSAMPLE = as.character(SUBSAMPLE)))

  message(glue(
    "Created measurements table: {nrow(edata_measurements)} measurements"
  ))

  edata_measurements
}
