# Created by use_targets().

# TODO: What might be useful.
# If we hit an error
# call tar workspace
# if triggered by pointblank sets, RETURN THE FAILING DATA

# # Load packages required to define the pipeline ----
suppressPackageStartupMessages({
  library(targets)
  library(tarchetypes) # better factories for watching many files
  library(eDataDRF) # schema/vocab functions
  library(crew) # parallel processing, faster execution?
  library(here) # salvage something from the horrible mess that is quarto working directories
  library(devtools) # load all functions
  library(quarto) # make beautiful documents, eventually
  library(pointblank)
  library(dplyr)
})

suppressMessages({
  i_am("Readme.md") # set wd to project root
  load_all(path = here())
})

# set pointblank action levels - when do we flag issues as serious
pb_action_levels <- action_levels(warn_at = 1, stop_at = 0.50)

options(
  targets.verbose = FALSE, # less chatter from targets itself
  pointblank.verbose = FALSE # if this option exists (not sure)
)


# # Set target options ----
tar_option_set(
  # Packages that your targets need for their tasks.
  packages = c(
    "sf",
    "sfhelper",
    "rnaturalearth",
    "rnaturalearthdata",
    "mapproj",
    "rlang",
    "data.table",
    "leaflet",
    "janitor",
    "shiny",
    "readxl",
    "arrow",
    "qs2",
    "tarchetypes", # extend targets
    "glue",
    "purrr",
    "lubridate",
    "stringr",
    "readr",
    "tibble",
    "tidyr",
    "ggplot2",
    "ggspatial",
    "shadowtext",
    "ggrepel",
    "dplyr",
    "dtplyr",
    "forcats",
    "viridis",
    "ggridges",
    "plotly",
    "eDataDRF",
    "pointblank",
    "here"
  ),
  format = "qs" # Optionally set the default storage format. qs is fast.
  #
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
  #
  # controller = crew::crew_controller_local(workers = 10, seconds_idle = 60)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}.
  # The following example is a controller for Sun Grid Engine (SGE).
  #
  #   controller = crew.cluster::crew_controller_sge(
  #     # Number of workers that the pipeline can scale up to:
  #     workers = 10,
  #     # It is recommended to set an idle time so workers can shut themselves
  #     # down if they are not running tasks.
  #     seconds_idle = 120,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.2".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# # Source all custom functions stored in ~/R ----
tar_source()

# # Pipeline ----
list(
  ## # Vannmiljø Data ----

  ### # Raw data ----

  #### # Copper measurements ----
  tar_target(
    vm_raw_copper,
    read_excel(
      path = "data/raw/vannmiljo/Vm_Copper_2025.12.05.xlsx",
      sheet = 1,
      guess_max = 138615
    )
  ),

  #### # Sites (3 files due to export limit) ----
  tar_target(
    vm_raw_sites,
    {
      read_excel(
        "data/raw/vannmiljo/Vm_Copper_Sites_2025.12.05-1.xlsx",
        guess_max = 10000
      ) |>
        add_row(read_excel(
          "data/raw/vannmiljo/Vm_Copper_Sites_2025.12.05-2.xlsx",
          guess_max = 10000
        )) |>
        add_row(read_excel(
          "data/raw/vannmiljo/Vm_Copper_Sites_2025.12.05-3.xlsx",
          guess_max = 10000
        ))
    }
  ),

  ### # Lookup tables ----

  #### # Medium lookup ----
  tar_target(
    vm_lookup_medium,
    read_csv(
      "data/clean/Vm_medium_lookup_matrix_filled.csv",
      guess_max = 100,
      show_col_types = FALSE
    ) |>
      rename_with(
        ~ paste0(., "_medium"),
        c(
          ENVIRON_COMPARTMENT,
          ENVIRON_COMPARTMENT_SUB,
          SPECIES_GROUP,
          SAMPLE_SPECIES,
          SPECIES_GENDER,
          SAMPLE_TISSUE,
          SITE_GEOGRAPHIC_FEATURE,
          SITE_GEOGRAPHIC_FEATURE_SUB
        )
      ) |>
      rename(MediumID_Name = Name, MediumID_Description = Description)
  ),

  #### # Vannkategori lookup ----
  tar_target(
    vm_lookup_vannkategori,
    read_csv("data/clean/vm_sites_codes_lookup.csv", show_col_types = FALSE) |>
      rename_with(
        ~ paste0(., "_vkat"),
        c(
          ENVIRON_COMPARTMENT,
          ENVIRON_COMPARTMENT_SUB,
          SITE_GEOGRAPHIC_FEATURE,
          SITE_GEOGRAPHIC_FEATURE_SUB,
          n
        )
      ) |>
      rename(
        Vannkategori_Name = Name,
        Vannkategori_Description = Description
      )
  ),

  #### # Methods lookup ----
  tar_target(
    vm_lookup_methods,
    {
      read_csv(
        "data/clean/vm_methods_lookup_filled.csv",
        show_col_types = FALSE
      ) |>
        group_by(PROTOCOL_CATEGORY, PROTOCOL_NAME) |>
        mutate(n = row_number()) |>
        mutate(
          CAMPAIGN_NAME = "Vm_2010_2025",
          PROTOCOL_ID = generate_protocol_id(
            PROTOCOL_CATEGORY,
            PROTOCOL_NAME,
            n,
            "Vm_2010_2025"
          )
        )
    }
  ),

  #### # Campaigns lookup ----
  tar_target(
    vm_lookup_campaigns,
    read_csv("data/clean/Vm_lookup_campaigns.csv", show_col_types = FALSE)
  ),

  #### # Units lookup ----
  tar_target(
    vm_lookup_units,
    read_excel("data/raw/vannmiljo/Vannmiljø_Enhet_2025-12-30.xlsx") |>
      rename(Unit_Name = Name, Unit_Description = Description)
  ),

  #### # Species lookup ----
  tar_target(
    vm_lookup_species,
    read_csv("data/clean/Vm_species_lookup.csv", show_col_types = FALSE)
  ),

  ### # Join Vannmiljø data ----

  #### # Join measurements and sites ----
  tar_target(
    vm_join_sites_measurements,
    left_join(
      vm_raw_copper,
      vm_raw_sites,
      by = c("Vannlok_kode" = "Vannlokalitetskode")
    )
  ),

  #### # Join all lookup tables ----
  tar_target(
    vm_join_sites_measurements_lookup,
    {
      vm_join_sites_measurements |>
        left_join(
          vm_lookup_medium,
          by = c(Medium_id = "MediumID")
        ) |>
        left_join(
          vm_lookup_vannkategori,
          by = c(Vannkategori = "VannkategoriID")
        ) |>
        left_join(
          vm_lookup_campaigns,
          by = c(Aktivitet_id = "ActivityID")
        ) |>
        left_join(
          vm_lookup_units,
          by = c(Enhet_id = "UnitID")
        ) |>
        left_join(
          vm_lookup_species,
          by = "VitenskapligNavn"
        ) |>
        mutate(SAMPLING_DATE = as.IDate(Tid_provetak))
    }
  ),

  ### # Filter Vannmiljø data ----

  #### # Filter by compartments ----
  tar_target(
    vm_filtered_compartments,
    vm_filter_compartments(
      vm_join_sites_measurements_lookup,
      compartments = c("Aquatic", "Biota", "*"),
      subcompartments = c(
        "Freshwater",
        "Aquatic Sediment",
        "Marine/Salt Water",
        "Brackish/Transitional Water",
        "Biota, Aquatic",
        "*"
      )
    )
  ),

  #### # Filter by site type ----
  # Remove the only Svalbard site, and any sites that are Polygons rather than points
  tar_target(
    vm_filtered_sites,
    vm_filter_sites(
      vm_filtered_compartments,
      exclude_sites = "Svalbard, ENSB-Kilde 2"
    )
  ),

  #### # Filter by date range ----
  tar_target(
    vm_filtered_dates,
    vm_filter_dates(
      vm_filtered_sites,
      date_start = as.IDate("2010-01-01"),
      date_end = as.IDate("2025-12-05")
    )
  ),

  ### # Resolve conflicts ----

  #### # Resolve compartment conflicts ----
  # Use site, medium and species lookups to "decide" which of
  # our compartments a given sample belongs to
  tar_target(
    vm_compartment_conflicts_resolved,
    resolve_compartment_conflicts(vm_filtered_dates)
  ),

  #### # Remove unresolved compartment conflicts ----
  # Separate step for the sake of reporting
  tar_target(
    vm_compartment_conflicts_resolved_removed,
    vm_compartment_conflicts_resolved |>
      filter(
        ENVIRON_COMPARTMENT_resolved != "FLAG: Compartment conflict.",
        ENVIRON_COMPARTMENT_SUB_resolved != "FLAG: Compartment conflict."
      )
  ),

  #### # Resolve geographic conflicts ----
  tar_target(
    vm_compartment_geo_conflicts_resolved,
    resolve_geographic_conflicts(vm_compartment_conflicts_resolved_removed)
  ),

  #### # Remove unresolved geographic conflicts ----
  # Separate step for the sake of reporting
  tar_target(
    vm_compartment_geo_conflicts_resolved_removed,
    vm_compartment_geo_conflicts_resolved |>
      filter(
        SITE_GEOGRAPHIC_FEATURE_resolved != "FLAG: Geographic conflict.",
        SITE_GEOGRAPHIC_FEATURE_SUB_resolved != "FLAG: Geographic conflict."
      )
  ),

  ### # Split sites with multiple geographic features ----

  #### # Split sites ----
  # Split sites where more than one GEO_FEATURE_SUB is reported
  # (e.g. sediment (benthos) vs water (column))
  tar_target(
    vm_sites_split,
    vm_split_sites(vm_compartment_geo_conflicts_resolved_removed)
  ),

  #### # Clean up split sites ------
  # Remove temporary columns from splitting process
  tar_target(
    vm_sites_split_clean,
    vm_sites_split |>
      select(-n_geo_combos, -geo_combo, -geo_suffix)
  ),

  ### # Convert Vannmiljø to eData Format ----

  #### # Campaign table ----
  tar_target(
    vm_edata_campaign,
    {
      vm_create_edata_campaign_table(
        vm_data = vm_sites_split_clean,
        campaign_name_short = "Vm_2010_2025",
        campaign_name = "Vannmiljø Copper Monitoring 2010-2025",
        date_start = as.IDate("2010-01-01"),
        date_end = as.IDate("2025-12-05"),
        organisation = "Miljødirektoratet",
        entered_by = "Sam Welch"
      ) |>
        mutate(
          source_file = "vannmiljø data transferred directly in _targets.R",
          read_timestamp = as.Date(today())
        )
    }
  ),

  #### # Reference table ----
  tar_target(
    vm_edata_reference,
    vm_create_edata_reference_table(
      vm_data = vm_sites_split_clean,
      reference_id = "VannmiljøCopper2010-2025",
      date_start = as.IDate("2010-01-01"),
      date_end = as.IDate("2025-12-05"),
      organisation = "Miljødirektoratet",
      entered_by = "Sam Welch"
    )
  ),

  #### # Parameters table ----
  tar_target(
    vm_edata_parameters,
    vm_create_edata_parameters_table(
      vm_data = vm_sites_split_clean,
      entered_by = "Sam Welch"
    )
  ),

  #### # Sites table ----
  tar_target(
    vm_edata_sites,
    # we also reproject here, since Vm data is in UT33M be default
    vm_create_edata_sites_table(
      vm_data = vm_sites_split_clean,
      entered_by = "Sam Welch"
    )
  ),

  #### # Methods table ----
  tar_target(vm_edata_methods, {
    # In the case of methods we ended up doing everything more-or-less by hand
    edata_methods <- vm_lookup_methods

    message(glue("Created methods table: {nrow(edata_methods)} protocols"))
  }),

  #### # Intermediate samples-biota table ----
  # This table contains both eData columns AND original Vannmiljø columns
  # It's used to create samples, biota, and measurements tables
  tar_target(
    vm_edata_intermediate,
    vm_create_intermediate_samples_biota_table(vm_data = vm_sites_split_clean)
  ),

  #### # Samples table ----
  tar_target(
    vm_edata_samples,
    vm_create_edata_samples_table(vm_intermediate = vm_edata_intermediate) |>
      # check number of rows hasn't changed
      row_count_match(count = nrow(vm_edata_intermediate))
  ),

  #### # Biota table ----
  tar_target(
    vm_edata_biota,
    vm_create_edata_biota_table(vm_intermediate = vm_edata_intermediate)
  ),

  #### # Measurements table ----
  tar_target(
    # TODO: THis has become very slow since we added grouping. not the end of the world, but how to fix?
    vm_edata_measurements,
    vm_create_edata_measurements_table(
      # TODO: Some cases where LOD/LOQ = ">", we ignore these.
      vm_edata_intermediate = vm_edata_intermediate |> filter(Operator != ">"),
      vm_lookup_methods = vm_lookup_methods,
      campaign_name_short = "Vm_2010_2025",
      reference_id = "VannmiljøCopper2010-2025"
    )
  ),

  ### # Validate eData tables ----

  #### # Run all validations ----
  tar_target(
    vm_edata_validation,
    pb_validate_all_edata_tables(
      campaign = vm_edata_campaign,
      reference = vm_edata_reference,
      parameters = vm_edata_parameters,
      sites = vm_edata_sites,
      samples = vm_edata_samples,
      biota = vm_edata_biota,
      measurements = vm_edata_measurements,
      creed_scores = NULL,
      actions = action_levels(),
      agent = TRUE,
      northern_hemisphere = TRUE
    )
  ),

  #### # Send a warning if something fails
  # Fixme: Seems not to work rn
  tar_target(
    vm_edata_validation_report,
    {
      if (
        !all(map_lgl(vm_edata_validation, .f = \(x) {
          all_passed(x)
        }))
      ) {
        warning("Error(s) in Vannmiljø validation.")
      }
    }
  ),

  #### # CREED Scores table ----
  # doesn't exist yet, haven't worked out how to do it

  ## # Literature Data (eData) ----

  ### # Load file paths ----
  # Create one target for the CSV files in /unzipped associated with each module
  tar_target(
    name = campaign_files,
    command = get_literature_csv_paths(module = "Campaign"),
    format = "file"
  ),

  tar_target(
    name = samples_files,
    command = get_literature_csv_paths(module = "Samples"),
    format = "file"
  ),

  tar_target(
    name = biota_files,
    command = get_literature_csv_paths(module = "Biota"),
    format = "file"
  ),

  tar_target(
    name = compartments_files,
    command = get_literature_csv_paths(module = "Compartments"),
    format = "file"
  ),

  tar_target(
    name = measurements_files,
    command = get_literature_csv_paths(module = "Measurements"),
    format = "file"
  ),

  tar_target(
    name = methods_files,
    command = get_literature_csv_paths(module = "Methods"),
    format = "file"
  ),

  tar_target(
    name = parameters_files,
    command = get_literature_csv_paths(module = "Parameters"),
    format = "file"
  ),

  tar_target(
    name = reference_files,
    command = get_literature_csv_paths(module = "Reference"),
    format = "file"
  ),

  tar_target(
    name = sites_files,
    command = get_literature_csv_paths(module = "Sites"),
    format = "file"
  ),

  # tar_target(
  #   name = creed_data_files,
  #   command = get_literature_csv_paths(module = "CREED_Data"),
  #   format = "file"
  # ),

  tar_target(
    name = creed_scores_files,
    command = get_literature_csv_paths(module = "CREED_Score"),
    format = "file"
  ),

  ### # Read eData by module ----
  # Read in the data for each module, and rbind across studies
  # so we have a single table per module.
  # We use initialise_*_tibble as part of the reading process to check
  # things are formatted how they should be (mostly works, see SAMPLING_DATE).
  # Uses data.table::fread for faster reading.

  #### # Measurements data ----
  # * We load measurements first because it's essentially the central table of
  # * our schema and we validate other tables against it
  tar_target(
    name = measurements_data,
    # some measurement files are missing MEASUREMENT_COMMENT
    # or CAMPAIGN_NAME_SHORT, but that doesn't matter really
    command = {
      fread_all_module_files(
        measurements_files,
        initialise_measurements_tibble
      ) |> # some dates are still messed up
        mutate(
          SAMPLING_DATE = parse_date_time(
            SAMPLING_DATE,
            orders = c("ymd", "dmy")
          ),
          PARAMETER_NAME = "Copper" # and for some reason this is blank somewhere
        ) |>
        standardise_IDate_all() |>
        add_row(vm_edata_measurements) |>
        pb_validate_measurements(agent = FALSE, actions = pb_action_levels)
    }
  ),

  #### # Campaign data ----
  tar_target(
    name = campaign_data,
    command = {
      fread_all_module_files(
        campaign_files,
        initialise_campaign_tibble
      ) |>
        standardise_IDate_all() |>
        add_row(vm_edata_campaign) |>
        pb_validate_campaign(agent = FALSE, actions = pb_action_levels) |>
        # do we have 1+ measurement corresponding to every campaign
        col_vals_in_set_verbose(
          columns = CAMPAIGN_NAME_SHORT,
          set = unique(measurements_data$CAMPAIGN_NAME_SHORT),
          actions = pb_action_levels,
          value_name = "Campaign Name Shorts"
        )
    }
  ),

  #### # Reference data ----
  tar_target(
    name = reference_data,
    command = {
      fread_all_module_files(
        reference_files,
        initialise_references_tibble
      ) |>
        standardise_IDate_all() |>
        add_row(vm_edata_reference) |>
        pb_validate_reference(agent = FALSE, actions = pb_action_levels) |>
        col_vals_in_set_verbose(
          columns = REFERENCE_ID,
          set = unique(measurements_data$REFERENCE_ID),
          actions = pb_action_levels,
          value_name = "Reference IDs"
        )
    }
  ),

  #### # Sites data ----
  tar_target(
    name = sites_data,
    command = {
      fread_all_module_files(sites_files, initialise_sites_tibble) |>
        # some dates are still messed up
        mutate(
          ENTERED_DATE = parse_date_time(ENTERED_DATE, orders = c("ymd", "dmy"))
        ) |>
        standardise_IDate_all() |>
        add_row(vm_edata_sites) |>
        pb_validate_sites(
          agent = FALSE,
          actions = pb_action_levels,
          northern_hemisphere = TRUE
        ) |>
        # do we have 1+ measurement corresponding to every site
        col_vals_in_set_verbose(
          columns = SITE_CODE,
          set = unique(measurements_data$SITE_CODE),
          actions = pb_action_levels,
          value_name = "Site Codes"
        )
    }
  ),

  #### # Parameters data ----
  tar_target(
    name = parameters_data,
    command = {
      # As we only have copper data there's no reason to overcomplicate things
      # and a parameters table with more than one row will cause our
      # joined dataset to grow proportionally
      # fread_all_module_files(
      #   parameters_files,
      #   initialise_parameters_tibble
      # ) |>
      #   standardise_IDate_all() |>
      vm_edata_parameters |>
        pb_validate_parameters(agent = FALSE, actions = pb_action_levels) |>
        row_count_match(count = 1)
    }
  ),

  #### # Compartments data ----
  # We never bothered to generate a Vannmiljø compartments table
  # and it was only ever an intermediate step anyway
  tar_target(
    name = compartments_data,
    command = {
      fread_all_module_files(
        compartments_files,
        initialise_compartments_tibble
      ) |>
        standardise_IDate_all()
    }
  ),

  #### # Methods data ----
  tar_target(
    name = methods_data,
    command = {
      fread_all_module_files(
        methods_files,
        initialise_methods_tibble
      ) |>
        standardise_IDate_all() |>
        add_row(
          vm_lookup_methods |>
            select(-ISO_ID, -n)
        ) |>
        pb_validate_methods(agent = FALSE, actions = pb_action_levels)
    }
  ),

  #### # Validate methods table against measurements table
  tar_target(name = methods_data_validation, command = {
    methods_agent <- create_agent(methods_data)
    # are all our methods used?
    create_agent(measurements_data) |>
      col_vals_make_set(
        brief = "Flag sampling protocols with ID not in methods_data table",
        columns = SAMPLING_PROTOCOL,
        set = pull(
          filter(
            methods_data,
            PROTOCOL_CATEGORY == "Sampling Protocol"
          ),
          PROTOCOL_ID
        ),
        actions = pb_action_levels
      ) |>
      col_vals_make_set(
        brief = "Flag analytical protocols with ID not in methods_data table",
        columns = ANALYTICAL_PROTOCOL,
        set = pull(
          filter(
            methods_data,
            PROTOCOL_CATEGORY == "Analytical Protocol"
          ),
          PROTOCOL_ID
        ),
        actions = pb_action_levels
      ) |>
      col_vals_make_set(
        brief = "Flag fractionation protocols with ID not in methods_data table",
        columns = FRACTIONATION_PROTOCOL,
        set = pull(
          filter(
            methods_data,
            PROTOCOL_CATEGORY == "Fractionation Protocol"
          ),
          PROTOCOL_ID
        ),
        actions = pb_action_levels
      ) |>
      col_vals_make_set(
        brief = "Flag extraction protocols with ID not in methods_data table",
        columns = EXTRACTION_PROTOCOL,
        set = pull(
          filter(
            methods_data,
            PROTOCOL_CATEGORY == "Extraction Protocol"
          ),
          PROTOCOL_ID
        ),
        actions = pb_action_levels
      ) |>
      interrogate()
  }),

  #### # Samples data ----
  tar_target(
    name = samples_data,
    # todo: the column SUBSAMPLE_ID is in initialise_samples_tibble() but not
    # any of our data extracted from the app. fread() warns us that it can't
    # find it in the CSVs, but as this is fine I've chosen to suppress.
    command = {
      suppressWarnings(fread_all_module_files(
        samples_files,
        initialise_samples_tibble
      )) |>
        standardise_IDate_all() |>
        add_row(vm_edata_samples) |>
        pb_validate_samples(agent = FALSE, actions = pb_action_levels)
    }
  ),

  #### # Biota data ----
  tar_target(
    name = biota_data,
    command = {
      fread_all_module_files(biota_files, initialise_biota_tibble) |>
        # some dates are still messed up
        mutate(
          SAMPLING_DATE = parse_date_time(
            SAMPLING_DATE,
            orders = c("ymd", "dmy")
          )
        ) |>
        standardise_IDate_all() |>
        add_row(vm_edata_biota) |>
        pb_validate_biota(agent = FALSE, actions = pb_action_levels)
    }
  ),

  #### # CREED scores data ----
  # FIXME: Enable once we have CREED data
  tar_target(
    name = creed_scores_data,
    command = {
      fread_all_module_files(
        creed_scores_files,
        initialise_CREED_scores_tibble
      ) |>
        pb_validate_creed_scores(agent = FALSE, actions = pb_action_levels)
    }
  ),

  ### # Get biota common names ----
  # Get common names from taxize if there are any new ones. Uses an API call.
  tar_target(
    name = API_biota_common_names,
    command = {
      get_common_names(
        biota_data,
        input_col = "SAMPLE_SPECIES",
        output_col = "SPECIES_COMMON_NAME",
        cache_path = here(
          "data/clean/species_common_names_cache.csv"
        ),
        db = "ncbi",
        verbose = FALSE
      )
    }
  ),

  ### # Validate literature and Vm data before joining it
  #### # Run all validations ----
  tar_target(
    data_validation,
    pb_validate_all_edata_tables(
      campaign = campaign_data,
      reference = reference_data,
      parameters = parameters_data,
      sites = sites_data,
      samples = samples_data,
      biota = API_biota_common_names,
      measurements = measurements_data,
      agent = FALSE,
      actions = pb_action_levels
    )
  ),

  # tar_target(
  #   data_validation_report,
  #   {
  #     if (
  #       !all(map_lgl(data_validation, .f = \(x) {
  #         all_passed(x)
  #       }))
  #     ) {
  #       warning("Error(s) in validation of all data.")
  #     }
  #   }
  # ),

  ### # Join eData into single table ----
  # TODO: extend for creed (which is largely missing)
  tar_target(
    name = literature_joined,
    command = {
      # dataset should be the same number of rows at beginning and end
      # joining can cause row duplication, best to be careful
      target_rows <- nrow(measurements_data)

      join_all_literature_modules(
        measurements_data = measurements_data,
        sites_data = sites_data,
        reference_data = reference_data,
        biota_data = API_biota_common_names, # FIXME: problems
        campaign_data = campaign_data,
        parameters_data = parameters_data,
        methods_data = methods_data
      ) |>
        row_count_match(target_rows)
    }
  ),

  ### # Validate our joined eData

  ### # Clean joined eData ----
  # Currently columns_to_drop is empty, so we don't drop anything...
  tar_target(
    name = literature_clean,
    command = {
      clean_joined_columns(
        data = literature_joined,
        columns_to_drop = c() # Add column names here as you identify them
      ) |>
        # some of our date columns have been reformatted wrongly. let's clean them up
        standardise_IDate_all()
    }
  ),

  ### # Standardise & impute eData ----
  # Create a merged OCEAN/COUNTRY column.
  # Impute values below LOQ or LOD with x / sqrt(2)
  tar_target(
    name = literature_clean_standardised,
    command = {
      literature_clean |>
        standardise_measured_units(
          value_columns = c(
            "MEASURED_VALUE",
            "UNCERTAINTY_UPPER",
            "UNCERTAINTY_LOWER"
          ),
          unit_column = "MEASURED_UNIT",
          remove_other = TRUE # I've used unit = "Other" to mark a few errant rows, so we remove them here.
        ) |>
        standardise_measured_units(
          value_columns = "LOQ_VALUE",
          unit_column = "LOQ_UNIT"
        ) |>
        standardise_measured_units(
          value_columns = "LOD_VALUE",
          unit_column = "LOD_UNIT"
        ) |>
        mutate(
          OCEAN_COUNTRY = merge_country_ocean(
            country = COUNTRY_ISO,
            ocean = OCEAN_IHO
          )
        ) |>
        impute_below_limits(
          measured_col = "MEASURED_VALUE_STANDARD",
          lod_col = "LOD_VALUE_STANDARD",
          loq_col = "LOQ_VALUE_STANDARD",
          impute_fn = function(x) {
            x / sqrt(2) # very basic, rather bad imputation
          },
          output_col = "MEASURED_OR_IMPUTED_VALUE_STANDARD"
        )
    }
  ),

  ### # Load literature parquet ----
  # TODO: I believe something I've done somewhere means that this doesn't
  # properly update. We'll have to come back to it.
  tar_target(
    name = load_literature_pqt,
    command = {
      literature_clean_standardised # add a dependency on save_literature_pqt even though we don't directly read it
    }
  ),

  ### # Data quality report ----
  # Check for missing data. Write a report for the Quarto.
  tar_target(
    name = data_quality_report,
    command = check_data_quality(load_literature_pqt)
  ),

  ## # Geography Data ----

  ### # Prepare WGS84 shapefiles ----
  # Set up WGS84 map shapefiles (oceans, countries), and add annotations
  tar_target(
    name = wgs84_geography,
    command = prepare_geography_wgs84(
      scale = 10,
      destdir = "data/raw/shapefiles/"
    )
  ),

  ### # Prepare polar projection shapefiles ----
  # Set up polar projection map shapefiles (oceans, countries), and add annotations
  tar_target(
    name = polar_geography,
    command = prepare_geography_polar(
      scale = 10,
      destdir = "data/raw/shapefiles/",
      crs = "EPSG:3575"
    )
  ),

  ## # Maps ----

  ### # Create WGS84 map ----
  tar_target(
    name = wgs84_map,
    command = create_study_area_map_wgs84(
      ocean_sf = wgs84_geography$marine_polys,
      country_sf = wgs84_geography$countries,
      arctic_circle_sf = wgs84_geography$arctic_circle,
      graticule_sf = wgs84_geography$graticule,
      suppress_warnings = TRUE
    )
  ),

  ### # Create polar projection map ----
  tar_target(
    name = polar_map,
    command = create_study_area_map_polar(
      ocean_sf = polar_geography$marine_polys,
      country_sf = polar_geography$countries,
      arctic_circle_sf = polar_geography$arctic_circle,
      graticule_sf = polar_geography$graticule,
      suppress_warnings = TRUE
    )
  ),

  ## # Toxicity Thresholds ----

  ### # Copper toxicity thresholds ----
  # TODO: We can add GeoTraces data here, although it may be too precise for our use:
  # https://geotraces.webodv.awi.de/IDP2021_v2%3EGEOTRACES_IDP2021_Seawater_Discrete_Sample_Data_v2/service/DataExtraction
  # In general, our big study area and many study compartments mean there's
  # loads of values we can use here. Self-restraint is probably wise.
  tar_target(
    name = copper_toxicity_thresholds,
    command = generate_copper_thresholds()
  ),

  ## # Quarto Reports ----

  ### # Index ----
  tar_quarto(
    name = render_index.qmd,
    path = "./index.qmd",
    quiet = FALSE, # generally we only need the first file complaining if something goes wrong
    extra_files = "_quarto.yml" # watch quarto.yml so we rebuild the full quarto output if it changes
  ),

  ### # Notebooks ----

  #### # QC notebook ----
  tar_quarto(
    name = render_nb01_pipeline,
    path = "./docs/NB01-pipeline.qmd",
    quiet = FALSE
  ),

  #### # Vannmiljø notebook ----
  tar_quarto(
    name = render_nb02_vannmiljo,
    path = "docs/NB02-vannmiljo.qmd",
    quiet = FALSE
  ),

  #### # Vannmiljø QC notebook ----
  tar_quarto(
    name = render_nb02_vannmiljo_qc,
    path = "docs/NB02-vannmiljo-qc.qmd",
    quiet = FALSE
  ),

  #### # Visualisation notebook ----
  tar_quarto(
    name = render_nb03_qc,
    path = "docs/NB03-qc.qmd",
    quiet = FALSE
  ),

  tar_quarto(
    name = render_nb03_visualisation,
    path = "docs/NB03-visualisation.qmd",
    quiet = FALSE
  ),

  #### # Map notebook ----
  tar_quarto(
    name = render_nb04_map,
    path = "docs/NB04-map.qmd",
    quiet = FALSE
  ),

  #### # Network notebook ----
  tar_quarto(
    name = render_nb05_network,
    path = "docs/NB05-network.qmd",
    quiet = FALSE
  ),

  #### # Weight of Evidence notebook ----
  tar_quarto(
    name = render_nb06_woe,
    path = "docs/NB06-WoE.qmd",
    quiet = FALSE
  ),

  #### # Emissions notebook ----
  tar_quarto(
    name = render_nb07_emissions,
    path = "docs/NB07-emissions.qmd",
    quiet = FALSE
  ),

  #### # Ecology notebook ----
  tar_quarto(
    name = render_nb08_ecology,
    path = "docs/NB08-ecology.qmd",
    quiet = FALSE
  ),

  ### # Appendices ----

  #### # Review protocol appendix ----
  tar_quarto(
    name = render_ap01_protocol,
    path = "docs/AP01-review-protocol.qmd",
    quiet = FALSE
  ),

  #### # Acknowledgements appendix ----
  tar_quarto(
    name = render_ap02_acknowledgements,
    path = "docs/AP02-acknowledgements.qmd",
    quiet = FALSE
  ),

  ## # Deployment ----

  ### # Publish to Posit Connect Cloud ----
  tar_target(
    name = deploy_posit_connect_cloud,
    command = {
      quarto_publish_site(
        server = "connect.posit.cloud",
        account = "sawelch-niva",
        render = "none"
      )

      # Create a deployment marker file with timestamp
      marker_file <- "_targets/user/data/deploy_timestamp.txt"
      dir.create(dirname(marker_file), showWarnings = FALSE, recursive = TRUE)
      writeLines(as.character(Sys.time()), marker_file)
      marker_file

      # Dependencies to trigger redeployment
      render_index.qmd
      render_nb01_pipeline
      render_nb02_vannmiljo
      render_nb02_vannmiljo_qc
      render_nb03_visualisation
      render_nb04_map
      render_nb05_network
      render_nb07_emissions
      render_nb08_ecology
    },
    format = "file"
  )

  # TODO: Are we allowed (statistically) to group similar compartments together?
  # i.e., if we do a t-test (or something) are our populations significantly different
  # do we need to do some sort of multi-factorial doodah

  # TODO: We should also do something with MEASURED_N vs actual replication. Hmm
)
