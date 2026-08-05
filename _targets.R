# Created by use_targets().

# # Load packages required to define the pipeline ----
# options(conflicts.policy = "strict") # error on load in case of any function name masking

{
  starttime <- Sys.time()

  library(targets, quietly = TRUE)
  library(tarchetypes) # better factories for watching many files
  library(qs2)
  library(eDataDRF) # schema/vocab functions
  library(crew) # parallel processing, faster execution?
  library(here) # salvage something from the horrible mess that is quarto working directories
  library(quarto) # make beautiful documents, eventually
  library(pointblank) # validation functions (TODO: Remove)
  suppressPackageStartupMessages(
    library(
      dplyr,
      mask.ok = c("filter", "lag", "intersect", "setdiff", "setequal", "union") # allow dplyr to mask functions we never use from base/stats
    )
  )
  library(testthat, mask.ok = c("test_file"))
  library(
    lubridate,
    mask.ok = c("as.difftime", "date", "intersect", "setdiff", "union")
  )
  library(
    data.table,
    # TODO: All these?
    mask.ok = c(
      "hour",
      "isoweek",
      "isoyear",
      "mday",
      "minute",
      "month",
      "quarter",
      "second",
      "wday",
      "week",
      "yday",
      "year",
      "between",
      "first",
      "last"
    )
  )
  library(purrr, mask.ok = c("transpose"))
  library(readr)
  library(readxl)
  message(paste(
    "Loaded packages in",
    round(Sys.time() - starttime, 2),
    "seconds"
  ))
}


{
  starttime <- Sys.time()
  here::i_am("Readme.md") # set wd to project root
  # Deliberately here::here(), not here_rel(): here_rel() is a STOPAEP function
  # and does not exist until this load_all() call has finished. This path is also
  # never recorded in tar_meta, so absoluteness costs nothing here. Every here()
  # call that DOES reach the store went to here_rel() on 2026-07-31; see
  # R/fct_paths.R for why.
  pkgload::load_all(path = here::here())
  message(paste(
    "Loaded local functions in",
    round(Sys.time() - starttime, 2),
    "seconds"
  ))
}

# set pointblank action levels - when do we flag issues as serious
pb_action_levels <- action_levels(warn_at = 1, stop_at = NULL)

options(
  targets.verbose = TRUE # chatter from targets itself
)

# NOTE: the per-group outlier tar_map() factory that used to live here was
# removed 2026-07-29 (PLAN.md P0.2), together with the 14 generated
# docs/NBXX-Distributions-*.qmd notebooks and their generator script. It is
# replaced by the triage layer (PLAN.md Phase 1). The underlying statistics
# survive in R/fct_outlier_detection.R and R/fct_statistics.R, and the group
# enumeration helpers in R/fct_outlier_groups.R.

# # Set target options ----
tar_option_set(
  format = "qs",
  workspace_on_error = TRUE,
  # Functions live in the STOPAEP package namespace (pkgload::load_all above),
  # not the global environment. Without this, targets does not hash them, so
  # editing any R/fct_*.R function invalidates NOTHING and tar_make() happily
  # reuses stale results. This is very likely the cause of the long-standing
  # "load_literature_pqt doesn't properly update" note.
  # Verified 2026-07-29: before this line, changing sample_triage_groups()
  # left tar_outdated() reporting "(none)".
  imports = "STOPAEP",
  # STOPAEP is attached by load_all() but never installed, so a crew worker
  # cannot library() it. Workers get it from .Rprofile instead. targets.qmd.
  packages = setdiff(.packages(), "STOPAEP"),
  # Drop target objects from memory once nothing needs them, and gc() between
  # targets. Costs some re-reading from the store; buys headroom in the main
  # process, which is the thing that has actually run out. targets.qmd section 4.
  memory = "transient",
  garbage_collection = TRUE,
  # Workers are capped by RAM, not by the 22 cores: 22 cores but 15.5 GB, often
  # ~3 GB free. Each worker is a full R process (~0.5 GB of package stack) that
  # is also sent a 65 MB copy of literature_analysis_ready per branch, and
  # geom_smooth() allocates heavily on top of that. workers = 6 exhausted memory
  # mid-branch; 3 completed but coexisted badly with an open Positron session.
  # targets.qmd section 4.
  controller = crew_controller_local(
    name = "local",
    # 2 beats 3 here, measured, even after slicing cut the per-worker footprint.
    # At 3 the aggregate branch time inflates from 117 s to 197 s: the workers
    # contend for memory rather than for cores. Do not raise this without
    # re-measuring aggregate branch seconds, not just wall clock.
    workers = 2,
    seconds_idle = 60
  )
)

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
      vm_create_edata_campaigns_table(
        vm_data = vm_sites_split_clean,
        campaign_prefix_short = "Vm_2010_2025",
        campaign_prefix = "Vannmiljø Copper Monitoring 2010-2025",
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
  # TODO: THis is also very slow. How fix?
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
  ####################################################################
  #### # Measurements table ----
  tar_target(
    # TODO: THis has become very slow since we added grouping. not the end of the world, but how to fix?
    vm_edata_measurements,
    vm_create_edata_measurements_table(
      # TODO: Some cases where LOD/LOQ = ">", we ignore these.
      vm_edata_intermediate = vm_edata_intermediate |> filter(Operator != ">"),
      vm_lookup_methods = vm_lookup_methods,
      campaign_prefix_short = "Vm_2010_2025",
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
  # tar_target(
  #   packages = c("purrr"),
  #   vm_edata_validation_report,
  #   {
  #     if (
  #       !all(map_lgl(vm_edata_validation, .f = \(x) {
  #         all_passed(x)
  #       }))
  #     ) {
  #       warning("Error(s) in Vannmiljø validation.")
  #     }
  #   }
  # ),

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
    packages = c("lubridate", "purrr"),
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
        col_vals_in_set(
          columns = CAMPAIGN_NAME_SHORT,
          set = unique(measurements_data$CAMPAIGN_NAME_SHORT),
          actions = pb_action_levels
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
        col_vals_in_set(
          columns = REFERENCE_ID,
          set = unique(measurements_data$REFERENCE_ID),
          actions = pb_action_levels
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
        col_vals_in_set(
          columns = SITE_CODE,
          set = unique(measurements_data$SITE_CODE),
          actions = pb_action_levels
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
        row_count_match(count = 1) # check table has one row
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
    packages = c("rlang"),
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
        # FIXME: this fails because none of our sample IDs pass regex... oops
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
        pb_validate_CREED_scores(agent = FALSE, actions = pb_action_levels)
    }
  ),

  ### # Get biota common names ----
  # English common names, so a reader has some intuition for whether a value is
  # plausible for the organism. Cached to CSV, so the APIs are hit once per
  # species and only for species not already there.
  #
  # WoRMS first, NCBI second (changed 2026-07-30). NCBI only carries common names
  # for taxa it happens to have annotated, which left 55 of 126 species unnamed
  # including Fucus vesiculosus and Littorina littorea. WoRMS is the authoritative
  # register for marine species and this dataset is overwhelmingly marine; adding
  # it took coverage from 71 to 94 of 128 species in about 12 seconds.
  #
  # The cache is hand-editable and a species already in it is never re-queried,
  # so a name that reads badly can simply be corrected in the CSV.
  tar_target(
    name = API_biota_common_names,
    command = {
      get_common_names(
        biota_data,
        input_col = "SAMPLE_SPECIES",
        output_col = "SPECIES_COMMON_NAME",
        cache_path = here_rel(
          "data/clean/species_common_names_cache.csv"
        ),
        dbs = c("worms", "ncbi"),
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

  ### # Analysis-ready data ----
  # The single data-hygiene step between the loaded data and the analysis:
  # drop rows whose measured value is NA, zero, or negative. See
  # R/fct_analysis_ready.R for the reasoning on each case (short version: a
  # stored 0 is a non-detect that lost its censoring flag, and 0/negative
  # values break the log10 scales used throughout the distribution plots).
  #
  # Filters on MEASURED_VALUE_STANDARD only -- deliberately NOT a whole-row
  # drop_na(), which would gut the dataset since many eData columns are
  # legitimately sparse.
  tar_target(
    name = literature_analysis_ready,
    command = drop_nonpositive_measurements(load_literature_pqt)
  ),

  # Companion report: what the filter above removed, per group, worst first.
  # Reads the *unfiltered* data on purpose -- it needs the rows that
  # literature_analysis_ready throws away. Check this before letting any
  # heavily-censored group become an AEP node.
  tar_target(
    name = literature_dropped_report,
    command = report_dropped_measurements(load_literature_pqt)
  ),

  ### # Calculate a summary table per group
  # - group by all categoricals, remove wet weight
  # - calculate two outlier flags, dip-test for departure from unimodality
  # - TODO: Weighted means
  #
  # Ranking and flag derivation are bolted on by add_triage_flags() at the end
  # (PLAN.md P1.4), so this reframe() stays a plain set of per-group statistics
  # and the interpretation lives somewhere testable.
  tar_target(
    name = summarise_literature_data,
    command = {
      literature_analysis_ready |>
        group_by(
          ENVIRON_COMPARTMENT,
          ENVIRON_COMPARTMENT_SUB,
          SPECIES_GROUP,
          SAMPLE_SPECIES,
          SAMPLE_TISSUE,
          SITE_GEOGRAPHIC_FEATURE,
          SITE_GEOGRAPHIC_FEATURE_SUB,
          # we split by unit type for summary
          MEASURED_UNIT_STANDARD
        ) |>
        # NA/zero/negative measured values are now dropped upstream by
        # literature_analysis_ready, so the filter that used to sit here is
        # redundant. Left as a comment because its removal is the reason this
        # target's results may shift slightly on the next rebuild.
        # REPLACED an inline copy of the outlier logic with the shared function,
        # 2026-08-05. Two things were wrong with the copy beyond the duplication:
        #
        # 1. Its RMZ ran on the RAW scale while its Tukey fences ran on log10,
        #    which made the RMZ criterion an upper-tail test in practice. See
        #    flag_outliers() for the measurement. The plots called flag_outliers()
        #    and this target did not, so moving one without the other would have
        #    left the summary table disagreeing with the panels it ranks.
        # 2. It was UNGATED, computing flags for groups of any size, while the
        #    dip test below is gated at dip_test_safe()'s min_n. So a group of
        #    four could be flagged for outliers but never tested for modality.
        #    flag_outliers() applies the same min_n = 10, so the two flags now
        #    abstain together and "untested" means the same thing for both.
        mutate(
          flag_outliers(MEASURED_VALUE_STANDARD)
        ) |>
        reframe(
          n = sum(MEASURED_N),
          n_sources = length(unique(REFERENCE_ID)),
          date_min = suppressWarnings(min(SAMPLING_DATE, na.rm = TRUE)),
          date_max = suppressWarnings(max(SAMPLING_DATE, na.rm = TRUE)),
          sd = sd(MEASURED_VALUE_STANDARD, na.rm = TRUE),
          mean = mean(MEASURED_VALUE_STANDARD, na.rm = TRUE),
          # Geometric mean and geometric SD, added 2026-08-04 on Sam's call:
          # "GSD is a reversal, you're right. but it clearly makes more sense
          # than SD of non-normal data."
          #
          # These concentrations are log-normal over many orders of magnitude, so
          # the arithmetic mean sits above almost every observation and the
          # arithmetic sd is dominated by the largest value. GSD is a
          # MULTIPLICATIVE factor: 3 means roughly threefold either side of the
          # geometric mean, and that is the sentence the methods section needs.
          #
          # log10 throughout, matching every plot axis in the project.
          # literature_analysis_ready has already dropped zeros and negatives, so
          # the logs are all finite.
          geo_mean = 10^mean(log10(MEASURED_VALUE_STANDARD), na.rm = TRUE),
          gsd = 10^sd(log10(MEASURED_VALUE_STANDARD), na.rm = TRUE),
          # FIXED 2026-07-30 (PLAN.md P1.5). This was sum(outlier_RMZ &
          # outlier_IQR), a count of *rows*, while n is sum(MEASURED_N), a count
          # of *measurements*. The ratio therefore divided a row count by a
          # measurement count and systematically under-fired wherever
          # MEASURED_N > 1. Sam's call: weight the outlier count by MEASURED_N so
          # numerator and denominator are the same quantity.
          #
          # na.rm because flag_outliers() returns NA flags where the group is
          # below min_n or the MAD is zero, and a single NA would otherwise blank
          # the whole group's count. Untested rows therefore count as
          # non-outliers, which is the conservative direction.
          n_double_outliers = sum(
            (outlier_RMZ & outlier_IQR) * MEASURED_N,
            na.rm = TRUE
          ),
          # The old row-count version, kept alongside so the two are comparable
          # and the change is auditable rather than silent.
          n_outlier_rows = sum(outlier_RMZ & outlier_IQR, na.rm = TRUE),
          median = median(MEASURED_VALUE_STANDARD),
          unit = unique(MEASURED_UNIT_STANDARD),
          # Constant within a group by construction: the group key includes
          # SAMPLE_SPECIES and the common name is a function of the species.
          # Carried through so the triage notebook can print it as an
          # aide-memoire under each heading.
          species_common_name = SPECIES_COMMON_NAME[1],
          # Hartigan's dip test for unimodality (NA below dip_test_safe()'s min_n)
          dip_p = dip_test_safe(MEASURED_VALUE_STANDARD)$dip_p,
          multimodal = dip_test_safe(MEASURED_VALUE_STANDARD)$multimodal
        ) |>
        add_triage_flags(literature_dropped_report)
    }
  ),

  ### # Sample-groups display table ----
  # Shared by index.qmd and docs/NBXX-Sample-Groups.qmd. The reshaping lives
  # here so the two documents cannot drift apart again; presentation lives in
  # sample_groups_flextable(). index.qmd filters this to the large groups.
  tar_target(
    name = sample_groups_table,
    command = build_sample_groups_table(summarise_literature_data, group_ids)
  ),

  ## # Group Triage ----
  # No longer a pilot as of 2026-07-30: n_sample = Inf takes ALL groups with
  # n >= 100, which is the PLAN.md P1.3 cutoff. At 25 sampled, two eligible
  # groups had no panels, which would have left holes in the Phase 2 contact
  # sheet with nothing to indicate they were missing rather than absent.
  #
  # The `triage_pilot_*` target names are vestigial. Renaming them would churn
  # the notebook's tar_read() calls for no gain; the plot filenames come from
  # group slugs, not target names.
  tar_target(
    name = triage_pilot_groups,
    command = sample_triage_groups(
      summary_data = summarise_literature_data,
      data = literature_analysis_ready,
      min_n = 100,
      n_sample = Inf,
      seed = 20260729, # irrelevant at Inf, kept so lowering n_sample stays stable
      ids = group_ids,
      # Named because they matter for a reason unrelated to size, not because
      # they are nearly big enough. Both algae groups: Ascophyllum nodosum
      # (Egg wrack, n = 70) and Fucus vesiculosus (Bladder wrack, n = 68).
      # Algae is one of the systems PLAN.md P3.5 names, and docs/NBXX-algae.qmd
      # is the prototype the project is modelled on, so deciding it without
      # panels would be deciding it blind.
      #
      # Lowering min_n to 68 instead would admit seven unrelated groups, because
      # eight others are interleaved between 68 and 99. Add ids here as P3.5
      # picks its systems.
      must_include = c("G033", "G036")
    )
  ),

  # Shared value-axis limits, computed once over the whole dataset so panels
  # are comparable within a group and across groups. Grouped by compartment;
  # see compute_triage_scale_limits() for what that does and does not buy you.
  tar_target(
    name = triage_scale_limits,
    command = compute_triage_scale_limits(literature_analysis_ready)
  ),

  # One element per group, each carrying its own group row and only the rows
  # that group's panels can need. Unbranched on purpose: slicing inside a
  # branched target would send the whole 65 MB table to every branch, which is
  # exactly the cost this removes. 1881 MB of serialisation per full run becomes
  # 243 MB. targets.qmd section 3.
  tar_target(
    name = triage_group_slices,
    command = split_triage_data(literature_analysis_ready, triage_pilot_groups),
    iteration = "list",
    # Overrides the global memory = "transient". This object is ~243 MB in
    # memory and is the dependency of all 29 branches, so transient made the
    # main process drop and re-read it once per branch, which serialised
    # dispatch and cost more than the parallelism gained. targets.qmd section 4.
    memory = "persistent"
  ),

  # One branch per group, so groups spread across crew workers and a changed
  # plot function redraws only the groups it touches.
  #
  # format = "file" so targets caches the PNGs themselves. Never return the
  # ggplots: they capture their input data and redraw at print time anyway.
  # Output dir has no leading underscore: Quarto skips underscore-prefixed
  # directories as project resources, which would break linked images.
  tar_target(
    name = triage_pilot_plots,
    # Calls the singular _for_group() function directly. Each branch is one
    # (grp, data) pair, so there is no groups table to map over and no second
    # target to zip against. targets.qmd section 3 for why zipping was avoided.
    command = write_triage_plots_for_group(
      data = triage_group_slices$data,
      grp = triage_group_slices$grp,
      dir = "triage",
      scale_limits = triage_scale_limits,
      # Reference lines. Borrowed across compartments, species and tissues on
      # purpose; read the header of R/fct_threshold_match.R before reading
      # anything into them.
      thresholds = copper_toxicity_thresholds
    ),
    pattern = map(triage_group_slices),
    format = "file"
  ),

  ### # Parent-level overviews ----
  # Panels f and g, one level up from the five per-group panels above. The
  # per-group panels cannot answer "should these groups be one group", because
  # each is drawn strictly inside one group key and the comparison is in the
  # panel next door. These sit at the sub-compartment and compare the levels
  # below it, which is the view the lump/split decision actually needs.
  #
  # One node per compartment x sub-compartment x UNIT. The unit is part of the
  # node because sub-compartments genuinely split on it, and a mixed panel would
  # show a units artefact three orders of magnitude wide that reads as a real
  # biological split.
  #
  # min_n matches triage_pilot_groups above, deliberately, and the groups
  # themselves are passed so a node with no *displayed* group beneath it is
  # dropped. The notebook only opens a heading for groups it is showing, and an
  # overview is emitted when its heading is emitted, so an unreachable node is a
  # PNG written and never referenced. Clearing min_n is not enough on its own:
  # a node's total sums over groups that may each be well under the bar.
  tar_target(
    name = triage_overview_node_table,
    command = triage_overview_nodes(
      literature_analysis_ready,
      min_n = 100,
      groups = triage_pilot_groups
    )
  ),

  # Same file-target treatment as triage_pilot_plots, and the same shared axis,
  # so an overview and the per-group panels beneath it can be read together.
  tar_target(
    name = triage_overview_plots,
    command = write_triage_overviews(
      data = literature_analysis_ready,
      nodes = triage_overview_node_table,
      dir = "triage",
      scale_limits = triage_scale_limits,
      thresholds = copper_toxicity_thresholds
    ),
    format = "file"
  ),

  ### # By-species panels ----
  # The innermost overview tier: one panel per species group, bands being
  # species x tissue. Tissue is in the band rather than pooled because it moves
  # the value further than species does (Fish / mg/kg (wet): median 5.20 in
  # liver against 0.228 in muscle), so pooling would dress tissue variation up
  # as species variation.
  #
  # Adding this tier is also why the sub-compartment overview stops at
  # SPECIES_GROUP; see triage_overview_stop_cols().
  tar_target(
    name = triage_species_node_table,
    command = triage_species_nodes(
      literature_analysis_ready,
      min_n = 100,
      groups = triage_pilot_groups
    )
  ),

  tar_target(
    name = triage_species_plots,
    command = write_species_overviews(
      data = literature_analysis_ready,
      nodes = triage_species_node_table,
      dir = "triage",
      scale_limits = triage_scale_limits,
      thresholds = copper_toxicity_thresholds
    ),
    format = "file"
  ),

  ## # Group identity ----
  # Stable accession numbers, read from a hand-allocated ledger. Allocation is
  # scripts/allocate_group_ids.R, run by hand: the ledger is the authority for
  # what a reference means, and a target that rewrote it could silently re-point
  # IDs already written into notes. Read the header of R/fct_group_ids.R for why
  # anything rank-derived would be unstable.
  # The PATH is its own `format = "file"` target so that editing the ledger
  # invalidates everything downstream. See the note on group_decisions_file
  # below: this was not tracked at all until 2026-08-05.
  tar_target(
    name = group_ids_file,
    command = here_rel("data/clean/group_ids.csv"),
    format = "file"
  ),
  tar_target(
    name = group_ids,
    command = read_group_ids(group_ids_file)
  ),

  ## # Grouping decisions ----
  # PLAN.md P2.2. The pipeline READS this file and never writes it. Scaffolding
  # and refreshing is scripts/scaffold_group_decisions.R, run by hand: writing a
  # hand-edited file from a target is how an afternoon of judgement gets silently
  # overwritten by a rebuild.
  #
  # `summarise_literature_data` is passed so read_group_decisions() can warn when
  # groups exist in the data but not in the file, which is how a stale decisions
  # file gets caught after new data arrives.
  #
  # Nothing downstream of this runs until the decisions exist. That is deliberate:
  # it stops the pipeline generating work that has not been asked for.
  #
  # THE PATH IS A `format = "file"` TARGET, and that is load-bearing. Found
  # 2026-08-05: the command took the path as a literal string, so targets hashed
  # the *command* and never the file. Editing group_decisions.csv by hand, which
  # is the entire decision workflow this file exists for, invalidated NOTHING.
  # Confirmed directly: six rows were appended to the csv and `tar_outdated()`
  # reported `(none)`.
  #
  # Same class of fault as the missing `imports = "STOPAEP"` (PLAN.md P1.1e), and
  # the same symptom: work that appears to have been done and silently was not.
  # It would have bitten hardest during Phase 2, where a day of judgement goes
  # into this file and every downstream figure needs to see it.
  tar_target(
    name = group_decisions_file,
    command = here_rel("data/clean/group_decisions.csv"),
    format = "file"
  ),
  tar_target(
    name = group_decisions,
    command = read_group_decisions(
      path = group_decisions_file,
      summary_data = summarise_literature_data
    )
  ),

  ## # AEP nodes ----
  # PLAN.md P3.1-P3.4, added 2026-08-05. Two hand-edited files, read and never
  # written, on the same contract as group_decisions.csv. Scaffolding is
  # scripts/scaffold_aep_nodes.R.
  #
  # A NODE IS NOT A SAMPLING GROUP. It may be one group, several, or a restricted
  # slice of either; docs/NBXX-algae.qmd defines its marine node with a latitude
  # cut that is not in the group key at all. Membership lives in
  # aep_node_members.csv and the restrictions are fixed columns on
  # aep_nodes.csv, rather than a filter expression in a cell that could not be
  # validated. See the header of R/fct_aep_nodes.R.
  #
  # Both paths are `format = "file"` from the outset, having watched
  # group_decisions.csv go untracked for a week (see above).
  tar_target(
    name = aep_nodes_file,
    command = here_rel("data/clean/aep_nodes.csv"),
    format = "file"
  ),
  tar_target(
    name = aep_nodes,
    command = read_aep_nodes(aep_nodes_file)
  ),
  tar_target(
    name = aep_node_members_file,
    command = here_rel("data/clean/aep_node_members.csv"),
    format = "file"
  ),
  tar_target(
    name = aep_node_members,
    command = read_aep_node_members(
      path = aep_node_members_file,
      nodes = aep_nodes,
      ids = group_ids
    )
  ),

  ### # Node report cards ----
  # PLAN.md P3.1. One row per node: the compact summary a node has to carry.
  # Arctic coverage is REPORTED, not filtered (Sam's call 2026-08-05); a global
  # 66.5 cut would drop 81% of measurements and leave the marine node on 258.
  tar_target(
    name = aep_node_cards,
    command = {
      cards <- aep_node_report_cards(
        aep_nodes,
        aep_node_members,
        literature_analysis_ready,
        group_ids
      )
      validate_aep_nodes(aep_nodes, aep_node_members, cards)
      cards
    }
  ),

  ### # Node coverage backlog ----
  # What no node has claimed yet, ranked by measurements. This is the complement
  # to Sam abandoning sequential notebook review on 2026-08-05: picking groups of
  # interest needs a visible record of what was not picked, so stopping is a
  # choice rather than an oversight.
  tar_target(
    name = aep_node_coverage,
    command = node_coverage(
      aep_node_members,
      summarise_literature_data,
      group_ids,
      decisions = group_decisions
    )
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
    packages = c("sf"),
    name = wgs84_geography,
    command = prepare_geography_wgs84(
      scale = 10,
      destdir = "data/raw/shapefiles/"
    )
  ),

  ### # Prepare polar projection shapefiles ----
  # Set up polar projection map shapefiles (oceans, countries), and add annotations
  tar_target(
    packages = c("sf"),
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
    packages = c("sf", "shadowtext"),
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
    packages = c("sf", "shadowtext"),
    name = polar_map,
    command = create_study_area_map_polar(
      ocean_sf = polar_geography$marine_polys,
      country_sf = polar_geography$countries,
      arctic_circle_sf = polar_geography$arctic_circle,
      graticule_sf = polar_geography$graticule,
      suppress_warnings = TRUE
    )
  ),

  # Outlier tar_map factory removed 2026-07-29 (PLAN.md P0.2); see note at top.

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
  # Trimmed 2026-07-29 (PLAN.md P0.2 / Phase 0). Quarto rendering is by far the
  # slowest part of this pipeline, so only documents under active work get a
  # render target. Everything else is parked: the .qmd files still exist and can
  # be rendered by hand, but the pipeline no longer rebuilds them, and
  # `project: render:` in _quarto.yml excludes them from `quarto render` too.
  #
  # Parked: NB01-08, AP01-03, NBXX-{Outliers,REACH,algae,fish,norske-utslipp,
  # reparfjorden}, _journals. (_planning.qmd was deleted as obsolete; PLAN.md
  # at the repo root replaces it.)
  #
  # To un-park one: add it back here AND to the render list in _quarto.yml.

  ### # Manuscript (html for review, docx for sharing) ----
  tar_quarto(
    name = render_index,
    path = "./index.qmd",
    quiet = FALSE, # generally we only need the first file complaining if something goes wrong
    extra_files = "_quarto.yml" # watch quarto.yml so we rebuild if it changes
  ),

  ### # Active working notebooks ----
  # The group summary table driving the Phase 2 grouping decisions.
  tar_quarto(
    name = render_nbxx_sample_groups,
    path = "docs/NBXX-Sample-Groups.qmd",
    quiet = FALSE
  )

  # TODO (Phase 2): add render_nbxx_triage for docs/NBXX-Triage.qmd once the
  # triage contact sheet exists.

  # TODO: Are we allowed (statistically) to group similar compartments together?
  # i.e., if we do a t-test (or something) are our populations significantly different
  # do we need to do some sort of multi-factorial doodah

  # TODO: We should also do something with MEASURED_N vs actual replication. Hmm
)
