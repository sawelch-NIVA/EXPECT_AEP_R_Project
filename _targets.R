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
    workers = 1,
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
      "data/clean/lookups/Vm_medium_lookup_matrix_filled.csv",
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
    read_csv(
      "data/clean/lookups/vm_sites_codes_lookup.csv",
      show_col_types = FALSE
    ) |>
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
        "data/clean/lookups/vm_methods_lookup_filled.csv",
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
    read_csv(
      "data/clean/lookups/Vm_lookup_campaigns.csv",
      show_col_types = FALSE
    )
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
    read_csv("data/clean/lookups/Vm_species_lookup.csv", show_col_types = FALSE)
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
          "data/clean/lookups/species_common_names_cache.csv"
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
  #
  # This is also where every measurement gets its `row_id` (2026-08-06), because
  # it is the hub: everything downstream inherits the column, and nothing
  # upstream needs it. Lower case on purpose -- SCREAMING_SNAKE in this project
  # means "column of the eData schema", and this is an administrative key of
  # ours that is not in the schema.
  #
  # It is NOT a sequential counter. See the header of R/fct_row_ids.R for why
  # that was rejected: positional ids shift under insertion, which would let a
  # hand-edited correction silently retarget a different measurement.
  tar_target(
    name = load_literature_pqt,
    command = {
      add_row_ids(
        literature_clean_standardised # add a dependency on save_literature_pqt even though we don't directly read it
      )
    }
  ),

  # Companion report: rows whose SAMPLE_ID was shared with another and had to be
  # broken apart by SUBSAMPLE. Non-empty means a data-entry defect in the source
  # extraction, so it is surfaced rather than absorbed quietly. Same pattern as
  # literature_dropped_report and unit_anomaly_report.
  tar_target(
    name = row_id_collisions,
    command = {
      coll <- report_row_id_collisions(load_literature_pqt)
      report_row_id_status(coll)
      coll
    }
  ),

  ### # Vannmiljø dataset summary (manuscript Methods) ----
  # Descriptive summary of the Vannmiljø contribution for index.qmd's
  # "Materials & Methods > Vannmiljø" section. Nothing downstream depends on
  # these; they exist as targets (not a script like summarise_ssb_employment.R)
  # because they read pipeline data and render_index picks up the dependency
  # from the tar_read() calls in the notebook (CLAUDE.md 4.4.2). See
  # R/fct_vm_summary.R.
  tar_target(
    name = vm_dataset_summary,
    command = summarise_vm_dataset(load_literature_pqt)
  ),
  # Cleaning funnel: row count after each Vannmiljø filter step. Fed the
  # intermediate vm_* targets rather than the joined data, so it lives in its
  # own target. Rendered into the SI / NB02, referenced from the manuscript.
  tar_target(
    name = vm_cleaning_funnel_table,
    command = vm_cleaning_funnel(c(
      raw = nrow(vm_raw_copper),
      compartments = nrow(vm_filtered_compartments),
      sites = nrow(vm_filtered_sites),
      dates = nrow(vm_filtered_dates),
      compartment_conflicts = nrow(vm_compartment_conflicts_resolved_removed),
      geographic_conflicts = nrow(
        vm_compartment_geo_conflicts_resolved_removed
      ),
      analysis = nrow(vm_sites_split_clean)
    ))
  ),

  ### # Unit corrections ----
  # PLAN.md 9b. Overriding measured values that arrived wrong from the source,
  # from a hand-edited CSV. The pipeline reads it and never writes it, same
  # contract as group_decisions.csv and aep_nodes.csv.
  #
  # WHY HERE. Above the hygiene step so a correction is applied before anything
  # is dropped or summarised, and below literature_clean_standardised so that
  # OUR conversions and THEIR errors stay separate concerns. Correcting further
  # down (in a notebook, or per AEP node) would leave the triage panels, the
  # summary table and group_decisions.csv all showing the uncorrected numbers
  # while the AEP showed the corrected ones, which is the worse failure.
  #
  # format = "file" is load-bearing: as a literal path string, targets would
  # hash the command and never the file, so editing the CSV would invalidate
  # nothing. Exactly the fault found in group_decisions (PLAN.md 9b), and it
  # would be far worse here.
  tar_target(
    name = unit_corrections_file,
    command = here_rel("data/clean/decisions/unit_corrections.csv"),
    format = "file"
  ),

  tar_target(
    name = unit_corrections,
    command = read_unit_corrections(unit_corrections_file)
  ),

  # Aborts on a stale correction, on one whose recorded row_ids no longer match
  # its selector, and on any row matched twice. See R/fct_unit_corrections.R for
  # why both a selector and a row id list are required.
  tar_target(
    name = literature_corrected,
    command = apply_unit_corrections(
      load_literature_pqt,
      unit_corrections,
      ids = group_ids
    )
  ),

  # Announced on every build. Overriding a national monitoring database should
  # not become invisible through familiarity.
  tar_target(
    name = unit_correction_report,
    command = {
      rep <- report_unit_corrections(literature_corrected, group_ids)
      report_unit_correction_status(rep)
      rep
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
    command = drop_nonpositive_measurements(literature_corrected)
  ),

  # Companion report: what the filter above removed, per group, worst first.
  # Reads the *unfiltered* data on purpose -- it needs the rows that
  # literature_analysis_ready throws away. Corrected, though, so its per-group
  # loss counts describe the same numbers everything else downstream sees.
  # Check this before letting any heavily-censored group become an AEP node.
  tar_target(
    name = literature_dropped_report,
    command = report_dropped_measurements(literature_corrected)
  ),

  ### # Calculate a summary table per group
  # - group by all categoricals, remove wet weight
  # - calculate two outlier flags, dip-test for departure from unimodality
  # - TODO: Weighted means
  #
  # Ranking and flag derivation are bolted on by add_triage_flags() at the end
  # (PLAN.md P1.4), so this reframe() stays a plain set of per-group statistics
  # and the interpretation lives somewhere testable.
  #
  # The aggregation moved to summarise_groups() (R/fct_summarise_groups.R) on
  # 2026-08-28 so docs/NBXX-rfjord-2.qmd can re-run the same logic on rows
  # trimmed to the A002 bounding box. Behaviour here is unchanged; the grouping,
  # outlier and dip-test comments now live with the function.
  tar_target(
    name = summarise_literature_data,
    command = summarise_groups(
      literature_analysis_ready,
      literature_dropped_report
    )
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
      #
      # Updated to composite form 2026-08-08 (scripts/migrate_group_ids_to_composite.R):
      # G033 -> G033-Ba-Cnr-A-nod-Sti-Mw, G036 -> G036-Ba-Cnr-F-ves-Sti-Mw,
      # G047 -> G047-Bf-Cnr-G-mor-Mus-Mw.
      must_include = c(
        "G033-Ba-Cnr-A-nod-Sti-Mw",
        "G036-Ba-Cnr-F-ves-Sti-Mw",
        "G047-Bf-Cnr-G-mor-Mus-Mw",
        "G043-Bf-Oot-G-mor-Mus-Md"
      )
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
      thresholds = copper_toxicity_thresholds,
      # Band labels carry their stable group ids, so a band on the panel can be
      # read back to its section in docs/groups/. Defined below; targets resolves
      # by name, not by position in the list.
      ids = group_ids
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
    command = here_rel("data/clean/decisions/group_ids.csv"),
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
    command = here_rel("data/clean/decisions/group_decisions.csv"),
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
    command = here_rel("data/clean/aep/aep_nodes.csv"),
    format = "file"
  ),
  tar_target(
    name = aep_nodes,
    command = read_aep_nodes(aep_nodes_file)
  ),
  tar_target(
    name = aep_node_members_file,
    command = here_rel("data/clean/aep/aep_node_members.csv"),
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

  ### # The AEP manifest and membership ----
  # PLAN.md P5.3, added 2026-08-06. Several AEPs over ONE pool of nodes rather
  # than a node set per AEP: a node carries four EPEQ scores and four written
  # justifications, and copying those per AEP multiplies Sam's judgement work by
  # the number of AEPs and then lets the copies drift. See the header of
  # R/fct_aep_manifest.R for the full argument.
  #
  # What varies per AEP is membership, layout (x/y on the membership file) and
  # SCOPE: a bounding box and date range on the manifest, intersected with each
  # node's own restrictions.
  tar_target(
    name = aep_manifest_file,
    command = here_rel("data/clean/aep/aep_manifest.csv"),
    format = "file"
  ),
  tar_target(
    name = aep_manifest,
    command = read_aep_manifest(aep_manifest_file)
  ),
  # One flat file per AEP since 2026-08-27 (aep_membership_<aep_id>.csv). The
  # list.files() command re-runs each build (cheap) and returns identical sorted
  # paths when nothing changed; format = "file" hashes every path, so adding or
  # removing an AEP's file invalidates aep_membership and the AEP subtree.
  tar_target(
    name = aep_membership_file,
    command = sort(list.files(
      here_rel("data/clean/aep"),
      pattern = "^aep_membership_.*\\.csv$",
      full.names = TRUE
    )),
    format = "file"
  ),
  tar_target(
    name = aep_membership,
    command = read_aep_membership(
      paths = aep_membership_file,
      nodes = aep_nodes,
      manifest = aep_manifest
    )
  ),

  ### # One scoped node table per AEP ----
  # The single place the loop over AEPs lives. Everything downstream takes an
  # ordinary nodes table and does not know AEPs exist.
  tar_target(
    name = aep_scoped,
    command = aep_scoped_nodes(aep_nodes, aep_membership, aep_manifest)
  ),

  ### # Node grouping boxes ----
  # Sam 2026-08-05: "Having everything say 'coastal' at the start is clearly a
  # bit silly." A shared property repeated in five labels needs somewhere else to
  # live. Groups nest and may overlap, so membership is many-to-many and lives in
  # a semicolon list per group. Nesting depth is DERIVED from containment rather
  # than declared, so it cannot drift out of step with the member lists.
  tar_target(
    name = aep_node_groups_file,
    command = here_rel("data/clean/aep/aep_node_groups.csv"),
    format = "file"
  ),
  tar_target(
    name = aep_node_groups,
    command = read_aep_node_groups(aep_node_groups_file, nodes = aep_nodes)
  ),

  ### # Node report cards ----
  # PLAN.md P3.1. One row per node: the compact summary a node has to carry.
  # Arctic coverage is REPORTED, not filtered (Sam's call 2026-08-05); a global
  # 66.5 cut would drop 81% of measurements and leave the marine node on 258.
  #
  # One row per node PER AEP since 2026-08-06: the same node resolves to
  # different data under different scopes, so `aep_id` is part of the key.
  tar_target(
    name = aep_node_cards,
    command = aep_all_report_cards(
      aep_scoped,
      aep_node_members,
      literature_analysis_ready,
      group_ids
    )
  ),

  ### # Shared value limits, computed ONCE across the whole node pool ----
  # Not per AEP. Limits derived from one AEP's nodes are that AEP's, and the
  # same node drawn on two different axes cannot be compared between them, which
  # is the whole reason a spatially restricted AEP is interesting.
  tar_target(
    name = aep_card_limits,
    command = node_card_limits(
      aep_nodes,
      aep_node_members,
      literature_analysis_ready,
      group_ids
    )
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

  ### # AEP edges ----
  # PLAN.md Phase 4. Scaffolding is scripts/scaffold_aep_edges.R, which proposes
  # every downward flow between placed nodes as `putative` and never removes
  # anything. Marking one `empirical` is a positive act requiring a citation.
  tar_target(
    name = aep_edges_file,
    command = here_rel("data/clean/aep/aep_edges.csv"),
    format = "file"
  ),
  tar_target(
    name = aep_edges,
    command = {
      e <- read_aep_edges(aep_edges_file, nodes = aep_nodes)
      validate_aep_edges(e, aep_nodes)
      e
    }
  ),

  ### # REACH sector data for the AEP source nodes ----
  # Added 2026-08-11, pulled out of docs/NBXX-REACH.qmd (see the header of
  # R/fct_reach.R). N004-N011's external_value/sd/n on aep_nodes.csv were typed
  # in by hand from reach_node_summary_data and stay hand-typed (Sam, per PLAN.md:
  # source nodes stay manual for now) -- what THIS chain buys is the per-year
  # series behind that headline figure, so node_cards can draw it rather
  # than the plain "no measured data" placeholder every other external node gets.
  #
  # NOT a group-id ledger. Sam considered and explicitly deferred giving REACH
  # sectors their own persisted ids analogous to the G-codes in fct_group_ids.R
  # (2026-08-11: "not now"); reach_node_sectors() is a named-vector lookup, not
  # an allocated ledger, and the two are not meant to look alike.
  tar_target(
    name = reach_prtd_file,
    command = here_rel("inst/extdata/emissions/REACH_copper_prtd.xlsx"),
    format = "file"
  ),
  tar_target(
    name = reach_sector_years,
    command = read_reach_sector_years(reach_prtd_file)
  ),
  tar_target(
    name = reach_node_sectors_data,
    command = reach_node_sectors(reach_sector_years)
  ),
  tar_target(
    name = reach_node_summary_data,
    command = reach_node_summary(reach_node_sectors_data)
  ),
  tar_target(
    name = reach_external_series_data,
    command = reach_external_series(reach_node_sectors_data)
  ),

  ### # PRTR & REACH: Hammerfest manuscript figure ----
  # index.qmd's "Norwegian PRTR and REACH Product Register" section. One
  # two-panel figure: (a) REACH net copper (tonnes in COMMERCE) scaled to
  # Hammerfest by employment share, (b) PRTR copper RELEASED (kg). Different
  # quantities, one figure. See R/fct_reach_hammerfest.R,
  # R/fct_prtr_hammerfest.R, R/fct_hammerfest_emissions.R.
  #
  # This partly un-parks the emissions work (PLAN.md 10). Only these
  # descriptive figures; the WoE assessments and source-node values stay
  # hand-transcribed.
  #
  # ssb_employment_hammerfest_sections.csv is written by
  # scripts/summarise_ssb_employment.R, NOT the pipeline (SSB is script-only,
  # same contract as summarise_prtr_emissions.R). Re-run that script before
  # tar_make if the SSB figures move; the format = "file" target below will
  # then invalidate the weighting.
  tar_target(
    name = ssb_employment_sections_file,
    command = here_rel(
      "data/clean/derived/ssb_employment_hammerfest_sections.csv"
    ),
    format = "file"
  ),
  tar_target(
    name = ssb_employment_sections,
    command = read_ssb_section_shares(ssb_employment_sections_file)
  ),
  tar_target(
    name = reach_hammerfest_weighted,
    command = weight_reach_to_hammerfest(
      reach_sector_years,
      ssb_employment_sections
    )
  ),
  tar_target(
    name = prtr_emissions_dir,
    command = here_rel("inst/extdata/emissions"),
    format = "file"
  ),
  tar_target(
    name = prtr_long,
    command = read_prtr_long(dir = prtr_emissions_dir)
  ),
  tar_target(
    name = prtr_hammerfest_series_data,
    command = prtr_hammerfest_series(prtr_long)
  ),
  tar_target(
    name = hammerfest_emissions_plot,
    command = write_hammerfest_emissions_panel(
      reach_hammerfest_weighted,
      prtr_hammerfest_series_data,
      here_rel("figures/fig04-hammerfest-emissions.png")
    ),
    format = "file"
  ),

  ### # Node report cards ----
  # PLAN.md 4.3, P3.2, P5.2. One PNG per node into images/node_cards/,
  # as a `format = "file"` target so the store caches the images rather than the
  # ggplot objects (CLAUDE.md 4.4).
  #
  # Badges render grey where a node is unscored, which is the honest state until
  # aep_nodes.csv is filled in: grey means "not assessed" and is deliberately
  # distinct from the colour for a score of 1.
  #
  # One SUBDIRECTORY PER AEP since 2026-08-06. A node appears in several AEPs
  # with different data behind it, so `N001.png` is not a unique name.
  tar_target(
    name = node_cards,
    command = write_aep_node_cards(
      scoped = aep_scoped,
      cards = aep_node_cards,
      members = aep_node_members,
      data = literature_analysis_ready,
      ids = group_ids,
      thresholds = copper_toxicity_thresholds,
      dir = here_rel("images/node_cards"),
      limits = aep_card_limits,
      external_series = reach_external_series_data
    ),
    format = "file"
  ),

  ### # Edge report cards ----
  # One compact card per non-rejected edge, per AEP, into
  # images/edge_cards/<aep_id>/. Based on the node card (write_node_cards())
  # but smaller: no distribution panel, no level-coloured background, a blank
  # line between the quantity and the counts. Putative vs empirical is carried
  # by the edge line style and by the card existing at all, so it is not
  # written on the card itself. See R/fct_edge_cards.R.
  tar_target(
    name = aep_edge_cards,
    command = write_aep_edge_cards(
      scoped = aep_scoped,
      edges = aep_edges,
      dir = here_rel("images/edge_cards")
    ),
    format = "file"
  ),

  ### # AEP diagrams: auto-assembly PARKED 2026-08-27 ----
  # Sam is assembling the AEP figures by hand from the node and edge card
  # images, so aep_diagrams and aep_diagrams_bare are no longer built. The
  # machinery stays available: write_aep_diagrams() and plot_aep() in
  # R/fct_aep_edges.R + R/fct_aep_nodes.R are unchanged, and re-adding a
  # tar_target() here brings it back. aep_scoped / aep_edges / aep_node_cards /
  # aep_node_groups / node_cards all still build, since the hand
  # assembly draws on the same cards.

  ### # Source unit errors ----
  # Added 2026-08-05, after three separate 1000x faults surfaced in one day from
  # the same ug/g-is-mg/kg misconception. Two of them were ours and are fixed;
  # this one is in data we do not control, so it can only be detected, not
  # prevented. See R/fct_unit_anomalies.R.
  #
  # REPORTS, NEVER CORRECTS. Rewriting a measured value on the strength of a
  # free-text comment is a scientific judgement. That judgement now has a home
  # of its own in data/clean/decisions/unit_corrections.csv; this target stays a detector.
  #
  # It reads literature_analysis_ready, which since 2026-08-06 sits DOWNSTREAM of
  # the corrections. That is deliberate and it changes what this target means: it
  # is now a shrinking to-do list rather than a static record. A group still
  # flagged here after a correction has been written means the correction was
  # insufficient, not that it is missing. The permanent record of what was
  # resolved, and why, lives in the corrections file.
  tar_target(
    name = unit_anomaly_report,
    command = {
      comments <- scan_comment_unit_flags(literature_analysis_ready)
      offsets <- scan_group_scale_offsets(literature_analysis_ready)
      report_unit_anomalies(comments, offsets)
      list(comments = comments, offsets = offsets)
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

  ## # Manuscript figures ----

  ### # Per-AEP matrix time series ----
  # Copper in cod / mussel / coastal water / sediment inside each AEP box, over
  # time, native units, one free y-axis per compartment. Water and sediment
  # carry their M-608 class (four classes -- copper skips M-608 III --
  # consistent with fig05-repparfjorden-concentrations); biota sit on a separate
  # above/below-PROREF scale. format = "file" per CLAUDE.md 4.4: the target
  # caches the PNG, not the ggplot. Embedded in _03-results.qmd's per-AEP
  # subsections, so render_index depends on both via a tar_read() there.
  tar_target(
    name = aep_matrix_timeseries_a001,
    command = write_aep_matrix_timeseries(
      "A001", literature_analysis_ready, copper_toxicity_thresholds,
      group_ids, aep_manifest
    ),
    format = "file"
  ),
  tar_target(
    name = aep_matrix_timeseries_a002,
    command = write_aep_matrix_timeseries(
      "A002", literature_analysis_ready, copper_toxicity_thresholds,
      group_ids, aep_manifest
    ),
    format = "file"
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

  ### # Figure-source notebook, upstream of the manuscript ----
  # docs/NBXX-rfjord-2.qmd is not really a site page: it exists to (re)draw the
  # study-area and Repparfjorden concentration maps the manuscript embeds
  # (figures/fig02-study-area.png, figures/fig05-repparfjorden-concentrations.png). It
  # tar_read()s aep_manifest and the literature targets, so editing a bounding
  # box now redraws these figures on tar_make() rather than needing a hand
  # `quarto render`. render_index is chained AFTER it by a hidden
  # tar_read(render_nbxx_rfjord) in _03-results.qmd (CLAUDE.md 4.4.2). Also listed
  # in _quarto.yml's `project: render:`, or a project build would skip it.
  # Note: it pulls Kartverket basemap tiles over the network, so it is slower and
  # less hermetic than the other render targets.
  tar_quarto(
    name = render_nbxx_rfjord,
    path = "docs/NBXX-rfjord-2.qmd",
    quiet = FALSE
  ),

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
  ),

  # How units are handled, how unit errors are found, and how to write a
  # correction. Built rather than parked because it reads unit_corrections.csv
  # and the anomaly report live: a stale copy of a document that calls itself
  # the source of truth is worse than not having one.
  #
  # Its tar_read() calls ARE its dependency declaration (CLAUDE.md 4.4.2), so
  # writing a correction rebuilds it automatically.
  tar_quarto(
    name = render_ap04_units,
    path = "docs/AP04-unit-corrections.qmd",
    quiet = FALSE
  ),

  # How the AEP is assembled from the six data/clean/aep/ CSVs. Built rather
  # than parked for the same reason as AP04: it reads those files live, and a
  # stale guide to a hand-edited schema is worse than none.
  #
  # Every table in it goes through tar_read() rather than readr, so its
  # dependency declaration (CLAUDE.md 4.4.2) covers all six files and editing
  # any of them rebuilds the page.
  tar_quarto(
    name = render_ap05_aep,
    path = "docs/AP05-aep.qmd",
    quiet = FALSE
  )

  # TODO (Phase 2): add render_nbxx_triage for docs/NBXX-Triage.qmd once the
  # triage contact sheet exists.

  # TODO: Are we allowed (statistically) to group similar compartments together?
  # i.e., if we do a t-test (or something) are our populations significantly different
  # do we need to do some sort of multi-factorial doodah

  # TODO: We should also do something with MEASURED_N vs actual replication. Hmm
)
