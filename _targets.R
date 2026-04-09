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
  pkgload::load_all(path = here::here())
  message(paste(
    "Loaded local functions in",
    round(Sys.time() - starttime, 2),
    "seconds"
  ))
}

# set pointblank action levels - when do we flag issues as serious
pb_action_levels <- action_levels(warn_at = 1, stop_at = 0.50)

options(
  targets.verbose = TRUE # chatter from targets itself
)


# # Set target options ----
tar_option_set(
  format = "qs"
)

message("Targets has got to the list() part")
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
  )
)
