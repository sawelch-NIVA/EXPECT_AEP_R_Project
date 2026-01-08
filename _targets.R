# Created by use_targets().

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # better factories for watching many files
library(crew) # parallel processing, faster execution?
library(here) # salvage something from the horrible mess that is quarto working directories
library(devtools) # load all functions
library(quarto) # make beautiful documents, eventually

i_am("Readme.md") # set wd to project root
load_all(path = here())

# Set target options:
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
    "plotly"
  ),
  format = "qs", # Optionally set the default storage format. qs is fast.
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
  # controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
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

# Source all custom functions stored in ~/R
tar_source()


list(
  # # Load raw Vannmiljø data ----
  # * These point to specific files, rather than the whole unzipped folder.
  # * If at some point you update the vannmilø data, you'll need to also
  # * update the file paths.
  ## # Raw data - Vannmiljø copper measurements ----
  tar_target(
    vm_raw_copper,
    read_excel(
      path = "data/raw/vannmiljo/Vm_Copper_2025.12.05.xlsx",
      sheet = 1,
      guess_max = 138615
    )
  ),

  ## # Raw data - Vannmiljø sites (3 files due to export limit) ----
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

  ## # Vannmiljø Lookup tables ----
  tar_target(
    vm_lookup_medium,
    read_csv(
      "data/clean/Vm_medium_lookup_matrix_filled.csv",
      guess_max = 100,
      show_col_types = FALSE
    )
  ),

  tar_target(
    vm_lookup_vannkategori,
    read_csv("data/clean/vm_sites_codes_lookup.csv", show_col_types = FALSE)
  ),

  tar_target(
    vm_lookup_analysis,
    read_excel("data/raw/vannmiljo/Vannmiljø_Analysemetode_2025-12-15.xlsx")
  ),

  tar_target(
    vm_lookup_sampling,
    read_excel(
      "data/raw/vannmiljo/Vannmiljø_Prøvetakingsmetode_2025-12-15.xlsx"
    )
  ),

  tar_target(
    vm_lookup_methods_raw,
    read_csv("data/clean/vm_methods_lookup_filled.csv", show_col_types = FALSE)
  ),

  tar_target(
    vm_lookup_campaigns,
    read_csv("data/clean/Vm_lookup_campaigns.csv", show_col_types = FALSE)
  ),

  tar_target(
    vm_lookup_units,
    read_excel("data/raw/vannmiljo/Vannmiljø_Enhet_2025-12-30.xlsx")
  ),

  tar_target(
    vm_lookup_species,
    read_csv("data/clean/Vm_species_lookup.csv", show_col_types = FALSE)
  ),

  # # Join Vannmiljø data -----

  # join measurements and sites together
  tar_target(
    vm_join_sites_measurements,
    (left_join(
      vm_raw_copper,
      vm_raw_sites,
      by = c("Vannlok_kode" = "Vannlokalitetskode")
    ))
  ),

  # Joining all the lookup tables is a mess but has to be done.
  tar_target(
    vm_join_sites_measurements_lookup,
    ({
      vm_join_sites_measurements |>
        left_join(
          # fix me
          # this doesn't work because we load stuff in targets
          # but then modify it in NB02.qmd. What do?
          # * We could also ask ourselves the question:
          # * Why bother putting lookups in targets?
          # Then again, it's probably good practice.
          vm_lookup_medium |>
            rename(MediumID_Name = Name, MediumID_Description = Description),
          by = c(Medium_id = "MediumID")
        ) |>
        left_join(
          vm_lookup_vannkategori |>
            rename(
              Vannkategori_Name = Name,
              Vannkategori_Description = Description
            ),
          by = c(Vannkategori = "VannkategoriID")
        ) |>
        left_join(
          vm_lookup_campaigns,
          by = c(Aktivitet_id = "ActivityID")
        ) |>
        left_join(
          vm_lookup_units |>
            rename(Unit_Name = Name, Unit_Description = Description),
          by = c(Enhet_id = "UnitID")
        ) |>
        left_join(
          vm_lookup_species,
          by = "VitenskapligNavn"
        )
    })
  ),

  # # Load eData files ----
  # # Create one target for the CSV files in /unzipped associated with each module
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

  # # Read eData by module ----
  # # Read in the data for each module, and rbind across studies it so we have a single table per module
  # We use initialise_*_tibble as part of the reading process to check things are formatted how they should be
  # (It (mostly) works, see SAMPLING_DATE below)
  # uses data.table::fread for faster reading
  tar_target(
    name = campaign_data,
    command = fread_all_module_files(
      campaign_files,
      initialise_campaign_tibble
    ) |>
      standardise_IDate_all()
  ),
  tar_target(
    name = samples_data,
    command = fread_all_module_files(
      samples_files,
      initialise_samples_tibble
    ) |>
      standardise_IDate_all()
  ),
  tar_target(
    name = biota_data,
    command = fread_all_module_files(biota_files, initialise_biota_tibble) |>
      standardise_IDate_all()
  ),
  tar_target(
    name = compartments_data,
    command = fread_all_module_files(
      compartments_files,
      initialise_compartments_tibble
    ) |>
      standardise_IDate_all()
  ),
  tar_target(
    name = measurements_data,
    command = fread_all_module_files(
      measurements_files,
      initialise_measurements_tibble
    ) |>
      standardise_IDate_all()
  ),
  tar_target(
    name = methods_data,
    command = fread_all_module_files(
      methods_files,
      initialise_methods_tibble
    ) |>
      standardise_IDate_all()
  ),
  tar_target(
    name = parameters_data,
    command = fread_all_module_files(
      parameters_files,
      initialise_parameters_tibble
    ) |>
      standardise_IDate_all()
  ),
  tar_target(
    name = reference_data,
    command = fread_all_module_files(
      reference_files,
      initialise_references_tibble
    ) |>
      standardise_IDate_all()
  ),
  tar_target(
    name = sites_data,
    command = fread_all_module_files(sites_files, initialise_sites_tibble) |>
      standardise_IDate_all()
  ),

  # FIXME: Enable once we have CREED data
  tar_target(
    name = creed_scores_data,
    command = fread_all_module_files(
      creed_scores_files,
      initialise_CREED_scores_tibble
    )
  ),

  ## # Join eData into single table ----
  # TODO: extend for creed (which is largely missing)
  tar_target(
    name = literature_joined,
    command = join_all_literature_modules(
      measurements_data = measurements_data,
      sites_data = sites_data,
      reference_data = reference_data,
      biota_data = biota_data,
      campaign_data = campaign_data,
      parameters_data = parameters_data,
      methods_data = methods_data
    )
  ),

  ## # Clean joined eData ----
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

  ## # Standardise & impute eData ----
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

  # TODO: I believe something I've done somewhere means that this doesn't properly update. We'll have to come back to it.
  tar_target(
    name = load_literature_pqt,
    command = {
      literature_clean_standardised # add a dependency on save_literature_pqt even though we don't directly read it
      # load_literature_parquet(
      #   input_path = "data/clean",
      #   filename = "literature_data.parquet"
      # )
    }
  ),

  # # Check for missing data. Write a report for the Quarto.
  tar_target(
    name = data_quality_report,
    command = check_data_quality(load_literature_pqt)
  ),

  # Geography data preparation targets ----

  # # Set up WGS84 map shapefiles (oceans, countries), and add annotations
  tar_target(
    name = wgs84_geography,
    command = prepare_geography_wgs84(
      scale = 10,
      destdir = "data/raw/shapefiles/"
    )
  ),

  # # Set up polar projection map shapefiles (oceans, countries), and add annotations
  tar_target(
    name = polar_geography,
    command = prepare_geography_polar(
      scale = 10,
      destdir = "data/raw/shapefiles/",
      crs = "EPSG:3575"
    )
  ),

  # # Map creation  ----

  ## # Create WGS84 map ----
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

  ## # Create Polar projection map ----
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

  # # Toxicity/safety threshholds!
  # TODO: We can add GeoTraces data here, although it may be too precise for our use:
  # https://geotraces.webodv.awi.de/IDP2021_v2%3EGEOTRACES_IDP2021_Seawater_Discrete_Sample_Data_v2/service/DataExtraction
  # In general, the our big study area and many study compartments mean there's loads of values we can use here. Self-restrain is probably wise.
  tar_target(
    name = copper_toxicity_thresholds,
    command = generate_copper_thresholds()
  ),

  # # Generate Quarto Files ----
  tar_quarto(
    name = index.qmd,
    path = "./index.qmd",
    quiet = FALSE, # generally we only need the first file complaining if something goes wrong
    extra_files = "_quarto.yml" # watch quarto.yml so we rebuild the full quarto output if it changes
  ),

  ## # Generate Quarto Notebooks ----
  # QC notebook
  tar_quarto(
    name = nb01_qc,
    path = "./docs/NB01-qc.qmd",
    quiet = TRUE
  ),

  # Vannmiljo notebook
  tar_quarto(
    name = nb02_vannmiljo,
    path = "docs/NB02-vannmiljo.qmd",
    quiet = TRUE
  ),

  # Visualisation notebook
  tar_quarto(
    name = nb03_visualisation,
    path = "docs/NB03-visualisation.qmd",
    quiet = TRUE
  ),

  # Map notebook
  tar_quarto(
    name = nb04_map,
    path = "docs/NB04-map.qmd",
    quiet = TRUE
  ),

  # Network notebook
  tar_quarto(
    name = nb05_network,
    path = "docs/NB05-network.qmd",
    quiet = TRUE
  ),

  # Emissions notebook
  tar_quarto(
    name = nb07_emissions,
    path = "docs/NB07-emissions.qmd",
    quiet = TRUE
  ),

  # Ecology notebook
  tar_quarto(
    name = nb08_ecology,
    path = "docs/NB08-ecology.qmd",
    quiet = TRUE
  ),

  ## # Generate Quarto Appendices ----
  tar_quarto(
    name = ap01_protocol,
    path = "docs/AP01-review-protocol.qmd",
    quiet = TRUE
  ),

  tar_quarto(
    name = ap02_acknowledgements,
    path = "docs/AP02-acknowledgements.qmd",
    quiet = TRUE
  ),

  # # Publish Site ----
  tar_target(
    name = deploy_posit_connect_cloud,
    command = {
      quarto_publish_site(
        server = "connect.posit.cloud",
        account = "sawelch-niva",
        render = "none"
      )
      index.qmd
      nb01_qc
      nb02_vannmiljo
      nb03_visualisation
      nb04_map
      nb05_network
      nb07_emissions
      nb08_ecology
    }
  )

  # TODO: Are we allowed (statistically) to group similar compartments together?
  # i.e., if we do a t-test (or something) are our populations significantly different
  # do we need to do some sort of multi-factorial doodah

  # TODO: We should also do something with MEASURED_N vs actual replication. Hmm
)
