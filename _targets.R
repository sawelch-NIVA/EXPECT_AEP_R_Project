# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # better factories for watching many files
library(crew) # parallel processing, faster execution?

# Set target options:
tar_option_set(
  # Packages that your targets need for their tasks.
  packages = c(
    "sf",
    "tidyverse",
    "sfhelper",
    "rnaturalearth",
    "rnaturalearthdata",
    "mapproj",
    "ggspatial",
    "shadowtext",
    "ggrepel",
    "rlang",
    "data.table",
    "dtplyr",
    "dplyr",
    "leaflet",
    "janitor",
    "shiny",
    "readxl",
    "purrr",
    "tibble",
    "arrow",
    "qs2",
    "STOPeData", # local package, used for format functions
    "tarchetypes" # extend targets
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

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  # watch all files for changes
  tar_files(
    watch_unzipped,
    command = get_literature_csv_tibble() |> pull(value)
  ),
  tar_target(
    "watch_CSVs_raw",
    command = fread(watch_unzipped),
    pattern = map(watch_unzipped)
  ),
  # Literature data reading targets ----
  # Currently this doesn't watch for changes in the source files, so we're clearly doing something wrong

  # FIXME: Enable once we have CREED data
  # tar_target(
  #   name = creed_scores_files,
  #   command = get_literature_csv_tibble(module = "CREED_Scores") |> pull(value)
  # ),

  # Read and combine all files per module (watching individual files via pattern)
  # fread = data.table's fast file reader
  tar_target(
    name = campaign_data,
    command = fread_all_module_files("Campaign", initialise_campaign_tibble)
  ),

  tar_target(
    name = samples_data,
    command = fread_all_module_files("Samples", initialise_samples_tibble)
  ),

  tar_target(
    name = biota_data,
    command = fread_all_module_files("Biota", initialise_biota_tibble)
  ),

  tar_target(
    name = compartments_data,
    command = fread_all_module_files(
      "Compartments",
      initialise_compartments_tibble
    )
  ),

  tar_target(
    name = measurements_data,
    command = fread_all_module_files(
      "Measurements",
      initialise_measurements_tibble
    )
  ),

  tar_target(
    name = methods_data,
    command = fread_all_module_files("Methods", initialise_methods_tibble)
  ),

  tar_target(
    name = parameters_data,
    command = fread_all_module_files("Parameters", initialise_parameters_tibble)
  ),

  tar_target(
    name = reference_data,
    command = fread_all_module_files("Reference", initialise_references_tibble)
  ),

  tar_target(
    name = sites_data,
    command = fread_all_module_files("Sites", initialise_sites_tibble)
  ),

  # FIXME: Enable once we have CREED data
  # tar_target(
  #   name = creed_scores_data,
  #   command = fread_all_module_files(
  #     "CREED_Scores",
  #     initialise_creed_scores_tibble
  #   ),
  #   pattern = map(creed_scores_files),
  #   iteration = "list"
  # )
  # merge together each module's data
  # Join and save literature data ----

  # tar_target(
  #   name = methods_spread,
  #   command = spread_methods_for_join(methods_data)
  # ),

  tar_target(
    name = literature_joined,
    command = join_all_literature_modules(
      measurements_data = measurements_data,
      sites_data = sites_data,
      reference_data = reference_data,
      # campaign_data = campaign_data,
      parameters_data = parameters_data,
      methods_data = methods_data
    )
  ),
  # create a megatable by merging measurements, sites, references, campaign, methods
  tar_target(
    name = literature_clean,
    command = clean_joined_columns(
      data = literature_joined,
      columns_to_drop = c() # Add column names here as you identify them
    )
  ),
  # save the resulting file as a parquet
  tar_target(
    name = save_literature_pqt,
    command = save_literature_parquet(
      data = literature_clean,
      output_path = "data/clean",
      filename = "literature_data.parquet"
    ),
    format = "file"
  ),
  # load the resulting file
  # Why this level of redundancy? Because with target, it means we can avoid constantly reloading CSVs unless they've actually changed
  tar_target(
    name = load_literature_pqt,
    command = load_literature_parquet(
      input_path = "data/clean",
      filename = "literature_data.parquet"
    )
  ),
  # set up maps
  # Geography data preparation targets ----

  # WGS84 geography data
  tar_target(
    name = wgs84_geography,
    command = prepare_geography_wgs84(
      scale = 10,
      destdir = "data/raw/shapefiles/"
    )
  ),

  # Polar projection geography data
  tar_target(
    name = polar_geography,
    command = prepare_geography_polar(
      scale = 10,
      destdir = "data/raw/shapefiles/",
      crs = "EPSG:3575"
    )
  ),

  # Map creation targets ----

  # WGS84 map
  tar_target(
    name = wgs84_map,
    command = create_study_area_map_wgs84(
      ocean_sf = wgs84_geography$marine_polys,
      country_sf = wgs84_geography$countries,
      arctic_circle_sf = wgs84_geography$arctic_circle,
      graticule_sf = wgs84_geography$graticule
    )
  ),

  # Polar projection map
  tar_target(
    name = polar_map,
    command = create_study_area_map_polar(
      ocean_sf = polar_geography$marine_polys,
      country_sf = polar_geography$countries,
      arctic_circle_sf = polar_geography$arctic_circle,
      graticule_sf = polar_geography$graticule
    )
  )

  # do analysis
)
