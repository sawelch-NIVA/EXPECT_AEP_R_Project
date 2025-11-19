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
    "STOPeData", # local package, used for format functions
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
    "dtplyr"
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
  # Literature data reading targets ----
  # File-watching targets for each module
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

  # FIXME: Enable once we have CREED data
  # tar_target(
  #   name = creed_scores_files,
  #   command = get_literature_csv_paths(module = "CREED_Scores"),
  #   format = "file"
  # ),

  # Data reading targets for each module
  tar_target(
    name = campaign_data,
    command = fread_all_module_files(campaign_files, initialise_campaign_tibble)
  ),
  tar_target(
    name = samples_data,
    command = fread_all_module_files(samples_files, initialise_samples_tibble)
  ),
  tar_target(
    name = biota_data,
    command = fread_all_module_files(biota_files, initialise_biota_tibble)
  ),
  tar_target(
    name = compartments_data,
    command = fread_all_module_files(
      compartments_files,
      initialise_compartments_tibble
    )
  ),
  tar_target(
    name = measurements_data,
    command = fread_all_module_files(
      measurements_files,
      initialise_measurements_tibble
    )
  ),
  tar_target(
    name = methods_data,
    command = fread_all_module_files(methods_files, initialise_methods_tibble)
  ),
  tar_target(
    name = parameters_data,
    command = fread_all_module_files(
      parameters_files,
      initialise_parameters_tibble
    )
  ),
  tar_target(
    name = reference_data,
    command = fread_all_module_files(
      reference_files,
      initialise_references_tibble
    )
  ),
  tar_target(
    name = sites_data,
    command = fread_all_module_files(sites_files, initialise_sites_tibble)
  ),

  # FIXME: Enable once we have CREED data
  # tar_target(
  #   name = creed_scores_data,
  #   command = fread_all_module_files(creed_scores_files, initialise_creed_scores_tibble)
  # ),

  # Join and save literature data ----
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

  # Create a megatable by merging measurements, sites, references, campaign, methods
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

  # Save the resulting file as a parquet
  tar_target(
    name = save_literature_pqt,
    command = save_literature_parquet(
      data = literature_clean,
      output_path = "data/clean",
      filename = "literature_data.parquet"
    ),
    format = "file"
  ),

  # Load the resulting file
  # Why this level of redundancy? Because with targets, it means we can avoid constantly reloading CSVs unless they've actually changed
  tar_target(
    name = load_literature_pqt,
    command = load_literature_parquet(
      input_path = "data/clean",
      filename = "literature_data.parquet"
    )
  ),

  # TODO: Ought to fix SAMPLING_DATE and YEAR too, which for some reason are chr and int
  # Probably clean data, /then/ save.
  # Standardise reported units
  tar_target(
    name = clean_literature_units,
    command = {
      standardise_measured_units(
        load_literature_pqt,
        value_column = "MEASURED_VALUE",
        unit_column = "MEASURED_UNIT"
      ) |>
        standardise_measured_units(
          value_column = "LOQ_VALUE",
          unit_column = "LOQ_UNIT"
        ) |>
        standardise_measured_units(
          value_column = "LOD_VALUE",
          unit_column = "LOD_UNIT"
        )
    }
  ),

  # Geography data preparation targets ----

  # # WGS84 map geometry
  tar_target(
    name = wgs84_geography,
    command = prepare_geography_wgs84(
      scale = 10,
      destdir = "data/raw/shapefiles/"
    )
  ),

  # # Polar projection map geometry
  tar_target(
    name = polar_geography,
    command = prepare_geography_polar(
      scale = 10,
      destdir = "data/raw/shapefiles/",
      crs = "EPSG:3575"
    )
  ),

  # Map creation  ----

  # # WGS84 map
  tar_target(
    name = wgs84_map,
    command = create_study_area_map_wgs84(
      ocean_sf = wgs84_geography$marine_polys,
      country_sf = wgs84_geography$countries,
      arctic_circle_sf = wgs84_geography$arctic_circle,
      graticule_sf = wgs84_geography$graticule
    )
  ),

  # # Polar projection map
  tar_target(
    name = polar_map,
    command = create_study_area_map_polar(
      ocean_sf = polar_geography$marine_polys,
      country_sf = polar_geography$countries,
      arctic_circle_sf = polar_geography$arctic_circle,
      graticule_sf = polar_geography$graticule
    )
  ),

  # TODO: The bounding box on this is pretty bad, because pretty much all our data points so far are actually in the Norwegian/Greenland sea. What do?
  # Map literature data points
  tar_target(
    name = wgs84_literature_map,
    command = map_literature_data_wgs84(
      wgs84_map = wgs84_map,
      literature_data = literature_clean
    )
  ),
  # TODO: Just make some big boxplots for each unit type
  # Boxplot using standardised units
  tar_target(
    name = measurement_by_year,
    command = make_measurement_boxplot(
      data = clean_literature_units,
      value_column = "MEASURED_VALUE_STANDARD",
      unit_column = "MEASURED_UNIT_STANDARD"
    )
  ),
  # # Toxicity/safety threshholds!
  # TODO: We can add GeoTraces data here, although it may be too precise for our use:
  # https://geotraces.webodv.awi.de/IDP2021_v2%3EGEOTRACES_IDP2021_Seawater_Discrete_Sample_Data_v2/service/DataExtraction
  # In general, the our big study area and many study compartments mean there's loads of values we can use here. Self-restrain is probably wise.
  tar_target(
    name = copper_toxicity_thresholds,
    command = generate_copper_thresholds()
  )

  # TODO: Imputation of missing values. What's best practice?
  # Ask KET/Chemists

  # TODO: Are we allowed (statistically) to group similar compartments together?
  # i.e., if we do a t-test (or something) are our populations significantly different
  # do we need to do some sort of multi-factorial doodah

  # TODO: We should also do something with MEASURED_N vs actual replication. Hmm

  # TODO: And the piece de registance -- the network digram
)
