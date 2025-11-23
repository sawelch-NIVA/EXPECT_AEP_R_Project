if (!exists("requirements_loaded")) {
  source("temp_R/scratchpad.r")
  load_all_requirements()
}

sites <- fread_all_module_files(
  get_literature_csv_paths(module = "Sites"),
  initialise_sites_tibble
)
