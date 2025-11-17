# File discovery ----

#' Get literature CSV file paths
#'
#' Lists CSV files from literature extraction directory, optionally filtered by module
#'
#' @param path Character string, path to directory containing CSV files
#' @param pattern Character string, file pattern to match
#' @param module Character string or NULL, module name to filter by (e.g., "Campaign")
#'
#' @return A tibble with one column 'value' containing file paths
#'
#' @importFrom dplyr filter as_tibble
#' @importFrom stringr str_detect
#' @importFrom magrittr |>
#'
#' @export
get_literature_csv_tibble <- function(
  path = "data/raw/LR_extractions/unzipped",
  pattern = ".csv",
  module = NULL
) {
  paths <- list.files(
    path = path,
    pattern = pattern,
    full.names = TRUE
  ) |>
    as_tibble()

  if (!is.null(module)) {
    paths <- paths |>
      filter(str_detect(value, module))
  }

  return(paths)
}

#' Literature module vocabulary
#'
#' Returns standardized names for literature extraction modules
#'
#' @return Character vector of module names
#'
#' @export
literature_module_vocab <- function() {
  c(
    "Campaign",
    "Samples",
    "Biota",
    "Compartments",
    "Measurements",
    "Methods",
    "Parameters",
    "Reference",
    "Sites",
    "CREED_Scores"
  )
}

# Reading functions ----

#' Read campaign CSV files with fread
#'
#' Uses data.table::fread for fast reading with explicit column types
#' based on the campaign tibble initialiser
#'
#' @param filepath Character string, path to CSV file
#'
#' @return A tibble with validated columns matching campaign structure
#'
#' @importFrom data.table fread
#' @importFrom dplyr summarise_all as_tibble
#' @importFrom purrr as_vector
#' @importFrom magrittr |>
#'
#' @export
read_campaign_csv <- function(filepath) {
  # Get expected column names and classes from initialiser
  expected_structure <- initialise_campaign_tibble()
  expected_names <- names(expected_structure)
  expected_classes <- expected_structure |>
    summarise_all(class) |>
    as_vector()

  # Read with fread using explicit specifications
  result <- fread(
    input = filepath,
    select = expected_names,
    colClasses = expected_classes
  ) |>
    as_tibble()

  return(result)
}

#' Read all campaign CSV files from literature module
#'
#' Reads all campaign files listed in the literature CSV tibble and combines them
#'
#' @return A tibble with all campaign data combined
#'
#' @importFrom dplyr pull bind_rows
#' @importFrom purrr map
#' @importFrom magrittr |>
#'
#' @export
read_all_campaign_files <- function() {
  # Get all campaign file paths
  campaign_paths <- get_literature_csv_tibble(module = "Campaign") |>
    pull(value)

  # Read each file and combine
  campaign_paths |>
    purrr::map(read_campaign_csv) |>
    bind_rows()
}

# Generalized version ----

#' Read module CSV files with fread
#'
#' Generic function to read any module's CSV files with type safety.
#' Uses an initialiser function to determine expected column structure.
#'
#' @param filepath Character string, path to CSV file
#' @param format_initialiser Function that returns expected tibble structure
#'
#' @return A tibble with validated columns matching the initialiser structure
#'
#' @importFrom data.table fread
#' @importFrom dplyr summarise_all as_tibble
#' @importFrom purrr as_vector
#' @importFrom magrittr |>
#'
#' @export
read_module_csv <- function(filepath, format_initialiser) {
  # Get expected structure from initialiser function
  expected_structure <- format_initialiser()
  expected_names <- names(expected_structure)
  expected_classes <- expected_structure |>
    summarise_all(class) |>
    as_vector()

  # Read with explicit specifications
  result <- fread(
    input = filepath,
    select = expected_names, # sets column order to expected (and, for better and worse, drops unexpected columns)
    colClasses = expected_classes # this /should/ mean that our downstream data is entirely consistent
  ) |>
    as_tibble()

  return(result)
}

#' Read all files for a given module
#'
#' Generic function to read and combine all CSV files for a specified module
#' using the appropriate format initialiser
#'
#' @param module_name Character string, name of module (e.g., "Campaign")
#' @param format_initialiser Function that returns expected tibble structure
#'
#' @return A tibble with all module data combined
#'
#' @importFrom dplyr pull bind_rows
#' @importFrom purrr map
#' @importFrom magrittr |>
#'
#' @export
read_all_module_files <- function(module_name, format_initialiser) {
  # Get all file paths for this module
  file_paths <- get_literature_csv_tibble(module = module_name) |>
    pull(value)

  # Read each file with appropriate initialiser
  file_paths |>
    purrr::map(\(x) read_module_csv(x, format_initialiser)) |>
    bind_rows()
}
