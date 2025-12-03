# File discovery ----

#' Get literature CSV file paths
#'
#' Lists CSV files from literature extraction directory, optionally filtered by module
#'
#' @param path Character string, path to directory containing CSV files
#' @param pattern Character string, file pattern to match
#' @param module Character string or NULL, module name to filter by (e.g., "Campaign")
#'
#' @return a vector of file paths
#'
#' @importFrom dplyr filter if_else
#' @importFrom glue glue
#'
#' @export
get_literature_csv_paths <- function(
  path = "data/raw/LR_extractions/unzipped",
  format = ".csv",
  module = NULL
) {
  stopifnot(
    "Module name module not valid. Call literature_module_vocab() for valid options." = module %in%
      literature_module_vocab()
  )

  # generate a regex to match .csv and the module name
  pattern <- if (is.null(module)) format else glue("({module}).*\\.csv$")

  paths <- list.files(
    path = path,
    pattern = pattern,
    full.names = TRUE
  )

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

#'
#' @export
fread_module_csv <- function(filepath, format_initialiser) {
  # Get expected structure from initialiser function
  expected_structure <- format_initialiser()
  expected_names <- names(expected_structure)
  expected_classes <- expected_structure |>
    summarise_all(class) |>
    as_vector()

  # Read with explicit specifications
  result <- fread(
    input = filepath,
    select = expected_names,
    colClasses = expected_classes,
    encoding = "UTF-8" # Force UTF-8 encoding
  ) |>
    as_tibble() |>
    mutate(across(where(\(x) inherits(x, "IDate")), as.Date))

  return(result)
}

#' Read all files for a given module
#'
#' Generic function to read and combine all CSV files for a specified module
#' using the appropriate format initialiser
#'
#' @param file_paths A tibble/df of file paths supplied by get_literature_csv_tibble
#' @param format_initialiser Function that returns expected tibble structure
#'
#' @return A tibble with all module data combined
#'
#' @importFrom dplyr pull bind_rows
#' @importFrom purrr map reduce
#'
#' @export
fread_all_module_files <- function(file_paths, format_initialiser) {
  stopifnot(
    "No paths found matching module name." = length(file_paths) > 0
  )

  # Read each file and reduce by binding rows sequentially
  file_paths |>
    purrr::map(\(x) {
      # message(sprintf("Reading: %s", x))
      fread_module_csv(x, format_initialiser)
    }) |>
    purrr::reduce(bind_rows, .init = format_initialiser())
}
