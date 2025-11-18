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

#'
#' @export
get_literature_csv_tibble <- function(
  path = "data/raw/LR_extractions/unzipped",
  pattern = ".csv",
  module = NULL
) {
  stopifnot(
    "Module name module not valid. Call literature_module_vocab() for valid options." = module %in%
      literature_module_vocab()
  )

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
    select = expected_names, # sets column order to expected (and, for better and worse, drops unexpected columns)
    colClasses = expected_classes # this generally works but can raise issues downstream
    # because IDate ~= Date here, but not when we call reduce() in fread_all_module_files()
  ) |>
    as_tibble() |>
    # Convert IDate columns back to standard Date for compatibility
    # Probably less efficient, definitely less work.
    mutate(across(where(\(x) inherits(x, "IDate")), as.Date))

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
#' @importFrom purrr map reduce
#'
#' @export
fread_all_module_files <- function(module_name, format_initialiser) {
  # Get all file paths for this module
  file_paths <- get_literature_csv_tibble(module = module_name) |>
    pull(value)

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

initialise_tibble_IDate <- function(tibble) {
  IDate_tibble <- tibble |>
    mutate(across(where(is.Date), .fns = as.IDate))

  return(IDate_tibble)
}
