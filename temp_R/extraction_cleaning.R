readr::read_csv(file = "data/raw/LR_extractions/")

# x Helper functions ----

# x Main workflow ----

## Get file lists for each pattern ----

file_lists <- purrr::map(
  purrr::set_names(patterns),
  ~ list.files(
    path = "data/raw/LR_extractions/unzipped",
    pattern = .x,
    full.names = TRUE
  )
)

# x Helper functions ----

#' Combine CSV files for a given module
#'
#' @param file_paths Character vector of file paths to read and combine
#' @return A single tibble with all data combined
combine_module_files <- function(file_paths, module) {
  stopifnot(
    module %in%
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
      ),
    length(file_paths) > 0
  )

  # Read all files and combine
  purrr::map_dfr(
    file_paths[[module]],
    ~ readr::read_csv(
      .x,
      show_col_types = FALSE,
      col_types = list(
        SUBSAMPLE = col_character(),
        SAMPLING_DATE = col_date(format = "YYYY-mm-DD") # this returns a lot of warnings but catches a few cooerced dates.
      )
    )
  )
}

# x Main workflow ----

## Combine files by module ----
sites_data <- purrr::map(
  file_lists,
  .f = function(x) {
    combine_module_files(x, module = "Campaign")
  }
)

sites_data <- combine_module_files(file_lists, "Sites")
