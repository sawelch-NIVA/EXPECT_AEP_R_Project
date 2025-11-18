#' Load literature data from parquet file
#'
#' Reads the cleaned, joined literature data from disk in Apache Arrow parquet format
#'
#' @param input_path Character string, path where parquet file is located
#' @param filename Character string, name of input file (default: "literature_data.parquet")
#'
#' @return A tibble with the literature data
#'
#' @importFrom arrow read_parquet
#' @importFrom dplyr as_tibble

#'
#' @export
load_literature_parquet <- function(
  input_path = "data/clean",
  filename = "literature_data.parquet"
) {
  # Construct full filepath
  full_path <- file.path(input_path, filename)

  # Check if file exists
  if (!file.exists(full_path)) {
    stop(sprintf("Parquet file not found at: %s", full_path))
  }

  message(sprintf("Loading literature data from: %s", full_path))

  # Read parquet file
  data <- read_parquet(full_path) |>
    as_tibble()

  message(sprintf("Loaded %s rows and %s columns", nrow(data), ncol(data)))

  return(data)
}
