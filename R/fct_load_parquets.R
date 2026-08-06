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
#' @importFrom cli cli_abort cli_inform
#'
#' @export
load_literature_parquet <- function(
  input_path = "data/clean/derived",
  filename = "literature_data.parquet"
) {
  # Construct full filepath
  full_path <- file.path(input_path, filename)

  # Check if file exists
  if (!file.exists(full_path)) {
    cli_abort("Parquet file not found at: {.path {full_path}}")
  }

  cli_inform("Loading literature data from: {.path {full_path}}")

  # Read parquet file
  data <- read_parquet(full_path) |>
    as_tibble()

  cli_inform("Loaded {nrow(data)} rows and {ncol(data)} columns")

  return(data)
}
