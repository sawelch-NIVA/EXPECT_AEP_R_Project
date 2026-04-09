#' Get Common Names for Scientific Species Names
#'
#' @param biota_data A data frame containing scientific species names
#' @param input_col Name of the column containing scientific names. Default is "SAMPLE_SPECIES"
#' @param output_col Name of the column to store common names. Default is "SPECIES_COMMON_NAME"
#' @param cache_path Path to the cache CSV file. Defaults to "data/clean/species_common_names_cache.csv"
#' @param db Database to query for common names. One of "ncbi", "itis", "tropicos", "eol", or "worms". Default is "ncbi"
#' @param verbose Logical. Print progress messages? Default is TRUE
#'
#' @return A data frame with common names joined to the input data
#' @export
#'
#' @importFrom dplyr pull filter select left_join mutate coalesce bind_rows
#' @importFrom readr read_csv write_csv
#' @importFrom tibble tibble
#' @importFrom here here
#' @importFrom taxize sci2comm
#' @importFrom rlang sym :=
#' @importFrom stringr str_to_sentence
#'
#' @examples
#' \dontrun{
#' biota_with_names <- get_common_names(biota_data)
#' biota_with_names <- get_common_names(biota_data, db = "worms", verbose = FALSE)
#' biota_with_names <- get_common_names(
#'   biota_data,
#'   input_col = "scientific_name",
#'   output_col = "common_name"
#' )
#' }
get_common_names <- function(
  biota_data,
  input_col = "SAMPLE_SPECIES",
  output_col = "SPECIES_COMMON_NAME",
  cache_path = here("data/clean/species_common_names_cache.csv"),
  db = "ncbi",
  verbose = TRUE
) {
  # Check for ENTREZ API key if using NCBI
  if (db == "ncbi" && Sys.getenv("ENTREZ_KEY") == "") {
    warning(
      "No ENTREZ_KEY found in environment. ",
      "API calls will be slower without a key. ",
      "Get one at https://www.ncbi.nlm.nih.gov/account/ ",
      "and set with Sys.setenv(ENTREZ_KEY = 'your_key_here')"
    )
  }

  # Convert column names to symbols for tidy evaluation
  input_sym <- sym(input_col)
  output_sym <- sym(output_col)

  # Load existing cache if it exists
  if (file.exists(cache_path)) {
    cached_names <- read_csv(cache_path, show_col_types = FALSE)
    if (verbose) message("Loaded cache with ", nrow(cached_names), " species")
  } else {
    cached_names <- tibble(
      scientific_name = character(),
      common_name = character(),
      db = character(),
      date_retrieved = as.Date(character())
    )
    if (verbose) message("No existing cache found, creating new cache")
  }

  # Get species that need common names
  species_to_lookup <- biota_data |>
    pull(!!input_sym) |>
    unique()

  # Filter out species already in cache for this specific db
  cached_for_db <- cached_names |> filter(db == !!db)
  new_species <- setdiff(species_to_lookup, cached_for_db$scientific_name)

  # Only call API if there are new species
  if (length(new_species) > 0) {
    if (verbose) {
      message(
        "Looking up ",
        length(new_species),
        " new species from ",
        db,
        "..."
      )
    }

    # Get common names from taxize
    common_names_list <- sci2comm(new_species, db = db, simplify = TRUE)

    # Convert to data frame
    new_results <- tibble(
      scientific_name = new_species,
      common_name = sapply(common_names_list, function(x) {
        # if it exists, convert to sentence case
        if (length(x) > 0) str_to_sentence(x[1]) else NA_character_
      }),
      db = db,
      date_retrieved = Sys.Date()
    )

    # Combine with existing cache
    updated_cache <- bind_rows(cached_names, new_results)

    # Save updated cache
    write_csv(updated_cache, cache_path)

    if (verbose) {
      message("Cache updated with ", nrow(new_results), " new species")
    }
  } else {
    if (verbose) {
      message("No new species to look up")
    }
    updated_cache <- cached_names
  }

  # Join cached names back to input data
  biota_data |>
    left_join(
      updated_cache |>
        filter(db == !!db) |>
        select(scientific_name, common_name),
      by = setNames("scientific_name", input_col)
    ) |>
    mutate(
      !!output_sym := common_name
    ) |>
    select(-common_name)
}
