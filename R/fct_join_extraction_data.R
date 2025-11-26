#' Join all literature module tables
#'
#' Joins Sites, Reference, Campaign, Parameters, and Methods to the Measurements
#' fact table using appropriate foreign keys
#'
#' @param measurements_data Tibble, the main fact table
#' @param sites_data Tibble, sites dimension table
#' @param reference_data Tibble, reference dimension table
#' @param campaign_data Tibble, campaign dimension table
#' @param parameters_data Tibble, parameters dimension table
#' @param methods_data Tibble, methods dimension table (will be spread internally)
#'
#' @return A tibble with all tables joined to measurements
#'
#' @importFrom dplyr left_join
#'
#' @export
join_all_literature_modules <- function(
  measurements_data,
  sites_data,
  reference_data,
  biota_data,
  campaign_data,
  parameters_data,
  methods_data
) {
  # Spread methods table first
  # Join everything to measurements fact table
  # Joins are sloooow, so we might as well filter out some columns first
  # FIXME: In the "real thing" we won't be able to assume just copper, but for now we will
  parameters_data <- parameters_data |> select(PARAMETER_NAME) |> distinct()
  reference_data <- reference_data |>
    select(YEAR, REFERENCE_ID, TITLE, DATA_SOURCE)
  sites_data <- sites_data |>
    select(-SITE_COORDINATE_SYSTEM, -ENTERED_DATE, -ENTERED_BY)
  biota_data <- biota_data |>
    select(
      SAMPLE_ID,
      SUBSAMPLE,
      SPECIES_GROUP,
      SAMPLE_SPECIES,
      SAMPLE_TISSUE,
      SAMPLE_SPECIES_LIFESTAGE,
      SAMPLE_SPECIES_GENDER
    )

  result <- measurements_data |>
    left_join(sites_data, by = "SITE_CODE") |>
    left_join(reference_data, by = "REFERENCE_ID") |>
    left_join(biota_data, by = c("SAMPLE_ID", "SUBSAMPLE")) |>
    left_join(campaign_data, by = "CAMPAIGN_NAME_SHORT") |>
    left_join(parameters_data, by = "PARAMETER_NAME")

  # this is, frankly, bad, but it allows us to join out methods to measurements in a way that makes analysis easier
  purrr::map(.x = protocol_categories_vocabulary(), .f = function(x) {
    category_name_snake <- str_replace(x, pattern = " ", replacement = "_") |>
      str_to_upper()
    type_name_snake <- str_replace(
      x,
      pattern = " Protocol",
      replacement = "_PROTOCOL_CLASS"
    ) |>
      str_to_upper()

    methods_filtered <- methods_data |>
      filter(PROTOCOL_CATEGORY == x) |>
      mutate(
        !!category_name_snake := PROTOCOL_ID,
        !!type_name_snake := PROTOCOL_NAME
      ) |>
      left_join(
        result,
        by = eval(category_name_snake)
      )
  })

  return(result)
}

#' Remove extraneous and duplicate columns
#'
#' Drops specified columns from the joined literature data
#'
#' @param data Tibble, the joined data from join_all_literature_modules
#' @param columns_to_drop Character vector, names of columns to remove
#'
#' @return A tibble with specified columns removed
#'
#' @importFrom dplyr select all_of

#'
#' @export
clean_joined_columns <- function(data, columns_to_drop = character()) {
  if (length(columns_to_drop) == 0) {
    return(data)
  }

  data |>
    select(-all_of(columns_to_drop))
}

#' Save literature data as parquet file
#'
#' Writes the cleaned, joined literature data to disk in Apache Arrow parquet format
#'
#' @param data Tibble, the cleaned data to save
#' @param output_path Character string, path where parquet file should be saved
#' @param filename Character string, name of output file (default: "literature_data.parquet")
#'
#' @return Invisible NULL (called for side effect of writing file)
#'
#' @importFrom arrow write_parquet
#'
#' @export
save_literature_parquet <- function(
  data,
  output_path = "data/clean",
  filename = "literature_data.parquet"
) {
  # Ensure output directory exists
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  # Construct full filepath
  full_path <- file.path(output_path, filename)

  # Write parquet file
  write_parquet(data, full_path)

  message(sprintf("Literature data saved to: %s", full_path))

  invisible(NULL)
}
