# Diagnostic left_join wrapper ----

#' Left join with diagnostic reporting for many-to-many relationships
#'
#' Wraps dplyr::left_join to catch many-to-many warnings and report
#' the key values involved in the problematic rows.
#'
#' @param x Left data frame
#' @param y Right data frame
#' @param by Join specification (passed to left_join)
#' @param ... Additional arguments passed to left_join
#' @param .report_n Maximum number of problematic rows to report (default 5)
#'
#' @return Result of left_join (possibly with many-to-many rows)
#' @importFrom dplyr left_join semi_join count across all_of filter
#' @importFrom cli cli_h1 cli_alert_warning cli_alert_info cli_text cli_alert_danger cli_inform
#' @export
left_join_diagnostic <- function(x, y, by = NULL, ..., .report_n = 5) {
  # Capture warnings during the join
  warnings_captured <- character(0)

  result <- withCallingHandlers(
    left_join(x, y, by = by, ...),
    warning = function(w) {
      warnings_captured <<- c(warnings_captured, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  # Check for many-to-many warning
  m2m_warning <- grep("many-to-many", warnings_captured, value = TRUE)

  if (length(m2m_warning) > 0) {
    # Determine join keys
    if (is.null(by)) {
      join_keys <- intersect(names(x), names(y))
    } else if (is.character(by)) {
      join_keys <- by
    } else {
      # Handle join_by() specifications
      join_keys <- names(by)
    }

    cli_h1("Many-to-many join diagnostic")
    cli_alert_danger("Detected an unexpected many-to-many relationship")
    cli_alert_info(
      "Join keys: {.field {paste(join_keys, collapse = ', ')}}"
    )

    # Parse row numbers from warning message
    x_row_match <- regmatches(
      m2m_warning[1],
      regexpr("Row (\\d+) of `x`", m2m_warning[1])
    )
    y_row_match <- regmatches(
      m2m_warning[1],
      regexpr("Row (\\d+) of `y`", m2m_warning[1])
    )

    x_row <- as.integer(gsub("\\D", "", x_row_match))
    y_row <- as.integer(gsub("\\D", "", y_row_match))

    # Report problematic x row
    if (length(x_row) > 0 && !is.na(x_row)) {
      cli_h1("Problematic row from x")
      cli_alert_warning(
        "Row {.val {x_row}} of {.var x} matches multiple rows in {.var y}"
      )
      cli_inform(paste(
        format(x[x_row, join_keys, drop = FALSE]),
        collapse = "\n"
      ))

      # Find all y rows matching this x row
      x_key_vals <- x[x_row, join_keys, drop = FALSE]
      y_matches <- semi_join(y, x_key_vals, by = join_keys)
      cli_alert_info(
        "Found {.val {nrow(y_matches)}} matching rows in {.var y}:"
      )
      cli_inform(paste(format(head(y_matches, .report_n)), collapse = "\n"))
      if (nrow(y_matches) > .report_n) {
        cli_text("{.emph ... and {nrow(y_matches) - .report_n} more}")
      }
    }

    # Report problematic y row
    if (length(y_row) > 0 && !is.na(y_row)) {
      cli_h1("Problematic row from y")
      cli_alert_warning(
        "Row {.val {y_row}} of {.var y} matches multiple rows in {.var x}"
      )
      cli_inform(paste(
        format(y[y_row, join_keys, drop = FALSE]),
        collapse = "\n"
      ))

      # Find all x rows matching this y row
      y_key_vals <- y[y_row, join_keys, drop = FALSE]
      x_matches <- semi_join(x, y_key_vals, by = join_keys)
      cli_alert_info(
        "Found {.val {nrow(x_matches)}} matching rows in {.var x}:"
      )
      cli_inform(paste(
        format(head(x_matches[, join_keys, drop = FALSE], .report_n)),
        collapse = "\n"
      ))
      if (nrow(x_matches) > .report_n) {
        cli_text("{.emph ... and {nrow(x_matches) - .report_n} more}")
      }
    }

    # Summary of key duplicates
    cli_h1("Key duplication summary")

    x_key_counts <- x |>
      count(across(all_of(join_keys)), name = "n") |>
      filter(n > 1) |>
      nrow()

    y_key_counts <- y |>
      count(across(all_of(join_keys)), name = "n") |>
      filter(n > 1) |>
      nrow()

    cli_alert_info(
      "Duplicate key combinations in {.var x}: {.val {x_key_counts}}"
    )
    cli_alert_info(
      "Duplicate key combinations in {.var y}: {.val {y_key_counts}}"
    )

    # Re-emit warning for downstream handling
    warning(m2m_warning[1], call. = FALSE)
  }

  # Re-emit any other warnings
  other_warnings <- setdiff(warnings_captured, m2m_warning)
  for (w in other_warnings) {
    warning(w, call. = FALSE)
  }

  result
}

#' Join all literature module tables
#'
#' Joins Sites, Reference, Campaign, Parameters, and Methods to the Measurements
#' fact table using appropriate foreign keys. Columns with duplicate names
#' receive explicit suffixes indicating their source table.
#'
#' @param measurements_data Tibble, the main fact table
#' @param sites_data Tibble, sites dimension table
#' @param reference_data Tibble, reference dimension table
#' @param biota_data Tibble, biota dimension table
#' @param campaign_data Tibble, campaign dimension table
#' @param parameters_data Tibble, parameters dimension table
#' @param methods_data Tibble, methods dimension table (will be spread internally)
#'
#' @return A tibble with all tables joined to measurements
#'
#' @importFrom dplyr left_join select distinct filter mutate
#' @importFrom stringr str_replace str_to_upper
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
  # Prepare dimension tables ----
  # FIXME: In the "real thing" we won't be able to assume just copper

  parameters_slim <- parameters_data |>
    select(PARAMETER_NAME) |>
    distinct()

  reference_slim <- reference_data |>
    select(YEAR, REFERENCE_ID, TITLE, DATA_SOURCE)

  sites_slim <- sites_data |>
    select(-SITE_COORDINATE_SYSTEM, -ENTERED_DATE, -ENTERED_BY)

  biota_slim <- biota_data |>
    select(
      SAMPLE_ID,
      SUBSAMPLE,
      SPECIES_GROUP,
      SAMPLE_SPECIES,
      SPECIES_COMMON_NAME,
      SAMPLE_TISSUE,
      SAMPLE_SPECIES_LIFESTAGE,
      SAMPLE_SPECIES_GENDER
    )

  # Join dimension tables to measurements ----

  ## # Sites ----
  # many measurements can be taken from each site
  # each measurement has one one site
  # some sites may have no measurements (data extraction artifact)
  joined_sites <- left_join_diagnostic(
    measurements_data,
    sites_slim,
    by = "SITE_CODE",
    suffix = c("_measurements", "_sites"),
    unmatched = "drop",
    relationship = "many-to-one"
  )

  ## # Reference ----
  # many measurements can correspond to one reference
  # each measurement has only one reference
  # all measurements should have a reference, and vice versa
  joined_reference <- left_join_diagnostic(
    joined_sites,
    reference_slim,
    by = "REFERENCE_ID",
    suffix = c("", "_reference"),
    unmatched = "error",
    relationship = "many-to-one"
  )

  # ## Biota ----
  # each measurement should have at most one biota row
  # each biota row can correspond to multiple measurements
  # a row in biota may not correspond to a measurement (again, artifact of extraction)
  joined_biota <- left_join_diagnostic(
    joined_reference,
    biota_slim,
    by = c("SAMPLE_ID", "SUBSAMPLE"),
    suffix = c("", "_biota"),
    unmatched = "drop",
    relationship = "one-to-many"
  )

  # some discrepancies between subsamples!
  # ignore numbesr
  # biota_slim |>
  #   group_by(SUBSAMPLE) |>
  #   filter(!str_detect(SUBSAMPLE, "^[0-9]{1,2}$")) |>
  #   filter(SUBSAMPLE %notin% unique(joined_reference$SUBSAMPLE)) |>
  #   reframe(n = n()) |>
  #   arrange(desc(n))

  # joined_reference |>
  #   filter(!str_detect(SUBSAMPLE, "^[0-9]{1,2}$")) |>
  #   reframe(n = n()) |>
  #   arrange(desc(n))

  ## # Campaign ----
  # many measurements can correspond to one campaign
  # each measurement has only one campaign
  # all measurements should have a campaign, and vice versa
  joined_campaign <- left_join_diagnostic(
    joined_biota,
    campaign_data,
    by = "CAMPAIGN_NAME_SHORT",
    suffix = c("", "_campaign"),
    # TODO: #11 None of joined_biota contains the Vm acidification campaign. I'm not really sure why this is, but I'm turning off unmatched = "error" for now
    # unmatched = "error",
    relationship = "many-to-one"
  )

  # ## Parameters ----
  # there's currenly only one parameter
  joined_parameters <- left_join_diagnostic(
    joined_campaign,
    parameters_slim,
    by = "PARAMETER_NAME",
    suffix = c("", "_parameters"),
    unmatched = "error",
    relationship = "many-to-one"
  )

  # Join methods by protocol category ----
  # Each category gets its own columns for protocol ID and name

  # ## Sampling Protocol ----
  methods_sampling <- methods_data |>
    filter(PROTOCOL_CATEGORY == "Sampling Protocol") |>
    mutate(
      SAMPLING_PROTOCOL = PROTOCOL_ID,
      SAMPLING_PROTOCOL_CLASS = PROTOCOL_NAME
    ) |>
    select(SAMPLING_PROTOCOL, SAMPLING_PROTOCOL_CLASS, CAMPAIGN_NAME)

  # one protocol can have many measurements
  # one measurement can only have one protocol
  # unneeded protocols may exist in data
  joined_sampling <- left_join_diagnostic(
    joined_parameters,
    methods_sampling,
    by = c("SAMPLING_PROTOCOL", "CAMPAIGN_NAME"),
    suffix = c("", "_sampling"),
    unmatched = "drop",
    relationship = "many-to-one"
  )

  # ## Fractionation Protocol ----
  methods_fractionation <- methods_data |>
    filter(PROTOCOL_CATEGORY == "Fractionation Protocol") |>
    mutate(
      FRACTIONATION_PROTOCOL = PROTOCOL_ID,
      FRACTIONATION_PROTOCOL_CLASS = PROTOCOL_NAME
    ) |>
    select(FRACTIONATION_PROTOCOL, FRACTIONATION_PROTOCOL_CLASS, CAMPAIGN_NAME)

  # one protocol can have many measurements
  # one measurement can only have one protocol
  # unneeded protocols may exist in data
  joined_fractionation <- left_join_diagnostic(
    joined_sampling,
    methods_fractionation,
    by = c("FRACTIONATION_PROTOCOL", "CAMPAIGN_NAME"),
    suffix = c("", "_fractionation"),
    unmatched = "drop",
    relationship = "many-to-one"
  )

  # ## Extraction Protocol ----
  # one protocol can have many measurements
  # one measurement can only have one protocol
  # unneeded protocols may exist in data
  methods_extraction <- methods_data |>
    filter(PROTOCOL_CATEGORY == "Extraction Protocol") |>
    mutate(
      EXTRACTION_PROTOCOL = PROTOCOL_ID,
      EXTRACTION_PROTOCOL_CLASS = PROTOCOL_NAME
    ) |>
    select(EXTRACTION_PROTOCOL, EXTRACTION_PROTOCOL_CLASS, CAMPAIGN_NAME)

  joined_extraction <- left_join_diagnostic(
    joined_fractionation,
    methods_extraction,
    by = c("EXTRACTION_PROTOCOL", "CAMPAIGN_NAME"),
    suffix = c("", "_extraction"),
    unmatched = "drop",
    relationship = "many-to-one"
  )

  # ## Analytical Protocol ----
  # one protocol can have many measurements
  # one measurement can only have one protocol
  # unneeded protocols may exist in data
  methods_analytical <- methods_data |>
    filter(PROTOCOL_CATEGORY == "Analytical Protocol") |>
    mutate(
      ANALYTICAL_PROTOCOL = PROTOCOL_ID,
      ANALYTICAL_PROTOCOL_CLASS = PROTOCOL_NAME
    ) |>
    select(ANALYTICAL_PROTOCOL, ANALYTICAL_PROTOCOL_CLASS, CAMPAIGN_NAME)

  joined_analytical <- left_join_diagnostic(
    joined_extraction,
    methods_analytical,
    by = c("ANALYTICAL_PROTOCOL", "CAMPAIGN_NAME"),
    suffix = c("", "_analytical"),
    unmatched = "drop",
    relationship = "many-to-one"
  )

  return(joined_analytical)
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
#' @importFrom cli cli_inform
#'
#' @export
save_literature_parquet <- function(
  data,
  output_path = "data/clean/derived",
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

  cli_inform("Literature data saved to: {.path {full_path}}")

  invisible(NULL)
}
