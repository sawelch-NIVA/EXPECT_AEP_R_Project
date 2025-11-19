#' Standardise measurement units and values
#'
#' Converts measurement values to standard units (mg/kg for dry/wet weight,
#' mg/L for concentration). Handles both Greek mu (μ) and micro sign (µ)
#' in microgram units. Skip row if the value is NA.
#'
#' @param data A tibble or data frame containing measurement data
#' @param value_column Character string. Name of column containing measurement values.
#'   Default is "MEASURED_VALUE"
#' @param unit_column Character string. Name of column containing measurement units.
#'   Default is "MEASURED_UNIT"
#'
#' @return The input data with two new columns added:
#'   - `{unit_column}_STANDARD`: Standardised unit (mg/kg (dry), mg/kg (wet), or mg/L)
#'   - `{value_column}_STANDARD`: Converted measurement value in standard units
#'
#' @examples
#' \dontrun{
#' data |>
#'   standardise_measured_units()
#'
#' data |>
#'   standardise_measured_units(
#'     value_column = "concentration",
#'     unit_column = "units"
#'   )
#' }
#'
#' @export
standardise_measured_units <- function(
  data,
  value_column = "MEASURED_VALUE",
  unit_column = "MEASURED_UNIT"
) {
  stopifnot(value_column %in% names(data), unit_column %in% names(data))

  # Create new column names
  unit_standard_col <- paste0(unit_column, "_STANDARD")
  value_standard_col <- paste0(value_column, "_STANDARD")

  data |>
    mutate(
      # FIXME: Needs to be extended to handle a wider range of SI prefixes
      # Standard units: mg/kg (dry), mg/kg (wet), mg/L
      !!unit_standard_col := case_when(
        str_detect(.data[[unit_column]], "dry") ~ "mg/kg (dry)",
        str_detect(.data[[unit_column]], "wet") ~ "mg/kg (wet)",
        str_detect(.data[[unit_column]], "L") ~ "mg/L",
        TRUE ~ NA_character_
      ),

      # Conversion factors
      !!value_standard_col := case_when(
        # Already in mg/kg or mg/L - no conversion
        .data[[unit_column]] %in%
          c("mg/kg (dry)", "mg/kg (wet)", "mg/L") ~ .data[[value_column]],

        # μg or µg to mg (divide by 1000)
        str_detect(.data[[unit_column]], "μg|µg") ~ .data[[value_column]] /
          1000,

        # Empty or missing units
        .data[[unit_column]] == "" | is.na(.data[[unit_column]]) ~ NA_real_,

        TRUE ~ NA_real_
      )
    )
}

#' Standardise a date column to IDate format
#'
#' Converts Date, character, or POSIXct columns to data.table's IDate format.
#' Handles character dates in both dmy and ymd formats.
#'
#' @param column A vector to be converted to IDate. Can be IDate, Date,
#'   character (in dmy or ymd format), or POSIXct.
#' @param verbose Logical. If TRUE, prints messages about conversions performed.
#'   Default is FALSE.
#'
#' @return An IDate vector
#'
#' @examples
#' standardise_IDate(as.Date("2024-01-15"))
#' standardise_IDate("15/01/2024", verbose = TRUE)
#' standardise_IDate("2024-01-15", verbose = TRUE)
#'
#' @export
standardise_IDate <- function(column, verbose = FALSE) {
  if (inherits(column, "IDate")) {
    if (verbose) {
      message(glue("Column already of class IDate."))
    }
    as.IDate(column)
  } else if (inherits_only(column, "Date")) {
    if (verbose) {
      message(glue("Column reformatted from Date to IDate."))
    }
    as.IDate(column)
  } else if (inherits_only(column, "character")) {
    # Try to detect date format
    sample_val <- column[!is.na(column)][1]

    # Simple heuristic: if contains "/" likely dmy, if "-" and starts with 4 digits likely ymd
    if (grepl("/", sample_val)) {
      if (verbose) {
        message(glue("Column reformatted from character (dmy) to IDate."))
      }
      as.IDate(dmy(column))
    } else if (grepl("^\\d{4}-", sample_val)) {
      if (verbose) {
        message(glue("Column reformatted from character (ymd) to IDate."))
      }
      as.IDate(ymd(column))
    } else {
      # Default to dmy for backwards compatibility
      if (verbose) {
        message(glue("Column reformatted from character (dmy) to IDate."))
      }
      as.IDate(dmy(column))
    }
  } else if (inherits(column, "POSIXct") || inherits(column, "POSIXlt")) {
    if (verbose) {
      message(glue("Column reformatted from POSIXct/POSIXlt to IDate."))
    }
    as.IDate(column)
  } else {
    stop(glue("Cannot convert column of class {class(column)} to IDate."))
  }
}


#' Standardise a date column to IDate format
#'
#' Converts Date, character, or POSIXct columns to data.table's IDate format.
#' Handles character dates in both dmy and ymd formats.
#'
#' @param column A vector to be converted to IDate. Can be IDate, Date,
#'   character (in dmy or ymd format), or POSIXct.
#' @param verbose Logical. If TRUE, prints messages about conversions performed.
#'   Default is FALSE.
#' @param char_format Character. The expected format for character dates.
#'   One of "dmy" (default) or "ymd". Only used when column is character.
#'
#' @return An IDate vector
#'
#' @examples
#' standardise_IDate(as.Date("2024-01-15"))
#' standardise_IDate("15/01/2024", verbose = TRUE)
#' standardise_IDate("2024-01-15", char_format = "ymd", verbose = TRUE)
#'
#' @importFrom data.table as.IDate
#' @importFrom lubridate dmy ymd
#' @importFrom glue glue
#' @export
standardise_IDate <- function(column, verbose = FALSE, char_format = "dmy") {
  if (inherits(column, "IDate")) {
    if (verbose) {
      message(glue("Column already of class IDate."))
    }
    as.IDate(column)
  } else if (inherits_only(column, "Date")) {
    if (verbose) {
      message(glue("Column reformatted from Date to IDate."))
    }
    as.IDate(column)
  } else if (inherits_only(column, "character")) {
    # Detect format from first non-NA value
    sample_val <- column[!is.na(column)][1]

    detected_format <- if (grepl("^\\d{4}", sample_val)) {
      "ymd"
    } else {
      "dmy"
    }

    # Use char_format argument, fallback to detected
    format_to_use <- char_format

    if (format_to_use == "ymd") {
      if (verbose) {
        message(glue("Column reformatted from character (ymd) to IDate."))
      }
      as.IDate(ymd(column))
    } else {
      if (verbose) {
        message(glue("Column reformatted from character (dmy) to IDate."))
      }
      as.IDate(dmy(column))
    }
  } else if (inherits(column, "POSIXct") || inherits(column, "POSIXlt")) {
    if (verbose) {
      message(glue("Column reformatted from POSIXct/POSIXlt to IDate."))
    }
    as.IDate(column)
  } else {
    stop(glue("Cannot convert column of class {class(column)} to IDate."))
  }
}


#' Standardise all date columns in a tibble to IDate format
#'
#' Applies `standardise_IDate()` to all columns containing "DATE" in their name
#' (case-insensitive).
#'
#' @param tibble A tibble or data.frame containing date columns
#' @param verbose Logical. If TRUE, prints messages about conversions performed.
#'   Default is FALSE.
#' @param char_format Character. The expected format for character dates.
#'   One of "dmy" (default) or "ymd". Passed to `standardise_IDate()`.
#'
#' @return A tibble with date columns converted to IDate format
#'
#' @examples
#' df <- tibble(
#'   SAMPLE_DATE = as.Date("2024-01-15"),
#'   VALUE_DATE = "15/01/2024",
#'   other_col = 1:2
#' )
#' standardise_IDate_all(df, verbose = TRUE)
#'
#' @importFrom dplyr mutate across contains
#' @export
standardise_IDate_all <- function(
  tibble,
  verbose = FALSE,
  char_format = "dmy"
) {
  tibble |>
    mutate(across(
      .cols = contains("DATE", ignore.case = TRUE),
      .fns = ~ standardise_IDate(
        .x,
        verbose = verbose,
        char_format = char_format
      )
    ))
}
