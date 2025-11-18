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
