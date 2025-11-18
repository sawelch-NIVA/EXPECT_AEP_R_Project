# Unit System Setup ----

#' Install custom ecotoxicology units into the units package
#'
#' Sets up custom unit definitions for dry/wet mass measurements and other
#' ecotoxicology-specific units that aren't in the standard SI system
#'
#' @export
#' @importFrom units install_unit
setup_ecotox_units <- function() {
  # Install dry and wet mass units ----
  # These are conceptually different "kinds" of grams
  install_unit(
    symbol = "g_dry",
    name = c("dry_gram", "gram_dry")
  )
  install_unit(
    symbol = "g_wet",
    name = c("wet_gram", "gram_wet")
  )
  install_unit(
    symbol = "kg_dry",
    def = "1000 g_dry",
    name = c("dry_kilogram", "kilogram_dry")
  )
  install_unit(
    symbol = "kg_wet",
    def = "1000 g_wet",
    name = c("wet_kilogram", "kilogram_wet")
  )

  # Install fractional dry/wet mass units ----
  install_unit(
    symbol = "mg_dry",
    def = "0.001 g_dry",
    name = c("dry_milligram", "milligram_dry")
  )
  install_unit(
    symbol = "mg_wet",
    def = "0.001 g_wet",
    name = c("wet_milligram", "milligram_wet")
  )
  install_unit(
    symbol = "ug_dry",
    def = "1e-6 g_dry",
    name = c("dry_microgram", "microgram_dry")
  )
  install_unit(
    symbol = "ug_wet",
    def = "1e-6 g_wet",
    name = c("wet_microgram", "microgram_wet")
  )
  install_unit(
    symbol = "ng_dry",
    def = "1e-9 g_dry",
    name = c("dry_nanogram", "nanogram_dry")
  )
  install_unit(
    symbol = "ng_wet",
    def = "1e-9 g_wet",
    name = c("wet_nanogram", "nanogram_wet")
  )
  install_unit(
    symbol = "pg_dry",
    def = "1e-12 g_dry",
    name = c("dry_picogram", "picogram_dry")
  )
  install_unit(
    symbol = "pg_wet",
    def = "1e-12 g_wet",
    name = c("wet_picogram", "picogram_wet")
  )

  message("Ecotoxicology units installed successfully")
}

#' Remove custom ecotoxicology units from the units package
#'
#' @export
#' @importFrom units remove_unit
cleanup_ecotox_units <- function() {
  # Remove all custom units ----
  remove_unit(
    symbol = c(
      "g_dry",
      "g_wet",
      "kg_dry",
      "kg_wet",
      "mg_dry",
      "mg_wet",
      "ug_dry",
      "ug_wet",
      "ng_dry",
      "ng_wet",
      "pg_dry",
      "pg_wet"
    ),
    name = c(
      "dry_gram",
      "gram_dry",
      "wet_gram",
      "gram_wet",
      "dry_kilogram",
      "kilogram_dry",
      "wet_kilogram",
      "kilogram_wet",
      "dry_milligram",
      "milligram_dry",
      "wet_milligram",
      "milligram_wet",
      "dry_microgram",
      "microgram_dry",
      "wet_microgram",
      "microgram_wet",
      "dry_nanogram",
      "nanogram_dry",
      "wet_nanogram",
      "nanogram_wet",
      "dry_picogram",
      "picogram_dry",
      "wet_picogram",
      "picogram_wet"
    )
  )

  message("Ecotoxicology units removed")
}


# Unit Conversion Function ----

#' Convert concentration values to base SI units using lookup table
#'
#' @param value Numeric vector of values to convert
#' @param unit Character vector of units to convert from
#' @param conversion_table Data frame with columns MEASURED_UNIT, BASE_SI_UNIT,
#'   CONVERSION_FACTOR, UNIT_COMMENTS (defaults to parameter_unit_vocabulary())
#' @param return_units Logical. If TRUE, returns a units object. If FALSE, returns numeric vector.
#' @param setup_units Logical. If TRUE, automatically sets up custom ecotox units. Default TRUE.
#'
#' @return Numeric vector or units object of converted values in base SI units
#' @export
#' @importFrom units set_units
convert_to_base_units <- function(
  value,
  unit,
  conversion_table = NULL,
  return_units = FALSE,
  setup_units = TRUE
) {
  # Load conversion table if not provided ----
  if (is.null(conversion_table)) {
    stop(
      "Conversion table not found. Designed to work with STOPeData::parameter_unit_vocabulary()"
    )
  }

  # Setup custom units if requested ----
  if (return_units && setup_units) {
    setup_ecotox_units()
  }

  # Create lookup vectors ----
  conversion_factors <- setNames(
    as.numeric(conversion_table$CONVERSION_FACTOR),
    conversion_table$MEASURED_UNIT
  )

  base_units <- setNames(
    conversion_table$BASE_SI_UNIT,
    conversion_table$MEASURED_UNIT
  )

  # Map measured units to units package notation ----
  units_notation <- c(
    "kg/m³" = "kg/m^3",
    "mol/m³" = "mol/m^3",
    "J/m²" = "J/m^2",
    "W/m²" = "W/m^2",
    "Gy/s" = "Gy/s"
  )

  # Check for unsupported units ----
  unsupported <- setdiff(unique(unit), names(conversion_factors))
  if (length(unsupported) > 0) {
    stop("Unsupported unit(s): ", paste(unsupported, collapse = ", "))
  }

  # Perform conversion ----
  converted <- value * conversion_factors[unit]

  # Return with units metadata if requested ----
  if (return_units) {
    # Get the target base unit for this conversion
    target_unit <- unique(base_units[unit])

    if (length(target_unit) > 1) {
      warning(
        "Multiple base units detected. Returning numeric vector without units."
      )
      return(converted)
    }

    # Handle dimensionless quantities ----
    if (target_unit == "dimensionless") {
      # For dimensionless, we don't set units (it's just a ratio)
      attr(converted, "base_unit") <- "dimensionless"
      return(converted)
    }

    # Convert notation for units package if needed ----
    if (target_unit %in% names(units_notation)) {
      target_unit <- units_notation[target_unit]
    }

    # Convert to units object ----
    converted <- set_units(converted, target_unit, mode = "standard")
  }

  return(converted)
}


unit_map <- c(
  # Weight/weight (dry) ----
  "g/g (dry)" = "g/g_dry",
  "mg/g (dry)" = "mg/g_dry",
  "μg/g (dry)" = "ug/g_dry",
  "ug/g (dry)" = "ug/g_dry",
  "ng/g (dry)" = "ng/g_dry",
  "pg/g (dry)" = "pg/g_dry",

  # Weight/weight (wet) ----
  "g/g (wet)" = "g/g_wet",
  "mg/g (wet)" = "mg/g_wet",
  "μg/g (wet)" = "ug/g_wet",
  "ug/g (wet)" = "ug/g_wet",
  "ng/g (wet)" = "ng/g_wet",
  "pg/g (wet)" = "pg/g_wet"
)
