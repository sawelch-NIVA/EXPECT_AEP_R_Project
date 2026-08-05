# Unit standardisation.
#
# REWRITTEN 2026-08-05 after Sam asked whether the Coteur discrepancy came from
# the extraction or from here. It was both, and the pipeline half was the worse
# of the two.
#
# THE BUG, because it is worth understanding before touching any of this.
#
# The old implementation decided two things from two INDEPENDENT rules and never
# checked them against each other:
#
#   * the standard unit, from whether the string contained "dry", "wet" or "L";
#   * the conversion factor, from whether the string contained "ug".
#
# So it read only the numerator prefix and assumed the denominator was kg or L.
# For `ug/g` that assumption is wrong, and wrong by exactly the factor that makes
# it invisible: **ug/g IS mg/kg**, a 1:1 conversion, but the old code divided by
# 1000 because it saw "ug". 93 rows across 11 references came out 1000x low.
#
# A unit is now PARSED: numerator prefix and denominator are read separately, the
# factor is derived from both, and the standard unit is derived from the
# denominator rather than from a second guess at the string. Anything that does
# not parse is reported by name and count instead of silently becoming NA.

#' Micro-Sign Variants
#'
#' The micro prefix reaches this data as three different codepoints, and a fourth
#' that is not a codepoint at all:
#'
#' * `U+03BC` GREEK SMALL LETTER MU
#' * `U+00B5` MICRO SIGN
#' * plain ASCII `u`, as typed
#' * `U+FFFD` REPLACEMENT CHARACTER, i.e. a micro sign that has already been
#'   destroyed by an encoding round-trip
#'
#' **`U+FFFD` is normalised to micro, and that is a judgement rather than a
#' rule.** It occurs on 18 rows, all `2000JulshamnTraceElementLevels`, all seal
#' tissue, all reading `<U+FFFD>g/g (wet)` with values from 0.8 to 18. Read as
#' ug/g those are ordinary copper concentrations for seal tissue; read as ng/g
#' they would be a thousandfold too low for any vertebrate, and as mg/g a
#' thousandfold too high. Micro is the only reading that is not absurd.
#'
#' It is normalised here rather than left to fail because the alternative was
#' what the old code did: return `NA`, have the row dropped by
#' [drop_nonpositive_measurements()], and lose a whole reference in silence.
#' [standardise_measured_units()] warns when it does this, so the repair stays
#' visible and the underlying file can still be fixed properly.
#'
#' @return A character vector of prefixes treated as micro.
#' @export
micro_sign_variants <- function() {
  c("μ", "µ", "�", "u")
}

#' Numerator Prefixes, as a Factor to Milligrams
#'
#' @return A named numeric vector.
#' @export
unit_numerator_factors <- function() {
  c("ng" = 1e-6, "ug" = 1e-3, "mg" = 1, "g" = 1e3, "kg" = 1e6)
}

#' Denominators, as a Factor to Kilograms or Litres
#'
#' Mass and volume are kept apart because they decide different standard units,
#' not merely different factors.
#'
#' @return A list of two named numeric vectors, `mass` and `volume`.
#' @export
unit_denominator_factors <- function() {
  list(
    mass = c("kg" = 1, "g" = 1e-3, "mg" = 1e-6),
    volume = c("L" = 1, "dL" = 1e-1, "mL" = 1e-3, "uL" = 1e-6)
  )
}

#' Normalise a Unit String
#'
#' Trims, collapses internal whitespace, and maps every micro variant onto a
#' plain `u`, so downstream matching has one spelling to deal with.
#'
#' @param unit A character vector of unit strings.
#' @return A character vector the same length.
#' @export
normalise_unit_string <- function(unit) {
  out <- trimws(as.character(unit))
  out <- gsub("\\s+", " ", out)
  # Only the ones that begin a prefix; a stray replacement character elsewhere in
  # the string is left alone so it still fails to parse and gets reported.
  for (v in setdiff(micro_sign_variants(), "u")) {
    out <- gsub(paste0(v, "g"), "ug", out, fixed = TRUE)
  }
  out
}

#' Parse a Unit into a Standard Unit and a Conversion Factor
#'
#' The whole point of this function is that the numerator and the denominator are
#' read **separately**. See the note at the top of this file for what happens
#' when they are not.
#'
#' The measurement basis (dry or wet) is required for mass-per-mass units,
#' because `mg/kg (dry)` and `mg/kg (wet)` are different standard units and are
#' not interconvertible without a moisture content this project does not hold. A
#' bare `mg/kg` is therefore *not* silently assigned to either.
#'
#' @param unit A character vector of unit strings.
#' @return A tibble with one row per input: `unit`, `standard` (the standard unit
#'   or `NA`), `factor` (multiply the value by this, or `NA`), and `reason`
#'   (why it failed to parse, or `NA`).
#' @export
parse_measured_unit <- function(unit) {
  norm <- normalise_unit_string(unit)
  n <- length(unit)

  out <- tibble::tibble(
    unit = as.character(unit),
    standard = NA_character_,
    factor = NA_real_,
    reason = NA_character_
  )
  if (n == 0) {
    return(out)
  }

  num_f <- unit_numerator_factors()
  den <- unit_denominator_factors()

  for (i in seq_len(n)) {
    u <- norm[i]
    if (is.na(u) || !nzchar(u)) {
      out$reason[i] <- "empty or missing"
      next
    }
    if (identical(u, "Other")) {
      out$reason[i] <- "marked Other"
      next
    }
    parts <- strsplit(u, "/", fixed = TRUE)[[1]]
    if (length(parts) != 2) {
      out$reason[i] <- "not a ratio of two units"
      next
    }

    numerator <- trimws(parts[1])
    # The basis rides on the denominator: "kg (dry)".
    denominator <- trimws(parts[2])
    basis <- if (grepl("dry", denominator, ignore.case = TRUE)) {
      "dry"
    } else if (grepl("wet", denominator, ignore.case = TRUE)) {
      "wet"
    } else {
      NA_character_
    }
    denominator <- trimws(sub("\\s*\\(.*\\)\\s*$", "", denominator))

    if (!numerator %in% names(num_f)) {
      out$reason[i] <- paste0("unrecognised numerator '", numerator, "'")
      next
    }

    if (denominator %in% names(den$mass)) {
      if (is.na(basis)) {
        # Not guessed. Assigning a bare mg/kg to dry would silently pool it with
        # a population it may not belong to, and the difference between dry and
        # wet weight in biota is routinely a factor of four or five.
        out$reason[i] <- "mass ratio with no (dry) or (wet) basis"
        next
      }
      out$standard[i] <- paste0("mg/kg (", basis, ")")
      out$factor[i] <- unname(num_f[numerator] / den$mass[denominator])
    } else if (denominator %in% names(den$volume)) {
      out$standard[i] <- "mg/L"
      out$factor[i] <- unname(num_f[numerator] / den$volume[denominator])
    } else {
      out$reason[i] <- paste0("unrecognised denominator '", denominator, "'")
    }
  }

  out
}

#' Standardise measurement units and values
#'
#' Converts measurement values to standard units (mg/kg for dry/wet weight,
#' mg/L for concentration) by parsing each unit with [parse_measured_unit()].
#' Can convert multiple value columns that share the same unit column.
#'
#' Units that cannot be parsed produce `NA` values, as before, but are now
#' **reported by name and row count** rather than vanishing. Silence was how a
#' 1000x error survived in this function; see the note at the top of the file.
#'
#' @param data A tibble or data frame containing measurement data
#' @param value_columns Character vector. Names of columns containing measurement
#'   values (e.g., c("MEASURED_VALUE", "MEASURED_UPPER", "MEASURED_LOWER")).
#'   Default is NULL (no value conversion). If NULL, only unit standardisation occurs.
#' @param unit_column Character string. Name of column containing measurement units.
#'   Default is NULL (no unit standardisation). If NULL, only value conversion occurs.
#' @param remove_other Logical. If TRUE, removes rows where the unit column
#'   contains "Other". Default is FALSE.
#'
#' @return The input data with new columns added depending on arguments:
#'   - If `unit_column` provided: `{unit_column}_STANDARD` with standardised units
#'   - If `value_columns` provided: `{value_column}_STANDARD` for each value column
#'   If `remove_other = TRUE`, rows with "Other" units are excluded.
#'
#' @examples
#' \dontrun{
#' # Convert both units and multiple value columns
#' data |>
#'   standardise_measured_units(
#'     value_columns = c("MEASURED_VALUE", "MEASURED_UPPER", "MEASURED_LOWER"),
#'     unit_column = "MEASURED_UNIT"
#'   )
#'
#' # Only standardise units
#' data |>
#'   standardise_measured_units(unit_column = "MEASURED_UNIT")
#'
#' # Convert single value column
#' data |>
#'   standardise_measured_units(
#'     value_columns = "MEASURED_VALUE",
#'     unit_column = "MEASURED_UNIT"
#'   )
#'
#' # Remove rows marked with "Other" units
#' data |>
#'   standardise_measured_units(
#'     unit_column = "MEASURED_UNIT",
#'     remove_other = TRUE
#'   )
#' }
#'
#' @importFrom dplyr mutate case_when filter
#' @importFrom stringr str_detect
#' @importFrom cli cli_abort
#' @export
standardise_measured_units <- function(
  data,
  value_columns = NULL,
  unit_column = NULL,
  remove_other = FALSE
) {
  # Check that at least one argument is provided ----
  if (is.null(value_columns) && is.null(unit_column)) {
    cli_abort(
      "At least one of {.arg value_columns} or {.arg unit_column} must be specified"
    )
  }

  # Check that specified columns exist ----
  if (!is.null(value_columns)) {
    missing_cols <- setdiff(value_columns, names(data))
    if (length(missing_cols) > 0) {
      cli_abort("Column(s) not found in data: {.val {missing_cols}}")
    }
  }
  if (!is.null(unit_column) && !unit_column %in% names(data)) {
    cli_abort(
      "Column {.arg unit_column} ({.val {unit_column}}) not found in data"
    )
  }

  # Check remove_other requirements ----
  if (remove_other && is.null(unit_column)) {
    cli_abort(
      "{.arg unit_column} must be specified when {.code remove_other = TRUE}"
    )
  }

  # Parse every unit ONCE, on the distinct values rather than per row: there are
  # about sixteen distinct unit strings against ~90,000 rows.
  if (!is.null(unit_column)) {
    distinct_units <- unique(data[[unit_column]])
    parsed <- parse_measured_unit(distinct_units)
    idx <- match(data[[unit_column]], parsed$unit)

    unit_standard_col <- paste0(unit_column, "_STANDARD")
    data[[unit_standard_col]] <- parsed$standard[idx]
    row_factor <- parsed$factor[idx]

    # Report what could not be parsed. `Other` and empty are expected and
    # deliberate, so they are not warned about; anything else is a unit the
    # project holds data in and cannot use, which Sam needs to know rather than
    # discover as a hole in a plot.
    unusable <- parsed[
      !is.na(parsed$reason) &
        !parsed$reason %in% c("empty or missing", "marked Other"),
      ,
      drop = FALSE
    ]
    if (nrow(unusable) > 0) {
      counts <- vapply(
        unusable$unit,
        function(u) sum(data[[unit_column]] %in% u),
        integer(1)
      )
      cli::cli_warn(c(
        "{.val {length(counts)}} unit{?s} in {.field {unit_column}} could not be converted, affecting {.val {sum(counts)}} row{?s}:",
        stats::setNames(
          paste0("{.val ", unusable$unit, "} (", counts, " rows): ", unusable$reason),
          rep("*", nrow(unusable))
        ),
        "i" = "These become {.code NA} and are dropped downstream by {.fn drop_nonpositive_measurements}."
      ))
    }

    # Repaired micro signs are worth a separate line: the value IS being used,
    # on an assumption, so it must not pass silently.
    mangled <- distinct_units[
      !is.na(distinct_units) & grepl("�", distinct_units)
    ]
    mangled_ok <- mangled[!is.na(parsed$standard[match(mangled, parsed$unit)])]
    if (length(mangled_ok) > 0) {
      n_rows <- sum(data[[unit_column]] %in% mangled_ok)
      cli::cli_warn(c(
        "Repaired {.val {length(mangled_ok)}} unit string{?s} containing a corrupted micro sign, affecting {.val {n_rows}} row{?s}.",
        "i" = "Read as micro; see {.fn micro_sign_variants} for the reasoning. Fix the source encoding to remove this warning."
      ))
    }
  }

  # Convert values if value_columns provided ----
  if (!is.null(value_columns)) {
    # Need unit_column for conversion logic
    if (is.null(unit_column)) {
      cli_abort("{.arg unit_column} must be specified when converting values")
    }

    for (value_col in value_columns) {
      value_standard_col <- paste0(value_col, "_STANDARD")
      # One multiplication, with the factor already derived from BOTH halves of
      # the unit. The old version's separate case_when() over the unit string is
      # what allowed the factor and the standard unit to disagree.
      data[[value_standard_col]] <- data[[value_col]] * row_factor
    }
  }

  # Remove "Other" rows if requested ----
  if (remove_other) {
    data <- data |>
      filter(is.na(.data[[unit_column]]) | .data[[unit_column]] != "Other")
  }

  data
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
#' @importFrom rlang inherits_only
#' @importFrom cli cli_abort cli_inform
#' @export
standardise_IDate <- function(column, verbose = FALSE, char_format = "dmy") {
  if (inherits(column, "IDate")) {
    if (verbose) {
      cli_inform("Column already of class IDate.")
    }
    as.IDate(column)
  } else if (inherits_only(column, "Date")) {
    if (verbose) {
      cli_inform("Column reformatted from Date to IDate.")
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
        cli_inform("Column reformatted from character (ymd) to IDate.")
      }
      as.IDate(ymd(column))
    } else {
      if (verbose) {
        cli_inform("Column reformatted from character (dmy) to IDate.")
      }
      as.IDate(dmy(column))
    }
  } else if (inherits(column, "POSIXct") || inherits(column, "POSIXlt")) {
    if (verbose) {
      cli_inform("Column reformatted from POSIXct/POSIXlt to IDate.")
    }
    as.IDate(column)
  } else {
    cli_abort("Cannot convert column of class {.cls {class(column)}} to IDate.")
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
