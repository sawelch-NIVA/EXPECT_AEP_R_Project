#' Slugify Strings into Target-Name-Safe Fragments
#'
#' Converts arbitrary strings (species names, compartment names, etc.) into
#' syntactically valid fragments (letters, digits, underscores only) suitable
#' for use in generated `targets` names.
#'
#' @param x A character vector.
#' @return A character vector of slugified strings, made unique.
slugify_name <- function(x) {
  x |>
    stringr::str_replace_all("[^A-Za-z0-9]+", "_") |>
    stringr::str_replace_all("^_+|_+$", "") |>
    make.unique(sep = "_")
}

#' Get Compartment / Sub-Compartment Outlier Groups
#'
#' Identifies all distinct ENVIRON_COMPARTMENT x ENVIRON_COMPARTMENT_SUB
#' combinations present in `data`. Intended as the `values` argument to a
#' `tarchetypes::tar_map()` outlier-analysis factory: column names are
#' prefixed with `.` so they never collide with the real data columns
#' referenced (via NSE) inside the generated targets.
#'
#' @param data A data frame with ENVIRON_COMPARTMENT and
#'   ENVIRON_COMPARTMENT_SUB columns (e.g. `load_literature_pqt`).
#' @return A tibble with one row per group: `.compartment`,
#'   `.subcompartment`, and a unique `.group_name` target-name fragment.
#' @export
get_compartment_groups <- function(data) {
  data |>
    dplyr::filter(
      !is.na(ENVIRON_COMPARTMENT),
      !is.na(ENVIRON_COMPARTMENT_SUB)
    ) |>
    dplyr::distinct(
      .compartment = ENVIRON_COMPARTMENT,
      .subcompartment = ENVIRON_COMPARTMENT_SUB
    ) |>
    dplyr::arrange(.compartment, .subcompartment) |>
    dplyr::mutate(
      .group_name = slugify_name(paste(.compartment, .subcompartment))
    )
}

#' Get Biota Category / Species / Tissue Outlier Groups
#'
#' Identifies all distinct SPECIES_GROUP x SAMPLE_SPECIES x SAMPLE_TISSUE
#' combinations present in `data`, restricted to the Biota compartment.
#' Intended as the `values` argument to a `tarchetypes::tar_map()`
#' outlier-analysis factory (see [get_compartment_groups()] for the naming
#' rationale).
#'
#' @param data A data frame with ENVIRON_COMPARTMENT, SPECIES_GROUP,
#'   SAMPLE_SPECIES and SAMPLE_TISSUE columns (e.g. `load_literature_pqt`).
#' @return A tibble with one row per group: `.species_group`, `.species`,
#'   `.tissue`, and a unique `.group_name` target-name fragment.
#' @export
get_biota_groups <- function(data) {
  data |>
    dplyr::filter(
      ENVIRON_COMPARTMENT == "Biota",
      !is.na(SPECIES_GROUP),
      !is.na(SAMPLE_SPECIES),
      !is.na(SAMPLE_TISSUE)
    ) |>
    dplyr::distinct(
      .species_group = SPECIES_GROUP,
      .species = SAMPLE_SPECIES,
      .tissue = SAMPLE_TISSUE
    ) |>
    dplyr::arrange(.species_group, .species, .tissue) |>
    dplyr::mutate(
      .group_name = slugify_name(paste(.species_group, .species, .tissue))
    )
}

#' Subset Data to One Compartment / Sub-Compartment Group
#'
#' @param data A data frame (e.g. `load_literature_pqt`).
#' @param compartment,subcompartment Literal values (not column names) to
#'   filter on, e.g. as substituted in by `tarchetypes::tar_map()`.
#' @return A filtered data frame, with rows missing a measured value removed.
#' @export
prepare_compartment_group_data <- function(data, compartment, subcompartment) {
  data |>
    dplyr::filter(
      .data$ENVIRON_COMPARTMENT == compartment,
      .data$ENVIRON_COMPARTMENT_SUB == subcompartment,
      !is.na(.data$MEASURED_VALUE_STANDARD)
    )
}

#' Subset Data to One Biota Category / Species / Tissue Group
#'
#' @param data A data frame (e.g. `load_literature_pqt`).
#' @param species_group,species,tissue Literal values (not column names) to
#'   filter on, e.g. as substituted in by `tarchetypes::tar_map()`.
#' @return A filtered data frame, with rows missing a measured value removed.
#' @export
prepare_biota_group_data <- function(data, species_group, species, tissue) {
  data |>
    dplyr::filter(
      .data$SPECIES_GROUP == species_group,
      .data$SAMPLE_SPECIES == species,
      .data$SAMPLE_TISSUE == tissue,
      !is.na(.data$MEASURED_VALUE_STANDARD)
    )
}
