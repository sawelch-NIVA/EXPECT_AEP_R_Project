# Temp

#' Run pointblank validation on a Samples table (masked sample regex)
#'
#' Applies pointblank validation rules to check data quality and schema
#' compliance for an eData Samples table. Checks that core identifier fields
#' are non-null, that environmental compartment fields contain values within
#' their controlled vocabularies, and that each `ENVIRON_COMPARTMENT_SUB` value
#' is consistent with its corresponding `ENVIRON_COMPARTMENT` parent.
#'
#' @param data Data frame containing Samples table data to validate
#' @param actions Action levels for pointblank agent (only used when `agent = TRUE`)
#' @param agent Logical. If `TRUE` (default), returns a pointblank agent object.
#'   If `FALSE`, returns the validated data with validation failures removed.
#'
#' @return If `agent = TRUE`, a pointblank agent object containing validation results.
#'   If `agent = FALSE`, the input data with validation failures removed.
#'
#' @details
#' Validation thresholds are configured using [pointblank::action_levels()].
#'
#' @seealso [pb_validate_edata_table()] for the underlying validation framework,
#'   [pb_validate_all_edata_tables()] to validate all tables at once,
#'   [environ_compartments_sub_vocabulary()] for the compartment hierarchy used in
#'   the consistency check,
#'   [example_samples_tibble()] for an example Samples table.
#'
#' @family validation
#'
#' @importFrom pointblank col_vals_not_null col_vals_in_set action_levels
#' @importFrom purrr flatten
#' @importFrom dplyr filter
#' @import eDataDRF
#' @export
#'
#' @examples
#' \dontrun{
#' pb_validate_samples(example_samples_tibble())
#' }
pb_validate_samples <- function(
  data,
  actions = action_levels(),
  agent = TRUE
) {
  compartment_sub_vocab <- environ_compartments_sub_vocabulary()

  # Only compartments that actually have sub-values defined
  non_null_compartments <- Filter(Negate(is.null), compartment_sub_vocab)

  apply_validations <- function(x) {
    # Start with core identifier and flat vocabulary checks
    # TODO: Missing labels
    agent_or_data <- x |>
      # Check SAMPLE_ID matches the format produced by generate_sample_id_with_components()
      # See sample_id_regex() for the pattern definition
      # col_vals_regex(
      #   label = "Check SAMPLE_ID matches sample_id_regex()",
      #   columns = SAMPLE_ID,
      #   regex = sample_id_regex(),
      #   actions = actions
      # ) |>
      col_vals_not_null(
        label = "Check SITE_CODE is not blank",
        columns = SITE_CODE,
        actions = actions
      ) |>
      # FIXME: This will obviously fail on new parameters. Will need a more intelligent approach in time.
      col_vals_in_set(
        label = "Check PARAMETER_NAME is in parameters_vocabulary()$PARAMETER_NAME",
        columns = PARAMETER_NAME,
        set = parameters_vocabulary() |> pull(PARAMETER_NAME),
        actions = actions
      ) |>
      # Environmental compartments
      col_vals_in_set(
        label = "Check ENVIRON_COMPARTMENT is in environ_compartments_vocabulary()",
        columns = ENVIRON_COMPARTMENT,
        set = environ_compartments_vocabulary(),
        actions = actions
      ) |>
      col_vals_in_set(
        label = "Check ENVIRON_COMPARTMENT_SUB is in environ_compartments_sub_vocabulary()",
        columns = ENVIRON_COMPARTMENT_SUB,
        set = environ_compartments_sub_vocabulary() |> flatten(),
        actions = actions
      )

    # Add one parent-child consistency check per non-NULL compartment
    for (comp in names(non_null_compartments)) {
      valid_subs <- non_null_compartments[[comp]]
      agent_or_data <- agent_or_data |>
        col_vals_in_set(
          label = paste("Check ENVIRON_COMPARTMENT_SUB is valid for", comp),
          columns = ENVIRON_COMPARTMENT_SUB,
          preconditions = (function(comp) {
            \(x) x |> filter(ENVIRON_COMPARTMENT == comp)
          })(comp),
          set = valid_subs,
          actions = actions
        )
    }

    agent_or_data
  }

  pb_validate_edata_table(
    data = data,
    table_name = "Samples",
    validation_steps = apply_validations,
    agent = agent,
    actions = actions
  )
}
