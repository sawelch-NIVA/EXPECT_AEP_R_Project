# # Lots of function wrappers around lots of pointblank validations

# library(pointblank)

# # hell of a name
# validate_vm_join_sites_measurements_lookup <- function(
#   vm_join_sites_measurements_lookup
# ) {
#   vm_join_sites_measurements_lookup |>
#     col_vals_not_null(
#       columns = vars(
#         Vannlokalitetsnavn,
#         Medium_id,
#         Registrerings_id,
#         `UTM33 Ost (X)`,
#         `UTM33 Nord (Y)`
#       )
#     ) |>
#     col_vals_in_set(
#       columns = vars(Medium_id),
#       set = vm_lookup_medium$MediumID,
#       label = "All Medium_ids have lookup matches",
#       actions = validation_config$lookup
#     ) |>
#     col_vals_in_set(
#       columns = vars(Enhet_id),
#       set = vm_lookup_units$UnitID,
#       label = "All Enhet_ids have lookup matches",
#       actions = validation_config$lookup
#     ) |>
#     col_vals_in_set(
#       columns = vars(Aktivitet_id),
#       set = vm_lookup_campaigns$ActivityID,
#       label = "All Aktivitet_ids have lookup matches",
#       actions = validation_config$lookup
#     ) |>
#     col_vals_in_set(
#       columns = vars(Vannkategori),
#       set = vm_lookup_vannkategori$VannkategoriID,
#       label = "All Vannkategori have lookup matches",
#       actions = validation_config$lookup
#     )
# }

# library(targets)

tar_load(vm_raw_copper)
tar_load(vm_raw_sites)
tar_load_everything()

tar_load(contains("vm_"))

vm_raw_sites |> pointblank::col_schema()

tt_summary_stats(vm_raw_copper)

# so - we need an easy-ass way to make a simple col schema for the input files
# I think we can pursue this approach up until we convert them to Vm data...
# though I suppose there's no point in doing it then, actually
# cos we're just going to do it again afterwards for all the eData
