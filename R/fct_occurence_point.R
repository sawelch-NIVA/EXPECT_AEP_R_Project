# plot_data <- tar_read(load_litearture_pqt) |>
#   filter(
#     ENVIRON_COMPARTMENT == "Biota",
#     !is.na(MEASURED_N)
#   )

# plot_data |>
#   ggplot(aes(
#     x = SAMPLE_SPECIES,
#     y = MEASURED_VALUE,
#     colour = MEASURED_N,
#     size = MEASURED_N
#   )) +
#   geom_point() +
#   facet_wrap(
#     facets = vars(SPECIES_GROUP),
#     scales = "free_x",
#     space = "free_x"
#   ) +
#   theme(axis.text.x = element_text(angle = 90), legend.position = "bottom") +
#   scale_x_discrete() +
#   scale_fill_viridis()
