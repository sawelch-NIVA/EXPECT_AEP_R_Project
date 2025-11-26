load_all_requirements()

# Extract ordered compartment values from vocabulary ----
compartment_order <- unlist(
  environ_compartments_sub_vocabulary(),
  use.names = FALSE
)

# Create plot with ordered axes ----
main_table |>
  group_by(REFERENCE_ID, ENVIRON_COMPARTMENT_SUB) |>
  distinct() |>
  reframe(count = n()) |>
  # Add factor levels for x-axis ordering
  mutate(
    ENVIRON_COMPARTMENT_SUB = factor(
      ENVIRON_COMPARTMENT_SUB,
      levels = compartment_order
    )
  ) |>
  # Join YEAR data for y-axis ordering
  left_join(main_table |> distinct(REFERENCE_ID, YEAR), by = "REFERENCE_ID") |>
  mutate(REFERENCE_ID = fct_reorder(REFERENCE_ID, YEAR)) |>
  ggplot(aes(y = REFERENCE_ID, x = ENVIRON_COMPARTMENT_SUB, fill = count)) +
  geom_tile() +
  geom_text(aes(
    label = count,
    color = if_else(count > mean(count), "black", "white")
  )) +
  scale_fill_viridis(name = "Sample Size (n)") +
  scale_color_identity() + # Use the calculated colors directly
  theme_aep_plots() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_blank()
  ) +
  labs(x = "Environmental Sub-Compartment")


main_table |>
  left_join(tar_read(reference_data)) |>
  left_join(tar_read(campaign_data)) |>
  group_by(
    CAMPAIGN_NAME,
    REFERENCE_ID,
    TITLE,
    YEAR,
    AUTHOR,
    CAMPAIGN_COMMENT
  ) |>
  reframe(SUM_N = sum(MEASURED_N))
