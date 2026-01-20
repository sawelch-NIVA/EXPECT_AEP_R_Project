library(ggrepel)

tar_load(names = c(creed_scores_data, measurements_data))
creed_sample_size <- creed_scores_data |>
  left_join(
    measurements_data |>
      group_by(REFERENCE_ID) |>
      reframe(Sample_Size = sum(MEASURED_N, na.rm = TRUE))
  ) |>
  select(-source_file, -read_timestamp)

woe_summary <- creed_sample_size |>
  mutate(
    highest_reliability = case_when(
      GOLD_RELIABILITY == "Reliable without restrictions" ~ 5,
      GOLD_RELIABILITY == "Reliable with restrictions" ~ 4,
      SILVER_RELIABILITY == "Reliable without restrictions" ~ 3,
      SILVER_RELIABILITY == "Reliable with restrictions" ~ 2,
      SILVER_RELIABILITY == "Not usable" ~ 1,
      .default = NA
    ),
    highest_relevance = case_when(
      GOLD_RELEVANCE == "Relevant without restrictions" ~ 5,
      GOLD_RELEVANCE == "Relevant with restrictions" ~ 4,
      SILVER_RELEVANCE == "Relevant without restrictions" ~ 3,
      SILVER_RELEVANCE == "Relevant with restrictions" ~ 2,
      SILVER_RELEVANCE == "Not usable" ~ 1,
      .default = NA
    ),
    creed_quotient = (highest_reliability + highest_relevance) / 10,
    log_sample_size = log10(Sample_Size),
    reference_pretty = str_replace(
      REFERENCE_ID,
      "^(\\d{4})([A-Z][a-z]*).*",
      "\\2\n(\\1)"
    )
  )


woe_summary |>
  group_by(highest_relevance, highest_reliability) |>
  mutate(
    n_in_cell = n(),
    cell_id = row_number(),
    # Create a grid within each cell
    grid_cols = ceiling(sqrt(n_in_cell)),
    x_offset = ((cell_id - 1) %% grid_cols) / (grid_cols + 1) - 0.3,
    y_offset = floor((cell_id - 1) / grid_cols) / (grid_cols + 1) - 0.3
  ) |>
  ungroup() |>
  ggplot(aes(
    x = highest_relevance + x_offset,
    y = highest_reliability + y_offset,
    size = Sample_Size,
    color = creed_quotient,
    label = reference_pretty
  )) +
  geom_vline(xintercept = 2, color = "grey") +
  geom_vline(xintercept = 4, color = "gold") +
  geom_hline(yintercept = 2, color = "grey") +
  geom_hline(yintercept = 4, color = "gold") +
  geom_point(alpha = 0.8) +
  geom_text_repel(size = 3, max.overlaps = 20, color = "black") +

  scale_size_continuous(range = c(2, 12), trans = "log10") +
  scale_color_viridis_b(begin = 0, end = 1) +
  scale_x_continuous(breaks = 1:5, limits = c(0.5, 5.5)) +
  scale_y_continuous(breaks = 1:5, limits = c(0.5, 5.5)) +

  labs(
    x = "Relevance",
    y = "Reliability",
    size = "Sample Size",
    color = "CREED Quotient \n(Relevance + Reliability)/10"
  ) +
  theme_minimal()
