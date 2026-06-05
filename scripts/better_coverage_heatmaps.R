# Part 1 ----
sub_compartment_coverage <- literature_merged_data |>
  filter(!str_detect("Biota", ENVIRON_COMPARTMENT)) |>
  mutate(
    year_sampled = year(SAMPLING_DATE),
    ENVIRON_COMPARTMENT_SUB = str_replace_all(
      ENVIRON_COMPARTMENT_SUB,
      "/",
      " /"
    )
  ) |>
  group_by(year_sampled, ENVIRON_COMPARTMENT_SUB, ENVIRON_COMPARTMENT) |>
  reframe(
    sum_MEASURED_N = sum(MEASURED_N),
    sum_lines_of_evidence = n_distinct(REFERENCE_ID)
  ) |>

  ggplot(aes(
    x = year_sampled,
    y = ENVIRON_COMPARTMENT_SUB,
  )) +
  geom_point(aes(colour = sum_MEASURED_N, size = sum_MEASURED_N)) +
  scale_size_binned_area(
    max_size = 15,
    name = "Sample size",
    guide = "legend",
    limits = c(1, 10000),
    n.breaks = 5,
    transform = "log10"
  ) +
  scale_colour_viridis_b(
    name = "Sample size",
    guide = "legend",
    limits = c(1, 10000),
    n.breaks = 5,
    transform = "log10"
  ) +
  scale_y_discrete(labels = ENVIRON_COMPARTMENT_SUB) +
  ggnewscale::new_scale_colour() +
  geom_text(aes(
    label = sum_lines_of_evidence,
    color = sum_MEASURED_N
  )) +
  scale_colour_stepsn(
    colours = c("white", "black"),
    guide = "none",
    values = c(0, 1),
    n.breaks = 5
  ) +
  scale_x_continuous(
    minor_breaks = 1985:2025,
    limits = c(1985, 2025),
    position = "top"
  ) +
  facet_wrap(
    facets = vars(ENVIRON_COMPARTMENT),
    ncol = 1,
    scales = "free_y",
    space = "free_y",
    strip.position = "left"
  ) +
  labs(
    x = "Sampling Date (year)",
    y = "",
    colour = "Sample size",
    size = "Sample size"
  ) +
  theme(
    panel.grid.minor.x = element_line(linewidth = 0.1, colour = "lightgrey"),
    strip.placement = "outside",
    panel.background = element_blank(),
    strip.background = element_blank()
  )
# Part 2 ----

# calculate number of spp. across each group for the whole period separately
SPECIES_GROUP_n_tbl <- literature_merged_data |>
  filter(!is.na(SPECIES_GROUP)) |>
  reframe(
    SPECIES_GROUP_n = glue(
      "{SPECIES_GROUP} <br> ({n_distinct(SAMPLE_SPECIES)} spp.)"
    ) |>
      # line break doesn't work... why!?
      as.character(),
    .by = SPECIES_GROUP
  ) |>
  distinct()


org_coverage <- literature_merged_data |>
  filter(!is.na(SPECIES_GROUP)) |>
  mutate(
    year_sampled = year(SAMPLING_DATE),
    SPECIES_SUPERGROUP = recode_values(
      SPECIES_GROUP,
      c("Fish", "Mammals", "Birds") ~ "Vertebrates",
      c("Moss/Hornworts", "Plants", "Algae") ~ "Archaeplastida",
      c("Other", "Ecosystem") ~ "Other",
      c("Worms", "Molluscs", "Invertebrates", "Crustaceans") ~ "Invertebrates",
      unmatched = "error",
    )
  ) |>
  group_by(year_sampled, SPECIES_GROUP, SPECIES_SUPERGROUP) |>
  reframe(
    sum_MEASURED_N = sum(MEASURED_N),
    sum_lines_of_evidence = n_distinct(REFERENCE_ID),
    distinct_species = n_distinct(SAMPLE_SPECIES),
  ) |>
  left_join(SPECIES_GROUP_n_tbl, by = "SPECIES_GROUP") |>
  ggplot(aes(
    x = year_sampled,
    y = SPECIES_GROUP_n,
  )) +
  geom_point(aes(colour = sum_MEASURED_N, size = sum_MEASURED_N)) +
  scale_x_continuous(minor_breaks = 1985:2025, limits = c(1985, 2025)) +
  scale_y_discrete() +
  scale_size_binned_area(
    max_size = 15,
    name = "Sample size",
    guide = "legend",
    limits = c(1, 10000),
    transform = "log10",
    n.breaks = 5
  ) +
  scale_colour_viridis_b(
    name = "Sample size",
    guide = "legend",
    n.breaks = 5,
    limits = c(1, 10000),
    transform = "log10"
  ) +
  ggnewscale::new_scale_colour() +
  geom_text(aes(
    label = sum_lines_of_evidence,
    color = sum_MEASURED_N
  )) +
  scale_colour_stepsn(
    colours = c("white", "black"),
    guide = "none",
    values = c(0, 1),
    n.breaks = 5
  ) +
  labs(
    x = "Sampling Date (year)",
    colour = "Sample size",
    size = "Sample size"
  ) +
  facet_wrap(
    facets = vars(SPECIES_SUPERGROUP),
    ncol = 1,
    scales = "free_y",
    space = "free_y",
    strip.position = "left"
  ) +
  theme(
    panel.grid.minor.x = element_line(linewidth = 0.1, colour = "lightgrey"),
    axis.text.y = element_markdown(),
    axis.title.y = element_blank(),
    strip.placement = "outside",
    panel.background = element_blank(),
    strip.background = element_blank()
  )


# And then, papers
# per paper we maybe want to check:
# does it cover a lot of years?
# does it cover a lot of compartments
# does it cover a lot of species groups?
# is it, after CREED, a good paper?
# TODO: Get CREED scores for all papers
literature_reference_data <- tar_read(reference_data)
# x axis label lookup ----
col_labels <- c(
  years_coverage = "Years\ncoverage",
  n_samples = "N samples",
  n_matrices = "N matrices",
  n_species_groups = "N species\ngroups",
  CREED_score = "CREED\nscore"
)

# ref ordering by n_samples ----
ref_order <- source_coverage |>
  filter(name == "n_samples") |>
  arrange(value) |>
  pull(ref_id_short)

source_coverage_image <- source_coverage |>
  mutate(
    # bold author, line break before year
    ref_id_short = str_replace(
      ref_id_short,
      "^(\\w+) \\((\\d+)\\)$",
      "**\\1**<br>(\\2)"
    ),
    # apply ordering
    ref_id_short = factor(
      ref_id_short,
      levels = str_replace(
        ref_order,
        "^(\\w+) \\((\\d+)\\)$",
        "**\\1**<br>(\\2)"
      )
    )
  ) |>
  group_by(name) |>
  mutate(
    value_scaled = scales::rescale(log1p(value), to = c(0, 1))
  ) |>
  ggplot(aes(x = name, y = ref_id_short, fill = value_scaled)) +
  geom_label(
    aes(label = value, fill = value_scaled),
    size = 3,
    colour = "black",
    hjust = 0.5,
    label.padding = unit(0.5, "lines"),
    border.colour = NA,
    label.r = unit(0.5, "lines")
  ) +
  scale_x_discrete(labels = col_labels, expand = FALSE, position = "top") +
  scale_fill_binned(
    palette = "RdYlGn",
    name = "Scaled value",
    breaks = c(0, 0.25, 0.5, 0.75, 1)
  ) +
  scale_colour_stepsn(
    colours = c("white", "black"),
    guide = "none",
    values = c(0, 1),
    n.breaks = 5
  ) +
  # theme_minimal(base_size = 11) +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_markdown(), # TODO: Failing because of the theme_minimal call?
    panel.background = element_blank(),
    axis.text.x.top = legend.position = "none",
    strip.background = element_blank()
  )


left_panel <- (sub_compartment_coverage / org_coverage) +
  plot_layout(guides = "collect", axis_titles = "collect")

(left_panel | source_coverage_image) +
  plot_layout(widths = c(1, 0.1)) +
  plot_annotation(tag_levels = "a", tag_suffix = ")")
