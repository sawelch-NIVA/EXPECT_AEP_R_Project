options(repr.plot.res = 600)

# Helper: parse REFERENCE_ID into a two-line y-axis label ----
format_ref_label <- function(id) {
  if (str_detect(id, "\\d{4}(-\\d{4})?")) {
    year_part <- str_extract(id, "\\d{4}(-\\d{4})?")
    remainder <- str_remove(id, "\\d{4}(-\\d{4})?")
    glue("**{remainder}**<br>{year_part}")
  } else {
    str_wrap(id, width = 40)
  }
}

# Prepare data for plotting ----
plot_data <- literature_merged_data |>
  group_by(REFERENCE_ID, ENVIRON_COMPARTMENT_SUB) |>
  distinct() |>
  reframe(count = sum(MEASURED_N, na.rm = TRUE)) |>
  left_join(
    literature_merged_data |> distinct(REFERENCE_ID, YEAR),
    by = "REFERENCE_ID"
  ) |>
  filter(!is.na(ENVIRON_COMPARTMENT_SUB), ENVIRON_COMPARTMENT_SUB != "") |>
  mutate(label_2line = map_chr(REFERENCE_ID, format_ref_label))

# Calculate text color threshold (on log10 scale) ----
threshold_value <- mean(
  log10(plot_data$count[plot_data$count > 0]),
  na.rm = TRUE
)

# Calculate total samples per reference for ordering (y-axis) ----
reference_order <- plot_data |>
  group_by(REFERENCE_ID) |>
  summarise(total = sum(count)) |>
  arrange(total) |>
  pull(REFERENCE_ID)

# Derive label order from reference_order ----
label_order <- map_chr(reference_order, format_ref_label)

# Calculate total samples per compartment for ordering (x-axis) ----
compartment_order <- plot_data |>
  group_by(ENVIRON_COMPARTMENT_SUB) |>
  summarise(total = sum(count)) |>
  arrange(desc(total)) |>
  pull(ENVIRON_COMPARTMENT_SUB)

# helper
wrap_at_slash <- function(x, width = 15) {
  x |>
    stringr::str_replace_all("/", " /") |>
    stringr::str_wrap(width = width)
}


# Part 1 ----
sub_compartment_coverage <- literature_merged_data |>
  filter(!str_detect("Biota", ENVIRON_COMPARTMENT)) |>
  mutate(year_sampled = year(SAMPLING_DATE)) |>
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
  scale_y_discrete(labels = wrap_at_slash) +
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
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor.x = element_line(linewidth = 0.1, colour = "lightgrey"),
    strip.placement = "outside"
  )
# Part 2 ----

# calculate number of spp. across each group for the whole period separately
SPECIES_GROUP_n_tbl <- literature_merged_data |>
  filter(!is.na(SPECIES_GROUP)) |>
  reframe(
    SPECIES_GROUP_n = glue(
      "{SPECIES_GROUP}<br>({n_distinct(SAMPLE_SPECIES)} spp.)"
    ),
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
  scale_y_discrete(labels = wrap_at_slash) +
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
    values = c(0.6, 1)
  ) +
  theme_minimal(base_size = 11) +
  labs(
    x = "Sampling Date (year)",
    y = "",
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
    strip.placement = "outside"
  )


sub_compartment_coverage /
  org_coverage +
  plot_layout(guides = "collect", axis_titles = "collect") +
  plot_annotation(tag_levels = "a", tag_suffix = ")")
