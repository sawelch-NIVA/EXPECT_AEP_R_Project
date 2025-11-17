vm_joined <- arrow::read_parquet(file = "data/clean/vm_copper_to_2024.parquet")

# What is most important to look at? Let's start with matrices, compartments, sites, time periods and campaigns.

# Data preparation and sorting ----
# Count occurrences of each campaign and create ordered factor
campaign_counts <- vm_joined |>
  dplyr::mutate(SAMPLE_DATE = lubridate::dmy(SAMPLE_DATE)) |>
  # filter to after 2015 and in arctic circle
  dplyr::filter(lubridate::year(SAMPLE_DATE) >= 2015, LATITUDE >= 66.56378) |>
  dplyr::mutate(
    CAMPAIGN_DESCRIPTION_EN = ifelse(
      is.na(CAMPAIGN_DESCRIPTION_EN),
      "Other",
      CAMPAIGN_DESCRIPTION_EN
    )
  ) |>
  dplyr::count(CAMPAIGN_DESCRIPTION_EN, sort = TRUE)

# Create new labels with sample sizes
campaign_labels <- paste0(
  campaign_counts$CAMPAIGN_DESCRIPTION_EN,
  " (n=",
  campaign_counts$n,
  ")"
)

# Create factor with levels ordered by count and new labels
vm_joined_sorted_filtered <- vm_joined |>
  dplyr::mutate(
    CAMPAIGN_DESCRIPTION_EN = rev(factor(
      CAMPAIGN_DESCRIPTION_EN,
      levels = campaign_counts$CAMPAIGN_DESCRIPTION_EN,
      labels = campaign_labels
    ))
  )

# Plot creation ----
campaigns_over_time <- ggplot2::ggplot(
  data = vm_joined_sorted_filtered,
  mapping = aes(
    x = SAMPLE_DATE,
    y = CAMPAIGN_DESCRIPTION_EN,
    color = MEASURED_VALUE
  )
) +
  ggplot2::scale_y_discrete(position = "right") +
  ggplot2::geom_count() +
  ggplot2::theme(legend.position = "none")
campaigns_over_time

# Plot 2: Histogram of MEASURED_VALUE by MEASURED_UNIT ----
measured_value_boxplot <- ggplot2::ggplot(
  data = dplyr::filter(vm_joined_sorted, MEASURED_UNIT != "ng/m3"),
  mapping = aes(
    x = MEASURED_VALUE,
    y = CAMPAIGN_DESCRIPTION_EN,
    colour = ENVIRON_COMPARTMENT_SUB,
  )
) +
  ggplot2::geom_point() +
  ggplot2::scale_x_log10() +
  ggplot2::facet_wrap(~MEASURED_UNIT, scales = "free_x", ncol = 3) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    # axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )

measured_value_boxplot

# Plot 3: Frequency diagram of ENVIRON_COMPARTMENT_SUB ----
compartment_frequency <- vm_joined_sorted |>
  dplyr::count(CAMPAIGN_DESCRIPTION_EN, ENVIRON_COMPARTMENT_SUB, sort = TRUE) |>
  ggplot2::ggplot(
    mapping = aes(
      x = ENVIRON_COMPARTMENT_SUB,
      y = CAMPAIGN_DESCRIPTION_EN,
      colour = ENVIRON_COMPARTMENT_SUB,
      size = n
    )
  ) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  ) +
  ggplot2::labs(
    x = "Environmental Compartment Sub"
  )

compartment_frequency

# Alternative: if you want them side by side ----
combined_plots_horizontal <- cowplot::plot_grid(
  campaigns_over_time,
  measured_value_boxplot,
  ncol = 2,
  nrow = 1,
  labels = c("A", "B"),
  label_size = 12,
  align = "h",
  axis = "l"
)

# techniques over time
technique_count <- vm_joined_sorted |>
  dplyr::count(
    SAMPLING_PROTOCOL_ID,
    YEAR = lubridate::year(SAMPLE_DATE),
    ENVIRON_COMPARTMENT_SUB,
    sort = TRUE
  ) |>
  dplyr::filter(SAMPLING_PROTOCOL_ID != "UKJENT") |>
  dplyr::mutate(
    # Extract core code (everything before colon or version suffix)
    core_code = stringr::str_extract(SAMPLING_PROTOCOL_ID, "^[^:]+(?=:|$)"),
    # Extract version (everything after colon, if present)
    version = stringr::str_extract(SAMPLING_PROTOCOL_ID, "(?<=:).+$")
  ) |>
  ggplot(mapping = aes(x = YEAR, y = n, colour = core_code, fill = core_code)) +
  geom_col() +
  facet_wrap(facets = vars(ENVIRON_COMPARTMENT_SUB), ncol = 1)
technique_count
