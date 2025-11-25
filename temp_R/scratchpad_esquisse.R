if (!exists("requirements_loaded")) {
  source("temp_R/scratchpad.r")
  load_all_requirements()
}

esquisse::esquisser(data = generate_copper_thresholds())

# Shitty boxplot
ggplot(main_table) +
  aes(
    x = ENVIRON_COMPARTMENT_SUB,
    y = MEASURED_VALUE_STANDARD,
    fill = COUNTRY_ISO
  ) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(
    vars(MEASURED_UNIT_STANDARD),
    scales = "free",
    ncol = 3L
  )
