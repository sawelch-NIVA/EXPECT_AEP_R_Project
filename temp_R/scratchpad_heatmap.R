if (!exists("requirements_loaded")) {
  source("temp_R/scratchpad.r")
  load_all_requirements()
}

# Network Diagram ----
unique_compartments <- main_table |>
  select(
    SITE_GEOGRAPHIC_FEATURE,
    SITE_GEOGRAPHIC_FEATURE_SUB,
    ENVIRON_COMPARTMENT,
    ENVIRON_COMPARTMENT_SUB
  ) |>
  distinct() |>
  filter(if_all(everything(), ~ !is.na(.)))


# https://ggraph.data-imaginist.com/
# lots of real cool options here
fake_compartments <- tibble(
  from = c(
    "wwtp",
    "STD",
    "natural weathering",
    "sediment",
    "water",
    "sediment",
    "benthic invertebrates"
  ),
  to = c(
    "water",
    "sediment",
    "water",
    "water",
    "sediment",
    "benthic invertebrates",
    "predators"
  )
)

# Fake COmpartments Graph ----
fake_compartments_graph <- as_tbl_graph(fake_compartments)

# plot using ggraph
ggraph(graph, layout = "graphopt") +
  geom_edge_bundle_force(
    aes(label = "text"),
    angle_calc = 'along',
    label_dodge = unit(2.5, 'mm'),
    arrow = arrow(length = unit(4, 'mm')),
    end_cap = circle(3, 'mm')
  ) +
  geom_node_point(size = 8, colour = "skyblue") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white')

# Real COmpartments Graph ----
real_compartments_graph <- as_tbl_graph(unique_compartments)

ggraph(real_compartments_graph, "tree") +
  geom_edge_diagonal()
