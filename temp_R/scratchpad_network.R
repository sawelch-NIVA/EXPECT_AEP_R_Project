if (!exists("requirements_loaded")) {
  source("temp_R/scratchpad.r")
  load_all_requirements()
}

# Network Diagram ----
unique_compartments_and_sites <- main_table |>
  select(
    SITE_GEOGRAPHIC_FEATURE,
    SITE_GEOGRAPHIC_FEATURE_SUB,
    ENVIRON_COMPARTMENT,
    ENVIRON_COMPARTMENT_SUB
  ) |>
  distinct() |>
  filter(if_all(everything(), ~ !is.na(.)))

unique_compartments <- main_table |>
  select(
    ENVIRON_COMPARTMENT,
    ENVIRON_COMPARTMENT_SUB
  ) |>
  distinct() |>
  filter(if_all(everything(), ~ !is.na(.)))

unique_compartments_and_species <- main_table |>
  select(
    ENVIRON_COMPARTMENT,
    ENVIRON_COMPARTMENT_SUB,
    SAMPLE_SPECIES
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

# Create edge list (from -> to relationships)
# Method 1: Explicit binding ----
edges <- bind_rows(
  unique_compartments |>
    select(from = ENVIRON_COMPARTMENT, to = ENVIRON_COMPARTMENT_SUB)
) |>
  distinct() |>
  filter(!is.na(from), !is.na(to), from != "Not relevant", to != "Not relevant")


# Then plot
graph <- as_tbl_graph(edges)

ggraph(graph, layout = 'tree') +
  geom_edge_link() +
  geom_node_point(size = 3) +
  geom_node_text(aes(label = name), hjust = 0, nudge_x = 0.1, size = 3) +
  theme_void()
