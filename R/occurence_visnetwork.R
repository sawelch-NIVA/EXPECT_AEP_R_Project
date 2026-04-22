# Nodes -----------------------------------------------------------------------
source_nodes <- tibble(
  name = unique(copper_emissions_by_source_summarised$source_category),
  node_type = "source",
  colour = "orange"
)

medium_nodes <- tibble(
  name = unique(copper_emissions_by_source_summarised$medium),
  node_type = "medium",
  colour = case_when(
    name == "vann" ~ "blue",
    name == "jord" ~ "brown",
    name == "luft" ~ "lightblue"
  )
)

organism_nodes <- tibble(
  name = c("fish", "zooplankton", "algae", "shellfish"),
  node_type = "organism",
  colour = "green"
)

nodes <- bind_rows(source_nodes, medium_nodes, organism_nodes) |>
  mutate(
    id = name,
    label = name,
    group = node_type,
    value = 10 # uniform size for now; swap for e.g. n_studies later
  )

# Links -----------------------------------------------------------------------
emission_links <- copper_emissions_by_source_summarised |>
  mutate(
    from = source_category,
    to = medium,
    value = mean_sum_emissions_kg,
    edge_type = "emission"
  ) |>
  select(from, to, value, edge_type)

# Putative medium -> organism links
# Adjust as your domain knowledge dictates
exposure_links <- tribble(
  ~from  , ~to           , ~value , ~edge_type ,
  "vann" , "fish"        , NA     , "exposure" ,
  "vann" , "zooplankton" , NA     , "exposure" ,
  "vann" , "algae"       , NA     , "exposure" ,
  "vann" , "shellfish"   , NA     , "exposure" ,
  "jord" , "algae"       , NA     , "exposure" ,
)

links <- bind_rows(emission_links, exposure_links) |>
  mutate(
    dashes = edge_type == "exposure", # putative edges dashed
    width = case_when(
      edge_type == "emission" ~ log1p(replace_na(value, 1)) / 3,
      TRUE ~ 1
    )
  )

# Plot ------------------------------------------------------------------------
visNetwork(nodes, links, width = "100%", height = "800px") |>
  visEdges(arrows = "to", smooth = TRUE) |>
  visGroups(groupname = "source", color = "orange") |>
  visNodes(shape = "box") |>
  visGroups(groupname = "medium", color = "steelblue") |>
  visGroups(groupname = "organism", color = "forestgreen") |>
  visOptions(highlightNearest = TRUE) |>
  visLegend() |>
  visLayout(randomSeed = 42) |>
  visConfigure(enabled = TRUE)
