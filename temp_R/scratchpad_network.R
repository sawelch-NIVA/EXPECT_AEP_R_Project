# Shiny App for Interactive Network Visualization ----
# Purpose: Test different ggraph layouts and aesthetics for copper flow network
# with data-driven nodes from measurements

# Libraries ----
library(shiny)
library(tidyverse)
library(ggraph)
library(tidygraph)
library(igraph)
library(glue)

# Data Preparation ----

# Simulated measurement data (replace with your actual data) ----
# This mimics the structure of your unique_compartments_and_sites
unique_compartments_and_sites <- tribble(
  ~SITE_GEOGRAPHIC_FEATURE            , ~SITE_GEOGRAPHIC_FEATURE_SUB , ~ENVIRON_COMPARTMENT , ~ENVIRON_COMPARTMENT_SUB      , ~sum_n ,
  "Bare land and lichen/moss"         , "Other"                      , "Biota"              , "Biota, Terrestrial"          ,     19 ,
  "Coastal, fjord"                    , "Not relevant"               , "Biota"              , "Biota, Aquatic"              ,     79 ,
  "Coastal, fjord"                    , "Other"                      , "Aquatic"            , "Aquatic Sediment"            ,      7 ,
  "Coastal, fjord"                    , "Other"                      , "Biota"              , "Biota, Aquatic"              ,      6 ,
  "Coastal, fjord"                    , "Water benthos"              , "Biota"              , "Biota, Aquatic"              ,    161 ,
  "Coastal, fjord"                    , "Water column, pelagic zone" , "Biota"              , "Biota, Aquatic"              ,    150 ,
  "Drainage, sewer, artificial water" , "Water surface"              , "Aquatic"            , "Aquatic Sediment"            ,     17 ,
  "Drainage, sewer, artificial water" , "Water surface"              , "Aquatic"            , "Freshwater"                  ,      3 ,
  "Drainage, sewer, artificial water" , "Water surface"              , "Aquatic"            , "Groundwater"                 ,      2 ,
  "Drainage, sewer, artificial water" , "Water surface"              , "Terrestrial"        , "Soil O Horizon (Organic)"    ,     16 ,
  "Estuary"                           , "Water benthos"              , "Aquatic"            , "Brackish/Transitional Water" ,      1 ,
  "Ocean, sea, territorial waters"    , "Water benthos"              , "Aquatic"            , "Aquatic Sediment"            ,      1 ,
  "Ocean, sea, territorial waters"    , "Water benthos"              , "Aquatic"            , "Brackish/Transitional Water" ,      1 ,
  "Ocean, sea, territorial waters"    , "Water column pelagic zone"  , "Biota"              , "Biota, Aquatic"              ,      5 ,
  "Ocean, sea, territorial waters"    , "Water column, pelagic zone" , "Biota"              , "Biota, Aquatic"              ,    460 ,
  "Ocean, sea, territorial waters"    , "Water surface"              , "Biota"              , "Biota, Aquatic"              ,     36 ,
  "River, stream, canal"              , "Other"                      , "Aquatic"            , "Freshwater"                  ,     31 ,
  "River, stream, canal"              , "Other"                      , "Biota"              , "Biota, Aquatic"              ,     11 ,
  "River, stream, canal"              , "Water surface"              , "Aquatic"            , "Freshwater"                  ,      3 ,
  "River, stream, canal"              , "Water surface"              , "Terrestrial"        , "Soil O Horizon (Organic)"    ,     24 ,
  "WWTP"                              , "Other"                      , "Aquatic"            , "Sludge"                      ,     11 ,
  "WWTP"                              , "Other"                      , "Aquatic"            , "Wastewater"                  ,      5
) |>
  mutate(
    compartment_name_n = glue("{ENVIRON_COMPARTMENT_SUB}\n(n={sum_n})"),
    node_id = glue(
      "{ENVIRON_COMPARTMENT_SUB}_{SITE_GEOGRAPHIC_FEATURE}_{SITE_GEOGRAPHIC_FEATURE_SUB}"
    )
  )

# Theoretical copper flow edges ----
theoretical_edges <- tribble(
  ~from                         , ~to                           , ~flow_type                , ~plausibility , ~magnitude   , ~evidence_quality , ~has_data ,

  # Atmospheric deposition ----
  "Atmospheric"                 , "Freshwater"                  , "deposition"              , 0.95          , "medium"     , "high"            , FALSE     ,
  "Atmospheric"                 , "Brackish/Transitional Water" , "deposition"              , 0.95          , "medium"     , "high"            , FALSE     ,
  "Atmospheric"                 , "Aquatic Sediment"            , "deposition"              , 0.90          , "low"        , "medium"          , FALSE     ,
  "Atmospheric"                 , "Soil O Horizon (Organic)"    , "deposition"              , 0.95          , "medium"     , "high"            , FALSE     ,

  # Terrestrial to aquatic ----
  "Soil O Horizon (Organic)"    , "Freshwater"                  , "runoff"                  , 0.95          , "high"       , "high"            , FALSE     ,
  "Soil O Horizon (Organic)"    , "Groundwater"                 , "percolation"             , 0.85          , "medium"     , "medium"          , FALSE     ,
  "Biota, Terrestrial"          , "Soil O Horizon (Organic)"    , "excretion_decomposition" , 0.90          , "low"        , "medium"          , FALSE     ,

  # Groundwater to surface ----
  "Groundwater"                 , "Freshwater"                  , "baseflow"                , 0.90          , "medium"     , "high"            , FALSE     ,
  "Groundwater"                 , "Brackish/Transitional Water" , "discharge"               , 0.75          , "low"        , "medium"          , FALSE     ,

  # Freshwater flows ----
  "Freshwater"                  , "Brackish/Transitional Water" , "advection"               , 0.95          , "high"       , "high"            , FALSE     ,
  "Freshwater"                  , "Aquatic Sediment"            , "sedimentation"           , 0.95          , "high"       , "high"            , FALSE     ,
  "Freshwater"                  , "Biota, Aquatic"              , "uptake"                  , 0.95          , "medium"     , "high"            , FALSE     ,

  # Brackish/transitional ----
  "Brackish/Transitional Water" , "Aquatic Sediment"            , "sedimentation"           , 0.95          , "high"       , "high"            , FALSE     ,
  "Brackish/Transitional Water" , "Biota, Aquatic"              , "uptake"                  , 0.95          , "medium"     , "high"            , FALSE     ,

  # Sediment processes ----
  "Aquatic Sediment"            , "Freshwater"                  , "resuspension"            , 0.85          , "medium"     , "high"            , FALSE     ,
  "Aquatic Sediment"            , "Brackish/Transitional Water" , "resuspension"            , 0.85          , "medium"     , "high"            , FALSE     ,
  "Aquatic Sediment"            , "Biota, Aquatic"              , "uptake"                  , 0.90          , "low"        , "medium"          , FALSE     ,
  "Aquatic Sediment"            , "Groundwater"                 , "diagenesis"              , 0.70          , "low"        , "low"             , FALSE     ,

  # Biota cycling ----
  "Biota, Aquatic"              , "Aquatic Sediment"            , "excretion_decomposition" , 0.95          , "medium"     , "high"            , FALSE     ,
  "Biota, Aquatic"              , "Freshwater"                  , "excretion"               , 0.90          , "low"        , "medium"          , FALSE     ,
  "Biota, Aquatic"              , "Brackish/Transitional Water" , "excretion"               , 0.90          , "low"        , "medium"          , FALSE     ,
  "Biota, Aquatic"              , "Biota, Terrestrial"          , "trophic_transfer"        , 0.80          , "low"        , "medium"          , FALSE     ,

  # Wastewater flows ----
  "Wastewater"                  , "Freshwater"                  , "discharge"               , 0.90          , "high"       , "high"            , FALSE     ,
  "Wastewater"                  , "Brackish/Transitional Water" , "discharge"               , 0.80          , "medium"     , "medium"          , FALSE     ,
  "Wastewater"                  , "Sludge"                      , "treatment"               , 0.95          , "high"       , "high"            , FALSE     ,

  # Sludge applications ----
  "Sludge"                      , "Soil O Horizon (Organic)"    , "application"             , 0.85          , "medium"     , "medium"          , FALSE     ,
  "Sludge"                      , "Freshwater"                  , "discharge"               , 0.60          , "low"        , "low"             , FALSE     ,

  # Terrestrial-atmospheric ----
  "Soil O Horizon (Organic)"    , "Atmospheric"                 , "resuspension"            , 0.70          , "low"        , "low"             , FALSE     ,
  "Biota, Terrestrial"          , "Atmospheric"                 , "volatilization"          , 0.40          , "negligible" , "low"             , FALSE
)

# Pollution sources data ----
pollution_sources <- tribble(
  ~source_name               , ~source_type , ~importance ,
  "Antifoulants"             , "source"     , "high"      ,
  "Smelting"                 , "source"     , "high"      ,
  "Mining"                   , "source"     , "high"      ,
  "Agriculture (fertilizer)" , "source"     , "medium"    ,
  "Atmospheric deposition"   , "source"     , "medium"    ,
  "Natural weathering"       , "source"     , "low"
)

# Source to compartment edges (placeholder) ----
source_edges <- tribble(
  ~from                      , ~to                           , ~flow_type         , ~plausibility , ~magnitude , ~evidence_quality , ~has_data ,
  "Antifoulants"             , "Brackish/Transitional Water" , "direct_release"   , 0.95          , "high"     , "high"            , FALSE     ,
  "Smelting"                 , "Atmospheric"                 , "emission"         , 0.95          , "high"     , "high"            , FALSE     ,
  "Mining"                   , "Freshwater"                  , "discharge"        , 0.90          , "high"     , "high"            , FALSE     ,
  "Mining"                   , "Aquatic Sediment"            , "tailing_disposal" , 0.85          , "high"     , "medium"          , FALSE     ,
  "Agriculture (fertilizer)" , "Soil O Horizon (Organic)"    , "application"      , 0.80          , "medium"   , "medium"          , FALSE     ,
  "Atmospheric deposition"   , "Atmospheric"                 , "transport"        , 0.95          , "medium"   , "high"            , FALSE     ,
  "Natural weathering"       , "Soil O Horizon (Organic)"    , "weathering"       , 0.95          , "low"      , "high"            , FALSE     ,
  "Natural weathering"       , "Freshwater"                  , "weathering"       , 0.95          , "low"      , "high"            , FALSE
)

# Combine all edges ----
all_edges <- bind_rows(theoretical_edges, source_edges)

# Create nodes data ----
# Get unique compartments from theoretical edges ----
compartment_nodes <- tibble(
  name = unique(c(theoretical_edges$from, theoretical_edges$to))
) |>
  filter(!name %in% pollution_sources$source_name) |>
  mutate(
    node_type = "compartment",
    ENVIRON_COMPARTMENT = case_when(
      str_detect(name, "Biota") ~ "Biota",
      str_detect(name, "Soil|Atmospheric") ~ "Terrestrial",
      TRUE ~ "Aquatic"
    ),
    sum_n = 0, # Theoretical nodes have no data yet
    importance = "medium"
  )

# Add source nodes ----
source_nodes <- pollution_sources |>
  select(name = source_name, node_type = source_type, importance) |>
  mutate(
    ENVIRON_COMPARTMENT = "Source",
    sum_n = NA_real_
  )

# Combine all nodes ----
all_nodes <- bind_rows(compartment_nodes, source_nodes) |>
  mutate(
    node_label = case_when(
      node_type == "source" ~ name,
      is.na(sum_n) ~ name,
      sum_n == 0 ~ glue("{name}\n(n=0)"),
      TRUE ~ glue("{name}\n(n={sum_n})")
    )
  )

# Create graph ----
graph <- tbl_graph(nodes = all_nodes, edges = all_edges, directed = TRUE)

# UI ----
ui <- fluidPage(
  titlePanel("Copper Flow Network Explorer"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      # Data source selection ----
      h4("Network Type"),
      radioButtons(
        "network_type",
        "Choose network:",
        choices = c(
          "Theoretical pathways only" = "theoretical",
          "With pollution sources" = "with_sources",
          "Data-driven (future)" = "data_driven"
        ),
        selected = "with_sources"
      ),

      hr(),

      # Layout selection ----
      h4("Layout Algorithm"),
      selectInput(
        "layout",
        "Choose layout:",
        choices = c(
          "Fruchterman-Reingold (force)" = "fr",
          "Tree (hierarchical)" = "tree",
          "Sugiyama (hierarchical, DAG)" = "sugiyama",
          "Kamada-Kawai (force)" = "kk",
          "Davidson-Harel (force)" = "dh",
          "DrL (force, large)" = "drl",
          "Circle" = "circle",
          "Star" = "star",
          "Grid" = "grid",
          "Nicely (auto-pick)" = "nicely",
          "Graphopt (force)" = "graphopt"
        ),
        selected = "fr"
      ),

      # Random seed ----
      numericInput("seed", "Random seed:", value = 42, min = 1),

      hr(),

      # Edge aesthetics ----
      h4("Edge Aesthetics"),
      selectInput(
        "edge_geom",
        "Edge geometry:",
        choices = c(
          "Fan" = "fan",
          "Link (straight)" = "link",
          "Arc" = "arc",
          "Diagonal" = "diagonal",
          "Diagonal2" = "diagonal2",
          "Elbow" = "elbow"
        ),
        selected = "fan"
      ),

      checkboxInput("show_edge_labels", "Show edge labels", value = FALSE),

      checkboxInput(
        "color_by_flow_type",
        "Color edges by flow type",
        value = TRUE
      ),

      checkboxInput(
        "edge_width_by_magnitude",
        "Edge width by magnitude",
        value = TRUE
      ),

      sliderInput(
        "edge_alpha_theoretical",
        "Theoretical edge transparency:",
        min = 0.1,
        max = 1,
        value = 0.3,
        step = 0.1
      ),

      sliderInput(
        "edge_alpha_data",
        "Data-backed edge transparency:",
        min = 0.1,
        max = 1,
        value = 0.8,
        step = 0.1
      ),

      conditionalPanel(
        condition = "input.edge_geom == 'diagonal2' || input.edge_geom == 'diagonal'",
        sliderInput(
          "edge_strength",
          "Edge curvature strength:",
          min = 0,
          max = 1,
          value = 0.8,
          step = 0.1
        )
      ),

      hr(),

      # Node aesthetics ----
      h4("Node Aesthetics"),
      sliderInput(
        "node_size",
        "Node size:",
        min = 5,
        max = 30,
        value = 15,
        step = 1
      ),

      checkboxInput(
        "node_shape_by_type",
        "Different shapes for sources",
        value = TRUE
      ),

      sliderInput(
        "node_text_size",
        "Text size:",
        min = 2,
        max = 8,
        value = 3.5,
        step = 0.5
      ),

      checkboxInput("wrap_labels", "Wrap node labels", value = TRUE),

      numericInput(
        "wrap_width",
        "Wrap width (chars):",
        value = 12,
        min = 5,
        max = 30
      ),

      hr(),

      # Plot dimensions ----
      h4("Plot Settings"),
      sliderInput(
        "plot_height",
        "Plot height (px):",
        min = 400,
        max = 1200,
        value = 800,
        step = 50
      ),

      sliderInput(
        "plot_width",
        "Plot width (px):",
        min = 400,
        max = 1200,
        value = 1000,
        step = 50
      )
    ),

    mainPanel(
      width = 9,
      plotOutput("network_plot", height = "800px"),
      hr(),
      h4("Network Summary"),
      verbatimTextOutput("network_summary"),
      hr(),
      h4("Reproducible Code:"),
      verbatimTextOutput("code_output")
    )
  )
)

# Server ----
server <- function(input, output, session) {
  # Reactive graph based on network type ----
  current_graph <- reactive({
    if (input$network_type == "theoretical") {
      graph |>
        activate(edges) |>
        filter(from %in% compartment_nodes$name, to %in% compartment_nodes$name)
    } else {
      graph
    }
  })

  # Network summary ----
  output$network_summary <- renderText({
    g <- current_graph()

    n_nodes <- g |> activate(nodes) |> as_tibble() |> nrow()
    n_edges <- g |> activate(edges) |> as_tibble() |> nrow()
    n_sources <- g |>
      activate(nodes) |>
      as_tibble() |>
      filter(node_type == "source") |>
      nrow()
    n_compartments <- g |>
      activate(nodes) |>
      as_tibble() |>
      filter(node_type == "compartment") |>
      nrow()

    glue(
      "
    Nodes: {n_nodes} ({n_sources} sources, {n_compartments} compartments)
    Edges: {n_edges}
    Network type: {input$network_type}
    "
    )
  })

  # Reactive plot ----
  output$network_plot <- renderPlot(
    {
      set.seed(input$seed)

      g <- current_graph()

      # Calculate edge properties ----
      g <- g |>
        activate(edges) |>
        mutate(
          edge_alpha = if_else(
            has_data,
            input$edge_alpha_data,
            input$edge_alpha_theoretical
          ),
          edge_width = if (input$edge_width_by_magnitude) {
            case_when(
              magnitude == "high" ~ 1.5,
              magnitude == "medium" ~ 1.0,
              magnitude == "low" ~ 0.5,
              TRUE ~ 0.3
            )
          } else {
            1.0
          }
        )

      # Base plot ----
      p <- ggraph(g, layout = input$layout)

      # Add edges based on geometry ----
      if (input$edge_geom == "fan") {
        if (input$color_by_flow_type && input$show_edge_labels) {
          p <- p +
            geom_edge_fan(
              aes(
                colour = flow_type,
                label = flow_type,
                alpha = edge_alpha,
                width = edge_width
              ),
              label_colour = "black",
              check_overlap = TRUE,
              angle_calc = "along",
              arrow = arrow(length = unit(3, 'mm')),
              start_cap = circle(3, 'mm'),
              end_cap = circle(3, 'mm')
            )
        } else if (input$color_by_flow_type) {
          p <- p +
            geom_edge_fan(
              aes(colour = flow_type, alpha = edge_alpha, width = edge_width),
              arrow = arrow(length = unit(3, 'mm')),
              start_cap = circle(3, 'mm'),
              end_cap = circle(3, 'mm')
            )
        } else if (input$show_edge_labels) {
          p <- p +
            geom_edge_fan(
              aes(label = flow_type, alpha = edge_alpha, width = edge_width),
              colour = "darkgrey",
              label_colour = "black",
              check_overlap = TRUE,
              angle_calc = "along",
              arrow = arrow(length = unit(3, 'mm')),
              start_cap = circle(3, 'mm'),
              end_cap = circle(3, 'mm')
            )
        } else {
          p <- p +
            geom_edge_fan(
              aes(alpha = edge_alpha, width = edge_width),
              colour = "darkgrey",
              arrow = arrow(length = unit(3, 'mm')),
              start_cap = circle(3, 'mm'),
              end_cap = circle(3, 'mm')
            )
        }
      } else if (input$edge_geom == "link") {
        if (input$color_by_flow_type && input$show_edge_labels) {
          p <- p +
            geom_edge_link(
              aes(
                colour = flow_type,
                label = flow_type,
                alpha = edge_alpha,
                width = edge_width
              ),
              label_colour = "black",
              check_overlap = TRUE,
              angle_calc = "along",
              arrow = arrow(length = unit(3, 'mm')),
              start_cap = circle(3, 'mm'),
              end_cap = circle(3, 'mm')
            )
        } else if (input$color_by_flow_type) {
          p <- p +
            geom_edge_link(
              aes(colour = flow_type, alpha = edge_alpha, width = edge_width),
              arrow = arrow(length = unit(3, 'mm')),
              start_cap = circle(3, 'mm'),
              end_cap = circle(3, 'mm')
            )
        } else if (input$show_edge_labels) {
          p <- p +
            geom_edge_link(
              aes(label = flow_type, alpha = edge_alpha, width = edge_width),
              colour = "darkgrey",
              label_colour = "black",
              check_overlap = TRUE,
              angle_calc = "along",
              arrow = arrow(length = unit(3, 'mm')),
              start_cap = circle(3, 'mm'),
              end_cap = circle(3, 'mm')
            )
        } else {
          p <- p +
            geom_edge_link(
              aes(alpha = edge_alpha, width = edge_width),
              colour = "darkgrey",
              arrow = arrow(length = unit(3, 'mm')),
              start_cap = circle(3, 'mm'),
              end_cap = circle(3, 'mm')
            )
        }
      } else if (input$edge_geom == "arc") {
        if (input$color_by_flow_type && input$show_edge_labels) {
          p <- p +
            geom_edge_arc(
              aes(
                colour = flow_type,
                label = flow_type,
                alpha = edge_alpha,
                width = edge_width
              ),
              label_colour = "black",
              check_overlap = TRUE,
              angle_calc = "along",
              arrow = arrow(length = unit(3, 'mm')),
              start_cap = circle(3, 'mm'),
              end_cap = circle(3, 'mm')
            )
        } else if (input$color_by_flow_type) {
          p <- p +
            geom_edge_arc(
              aes(colour = flow_type, alpha = edge_alpha, width = edge_width),
              arrow = arrow(length = unit(3, 'mm')),
              start_cap = circle(3, 'mm'),
              end_cap = circle(3, 'mm')
            )
        } else if (input$show_edge_labels) {
          p <- p +
            geom_edge_arc(
              aes(label = flow_type, alpha = edge_alpha, width = edge_width),
              colour = "darkgrey",
              label_colour = "black",
              check_overlap = TRUE,
              angle_calc = "along",
              arrow = arrow(length = unit(3, 'mm')),
              start_cap = circle(3, 'mm'),
              end_cap = circle(3, 'mm')
            )
        } else {
          p <- p +
            geom_edge_arc(
              aes(alpha = edge_alpha, width = edge_width),
              colour = "darkgrey",
              arrow = arrow(length = unit(3, 'mm')),
              start_cap = circle(3, 'mm'),
              end_cap = circle(3, 'mm')
            )
        }
      } else if (input$edge_geom == "diagonal") {
        if (input$color_by_flow_type && input$show_edge_labels) {
          p <- p +
            geom_edge_diagonal(
              aes(
                colour = flow_type,
                label = flow_type,
                alpha = edge_alpha,
                width = edge_width
              ),
              strength = input$edge_strength,
              label_colour = "black",
              check_overlap = TRUE,
              angle_calc = "along",
              arrow = arrow(length = unit(3, 'mm')),
              start_cap = circle(3, 'mm'),
              end_cap = circle(3, 'mm')
            )
        } else if (input$color_by_flow_type) {
          p <- p +
            geom_edge_diagonal(
              aes(colour = flow_type, alpha = edge_alpha, width = edge_width),
              strength = input$edge_strength,
              arrow = arrow(length = unit(3, 'mm')),
              start_cap = circle(3, 'mm'),
              end_cap = circle(3, 'mm')
            )
        } else if (input$show_edge_labels) {
          p <- p +
            geom_edge_diagonal(
              aes(label = flow_type, alpha = edge_alpha, width = edge_width),
              colour = "darkgrey",
              strength = input$edge_strength,
              label_colour = "black",
              check_overlap = TRUE,
              angle_calc = "along",
              arrow = arrow(length = unit(3, 'mm')),
              start_cap = circle(3, 'mm'),
              end_cap = circle(3, 'mm')
            )
        } else {
          p <- p +
            geom_edge_diagonal(
              aes(alpha = edge_alpha, width = edge_width),
              colour = "darkgrey",
              strength = input$edge_strength,
              arrow = arrow(length = unit(3, 'mm')),
              start_cap = circle(3, 'mm'),
              end_cap = circle(3, 'mm')
            )
        }
      } else if (input$edge_geom == "diagonal2") {
        if (input$color_by_flow_type && input$show_edge_labels) {
          p <- p +
            geom_edge_diagonal2(
              aes(
                colour = flow_type,
                label = flow_type,
                alpha = edge_alpha,
                width = edge_width
              ),
              strength = input$edge_strength,
              label_colour = "black",
              check_overlap = TRUE,
              angle_calc = "along",
              arrow = arrow(length = unit(3, 'mm')),
              start_cap = circle(3, 'mm'),
              end_cap = circle(3, 'mm')
            )
        } else if (input$color_by_flow_type) {
          p <- p +
            geom_edge_diagonal2(
              aes(colour = flow_type, alpha = edge_alpha, width = edge_width),
              strength = input$edge_strength,
              arrow = arrow(length = unit(3, 'mm')),
              start_cap = circle(3, 'mm'),
              end_cap = circle(3, 'mm')
            )
        } else if (input$show_edge_labels) {
          p <- p +
            geom_edge_diagonal2(
              aes(label = flow_type, alpha = edge_alpha, width = edge_width),
              colour = "darkgrey",
              strength = input$edge_strength,
              label_colour = "black",
              check_overlap = TRUE,
              angle_calc = "along",
              arrow = arrow(length = unit(3, 'mm')),
              start_cap = circle(3, 'mm'),
              end_cap = circle(3, 'mm')
            )
        } else {
          p <- p +
            geom_edge_diagonal2(
              aes(alpha = edge_alpha, width = edge_width),
              colour = "darkgrey",
              strength = input$edge_strength,
              arrow = arrow(length = unit(3, 'mm')),
              start_cap = circle(3, 'mm'),
              end_cap = circle(3, 'mm')
            )
        }
      } else if (input$edge_geom == "elbow") {
        if (input$color_by_flow_type && input$show_edge_labels) {
          p <- p +
            geom_edge_elbow(
              aes(
                colour = flow_type,
                label = flow_type,
                alpha = edge_alpha,
                width = edge_width
              ),
              label_colour = "black",
              check_overlap = TRUE,
              angle_calc = "along",
              arrow = arrow(length = unit(3, 'mm')),
              start_cap = circle(3, 'mm'),
              end_cap = circle(3, 'mm')
            )
        } else if (input$color_by_flow_type) {
          p <- p +
            geom_edge_elbow(
              aes(colour = flow_type, alpha = edge_alpha, width = edge_width),
              arrow = arrow(length = unit(3, 'mm')),
              start_cap = circle(3, 'mm'),
              end_cap = circle(3, 'mm')
            )
        } else if (input$show_edge_labels) {
          p <- p +
            geom_edge_elbow(
              aes(label = flow_type, alpha = edge_alpha, width = edge_width),
              colour = "darkgrey",
              label_colour = "black",
              check_overlap = TRUE,
              angle_calc = "along",
              arrow = arrow(length = unit(3, 'mm')),
              start_cap = circle(3, 'mm'),
              end_cap = circle(3, 'mm')
            )
        } else {
          p <- p +
            geom_edge_elbow(
              aes(alpha = edge_alpha, width = edge_width),
              colour = "darkgrey",
              arrow = arrow(length = unit(3, 'mm')),
              start_cap = circle(3, 'mm'),
              end_cap = circle(3, 'mm')
            )
        }
      }

      # Add nodes ----
      if (input$node_shape_by_type) {
        p <- p +
          geom_node_point(
            aes(colour = ENVIRON_COMPARTMENT, shape = node_type),
            size = input$node_size
          ) +
          scale_shape_manual(values = c("source" = 15, "compartment" = 16))
      } else {
        p <- p +
          geom_node_point(
            aes(colour = ENVIRON_COMPARTMENT),
            size = input$node_size
          )
      }

      # Add labels ----
      label_text <- if (input$wrap_labels) {
        expr(str_wrap(node_label, !!input$wrap_width))
      } else {
        expr(node_label)
      }

      p <- p +
        geom_node_text(
          aes(label = !!label_text),
          size = input$node_text_size
        ) +
        scale_edge_alpha_identity() +
        scale_edge_width_identity() +
        theme_void() +
        theme(legend.position = "right")

      p
    },
    height = function() input$plot_height,
    width = function() input$plot_width
  )

  # Generate code ----
  output$code_output <- renderText({
    # Build conditional strings ----
    colour_aes <- if (input$color_by_flow_type) "colour = flow_type, " else ""
    label_aes <- if (input$show_edge_labels) "label = flow_type, " else ""
    colour_param <- if (input$color_by_flow_type) {
      ""
    } else {
      '    colour = "darkgrey",'
    }
    label_params <- if (input$show_edge_labels) {
      '    label_colour = "black",
    check_overlap = TRUE,
    angle_calc = "along",'
    } else {
      ""
    }

    strength_param <- if (input$edge_geom %in% c("diagonal", "diagonal2")) {
      glue("    strength = {input$edge_strength},")
    } else {
      ""
    }

    shape_aes <- if (input$node_shape_by_type) ", shape = node_type" else ""
    shape_scale <- if (input$node_shape_by_type) {
      '  scale_shape_manual(values = c("source" = 15, "compartment" = 16)) +'
    } else {
      ""
    }

    label_code <- if (input$wrap_labels) {
      glue("str_wrap(node_label, {input$wrap_width})")
    } else {
      "node_label"
    }

    glue(
      '
# Network visualization code ----

set.seed({input$seed})

# Calculate edge properties ----
graph_prepared <- graph |>
  activate(edges) |>
  mutate(
    edge_alpha = if_else(has_data, {input$edge_alpha_data}, {input$edge_alpha_theoretical}),
    edge_width = case_when(
      magnitude == "high" ~ 1.5,
      magnitude == "medium" ~ 1.0,
      magnitude == "low" ~ 0.5,
      TRUE ~ 0.3
    )
  )

# Plot ----
ggraph(graph_prepared, layout = "{input$layout}") +
  geom_edge_{input$edge_geom}(
    aes({colour_aes}{label_aes}alpha = edge_alpha, width = edge_width),
{colour_param}
{strength_param}
{label_params}
    arrow = arrow(length = unit(3, "mm")),
    start_cap = circle(3, "mm"),
    end_cap = circle(3, "mm")
  ) +
  geom_node_point(
    aes(colour = ENVIRON_COMPARTMENT{shape_aes}),
    size = {input$node_size}
  ) +
{shape_scale}
  geom_node_text(
    aes(label = {label_code}),
    size = {input$node_text_size}
  ) +
  scale_edge_alpha_identity() +
  scale_edge_width_identity() +
  theme_void() +
  theme(legend.position = "right")
    '
    )
  })
}

# Run app ----
shinyApp(ui = ui, server = server)
