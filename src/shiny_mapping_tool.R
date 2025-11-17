# Your existing functions (simplified versions)
ne_get <- function(scale, type, category, destdir = "data/raw/shapefiles/") {
  filepath <- file.path(destdir, paste0("ne_", scale, "m_", type, ".shp"))
  if (file.exists(filepath)) {
    rnaturalearth::ne_load(scale, type, category, destdir)
  } else {
    rnaturalearth::ne_download(scale, type, category, destdir)
  }
}

st_line_split <- function(
  geometry,
  line = c(0, 180),
  direction = "horizontal"
) {
  # Get the bounding box of the input geometry
  bbox <- st_bbox(geometry)
  geometry_crs <- st_crs(geometry)

  if (direction == "horizontal") {
    # Split along horizontal line (e.g., latitude)
    lat_split <- line[1]
    # Create bounding boxes for north and south parts
    bbox_north <- st_bbox(
      c(
        bbox$xmin,
        ymin = lat_split,
        bbox$xmax,
        bbox$ymax
      ),
      crs = geometry_crs
    )
    bbox_south <- st_bbox(
      c(
        bbox$xmin,
        bbox$ymin,
        bbox$xmax,
        ymax = lat_split
      ),
      crs = geometry_crs
    )
  } else if (direction == "vertical") {
    # Split along vertical line (e.g., longitude)
    lon_split <- line[1]
    # Create bounding boxes for east and west parts
    bbox_east <- st_bbox(
      c(
        xmin = lon_split,
        bbox$ymin,
        bbox$xmax,
        bbox$ymax
      ),
      crs = geometry_crs
    )
    bbox_west <- st_bbox(
      c(
        bbox$xmin,
        bbox$ymin,
        xmax = lon_split,
        bbox$ymax
      ),
      crs = geometry_crs
    )
    bbox_north <- bbox_east
    bbox_south <- bbox_west
  }
  # Convert to bbox objects and crop
  part1 <- tryCatch(
    st_crop(geometry, bbox_north),
    error = function(e) NULL
  )
  part2 <- tryCatch(
    st_crop(geometry, bbox_south),
    error = function(e) NULL
  )
  # Combine results
  parts <- list()
  if (!is.null(part1) && nrow(part1) > 0) {
    parts <- append(parts, list(part1))
  }
  if (!is.null(part2) && nrow(part2) > 0) {
    parts <- append(parts, list(part2))
  }
  if (length(parts) > 0) {
    return(do.call(rbind, parts))
  } else {
    return(geometry) # Return original if splitting failed
  }
}

# UI
ui <- fluidPage(
  titlePanel("Arctic Map Tweaker"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      # Map type
      radioButtons(
        "map_type",
        "Map Type:",
        choices = list("Polar" = "polar", "Rectangular" = "rect"),
        selected = "polar"
      ),

      # Colors
      h4("Colors"),
      textInput("ocean_color", "Ocean Color", value = "#9dd9fe"),
      textInput("ocean_dark", "Dark Ocean Color", value = "#6e98b2"),
      textInput("land_color", "Land Color", value = "#e3d7bf"),
      textInput("highlight_land", "Highlight Land", value = "#5c887b"),
      textInput("bg_color", "Background", value = "#6e98b2"),

      # Viewport (for polar)
      conditionalPanel(
        condition = "input.map_type == 'polar'",
        h4("Viewport"),
        sliderInput(
          "xlim",
          "X Range",
          min = -15000000,
          max = 15000000,
          value = c(-3000000, 3000000),
          step = 200000
        ),
        sliderInput(
          "ylim",
          "Y Range",
          min = -8000000,
          max = 8000000,
          value = c(-4200000, 3200000),
          step = 200000
        )
      ),

      # Labels
      h4("Labels"),
      checkboxInput("show_ocean_labels", "Show Ocean Labels", TRUE),
      checkboxInput("show_country_labels", "Show Country Labels", TRUE),
      checkboxInput("show_graticules", "Show Grid Lines", TRUE),
      checkboxInput("show_arctic_circle", "Show Arctic Circle", TRUE),
      checkboxInput("show_axis_labels", "Show Axis Labels", TRUE),

      # Size controls
      sliderInput(
        "line_width",
        "Boundary Width",
        min = 0.1,
        max = 1,
        value = 0.2,
        step = 0.1
      ),

      h4("Text Sizes"),
      sliderInput(
        "land_text_size",
        "Land Labels",
        min = 1,
        max = 8,
        value = 5,
        step = 0.5
      ),
      sliderInput(
        "major_ocean_text_size",
        "Major Ocean Labels",
        min = 1,
        max = 8,
        value = 4,
        step = 0.5
      ),
      sliderInput(
        "minor_ocean_text_size",
        "Minor Ocean Labels",
        min = 0,
        max = 8,
        value = 3,
        step = 0.5
      )
    ),

    mainPanel(
      width = 9,
      plotOutput("map", height = "700px")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Load data once - using your actual data loading code
  data_loaded <- reactive({
    tryCatch(
      {
        # Ocean shapefiles
        ne_10m_geography_marine_polys <- ne_get(
          scale = 10,
          type = "geography_marine_polys",
          category = "physical",
          destdir = "data/raw/shapefiles/"
        ) |>
          select(name_en, geometry) |>
          rename(name = name_en)

        # Bisect the Atlantic
        atlantic <- ne_10m_geography_marine_polys |>
          group_by(name) |>
          reframe(n = row_number(), geometry) |>
          filter(name == "Atlantic Ocean", n == 1) |>
          st_as_sf()
        atlantic_split <- st_line_split(
          atlantic,
          c(0, 180),
          direction = "horizontal"
        ) |>
          mutate(name = c("North Atlantic Ocean", "South Atlantic Ocean")) |>
          select(-n)

        # re-add
        ne_10m_geography_marine_polys <- ne_10m_geography_marine_polys |>
          filter(name != "Atlantic Ocean") |>
          rbind(atlantic_split)

        # Country shapefiles
        ne_10m_country_polys <- rnaturalearth::ne_countries(scale = 10) |>
          mutate(
            highlight_name = case_match(
              name,
              c("Norway", "Greenland", "Iceland") ~ TRUE,
              .default = FALSE
            )
          ) |>
          select(name, geometry, highlight_name) |>
          filter(name %notin% c("Norway", "Antarctica"))

        # get continental Norway, Jan Mayen, and Svalbard separately
        ne_10m_norway_poly <- ne_states(geounit = "norway") |>
          select(name, geometry) |>
          st_crop(c(xmin = 0, xmax = 40, ymin = 57, ymax = 90)) |>
          reframe(name = "Norway", geometry = st_union(geometry))

        ne_10m_janmayen_poly <- ne_states(geounit = "norway") |>
          filter(name == "Nordland") |>
          st_crop(c(xmin = 5, xmax = -10, ymin = 70, ymax = 90)) |>
          select(name, geometry) |>
          mutate(name = "Jan \nMayen")

        ne_10m_svalbard_poly <- ne_states(geounit = "svalbard") |>
          select(name, geometry) |>
          mutate(name = "Svalbard & \nBear Island")

        ne_10m_norway_combined <- rbind(
          ne_10m_norway_poly,
          ne_10m_janmayen_poly,
          ne_10m_svalbard_poly
        ) |>
          mutate(highlight_name = TRUE) |>
          st_as_sf()

        countries_combined <- rbind(
          ne_10m_norway_combined,
          ne_10m_country_polys
        ) |>
          st_as_sf()

        # Ocean annotations and styling
        oceans_processed <- ne_10m_geography_marine_polys |>
          mutate(
            highlight_name = name %in%
              c(
                "Norwegian Sea",
                "North Atlantic Ocean",
                "Greenland Sea",
                "Barents Sea",
                "Arctic Ocean",
                "Vestfjorden",
                "Storfjorden",
                "Denmark Strait"
              ),
            major_body = name %in%
              c(
                "Norwegian Sea",
                "North Atlantic Ocean",
                "Greenland Sea",
                "Barents Sea",
                "Arctic Ocean",
                "Kara Sea",
                "North Sea",
                "Davis Strait",
                "Baffin Bay",
                "Lincoln Sea"
              )
          ) |>
          st_as_sf() |>
          group_by(name) |>
          mutate(
            count = n(),
            ocean_color = case_when(
              highlight_name ~ "#9dd9fe",
              TRUE ~ "#6e98b2"
            )
          )

        list(countries = countries_combined, oceans = oceans_processed)
      },
      error = function(e) {
        # Fallback to simple natural earth data
        countries <- rnaturalearth::ne_countries(
          scale = 50,
          returnclass = "sf"
        ) %>%
          filter(continent %in% c("Europe", "North America", "Asia")) %>%
          mutate(highlight_name = name %in% c("Norway", "Greenland", "Iceland"))

        oceans <- rnaturalearth::ne_download(
          scale = 50,
          type = "geography_marine_polys",
          category = "physical",
          returnclass = "sf"
        ) %>%
          mutate(
            highlight_name = name %in%
              c("Arctic Ocean", "Norwegian Sea", "Barents Sea"),
            ocean_color = ifelse(highlight_name, "#9dd9fe", "#6e98b2")
          )

        list(countries = countries, oceans = oceans)
      }
    )
  })

  output$map <- renderPlot({
    req(data_loaded())

    countries <- data_loaded()$countries
    oceans <- data_loaded()$oceans

    # Transform for polar if needed
    if (input$map_type == "polar") {
      countries <- st_transform(countries, "EPSG:3575")
      oceans <- st_transform(oceans, "EPSG:3575")
    }

    # Build plot
    p <- ggplot()

    # Oceans
    if (!is.null(oceans)) {
      p <- p +
        geom_sf(
          data = oceans,
          aes(fill = ocean_color),
          color = "black",
          linewidth = input$line_width * 0.5
        )
    }

    # Countries
    p <- p +
      geom_sf(
        data = countries,
        aes(
          fill = ifelse(highlight_name, input$highlight_land, input$land_color)
        ),
        color = "black",
        linewidth = input$line_width
      )

    # Grid lines
    if (input$show_graticules) {
      graticule <- st_graticule(
        lon = seq(-180, 180, 15), # Every 15 degrees longitude
        lat = seq(40, 90, 5), # Every 5 degrees latitude
        crs = st_crs(countries)
      )
      p <- p +
        geom_sf(data = graticule, color = "gray40", alpha = 0.5, size = 0.5)
    }

    # Labels with ggrepel
    if (input$show_country_labels) {
      countries_labeled <- countries %>%
        filter(highlight_name) %>%
        mutate(
          centroid = st_centroid(geometry),
          lon = st_coordinates(centroid)[, 1],
          lat = st_coordinates(centroid)[, 2]
        )

      p <- p +
        geom_text_repel(
          data = countries_labeled,
          aes(x = lon, y = lat, label = name),
          size = input$land_text_size,
          color = "#2C3E50",
          bg.color = "white",
          bg.r = 0.1,
          force = 2,
          max.overlaps = Inf,
          min.segment.length = 0,
          segment.size = 0.3,
          segment.color = "gray60"
        )
    }

    if (input$show_ocean_labels && !is.null(oceans)) {
      # Prepare ocean label data
      oceans_labeled <- oceans %>%
        filter(highlight_name) %>%
        mutate(
          centroid = st_centroid(geometry),
          lon = st_coordinates(centroid)[, 1],
          lat = st_coordinates(centroid)[, 2]
        )

      # Major ocean labels
      major_oceans <- oceans_labeled %>% filter(major_body)
      if (nrow(major_oceans) > 0) {
        p <- p +
          geom_text_repel(
            data = major_oceans,
            aes(x = lon, y = lat, label = name),
            size = input$major_ocean_text_size,
            color = "#EEE",
            bg.color = "#2C3E50",
            bg.r = 0.1,
            fontface = "bold.italic",
            force = 2,
            max.overlaps = Inf,
            min.segment.length = 0,
            segment.size = 0.3,
            segment.color = "lightblue"
          )
      }

      # Minor ocean labels
      minor_oceans <- oceans_labeled %>% filter(!major_body)
      if (nrow(minor_oceans) > 0) {
        p <- p +
          geom_text_repel(
            data = minor_oceans,
            aes(x = lon, y = lat, label = name),
            size = input$minor_ocean_text_size,
            color = "#EEE",
            bg.color = "#2C3E50",
            bg.r = 0.1,
            fontface = "italic",
            force = 1.5,
            max.overlaps = Inf,
            min.segment.length = 0,
            segment.size = 0.2,
            segment.color = "lightblue"
          )
      }
    }

    # Styling
    p <- p +
      scale_fill_identity() +
      theme_void() +
      theme(
        panel.background = element_rect(fill = input$bg_color, color = NA),
        plot.margin = margin(0, 0, 0, 0),
        legend.position = "none",
        axis.title = if (input$show_axis_labels) {
          element_text()
        } else {
          element_blank()
        },
        axis.text = if (input$show_axis_labels) {
          element_text()
        } else {
          element_blank()
        },
        axis.ticks = if (input$show_axis_labels) {
          element_line()
        } else {
          element_blank()
        }
      )

    # Coordinate system
    if (input$map_type == "polar") {
      p <- p +
        coord_sf(
          crs = st_crs("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=0"),
          xlim = input$xlim,
          ylim = input$ylim,
          expand = FALSE
        )
    } else {
      p <- p +
        coord_sf(
          xlim = c(-180, 180),
          ylim = c(40, 90),
          expand = FALSE
        )
    }

    p
  })
}

# Run app
shinyApp(ui = ui, server = server)
