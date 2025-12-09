# Setup ----
library(leaflet)
library(sf)
library(dplyr)

# Turn off spherical geometry ----
sf::sf_use_s2(FALSE)

# Prepare spatial data ----
data_sf <- literature_merged_data |>
  filter(
    !is.na(LATITUDE),
    !is.na(LONGITUDE),
    !is.na(MEASURED_OR_IMPUTED_VALUE_STANDARD)
  ) |>
  st_as_sf(
    coords = c("LONGITUDE", "LATITUDE"),
    crs = st_crs("WGS84"),
    remove = FALSE
  )

environ_compartments_vocabulary <- environ_compartments_vocabulary() |>
  setdiff(c("Not relevant", "Not reported", "Other"))

# Create colour palette for main compartments ----
compartment_pal <- colorFactor(
  palette = palette.colors(n = 4, palette = "Tableau"),
  domain = environ_compartments_vocabulary
)

# Create colour palette for measured values ----
value_range <- range(data_sf$MEASURED_VALUE_STANDARD, na.rm = TRUE)
value_pal <- colorNumeric(
  palette = "viridis",
  domain = value_range,
  na.color = "gray"
)

# Create sub-compartment letter codes ----
subcomp_codes <- c(
  # Aquatic
  "Freshwater" = "F",
  "Marine/Salt Water" = "M",
  "Brackish/Transitional Water" = "B",
  "Groundwater" = "G",
  "Wastewater" = "WW",
  "Liquid Growth Medium" = "LGM",
  "Rainwater" = "R",
  "Stormwater" = "SW",
  "Leachate" = "L",
  "Aquatic Sediment" = "AS",
  "Porewater" = "P",
  "Sludge" = "Sl",
  # Atmospheric
  "Indoor Air" = "IA",
  "Outdoor Air" = "OA",
  # Terrestrial
  "Terrestrial Biological Residue" = "TBR",
  "Soil H Horizon (Peat)" = "H",
  "Soil O Horizon (Organic)" = "O",
  "Soil A Horizon (Topsoil)" = "A",
  "Soil E Horizon (Mineral)" = "E",
  "Soil S Horizon (Mineral)" = "S",
  "Soil C Horizon (Parent Material)" = "C",
  "Soil R Horizon (Bedrock)" = "R",
  # Biota
  "Biota, Terrestrial" = "BT",
  "Biota, Aquatic" = "BA",
  "Biota, Atmospheric" = "BAm",
  "Biota, Other" = "BO"
)

# Add codes to data ----
data_sf <- data_sf |>
  mutate(
    SUBCOMP_CODE = subcomp_codes[ENVIRON_COMPARTMENT_SUB],
    SUBCOMP_CODE = ifelse(is.na(SUBCOMP_CODE), "?", SUBCOMP_CODE)
  )

# Define map colours ----
marine_colors <- list(
  default = "lightblue",
  highlight = "darkblue"
)

country_colours <- list(
  default = "lightgreen",
  highlight = "darkgreen"
)

# Calculate polygon centroids for labels ----
marine_centroids <- wgs84_geo$marine_polys |>
  filter(highlight_name) |>
  st_centroid()

country_centroids <- wgs84_geo$countries |>
  filter(
    highlight_name,
    !name %in% c("Greenland", "Iceland", "Norway")
  ) |>
  st_centroid()

# Create study connection lines ----
study_lines <- data_sf |>
  st_drop_geometry() |>
  group_by(REFERENCE_ID) |>
  filter(n() > 1) |>
  summarise(
    coords = list(cbind(LONGITUDE, LATITUDE)),
    .groups = "drop"
  )

# Convert to linestring sf object ----
study_lines_sf <- study_lines |>
  rowwise() |>
  mutate(
    geometry = list(st_linestring(coords))
  ) |>
  ungroup() |>
  select(-coords) |>
  st_sf(crs = st_crs("WGS84"))

# Add mine tailings with coordinates ----
mine_tailings_coords <- tribble(
  ~MINE_NAME                                            , ~SITE_NAME                                 , ~ORE_TYPE                        , ~ACTIVE_2013 , ~KEY_DETAILS                                                 , ~SOURCE                   , ~LATITUDE , ~LONGITUDE , ~CONFIDENCE ,
  "AS Sydvaranger"                                      , "Langfjorden, Finnmark"                    , "Iron ore"                       , FALSE        , "Slambanken. Terminated 1976"                                , "Kvassnes & Iversen 2013" , 69.650    , 29.700     ,           3 ,
  "AS Sydvaranger and Sydvaranger Gruve AS"             , "Bøkfjorden, Finnmark"                    , "Iron ore"                       , TRUE         , "Permit: 4M tonnes/yr; 2.6M discharged 2012"                 , "Kvassnes & Iversen 2013" , 69.720    , 30.040     ,           4 ,
  "Sibelco Nordic, division Stjernøya"                 , "Stjernsundet, Finnmark"                   , "Nepheline syenite with biotite" , TRUE         , "Permit: 300k tonnes/yr; 216k discharged 2012"               , "Kvassnes & Iversen 2013" , 70.267    , 22.612     ,           5 ,
  "Altens Kobberverk"                                   , "Kåfjord, Finnmark"                       , "Sulfide ore"                    , FALSE        , "Terminated 1909"                                            , "Kvassnes & Iversen 2013" , 69.935    , 23.045     ,           4 ,
  "Folldal Verk"                                        , "Repparfjorden, Finnmark"                  , "Copper sulfides in carbonate"   , FALSE        , "~1M tonnes over 7 years. Terminated 1964"                   , "Kvassnes & Iversen 2013" , 70.509    , 24.151     ,           5 ,
  "Bjørkåsen Gruver Nikkel og Olivin"                 , "Ballangsfjorden, Nordland"                , "Copper sulfide with quartz"     , FALSE        , "Ballangsleira"                                              , "Kvassnes & Iversen 2013" , 68.348    , 16.900     ,           3 ,
  "Nikkel og Olivin"                                    , "Ballangsfjorden, Nordland (Forneset)"     , "Nickel sulfide with olivine"    , TRUE         , "Deposit at Forneset"                                        , "Kvassnes & Iversen 2013" , 68.370    , 16.830     ,           3 ,
  "Skaland Graphite AS"                                 , "Bergsfjorden, Troms"                      , "Graphite ore"                   , FALSE        , "Permit: 40k tonnes/yr; 21k discharged 2012"                 , "Kvassnes & Iversen 2013" , 69.445    , 17.130     ,           4 ,
  "Senjens Nikkelverk i Hamn"                           , "Bergsfjorden, Troms"                      , "Nickel ore"                     , FALSE        , "1872-1886"                                                  , "Kvassnes & Iversen 2013" , 69.416    , 17.166     ,           4 ,
  "Sulitjelma gruber"                                   , "Fauskevika, Nordland"                     , "Sulfide ore"                    , FALSE        , "Terminated 1991"                                            , "Kvassnes & Iversen 2013" , 67.132    , 16.050     ,           3 ,
  "Rana Gruber"                                         , "Ranfjorden, Nordland"                     , "Iron ore with carbonate"        , TRUE         , "Permit: 1.25M-2.5M tonnes/yr; 2.08M discharged 2012"        , "Kvassnes & Iversen 2013" , 66.310    , 14.140     ,           4 ,
  "Mofjellet Gruber"                                    , "Ranfjorden, Nordland"                     , "Sulfide ore"                    , FALSE        , "Terminated 1987"                                            , "Kvassnes & Iversen 2013" , 66.330    , 14.250     ,           3 ,
  "Bleikvassli gruber"                                  , "Ranfjorden, Nordland"                     , "Sulfide ore with Pb, Cu, Zn"    , FALSE        , "Terminated 1997"                                            , "Kvassnes & Iversen 2013" , 65.860    , 14.115     ,           3 ,
  "Båsmoen gruver"                                     , "Ranfjorden, Nordland"                     , "Sulfide ore"                    , FALSE        , "Terminated 1937"                                            , "Kvassnes & Iversen 2013" , 66.340    , 14.040     ,           3 ,
  "The Quartz Corp AS (former: Norwegian Crystallites)" , "Tysfjord, Nordland"                       , "Quartz"                         , TRUE         , "Permit: 30k tonnes/yr (70% may be tailings)"                , "Kvassnes & Iversen 2013" , 68.040    , 16.040     ,           4 ,
  "Norcem AS Kjøpsvik"                                 , "Tysfjord, Nordland"                       , "Calcium carbonate"              , TRUE         , "Reduced from ~10k to 3k tonnes/yr in 2012"                  , "Kvassnes & Iversen 2013" , 68.082    , 16.265     ,           4 ,
  "Kongsmoen"                                           , "Kongsmoen at Follafjorden, N-Trøndelag"  , "Copper sulfides"                , FALSE        , "Waste from docks"                                           , "Kvassnes & Iversen 2013" , 64.886    , 12.460     ,           3 ,
  "Verdalskalk AS"                                      , "Trondheimsfjorden, N-Trøndelag"          , "Calcium carbonate"              , FALSE        , "Small. Terminated 2009"                                     , "Kvassnes & Iversen 2013" , 63.793    , 11.482     ,           4 ,
  "Hokstad Kisgruber"                                   , "Ytterøya at Levanger, N-Trøndelag"      , "Sulfide ore"                    , FALSE        , "Terminated ca. 1918"                                        , "Kvassnes & Iversen 2013" , 63.802    , 11.144     ,           3 ,
  "Fosdalens Bergverk / Nye Fosdalen Bergvrk"           , "Beitstafjorden, N-Trøndelag"             , "Iron ore"                       , FALSE        , "Terminated 1997"                                            , "Kvassnes & Iversen 2013" , 64.080    , 11.300     ,           3 ,
  "Killingdal Grubeselskap"                             , "Ilsvika in Trondheimsfjord, S-Trøndelag" , "Copper sulfide ore"             , FALSE        , "Mining 1674-1986"                                           , "Kvassnes & Iversen 2013" , 63.450    , 10.350     ,           4 ,
  "Meråker Gruber (N-Trøndelag)"                      , "Hommelvika, S-Trøndelag"                 , "Copper sulfide ore"             , FALSE        , "Waste from docks"                                           , "Kvassnes & Iversen 2013" , 63.421    , 10.806     ,           4 ,
  "Hustadmarmor AS"                                     , "Frænfjorden, Møre og Romsdal"           , "Calcium carbonate"              , TRUE         , "Permit: acceptable conditions; 327k tonnes discharged 2012" , "Kvassnes & Iversen 2013" , 62.833    ,  7.080     ,           4 ,
  "AS Olivin"                                           , "Åheimsfjorden, Møre og Romsdal"         , "Olivine"                        , FALSE        , "Plans for fjord deposit in 1977; exact data missing"        , "Kvassnes & Iversen 2013" , 62.080    ,  5.835     ,           3 ,
  "Svanøy Gruve"                                       , "Førdefjorden, Sogn og Fjordane"          , "Copper sulfide"                 , FALSE        , "Waste from docks"                                           , "Kvassnes & Iversen 2013" , 61.355    ,  5.075     ,           3 ,
  "Hosanger Nikkelverk"                                 , "Lonevågen at Osterøy, Hordaland"        , "Sulfide ore with nickel"        , FALSE        , "Waste from docks"                                           , "Kvassnes & Iversen 2013" , 60.540    ,  5.460     ,           3 ,
  "Stordø kisgruber"                                   , "Sagvågen, Hordaland"                     , "Sulfide ore"                    , FALSE        , "Mining 1908-1968; waste from docks"                         , "Kvassnes & Iversen 2013" , 59.778    ,  5.370     ,           3 ,
  "Goldmines at Bømlo"                                 , "Lyklingfjorden, Hordaland"                , "Sulfide ore"                    , FALSE        , "Waste into sea"                                             , "Kvassnes & Iversen 2013" , 59.695    ,  5.165     ,           3 ,
  "Gravdal Kisgruve"                                    , "Hardangerfjorden, Hordaland"              , "Sulfide ore"                    , FALSE        , "Mining 1864-1964; waste from docks"                         , "Kvassnes & Iversen 2013" , 60.085    ,  6.050     ,           2 ,
  "Varaldsøy Vigsnes Kobberwerk"                       , "Hardangerfjorden, Hordaland"              , "Sulfide ore, copper rich"       , FALSE        , "Waste from docks"                                           , "Kvassnes & Iversen 2013" , 60.085    ,  5.985     ,           3 ,
  "Titania AS"                                          , "Vigsnesbukta/Føynfjorden, Rogaland"      , "Ilmenite ore"                   , FALSE        , "Depositing 1964-1984"                                       , "Kvassnes & Iversen 2013" , 58.325    ,  6.355     ,           4 ,
  "Titania AS"                                          , "Jøssingfjorden, Rogaland"                , "Ilmenite ore"                   , FALSE        , "Deposit 1984-1994"                                          , "Kvassnes & Iversen 2013" , 58.317    ,  6.340     ,           4 ,
  "Titania AS"                                          , "Dyngadjupet, Rogaland"                    , "Ilmenite ore"                   , FALSE        , "Deposit 1984-1994"                                          , "Kvassnes & Iversen 2013" , 58.300    ,  6.290     ,           3
)

# Create map ----
map <- leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>

  # Add ocean polygons (no popups) ----
  addPolygons(
    data = wgs84_geo$marine_polys |> filter(highlight_name),
    fillColor = ~ ifelse(
      highlight_name == TRUE,
      marine_colors[["highlight"]],
      marine_colors[["default"]]
    ),
    fillOpacity = ~ ifelse(highlight_name == TRUE, 0.3, 0),
    color = "black",
    weight = 1,
    group = "Geography"
  ) |>

  # Add country polygons (no popups) ----
  addPolygons(
    data = wgs84_geo$countries |> filter(highlight_name),
    fillColor = ~ ifelse(
      highlight_name == TRUE,
      country_colours[["highlight"]],
      country_colours[["default"]]
    ),
    fillOpacity = ~ ifelse(highlight_name == TRUE, 0.3, 0),
    weight = 1,
    color = "black",
    group = "Geography"
  ) |>

  # Add permanent labels for oceans ----
  addLabelOnlyMarkers(
    data = marine_centroids,
    label = ~name,
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = "center",
      textOnly = TRUE,
      style = list(
        "color" = "darkblue",
        "font-weight" = "bold",
        "font-size" = "14px",
        "text-shadow" = "1px 1px 2px white, -1px -1px 2px white"
      )
    ),
    group = "Geography"
  ) |>

  # Add permanent labels for countries ----
  addLabelOnlyMarkers(
    data = country_centroids,
    label = ~name,
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = "center",
      textOnly = TRUE,
      style = list(
        "color" = "darkgreen",
        "font-weight" = "bold",
        "font-size" = "12px",
        "text-shadow" = "1px 1px 2px white, -1px -1px 2px white"
      )
    ),
    group = "Geography"
  ) |>

  # Add study connection lines ----
  addPolylines(
    data = study_lines_sf,
    color = "purple",
    weight = 2,
    opacity = 0.5,
    dashArray = "5, 5",
    label = ~ paste0("Study: ", REFERENCE_ID),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal")
    ),
    group = "Study Connections"
  ) |>

  # Add mine tailings markers ----
  addMarkers(
    data = mine_tailings_coords,
    lng = ~LONGITUDE,
    lat = ~LATITUDE,
    icon = list(
      iconUrl = "https://upload.wikimedia.org/wikipedia/commons/8/8c/Mining_symbol.svg",
      iconSize = c(15, 15)
    ),
    popup = ~ paste0(
      "<b>Mine:</b> ",
      MINE_NAME,
      "<br>",
      "<b>Location:</b> ",
      SITE_NAME,
      "<br>",
      "<b>Ore Type:</b> ",
      ORE_TYPE,
      "<br>",
      "<b>Status:</b> ",
      ifelse(ACTIVE_2013, "Active (2013)", "Terminated"),
      "<br>",
      "<b>Details:</b> ",
      KEY_DETAILS,
      "<br>",
      "<b>Coordinate Confidence:</b> ",
      CONFIDENCE,
      "/5"
    ),
    group = "Mine Tailings"
  )

# Add layers for each main compartment ----
for (comp in environ_compartments_vocabulary) {
  data_subset <- data_sf |>
    filter(ENVIRON_COMPARTMENT == comp)

  if (nrow(data_subset) > 0) {
    # Build popup HTML with conditional species info
    popup_html <- if (comp == "Biota") {
      ~ paste0(
        "<b>Reference:</b> ",
        REFERENCE_ID,
        "<br>",
        "<b>Site:</b> ",
        SITE_CODE,
        "<br>",
        "<b>Date:</b> ",
        SAMPLING_DATE,
        "<br>",
        "<b>Compartment:</b> ",
        ENVIRON_COMPARTMENT,
        "<br>",
        "<b>Sub-compartment:</b> ",
        ENVIRON_COMPARTMENT_SUB,
        " (",
        SUBCOMP_CODE,
        ")",
        "<br>",
        "<b>Species:</b> ",
        SAMPLE_SPECIES,
        "<br>",
        "<b>Measured Value:</b> ",
        round(MEASURED_VALUE_STANDARD, 3),
        " ",
        MEASURED_UNIT_STANDARD
      )
    } else {
      ~ paste0(
        "<b>Reference:</b> ",
        REFERENCE_ID,
        "<br>",
        "<b>Site:</b> ",
        SITE_CODE,
        "<br>",
        "<b>Date:</b> ",
        SAMPLING_DATE,
        "<br>",
        "<b>Compartment:</b> ",
        ENVIRON_COMPARTMENT,
        "<br>",
        "<b>Sub-compartment:</b> ",
        ENVIRON_COMPARTMENT_SUB,
        " (",
        SUBCOMP_CODE,
        ")",
        "<br>",
        "<b>Measured Value:</b> ",
        round(MEASURED_VALUE_STANDARD, 3),
        " ",
        MEASURED_UNIT_STANDARD
      )
    }

    map <- map |>
      addCircleMarkers(
        data = data_subset,
        fillColor = ~ value_pal(MEASURED_VALUE_STANDARD),
        color = compartment_pal(comp),
        weight = 2,
        fillOpacity = 0.7,
        radius = 6,
        label = ~SUBCOMP_CODE,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "center",
          textOnly = TRUE,
          style = list(
            "color" = compartment_pal(comp),
            "font-weight" = "normal",
            "font-size" = "8px"
          )
        ),
        popup = popup_html,
        group = "Samples"
      )
  }
}

# Add site labels as separate toggleable layer ----
map <- map |>
  addLabelOnlyMarkers(
    data = data_sf,
    label = ~SITE_CODE,
    labelOptions = labelOptions(
      noHide = FALSE,
      direction = "top",
      textOnly = TRUE,
      style = list(
        "color" = "black",
        "font-size" = "9px",
        "background" = "rgba(255, 255, 255, 0.7)",
        "padding" = "2px"
      )
    ),
    group = "Site Labels"
  ) |>

  # Add layer controls ----
  addLayersControl(
    baseGroups = character(0),
    overlayGroups = c(
      "Geography",
      "Study Connections",
      "Mine Tailings",
      "Samples",
      "Site Labels"
    ),
    options = layersControlOptions(collapsed = FALSE)
  ) |>

  # Add legends ----
  addLegend(
    position = "bottomright",
    pal = compartment_pal,
    values = environ_compartments_vocabulary,
    title = "Compartment<br>(border colour)",
    opacity = 1
  ) |>
  addLegend(
    position = "bottomleft",
    pal = value_pal,
    values = data_sf$MEASURED_VALUE_STANDARD,
    title = "Measured Value<br>(fill colour)",
    opacity = 0.8,
    labFormat = labelFormat(digits = 1)
  )

# Display map ----
map


## mine tailing sites
# very unreliable data
mine_tailings_coords <- tibble::tibble(
  id = 1:33,
  company = c(
    "AS Sydvaranger",
    "Sydvaranger Gruve AS",
    "Sibelco Nordic (Stjernøya)",
    "Altens Kobberverk",
    "Folldal Verk",
    "Bjørkåsen Gruver",
    "Nikkel og Olivin",
    "Skaland Graphite AS",
    "Senjens Nikkelverk",
    "Sulitjelma gruber",
    "Rana Gruber",
    "Mofjellet Gruber",
    "Bleikvassli gruber",
    "Båsmoen gruver",
    "The Quartz Corp AS",
    "Norcem AS Kjøpsvik",
    "Kongsmoen",
    "Verdalskalk AS",
    "Hokstad Kisgruber",
    "Fosdalens Bergverk",
    "Killingdal Grubeselskap",
    "Meråker Gruber",
    "Hustadmarmor AS",
    "AS Olivin",
    "Svanøy Gruve",
    "Hosanger Nikkelverk",
    "Stordø kisgruber",
    "Bømlo Goldmines",
    "Gravdal Kisgruve",
    "Varaldsøy Vigsnes Kobberwerk",
    "Titania AS",
    "Titania AS",
    "Titania AS"
  ),
  fjord_location = c(
    "Langfjorden",
    "Bøkfjorden",
    "Stjernsundet",
    "Kåfjord",
    "Repparfjorden",
    "Ballangsfjorden",
    "Ballangsfjorden",
    "Bergsfjorden",
    "Bergsfjorden",
    "Fauskevika",
    "Ranfjorden",
    "Ranfjorden",
    "Ranfjorden",
    "Ranfjorden",
    "Tysfjord",
    "Tysfjord",
    "Follafjorden",
    "Trondheimsfjorden",
    "Ytterøya",
    "Beitstafjorden",
    "Ilsvika",
    "Hommelvika",
    "Frænfjorden",
    "Åheimsfjorden",
    "Førdefjorden",
    "Lonevågen",
    "Sagvågen",
    "Lyklingfjorden",
    "Hardangerfjorden",
    "Hardangerfjorden",
    "Jøssingfjorden",
    "Jøssingfjorden",
    "Dyngadjupet"
  ),
  latitude = c(
    69.650,
    69.720,
    70.267,
    69.935,
    70.509,
    68.348,
    68.370,
    69.445,
    69.416,
    67.132,
    66.310,
    66.330,
    65.860,
    66.340,
    68.040,
    68.082,
    64.886,
    63.793,
    63.802,
    64.080,
    63.450,
    63.421,
    62.833,
    62.080,
    61.355,
    60.540,
    59.778,
    59.695,
    60.085,
    60.085,
    58.325,
    58.317,
    58.300
  ),
  longitude = c(
    29.700,
    30.040,
    22.612,
    23.045,
    24.151,
    16.900,
    16.830,
    17.130,
    17.166,
    16.050,
    14.140,
    14.250,
    14.115,
    14.040,
    16.040,
    16.265,
    12.460,
    11.482,
    11.144,
    11.300,
    10.350,
    10.806,
    7.080,
    5.835,
    5.075,
    5.460,
    5.370,
    5.165,
    6.050,
    5.985,
    6.355,
    6.340,
    6.290
  ),
  confidence = c(
    3,
    4,
    5,
    4,
    5,
    3,
    3,
    4,
    4,
    3,
    4,
    3,
    3,
    3,
    4,
    4,
    3,
    4,
    3,
    3,
    4,
    4,
    4,
    3,
    3,
    3,
    3,
    3,
    2,
    3,
    4,
    4,
    3
  ),
  source_method = c(
    "NIVA research; Slambanken in outer Langfjorden",
    "Kirkenes harbor discharge pipeline location",
    "Mindat.org/NGU; precise Lillebukt Mine coordinates",
    "Wikidata; Kåfjord fjord center",
    "Science of Total Environment paper; precise deposit location",
    "Inner Ballangsfjorden at Ballangsleira",
    "Forneset deposit on Ballangsfjorden",
    "Skaland village on Senja; Bergsfjorden discharge",
    "Hamn i Senja; historic nickel works slag",
    "Sandnes/Langvatnet deposit area",
    "Inner Ranfjorden; Gullsmedvik facility",
    "Mofjellet discharge area estimate",
    "Kjøkkenbukta/Store Bleikvatn; shipped from Andfiskå",
    "Båsmoen area west of Mo i Rana",
    "Drag village; quartz processing facility",
    "Kjøpsvik cement works on Tysfjorden",
    "Kongsmoen harbor; Skorovas taubane terminus",
    "Verdal Havn; shipping facility coordinates",
    "Ytterøy ferry/church area",
    "Ressemlia shipping area at Malm",
    "Fagervika cove; flotation works",
    "Hommelvika harbor; GPS coordinates",
    "Scientific literature; offshore from Elnesvågen",
    "Inner Vanylvsfjorden; Sibelco terminal",
    "Svanøy island; historic copper mine",
    "Lonevågen bay; Bysheim dock waste",
    "Grunnavågsneset harbor area",
    "Lyklingfjorden bay; Lykling mines",
    "Øynefjorden, Kvam; low confidence estimate",
    "Fjord west of Varaldsøy island",
    "Jøssingfjord inner disposal area 1960-1984",
    "Jøssingfjord center; multiple verified sources",
    "Offshore basin; 113m discharge depth"
  )
)

## aquaculture data
# https://inspire-geoportal.ec.europa.eu/srv/api/records/4ca8af5e-ffc7-4636-847d-4eca92c4a3b0

## copper agricultural application data
# https://relacs-project.eu/wp-content/uploads/2022/03/Tamm_et_al_2022-_Use_of_Copper_Based_Fungicides_Organic_Agriculture_inTwelve-_European-_Countries.pdf
# but all this says is 0.5 tonnes over the whole country
# this paper identifies key apple-growing sites, but we don't have much more speicificity than fylke/kommune level. probably enough to get started with though.

# average ocean copper concentrations + fluxes
# this paper: https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2023GB007769
