if (!exists("requirements_loaded")) {
  source("temp_R/scratchpad.r")
  load_all_requirements()
}

## Test Maps ----
# Prepare all data for WGS84 map
wgs84_geo <- prepare_geography_wgs84()

wgs84_map <- create_study_area_map_wgs84(
  ocean_sf = wgs84_geo$marine_polys,
  country_sf = wgs84_geo$countries,
  arctic_circle_sf = wgs84_geo$arctic_circle,
  graticule_sf = wgs84_geo$graticule
)

wgs84_map

# Prepare all data for polar map
polar_geo <- prepare_geography_polar()

polar_map <- create_study_area_map_polar(
  ocean_sf = polar_geo$marine_polys,
  country_sf = polar_geo$countries,
  arctic_circle_sf = polar_geo$arctic_circle,
  graticule_sf = polar_geo$graticule
)

# map with data points

data <- load_literature_parquet()

data_sf <- data |>
  dplyr::filter(!is.na(LATITUDE)) |>
  dplyr::filter(!is.na(LONGITUDE)) |> # we shouldn't have anything without these values... but we do. oops.
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = sf::st_crs("WGS84"))

wgs84_map +
  geom_sf(
    data = data_sf,
    fill = "red"
  ) + # I guess we need to constrain again?
  coord_sf(
    xlim = c(
      get_study_area_bbox()[[1]],
      get_study_area_bbox()[[3]]
    ),
    ylim = c(
      get_study_area_bbox()[[2]],
      get_study_area_bbox()[[4]]
    )
  )


library(leaflet)
library(sf)

leaflet() %>%
  addTiles() %>%
  addPolygons(
    data = wgs84_geo$marine_polys |> filter(highlight_name),
    fillColor = ~ ifelse(
      highlight_name == TRUE,
      marine_colors[["default"]],
      marine_colors[["highlight"]]
    ),
    fillOpacity = ~ ifelse(
      highlight_name == TRUE,
      0.5,
      0
    ),
    color = "black",
    weight = 1,
    popup = ~name,
    group = "Oceans"
  ) %>%
  addPolygons(
    data = wgs84_geo$countries |> filter(highlight_name),
    fillColor = ~ ifelse(
      highlight_name == TRUE,
      country_colours[["default"]],
      country_colours[["highlight"]]
    ),
    fillOpacity = ~ ifelse(
      highlight_name == TRUE,
      0.5,
      0
    ),
    weight = 1,
    color = "black",
    popup = ~name,
    group = "Countries"
  ) %>%
  addCircleMarkers(
    data = data_sf,
    popup = ~REFERENCE_ID,
    group = "Samples",
    clusterOptions = markerClusterOptions()
  ) |>
  addLabelOnlyMarkers(data = data_sf, label = ~SITE_CODE) |>
  addLayersControl(
    overlayGroups = c("Oceans", "Countries", "Samples"),
    options = layersControlOptions(collapsed = FALSE)
  )


## mine tailing sites
mine_tailings <- tribble(
  ~MINE_NAME                                            , ~SITE_NAME                                 , ~ORE_TYPE                        , ~ACTIVE_2013 , ~KEY_DETAILS                                                 , ~SOURCE                   ,
  "AS Sydvaranger"                                      , "Langfjorden, Finnmark"                    , "Iron ore"                       , FALSE        , "Slambanken. Terminated 1976"                                , "Kvassnes & Iversen 2013" ,
  "AS Sydvaranger and Sydvaranger Gruve AS"             , "Bøkfjorden, Finnmark"                    , "Iron ore"                       , TRUE         , "Permit: 4M tonnes/yr; 2.6M discharged 2012"                 , "Kvassnes & Iversen 2013" ,
  "Sibelco Nordic, division Stjernøya"                 , "Stjernsundet, Finnmark"                   , "Nepheline syenite with biotite" , TRUE         , "Permit: 300k tonnes/yr; 216k discharged 2012"               , "Kvassnes & Iversen 2013" ,
  "Altens Kobberverk"                                   , "Kåfjord, Finnmark"                       , "Sulfide ore"                    , FALSE        , "Terminated 1909"                                            , "Kvassnes & Iversen 2013" ,
  "Folldal Verk"                                        , "Repparfjorden, Finnmark"                  , "Copper sulfides in carbonate"   , FALSE        , "~1M tonnes over 7 years. Terminated 1964"                   , "Kvassnes & Iversen 2013" ,
  "Bjørkåsen Gruver Nikkel og Olivin"                 , "Ballangsfjorden, Nordland"                , "Copper sulfide with quartz"     , FALSE        , "Ballangsleira"                                              , "Kvassnes & Iversen 2013" ,
  "Nikkel og Olivin"                                    , "Ballangsfjorden, Nordland (Forneset)"     , "Nickel sulfide with olivine"    , TRUE         , "Deposit at Forneset"                                        , "Kvassnes & Iversen 2013" ,
  "Skaland Graphite AS"                                 , "Bergsfjorden, Troms"                      , "Graphite ore"                   , FALSE        , "Permit: 40k tonnes/yr; 21k discharged 2012"                 , "Kvassnes & Iversen 2013" ,
  "Senjens Nikkelverk i Hamn"                           , "Bergsfjorden, Troms"                      , "Nickel ore"                     , FALSE        , "1872-1886"                                                  , "Kvassnes & Iversen 2013" ,
  "Sulitjelma gruber"                                   , "Fauskevika, Nordland"                     , "Sulfide ore"                    , FALSE        , "Terminated 1991"                                            , "Kvassnes & Iversen 2013" ,
  "Rana Gruber"                                         , "Ranfjorden, Nordland"                     , "Iron ore with carbonate"        , TRUE         , "Permit: 1.25M-2.5M tonnes/yr; 2.08M discharged 2012"        , "Kvassnes & Iversen 2013" ,
  "Mofjellet Gruber"                                    , "Ranfjorden, Nordland"                     , "Sulfide ore"                    , FALSE        , "Terminated 1987"                                            , "Kvassnes & Iversen 2013" ,
  "Bleikvassli gruber"                                  , "Ranfjorden, Nordland"                     , "Sulfide ore with Pb, Cu, Zn"    , FALSE        , "Terminated 1997"                                            , "Kvassnes & Iversen 2013" ,
  "Båsmoen gruver"                                     , "Ranfjorden, Nordland"                     , "Sulfide ore"                    , FALSE        , "Terminated 1937"                                            , "Kvassnes & Iversen 2013" ,
  "The Quartz Corp AS (former: Norwegian Crystallites)" , "Tysfjord, Nordland"                       , "Quartz"                         , TRUE         , "Permit: 30k tonnes/yr (70% may be tailings)"                , "Kvassnes & Iversen 2013" ,
  "Norcem AS Kjøpsvik"                                 , "Tysfjord, Nordland"                       , "Calcium carbonate"              , TRUE         , "Reduced from ~10k to 3k tonnes/yr in 2012"                  , "Kvassnes & Iversen 2013" ,
  "Kongsmoen"                                           , "Kongsmoen at Follafjorden, N-Trøndelag"  , "Copper sulfides"                , FALSE        , "Waste from docks"                                           , "Kvassnes & Iversen 2013" ,
  "Verdalskalk AS"                                      , "Trondheimsfjorden, N-Trøndelag"          , "Calcium carbonate"              , FALSE        , "Small. Terminated 2009"                                     , "Kvassnes & Iversen 2013" ,
  "Hokstad Kisgruber"                                   , "Ytterøya at Levanger, N-Trøndelag"      , "Sulfide ore"                    , FALSE        , "Terminated ca. 1918"                                        , "Kvassnes & Iversen 2013" ,
  "Fosdalens Bergverk / Nye Fosdalen Bergvrk"           , "Beitstafjorden, N-Trøndelag"             , "Iron ore"                       , FALSE        , "Terminated 1997"                                            , "Kvassnes & Iversen 2013" ,
  "Killingdal Grubeselskap"                             , "Ilsvika in Trondheimsfjord, S-Trøndelag" , "Copper sulfide ore"             , FALSE        , "Mining 1674-1986"                                           , "Kvassnes & Iversen 2013" ,
  "Meråker Gruber (N-Trøndelag)"                      , "Hommelvika, S-Trøndelag"                 , "Copper sulfide ore"             , FALSE        , "Waste from docks"                                           , "Kvassnes & Iversen 2013" ,
  "Hustadmarmor AS"                                     , "Frænfjorden, Møre og Romsdal"           , "Calcium carbonate"              , TRUE         , "Permit: acceptable conditions; 327k tonnes discharged 2012" , "Kvassnes & Iversen 2013" ,
  "AS Olivin"                                           , "Åheimsfjorden, Møre og Romsdal"         , "Olivine"                        , FALSE        , "Plans for fjord deposit in 1977; exact data missing"        , "Kvassnes & Iversen 2013" ,
  "Svanøy Gruve"                                       , "Førdefjorden, Sogn og Fjordane"          , "Copper sulfide"                 , FALSE        , "Waste from docks"                                           , "Kvassnes & Iversen 2013" ,
  "Hosanger Nikkelverk"                                 , "Lonevågen at Osterøy, Hordaland"        , "Sulfide ore with nickel"        , FALSE        , "Waste from docks"                                           , "Kvassnes & Iversen 2013" ,
  "Stordø kisgruber"                                   , "Sagvågen, Hordaland"                     , "Sulfide ore"                    , FALSE        , "Mining 1908-1968; waste from docks"                         , "Kvassnes & Iversen 2013" ,
  "Goldmines at Bømlo"                                 , "Lyklingfjorden, Hordaland"                , "Sulfide ore"                    , FALSE        , "Waste into sea"                                             , "Kvassnes & Iversen 2013" ,
  "Gravdal Kisgruve"                                    , "Hardangerfjorden, Hordaland"              , "Sulfide ore"                    , FALSE        , "Mining 1864-1964; waste from docks"                         , "Kvassnes & Iversen 2013" ,
  "Varaldsøy Vigsnes Kobberwerk"                       , "Hardangerfjorden, Hordaland"              , "Sulfide ore, copper rich"       , FALSE        , "Waste from docks"                                           , "Kvassnes & Iversen 2013" ,
  "Titania AS"                                          , "Vigsnesbukta/Føynfjorden, Rogaland"      , "Ilmenite ore"                   , FALSE        , "Depositing 1964-1984"                                       , "Kvassnes & Iversen 2013" ,
  "Titania AS"                                          , "Jøssingfjorden, Rogaland"                , "Ilmenite ore"                   , FALSE        , "Deposit 1984-1994"                                          , "Kvassnes & Iversen 2013" ,
  "Titania AS"                                          , "Dyngadjupet, Rogaland"                    , "Ilmenite ore"                   , FALSE        , "Deposit 1984-1994"                                          , "Kvassnes & Iversen 2013"
)

## aquaculture data
# https://inspire-geoportal.ec.europa.eu/srv/api/records/4ca8af5e-ffc7-4636-847d-4eca92c4a3b0

## copper agricultural application data
# https://relacs-project.eu/wp-content/uploads/2022/03/Tamm_et_al_2022-_Use_of_Copper_Based_Fungicides_Organic_Agriculture_inTwelve-_European-_Countries.pdf
# but all this says is 0.5 tonnes over the whole country
# this paper identifies key apple-growing sites, but we don't have much more speicificity than fylke/kommune level. probably enough to get started with though.

# average ocean copper concentratons + fluxes
# this paper: https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2023GB007769
