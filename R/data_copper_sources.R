#' Mine tailings sites with coordinates
#'
#' Returns a tibble containing information about mine tailings disposal sites
#' in Norwegian fjords, including geographic coordinates, ore types, and
#' operational status.
#'
#' @return A tibble with 34 rows and 9 columns:
#' \describe{
#'   \item{MINE_NAME}{Name of the mining operation}
#'   \item{SITE_NAME}{Location where tailings were deposited}
#'   \item{ORE_TYPE}{Type of ore extracted}
#'   \item{ACTIVE_2013}{Logical indicating if site was active in 2013}
#'   \item{KEY_DETAILS}{Additional information about permits and operations}
#'   \item{SOURCE}{Data source reference}
#'   \item{LATITUDE}{Latitude coordinate (decimal degrees)}
#'   \item{LONGITUDE}{Longitude coordinate (decimal degrees)}
#'   \item{CONFIDENCE}{Confidence level of coordinate accuracy (1-5)}
#' }
#'
#' @source Kvassnes & Iversen 2013
#' @importFrom tibble tribble
#'
#' @examples
#' mine_tailings_coords()
#'
#' @export
mine_tailings_coords <- function() {
  tribble(
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
}


# N.B: Re Ronny Berdinsen, Mattilsynet (2025.12.15)

# Hei

# Tallene ble korrigerte pga en feilrapportering i 2022. Riktige tall er:

# -	2022: 4332 liter (9482liter - 5150 liter)
# -	2023: 2400 liter

# Merk også at statistikken ikke viser plantevernmidler brukt pr år, men importert mengde. Det er ikke gitt at det som er importert i
# eksempelvis 2022 er brukt av bonden samme år.

#' Copper oxide measurements by year
#'
#' Returns a tibble containing annual copper oxide measurements in kilograms
#' along with the average value across years.
#'
#' @return A tibble with 5 rows and 3 columns:
#' \describe{
#'   \item{year}{Year of measurement (2020-2024)}
#'   \item{amount_kg}{Amount of copper oxide in kilograms}
#'   \item{average}{Average amount across all years (4051 kg)}
#' }
#'
#' @importFrom tibble tribble
#'
#' @examples
#' copper_oxide_sales()
#'
#' @export
copper_oxide_sales <- function() {
  tribble(
    ~year , ~amount_kg , ~average_kg ,
     2020 ,       4051 ,        4051 ,
     2021 ,       4594 ,        4051 ,
     2022 ,       4332 ,        4051 ,
     2023 ,       2400 ,        4051 ,
     2024 ,       3189 ,        4051
  )
}

municipality_centroids <- function() {
  tribble(
    ~name         , ~x    , ~y    , ~source                                  ,
    "Lier"        , 10.24 , 59.78 , "approximate municipal centroid (WGS84)" ,
    "Sande"       , 10.22 , 59.60 , "approximate municipal centroid (WGS84)" ,
    "Svelvik"     , 10.41 , 59.60 , "approximate municipal centroid (WGS84)" ,
    "Øvre Eiker" ,  9.88 , 59.77 , "approximate municipal centroid (WGS84)" ,
    "Leikanger"   ,  6.81 , 61.18 , "approximate municipal centroid (WGS84)" ,
    "Ullensvang"  ,  6.68 , 60.32 , "approximate municipal centroid (WGS84)" ,
    "Sauherad"    ,  9.28 , 59.42 , "approximate municipal centroid (WGS84)"
  )
}


expect_case_studies <- function() {
  tribble(
    ~site_name                     , ~lat  , ~lon  , ~summary                                                                                                                                                                                                                                                         ,
    "Vågsfjorden/Harstad"         , 68.80 , 16.55 , "Multiple pollution sources: sewage, shipping, fishery, fish processing, aquaculture, other industrial activities (n=5). >150 data points in Vannmiljø with long-term monitoring. Harbour clean-up in Harstadbotn (some data may be outdated due to dredging)." ,

    "Ranfjorden/Mo i Rana"         , 66.31 , 14.14 , "Multiple pollution sources: metallurgy, shipping, sewage, fish processing, fishery, other industrial activities (n=6). >60 data points with multi-year monitoring. Historic submarine tailing disposal."                                                        ,

    "Sørfjorden/Hardangerfjorden" , 60.05 ,  6.25 , "Multiple submarine tailing disposal sites. Well-studied copper concentrations in sediment and echinoderms. Highest concentrations at source, return to background by Hardangerfjorden. Main literature focus site."                                             ,

    "Flekkefjorden/Dalsfjorden"    , 58.30 ,  6.66 , "Multiple submarine tailing disposal sites. Well-studied copper in sediment and echinoderms. Blue mussel biomonitoring data available."                                                                                                                          ,

    "Kola Peninsula vicinity"      , 69.00 , 30.50 , "Vicinity of Kola Nickel Smelter (Russia/Finland/Finnmark border). Aerial deposition of copper. Covered in literature."                                                                                                                                          ,

    "Kongsfjorden, Svalbard"       , 78.93 , 12.50 , "Literature coverage but little direct copper pollution. Opportunistic sampling (e.g., dead seals, ocean sampling)."
  )
}
