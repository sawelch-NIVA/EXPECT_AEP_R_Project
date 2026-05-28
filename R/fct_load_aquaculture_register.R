library(sf)
library(here)
library(tidyverse)

aquaculture <- sf::read_sf(here(
    "inst/extdata/shapefiles/Aquaculture_Locations/Fiskeridir_Akvakulturregistere.shx"
)) # that was easy

# View summary in browser
# aquaculture |> sf::st_drop_geometry() |> dfSummary()

# great columns!
aquaculture |> glimpse()


# actually, they have an API
# "https://api.fiskeridir.no/pub-aqua/api/swagger-ui/index.html?configUrl=/pub-aqua/api/api-docs/swagger-config"

aquaculture_clean <- aquaculture |>
    mutate(
        .keep = "none",
        capacity = kapasitet_,
        capacity_temp = tempcapaci,
        capacity_unit = recode_values(
            kapasitet1,
            "TN" ~ "tonnes",
            "DA" ~ "dekar?",
            "STK" ~ "individuals",
            "M3" ~ "m^3",
            "M2" ~ "m^2"
        ),
        placement = recode_values(
            plassering,
            "SJØ" ~ "Sea",
            "LAND" ~ "Terrestrial",
            "HAV" ~ "Ocean"
        ),
        medium = vannmiljo,
        fylke,
        kommune,
        lat,
        lon,
        til_arter,
        geometry
    )

# TODO
# I think before trying to do anything with the aquaculture data I should... actually read that report from Maj? Then email her.
