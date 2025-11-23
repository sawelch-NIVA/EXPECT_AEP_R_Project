# main_table <- arrow::read_parquet("data/clean/literature_data.parquet")

# geo_stuff <- main_table |>
#   select(COUNTRY_ISO, OCEAN_IHO) |>
#   distinct()

#   COUNTRY_ISO                OCEAN_IHO
#   <chr>                  <chr>
# 1 Norway                 "Norway"
# 2 Not reported           "Greenland Sea"
# 3 Russia                 "Barents Sea"
# 4 Norway                 "Barents Sea"
# 5 NA                      NA
# 6 Svalbard and Jan Mayen "Not relevant"
# 7 Norway                 ""
# 8 Not reported           "Barents Sea"
# 9 Norway                 "Not relevant"
