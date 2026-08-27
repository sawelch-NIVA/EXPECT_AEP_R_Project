# Builds the two-panel geographic locator figure for each spatially-scoped AEP
# (A002 Repparfjorden, A003 Sorfjorden):
#   a) the AEP's bounding box in red over the whole study area
#   b) a zoom to the box + 20% margin, on a fresh high-res NE coastline, with
#      the sampling sites that fall inside the box
#
# Not wired into the pipeline (targets). It reads three targets and writes to
# figures/. Re-run by hand with:
#   Rscript scripts/build_geographic_aep_figure.R

suppressMessages({
  library(targets)
  library(ggplot2)
  library(sf)
  library(dplyr)
  library(patchwork)
  pkgload::load_all(".", quiet = TRUE)
})

sf::sf_use_s2(FALSE)

wgs84_geo <- tar_read(wgs84_geography)
manifest <- tar_read(aep_manifest)
lit <- tar_read(load_literature_pqt)

poi_all <- readr::read_csv(
  here::here("data/clean/points_of_interest.csv"),
  show_col_types = FALSE
)

# One site per distinct location, with its compartment for colouring.
sites <- lit |>
  filter(!is.na(LONGITUDE), !is.na(LATITUDE)) |>
  distinct(SITE_CODE, LONGITUDE, LATITUDE, ENVIRON_COMPARTMENT) |>
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE)

slug <- c(A002 = "repparfjorden", A003 = "sorfjorden")
poi_name <- c(A002 = "Repparfjorden", A003 = "Sorfjorden")

for (id in c("A002", "A003")) {
  poi <- poi_all |> filter(name == poi_name[[id]])
  fig <- aep_locator_figure(
    aep_id = id,
    manifest = manifest,
    wgs84_geo = wgs84_geo,
    sites = sites,
    poi = poi
  )
  path <- here::here("figures", sprintf("aep-locator-%s.png", slug[[id]]))
  ggsave(path, fig, width = 10, height = 6, dpi = 300, bg = "white") |>
    suppressWarnings()
  cat("wrote", path, "\n")
}
