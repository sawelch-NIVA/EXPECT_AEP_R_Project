# One-shot experiment: can annotation_map_tile() give us a nicer basemap for
# Repparfjorden than the geom_polygon(norway_map) coastline used in
# docs/NBXX-reparfjorden.qmd? Not wired into the pipeline. Run by hand:
#   Rscript scripts/try_repparfjorden_tile_map.R
# Output: images/dev/repparfjorden-tile-map.png
#
# Real bug found and worked around here, not just a scripting mistake: with
# annotation_map_tile() as the first layer and no explicit crs on coord_sf(),
# ggspatial/ggplot2 pick EPSG:3857 (Web Mercator) as the panel's working CRS,
# but leave x_range/y_range as the raw lon/lat degree numbers (23.85-24.55,
# 70.38-70.62) instead of reprojecting them. ggspatial then reads those
# degrees as if they were Mercator *metres*, treats the resulting ~0.7 m
# bbox as degenerate, and bails out with "bounding box is too small" --
# silently returning a blank tile grob. Forcing coord_sf(crs = 4326) below
# makes the working CRS match the units the range is actually in, which
# fixes it.

suppressMessages({
  library(ggplot2)
  library(ggspatial)
  library(sf)
  library(rosm)
})

# Which basemap to render. Both sources are registered below regardless (each
# only fails loudly if you actually select it without its key set).
#   "thunderforest_landscape" -- hillshade + landcover, no place-name labels
#     at this zoom (tested 2026-08-26), only route-number shields.
#   "tracestrack_topo"        -- topographic style, but per Tracestrack's own
#     API docs "only combined (base and label merged) tile names are
#     supported" -- there is no label-free variant of this endpoint, place
#     names WILL be baked in. Using topo_en (English labels); topo_ (language-
#     neutral) would keep local Norwegian names instead. Also hit a real,
#     unresolved, non-deterministic rendering bug with this source
#     specifically (2026-08-26): identical code/cached tiles produced a
#     blank image, a colour-corrupted image, and a correct image across
#     three consecutive runs -- see the writeup in
#     quarto-ms-template/manuscript/supplementary/tiles-mre.qmd. Always
#     eyeball the actual output file before trusting it.
#
# Defaulting to thunderforest_landscape: it's the one that reliably works.
tile_type <- "thunderforest_landscape"

# Same Sys.getenv() pattern as PUSHOVER_USER/PUSHOVER_APP in run_pipeline.R --
# keys live in .Renviron, never in this file.

thunderforest_api_key <- Sys.getenv("THUNDERFOREST_API_KEY")
if (tile_type == "thunderforest_landscape" && thunderforest_api_key == "") {
  stop(
    "THUNDERFOREST_API_KEY is not set. Get a free key at ",
    "https://www.thunderforest.com/ and add THUNDERFOREST_API_KEY=<key> ",
    "to .Renviron (project or user), then restart R."
  )
}
if (thunderforest_api_key != "") {
  # Thunderforest requires an API key on every request now (rosm's built-in
  # "thunderforestlandscape"/"thunderforestoutdoors" types predate that and
  # have no key in their hardcoded URL, so they 401). Registering our own
  # source with the key spliced into the URL works around it.
  register_tile_source(
    thunderforest_landscape = source_from_url_format(
      paste0(
        "https://api.thunderforest.com/landscape/${z}/${x}/${y}.png?apikey=",
        thunderforest_api_key
      ),
      extension = "png",
      max_zoom = 22
    )
  )
}

tracestrack_api_key <- Sys.getenv("TRACESTRACK_API_KEY")
if (tile_type == "tracestrack_topo" && tracestrack_api_key == "") {
  stop(
    "TRACESTRACK_API_KEY is not set. Add TRACESTRACK_API_KEY=<key> to ",
    ".Renviron (project or user), then restart R."
  )
}
if (tracestrack_api_key != "") {
  # Tile API serves webp by default, but rosm's tile.loadimage() only knows
  # png/jpeg -- request .png explicitly. Tiles come back 512x512 (retina-ish)
  # rather than the usual 256; rosm infers tile pixel size from the actual
  # downloaded image (check.dimensions()) rather than assuming 256, so this
  # is not a problem.
  register_tile_source(
    tracestrack_topo = source_from_url_format(
      paste0(
        "https://tile.tracestrack.com/topo_en/${z}/${x}/${y}.png?key=",
        tracestrack_api_key
      ),
      extension = "png",
      max_zoom = 18
    )
  )
}

# Repparfjorden itself, per data/clean/points_of_interest.csv
repparfjorden <- data.frame(
  lon = 24.1,
  lat = 70.5,
  label = "Repparfjorden"
)

repparfjorden_sf <- st_as_sf(
  repparfjorden,
  coords = c("lon", "lat"),
  crs = 4326
)

# Tight box around the fjord itself (it's ~14 km long), not the wide
# (23-25, 70-71) box used in the notebook -- that's ~55 x 111 km, too coarse
# to see fjord-scale detail once real tiles are doing the work.
rf_bbox <- st_bbox(
  c(xmin = 23.85, xmax = 24.55, ymin = 70.38, ymax = 70.62),
  crs = st_crs(4326)
)

p <- ggplot() +
  annotation_map_tile(
    tile_type,
    zoomin = 0,
    cachedir = "rosm.cache/"
  ) +
  layer_spatial(repparfjorden_sf, size = 3, colour = "red") +
  geom_text(
    data = repparfjorden,
    aes(x = lon, y = lat, label = label),
    colour = "red",
    fontface = "bold",
    vjust = -1.2,
    size = 4
  ) +
  coord_sf(
    crs = sf::st_crs(4326),
    xlim = c(rf_bbox["xmin"], rf_bbox["xmax"]),
    ylim = c(rf_bbox["ymin"], rf_bbox["ymax"]),
    expand = FALSE
  ) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr", which_north = "true") +
  theme_void() +
  labs(title = "Repparfjorden") +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 4)
    )
  )

p

dir.create("images/dev", showWarnings = FALSE, recursive = TRUE)
out_path <- sprintf("images/dev/repparfjorden-tile-map-%s.png", tile_type)
ggsave(out_path, p, width = 8, height = 6.5, dpi = 200, bg = "white")

cat("saved", out_path, "\n")
