# Rasterise the node-card SVG icons to PNG. Hand-run, never a target.
#
# The card renderer (R/fct_card_icons.R) loads PNGs, not SVGs: rsvg's in-memory
# raster path segfaults on this machine, and a pre-baked PNG is deterministic and
# keeps the render path free of a fragile native call. The SVGs in
# inst/extdata/icons/ are the source of truth; this bakes them.
#
# Needs the `rsvg` package (Suggests). Run after editing an icon SVG:
#   Rscript scripts/bake_card_icons.R

if (!requireNamespace("rsvg", quietly = TRUE)) {
  stop("Install the 'rsvg' package to bake card icons: install.packages('rsvg')")
}

here::i_am("Readme.md")
icon_dir <- here::here("inst/extdata/icons")
svgs <- list.files(icon_dir, pattern = "\\.svg$", full.names = TRUE)
if (length(svgs) == 0) stop("No SVGs in ", icon_dir)

# 128 px is a few times the on-card footprint (~48 px at 300 dpi), so the
# rasterGrob downscale in card_icon_grob() stays clean.
for (svg in svgs) {
  png <- sub("\\.svg$", ".png", svg)
  rsvg::rsvg_png(svg, png, width = 128L, height = 128L)
  message("baked ", basename(png))
}
