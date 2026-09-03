# REACH copper by PRODUCT-USE category (added 2026-09-03).
#
# The sector side (R/fct_reach.R) feeds the AEP source nodes. This side is a
# single manuscript figure -- what copper is declared to be USED FOR in
# Norwegian commerce -- for the introduction (fig02). Logic pulled out of
# docs/NBXX-REACH.qmd so the notebook and the figure target cannot drift, same
# reasoning as read_reach_sector_years().

#' Norwegian to English Names for the REACH Product-Use Categories
#'
#' The `Tekst` values on the "kobber sum på produkttype" sheet are Norwegian
#' Product Register use categories (Produktregisteret's *anvendelseskategorier*).
#' Translations are a best-effort LLM pass (Sam), carried with a `confidence`
#' flag and, where the source term is itself vague rather than merely hard to
#' translate, a short `note`. Miljodirektoratet maintains the definitive
#' category list and is the place to resolve the genuinely ambiguous ones (e.g.
#' "Process control agents").
#'
#' @return A tibble: `category_no`, `category_en`, `confidence`, `note`.
#' @export
reach_product_category_translations <- function() {
  tibble::tribble(
    ~category_no, ~category_en, ~confidence, ~note,
    "BRENSELTILSETNINGER", "Fuel additives", "high", NA_character_,
    "Absorpsjons/ og adsorpsjonsmaterialer", "Absorption and adsorption materials", "high", NA_character_,
    "ANDRE SMØREMIDLER", "Other lubricants", "high", NA_character_,
    "ARMERINGSMIDLER", "Reinforcing agents", "medium", "could be 'armouring agents' in some contexts",
    "Bindemidler", "Binders", "high", NA_character_,
    "Biocider", "Biocides", "high", NA_character_,
    "Borekjemikalier inkl råolje/gass", "Drilling chemicals incl. crude oil/gas", "high", NA_character_,
    "BOREOLJER", "Drilling oils", "high", NA_character_,
    "BUNNFELLINGSHINDRENDE MIDLER, GENERELT", "Anti-precipitation agents, general", "medium", "could be anti-settling / anti-sedimentation",
    "Fyllingsmidler", "Fillers", "high", NA_character_,
    "GJØDNING, GENERELT", "Fertilisers, general", "high", NA_character_,
    "Impregnering", "Impregnation agents", "high", NA_character_,
    "INSEKTSMIDDEL, INSEKTMIDLER OG ANDRE MIDLER MOT SKADEDYR PÅ PLANTER", "Insecticides and other plant pest control agents", "high", NA_character_,
    "Konstruksjonsmaterialer", "Construction materials", "high", NA_character_,
    "Lim", "Adhesives", "high", NA_character_,
    "Maling", "Paint", "high", NA_character_,
    "PH-REGULERENDE MIDLER, GENERELT", "pH-regulating agents, general", "high", NA_character_,
    "Prosessregulerendemidler", "Process control agents", "medium", "vague in source; industrial process aids (flocculants, defoamers, pH/redox control)",
    "Rengjøring", "Cleaning agents", "high", NA_character_,
    "Rustbeskyttelse", "Corrosion protection", "high", NA_character_,
    "SALT TIL GALVANISKE BAD", "Salts for electroplating baths", "high", NA_character_,
    "Sprengstoff", "Explosives", "high", NA_character_,
    "STØPEMASSER, GENERELT", "Casting compounds, general", "medium", "could be moulding compounds",
    "SYNTESERÅVARER OG MELLOMPRODUKTER", "Synthesis raw materials and intermediates", "high", "feedstock / intermediate chemicals for further manufacture",
    "Trykkfarger", "Printing inks", "high", NA_character_,
    "BLEKERE TIL FOTOGRAFISK FILM", "Bleaching agents for photographic film", "high", NA_character_,
    "BRUNERINGSSALTER", "Browning salts", "medium", "metal darkening / bluing salts",
    "Glasur, emalje", "Glaze, enamel", "high", NA_character_,
    "Herdere", "Hardeners / curing agents", "high", NA_character_,
    "Loddemidler", "Soldering agents / fluxes", "medium", "broad; could be just 'solders'",
    "BILPLEIEMIDLER, GENERELT", "Car care products, general", "high", NA_character_,
    "FLUSSMIDLER (SVEISING)", "Fluxes (welding)", "high", NA_character_,
    "PIGMENT TIL GLASURER, EMALJER OG GLASS", "Pigments for glazes, enamels and glass", "high", NA_character_,
    "Metalloverflatebehandlingsmidler", "Metal surface treatment agents", "high", NA_character_,
    "ANTIOKSIDANTER (ANTIOZONANTER)", "Antioxidants (antiozonants)", "high", NA_character_,
    "Poler og pleieMIDLER", "Polishes and care products", "medium", "odd capitalisation in source",
  )
}

#' Read REACH Copper Net Quantities by Product-Use Category and Year
#'
#' One row per (category, year) declaration on the "kobber sum på
#' produkttype" sheet, with the English category name joined on and the quantity
#' in kg. Net quantity is (imported + produced) minus exported, as for
#' [read_reach_sector_years()].
#'
#' @param path Path to the REACH xlsx.
#' @return A tibble: `product_type` (raw Norwegian), `category_en`,
#'   `confidence`, `note`, `year`, `netto_tonn`, `net_kg`.
#' @export
read_reach_product_years <- function(
  path = here_rel("inst/extdata/emissions/REACH_copper_prtd.xlsx")
) {
  readxl::read_excel(path, sheet = "kobber sum på produkttype") |>
    dplyr::rename(
      year = "AmountYear",
      netto_tonn = "Netto mengde i tonn",
      product_type = "Tekst"
    ) |>
    dplyr::mutate(
      year = as.integer(.data$year),
      netto_tonn = as.numeric(.data$netto_tonn),
      product_type = trimws(.data$product_type)
    ) |>
    dplyr::left_join(
      reach_product_category_translations(),
      by = dplyr::join_by("product_type" == "category_no")
    ) |>
    dplyr::mutate(net_kg = .data$netto_tonn * 1000)
}

#' Row-Level REACH Product Rows With Categories Lumped to Top N Plus "Other"
#'
#' Zero and negative rows are dropped first: a negative net quantity is a net
#' export, cannot carry a weight in [forcats::fct_lump_n()], and is not a "use".
#' This is the one place the lumping happens, so the bar panel and the trend
#' panel of the figure share exactly the same seven winners.
#'
#' @param product_years Output of [read_reach_product_years()].
#' @param n_lump Categories to keep before folding the rest to "Other".
#' @return `product_years`, filtered to positive rows, with `category_en`
#'   replaced by the lumped factor.
#' @export
reach_product_lumped <- function(product_years, n_lump = 7L) {
  product_years |>
    dplyr::filter(.data$netto_tonn > 0) |>
    dplyr::mutate(category_en = forcats::fct_lump_n(
      .data$category_en,
      n = n_lump, w = .data$netto_tonn, other_level = "Other"
    ))
}

#' Summarise REACH Product Categories, Top N Plus "Other"
#'
#' `sd_net_kg` is `NA` for any category with a single reporting year, which the
#' figure draws as a bar with no whisker. `n_years_reported` counts *distinct
#' years*, not rows -- "Other" pools many categories, so a row count there would
#' run into the hundreds.
#'
#' @param product_years Output of [read_reach_product_years()].
#' @param n_lump Categories to keep before folding the rest to "Other".
#' @return A tibble: `category_en`, `mean_net_kg`, `sd_net_kg`,
#'   `n_years_reported`, ordered by `mean_net_kg` descending.
#' @export
reach_product_summary <- function(product_years, n_lump = 7L) {
  reach_product_lumped(product_years, n_lump) |>
    dplyr::reframe(
      mean_net_kg = mean(.data$net_kg),
      sd_net_kg = stats::sd(.data$net_kg),
      n_years_reported = dplyr::n_distinct(.data$year),
      .by = "category_en"
    ) |>
    dplyr::distinct() |>
    dplyr::arrange(dplyr::desc(.data$mean_net_kg))
}

#' REACH Product Net Copper per Category per Year (Panel b)
#'
#' The same lumped categories as [reach_product_summary()], summed within each
#' (category, year). "Other" is a sum over its many sub-categories for that year.
#' A category with only negative (export) declarations in a given year simply has
#' no point that year.
#'
#' @param product_years Output of [read_reach_product_years()].
#' @param n_lump As [reach_product_lumped()].
#' @return A tibble: `category_en`, `year`, `net_kg`.
#' @export
reach_product_year_series <- function(product_years, n_lump = 7L) {
  reach_product_lumped(product_years, n_lump) |>
    dplyr::reframe(
      net_kg = sum(.data$net_kg),
      .by = c("category_en", "year")
    ) |>
    dplyr::arrange(.data$category_en, .data$year)
}

#' A Category -> Colour Map Shared by Both Panels
#'
#' `"Other"` is grey and always last; the rest take Dark2 hues in the order
#' given (the summary's mean-descending order), so the bars in panel (a) and the
#' lines in panel (b) agree.
#'
#' @param categories A character vector of category names, in the desired
#'   legend order (any `"Other"` is moved to the end).
#' @return A named character vector, category to hex colour.
#' @export
reach_product_palette <- function(categories) {
  cats <- unique(as.character(categories))
  cats <- c(setdiff(cats, "Other"), intersect("Other", cats))
  real <- setdiff(cats, "Other")
  real_cols <- if (length(real) <= 7L) {
    RColorBrewer::brewer.pal(8, "Dark2")[seq_len(max(length(real), 1L))]
  } else {
    grDevices::colorRampPalette(
      RColorBrewer::brewer.pal(8, "Dark2")[1:7]
    )(length(real))
  }
  stats::setNames(
    c(real_cols, rep("grey75", length(cats) - length(real))),
    cats
  )
}

#' Compact Log Labels Shared by Both Panels
#' @noRd
.reach_log_labels <- function() {
  scales::label_number(scale_cut = scales::cut_short_scale())
}

#' Panel (a): Bar Chart of Mean Net Copper by REACH Product Category
#'
#' Each y-axis label carries the number of reporting years in brackets; bars
#' show mean net copper on a log axis, with a +/- 1 SD whisker wherever more
#' than one year was reported. The lower whisker is clamped to `mean / 20`, or
#' it would run off the bottom of the log axis.
#'
#' @param summary Output of [reach_product_summary()].
#' @param palette A category -> colour map from [reach_product_palette()];
#'   computed from `summary` when `NULL`.
#' @return A ggplot.
#' @export
reach_product_plot <- function(summary, palette = NULL) {
  if (is.null(palette)) {
    palette <- reach_product_palette(summary$category_en)
  }
  d <- summary |>
    dplyr::mutate(
      category_en = factor(
        as.character(.data$category_en), levels = names(palette)
      ),
      label = paste0(
        .data$category_en, "  (", .data$n_years_reported, " yr)"
      ),
      label = forcats::fct_reorder(.data$label, .data$mean_net_kg),
      err_lo = pmax(
        .data$mean_net_kg - .data$sd_net_kg, .data$mean_net_kg / 20
      ),
      err_hi = .data$mean_net_kg + .data$sd_net_kg
    )

  ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = .data$label, y = .data$mean_net_kg, fill = .data$category_en
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$err_lo, ymax = .data$err_hi),
      width = 0.3, linewidth = 0.4, colour = "grey25", na.rm = TRUE
    ) +
    ggplot2::scale_fill_manual(values = palette, guide = "none") +
    ggplot2::scale_y_continuous(
      labels = .reach_log_labels(), transform = "log10",
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank()) +
    ggplot2::labs(
      x = NULL,
      y = "Mean net copper (kg/yr); whiskers ± 1 SD across reporting years"
    )
}

#' Panel (b): Net Copper per REACH Product Category Over Time
#'
#' Same categories, same colours, same log axis as panel (a).
#'
#' @param year_series Output of [reach_product_year_series()].
#' @param palette A category -> colour map from [reach_product_palette()].
#' @return A ggplot.
#' @export
reach_product_trend_plot <- function(year_series, palette) {
  d <- year_series |>
    dplyr::mutate(category_en = factor(
      as.character(.data$category_en), levels = names(palette)
    ))
  yrs <- sort(unique(d$year))

  ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = .data$year, y = .data$net_kg,
      colour = .data$category_en, group = .data$category_en
    )
  ) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(size = 1.6) +
    ggplot2::scale_colour_manual(
      values = palette, name = NULL,
      labels = function(x) stringr::str_wrap(x, 30)
    ) +
    ggplot2::scale_x_continuous(breaks = yrs) +
    ggplot2::scale_y_continuous(
      labels = .reach_log_labels(), transform = "log10"
    ) +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 4, byrow = TRUE)) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 9)
    ) +
    ggplot2::labs(x = NULL, y = "Net copper (kg/yr)")
}

#' The Two-Panel REACH Product Figure
#'
#' (a) mean net copper by category, (b) the same categories over time. Built
#' from the row-level `product_years` so the lumping is done once and both
#' panels agree.
#'
#' @param product_years Output of [read_reach_product_years()].
#' @param n_lump As [reach_product_lumped()].
#' @return A patchwork object.
#' @export
reach_product_figure <- function(product_years, n_lump = 7L) {
  summary <- reach_product_summary(product_years, n_lump)
  series <- reach_product_year_series(product_years, n_lump)
  palette <- reach_product_palette(summary$category_en)

  pa <- reach_product_plot(summary, palette)
  pb <- reach_product_trend_plot(series, palette)

  patchwork::wrap_plots(pa, pb, ncol = 1, heights = c(1, 0.85)) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(tag_levels = "a", tag_suffix = ")") &
    ggplot2::theme(
      legend.position = "bottom",
      plot.tag = ggplot2::element_text(face = "bold")
    )
}

#' Draw the REACH Product Figure and Write it to `figures/`
#'
#' @param product_years Output of [read_reach_product_years()].
#' @param path Output PNG path.
#' @param n_lump As [reach_product_lumped()].
#' @param width,height,dpi Canvas.
#' @return `path`.
#' @export
write_reach_product_figure <- function(
  product_years, path = here_rel("figures/fig02-reach-products.png"),
  n_lump = 7L, width = 10.5, height = 8.5, dpi = 200
) {
  p <- reach_product_figure(product_years, n_lump)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  ggplot2::ggsave(
    path, p,
    width = width, height = height, dpi = dpi, bg = "white",
    device = ragg::agg_png
  )
  path
}
