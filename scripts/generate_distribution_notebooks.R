# Scaffold generator for the per-compartment / per-species-group
# distribution notebooks (docs/NBXX-Distributions-<Name>.qmd).
#
# This is a ONE-TIME scaffold, not a build step: it writes real .qmd files
# that you then hand-edit (add interpretation, delete sections you don't
# care about, restructure) exactly like any other file in docs/. It is
# never re-run automatically, and re-running it manually is safe:
#   - a file that doesn't exist yet is created in full
#   - a file that already exists is left untouched EXCEPT that any group
#     not yet represented in it (detected by chunk label) gets its section
#     appended at the end -- your existing prose/edits are never rewritten
#     or removed.
# New groups only show up here after load_literature_pqt has actually been
# rebuilt with them -- see the two-pass caveat documented in _targets.R
# next to outlier_groups_compartment / outlier_groups_biota.
#
# docs/NBXX-Outliers.qmd (the G. morhua liver case study) is intentionally
# untouched by this script -- it stays as the fully hand-authored deep dive
# this factory/report machinery was built from.
#
# Usage: source this file, or `Rscript scripts/generate_distribution_notebooks.R`

here::i_am("Readme.md")
suppressPackageStartupMessages(pkgload::load_all(quiet = TRUE))
suppressPackageStartupMessages(library(dplyr))

MIN_N <- 10
DOCS_DIR <- here::here("docs")

data <- targets::tar_read(load_literature_pqt)
compartment_groups <- get_compartment_groups(data)
biota_groups <- get_biota_groups(data)

# ---- Templating ------------------------------------------------------
# NOTE: glue's default {}  delimiters would collide with the ```{r} chunk
# fences and R code braces we're writing out, so every glue() call below
# uses <<...>> instead.

qmd_header <- function(title) {
  glue::glue(
    .open = "<<",
    .close = ">>",
    '---
title: "<<title>>"
format: html
---

::: callout-note
This notebook was scaffolded by `scripts/generate_distribution_notebooks.R`
from the outlier-analysis targets factory (`_targets.R`,
`R/fct_outlier_groups.R`, `R/fct_outlier_detection.R`,
`R/fct_outlier_report.R`). The plots/tables below are generated, but
analysis and interpretation are added by hand, section by section.
Re-running the generator only appends sections for groups not yet present
here -- it never rewrites or removes anything you have written.

See [NBXX-Outliers.qmd](NBXX-Outliers.qmd) for the fully hand-authored
*G. morhua* liver case study this workflow was built from, including
year/site/campaign breakdowns and the reasoning behind the outlier flags.
:::

```{r}
#| label: setup
library(tidyverse)
library(targets)
library(patchwork)
library(flextable)
pkgload::load_all(quiet = TRUE)

set_flextable_defaults(
  font.size = 10,
  font.family = "Arial",
  digits = 2,
  theme_fun = theme_vanilla,
  padding = 1
)

ggplot_base_size <- 20
theme_set(theme_minimal(base_size = ggplot_base_size, base_family = "Aptos"))
```
'
  )
}

# One section per group: a heading, a TODO prompt for manual analysis, a
# read chunk, three flagged/faceted distribution plots + tables (year,
# site, campaign), and one whole-group Winsorization comparison plot.
render_section <- function(target_name, group_name, group_label, heading) {
  var_name <- paste0("grp_", group_name)

  glue::glue(
    .open = "<<",
    .close = ">>",
    '
## <<heading>>

<<group_label>>. Flags are computed separately within each year/site/campaign
group below (Tukey fences x RMZ, `n < <<MIN_N>>` not tested); the final
figure compares Winsorization/trimming of the pooled distribution.

TODO: add your own analysis/interpretation of this group here.

```{r}
#| label: read-<<group_name>>
<<var_name>> <- tar_read(<<target_name>>)
<<var_name>>$data <- <<var_name>>$data |>
  mutate(SAMPLING_YEAR = factor(year(SAMPLING_DATE)))
<<var_name>>_unit <- <<var_name>>$summary$unit
```

```{r}
#| label: fig-outlier-year-<<group_name>>
#| fig-cap: "<<group_label>>: measured value by sampling year."
#| fig-height: !expr outlier_fig_height(<<var_name>>$data$SAMPLING_YEAR)
plot_outlier_distribution(
  <<var_name>>$data,
  facet_col = "SAMPLING_YEAR",
  x_label = paste0("Measured value (", <<var_name>>_unit, ")"),
  y_label = "Year",
  min_n = <<MIN_N>>
)
```

```{r}
#| label: tbl-outlier-year-<<group_name>>
outlier_flag_table(flag_by_facet(<<var_name>>$data, "SAMPLING_YEAR", min_n = <<MIN_N>>))
```

```{r}
#| label: fig-outlier-site-<<group_name>>
#| fig-cap: "<<group_label>>: measured value by site."
#| fig-height: !expr outlier_fig_height(<<var_name>>$data$SITE_CODE, base = 4)
plot_outlier_distribution(
  <<var_name>>$data,
  facet_col = "SITE_CODE",
  x_label = paste0("Measured value (", <<var_name>>_unit, ")"),
  y_label = "Site",
  panel_facet = "SITE_GEOGRAPHIC_FEATURE",
  min_n = <<MIN_N>>
)
```

```{r}
#| label: tbl-outlier-site-<<group_name>>
outlier_flag_table(flag_by_facet(<<var_name>>$data, "SITE_CODE", min_n = <<MIN_N>>))
```

```{r}
#| label: fig-outlier-campaign-<<group_name>>
#| fig-cap: "<<group_label>>: measured value by campaign."
#| fig-height: !expr outlier_fig_height(<<var_name>>$data$CAMPAIGN_NAME_SHORT)
plot_outlier_distribution(
  <<var_name>>$data,
  facet_col = "CAMPAIGN_NAME_SHORT",
  x_label = paste0("Measured value (", <<var_name>>_unit, ")"),
  y_label = "Campaign",
  min_n = <<MIN_N>>
)
```

```{r}
#| label: tbl-outlier-campaign-<<group_name>>
outlier_flag_table(flag_by_facet(<<var_name>>$data, "CAMPAIGN_NAME_SHORT", min_n = <<MIN_N>>))
```

```{r}
#| label: fig-outlier-treatment-<<group_name>>
#| fig-cap: "<<group_label>>: effect of outlier treatment on the pooled distribution."
plot_outlier_treatment_comparison(
  <<var_name>>,
  x_label = paste0("Measured value (", <<var_name>>_unit, ")")
)
```
'
  )
}

# ---- File assembly / idempotent writing -------------------------------

group_name_present <- function(file_path, group_name) {
  lines <- readLines(file_path, warn = FALSE)
  any(grepl(paste0("label: read-", group_name, "$"), lines, fixed = FALSE))
}

write_or_append <- function(file_path, title, sections) {
  if (!file.exists(file_path)) {
    writeLines(
      paste(c(qmd_header(title), sections), collapse = "\n\n"),
      file_path
    )
    cat(
      "Created",
      basename(file_path),
      "with",
      length(sections),
      "section(s)\n"
    )
    return(invisible())
  }

  missing <- vapply(
    seq_along(sections),
    \(i) !group_name_present(file_path, names(sections)[i]),
    logical(1)
  )

  if (!any(missing)) {
    cat("Up to date:", basename(file_path), "\n")
    return(invisible())
  }

  cat(
    "Appending",
    sum(missing),
    "new section(s) to",
    basename(file_path),
    ":",
    paste(names(sections)[missing], collapse = ", "),
    "\n"
  )
  cat(
    paste(
      c(
        "",
        paste0(
          "<!-- Sections appended ",
          Sys.Date(),
          " by scripts/generate_distribution_notebooks.R -->"
        ),
        sections[missing]
      ),
      collapse = "\n\n"
    ),
    file = file_path,
    append = TRUE
  )
}

# ---- Compartment notebooks ---------------------------------------------

for (compartment_name in unique(compartment_groups$.compartment)) {
  rows <- compartment_groups |> filter(.compartment == compartment_name)

  sections <- rows |>
    purrr::pmap_chr(function(.compartment, .subcompartment, .group_name) {
      render_section(
        target_name = paste0("outlier_compartment_", .group_name),
        group_name = .group_name,
        group_label = paste(.compartment, .subcompartment, sep = " / "),
        heading = .subcompartment
      )
    })
  names(sections) <- rows$.group_name

  file_name <- paste0(
    "NBXX-Distributions-",
    slugify_name(compartment_name),
    ".qmd"
  )
  write_or_append(
    file.path(DOCS_DIR, file_name),
    title = paste0("Distributions: ", compartment_name, " Compartment"),
    sections = sections
  )
}

# ---- Biota species-group notebooks --------------------------------------

for (species_group_name in unique(biota_groups$.species_group)) {
  rows <- biota_groups |> filter(.species_group == species_group_name)

  sections <- rows |>
    purrr::pmap_chr(function(.species_group, .species, .tissue, .group_name) {
      render_section(
        target_name = paste0("outlier_biota_", .group_name),
        group_name = .group_name,
        group_label = paste(.species_group, .species, .tissue, sep = " / "),
        heading = paste0(.species, ": ", .tissue)
      )
    })
  names(sections) <- rows$.group_name

  file_name <- paste0(
    "NBXX-Distributions-",
    slugify_name(species_group_name),
    ".qmd"
  )
  write_or_append(
    file.path(DOCS_DIR, file_name),
    title = paste0("Distributions: ", species_group_name),
    sections = sections
  )
}
