# CLAUDE.md

Orientation notes for working in this repo. Written 2026-07-29 from a read-only
skim of the tree at commit `d80033e`. Sections marked **UNCERTAIN** are my
inference, not established fact, and should be corrected by Sam rather than
acted on.

## 1. Purpose

A research compendium for a single paper: *"Copper Pollution in the Arctic: An
Aggregate Exposure Pathway for Understanding Sources, Sinks, and
Bioavailability"* (Welch et al., NIVA / Akvaplan-NIVA / NMBU).

The scientific goal is to assemble a conceptual, weight-of-evidence-based
**Aggregate Exposure Pathway (AEP)** for copper in Norwegian / Arctic marine and
freshwater systems: where copper is emitted, how it moves, where it accumulates,
and what concentrations organisms actually see. It is explicitly *not* a
modelling paper. The deliverables are:

- `index.qmd`, the manuscript draft (currently prose plus embedded figures, with
  inline `**Comment:**` notes to co-authors left in the text).
- A set of `docs/*.qmd` notebooks that document data provenance, QC, and
  exploratory analysis, published as a Quarto website.
- A supporting R package (`STOPAEP`) of project functions in `R/`.

Two data streams feed it:

1. **Vannmiljø** (Norwegian Environment Agency water-quality database), bulk
   exported and heavily cleaned/reshaped in-pipeline.
2. **Literature data**, extracted by hand through a systematic review into the
   **eData** format (schema and vocabularies come from the `NIVANorge/eDataDRF`
   package), with per-study quality scores under a **CREED** criteria set.

Both are converted into the same eData table shape (campaigns, references,
sites, parameters, methods, samples, biota, measurements) and joined into one
long measurements table. Everything downstream reads from that.

## 2. Mechanics

### 2.1 Layout

| Path | Role |
|---|---|
| `_targets.R` | The whole pipeline, ~1300 lines, single `list()` of targets |
| `R/fct_*.R` | Project functions, loaded via `pkgload::load_all()` (this is an R package, `STOPAEP`) |
| `docs/*.qmd` | Notebooks, rendered into the website |
| `index.qmd` | Manuscript draft |
| `scripts/*.R` | Ad-hoc and one-shot scripts, **not** part of the pipeline |
| `data/raw/` | Bulk inputs, untouched |
| `data/clean/` | Split into subdirectories 2026-08-06, see 2.1.1 |
| `_targets/` | Target store (`qs` format), gitignored |
| `_site/`, `_freeze/`, `.quarto/` | Quarto build output and caches, gitignored |
| `tests/testthat/` | 29 test files against 49 in `R/`, as of 2026-08-07. No longer the "effectively unused" single file this once was. |

### 2.1.1 `data/clean/` is organised by authority, not by topic (2026-08-06)

It had grown to thirty files in one flat directory. The split is by **who owns
the file**, because that is the distinction this project keeps relying on and the
one a flat listing hid:

| Directory | Contents | Contract |
|---|---|---|
| `aep/` | `aep_nodes`, `aep_node_members`, `aep_node_groups`, `aep_edges`, `aep_manifest`, `aep_membership` | Hand-edited. Pipeline reads, **never writes**. |
| `decisions/` | `group_ids`, `group_decisions`, `unit_corrections` | Hand-edited. Same contract. |
| `lookups/` | `Vm_*`/`vm_*` lookups, `species_common_names_cache`, `CREED_Copper_Purpose.yml` | Reference tables, occasionally hand-corrected. |
| `spatial/` | `study_area_shapefile.*` | Static geometry. |
| `derived/` | `prtr_emissions_summary`, `aep_literature_summary_claude`, `literature_data.parquet` | **Written by a script or the pipeline.** Safe to regenerate, never hand-edit. |
| `archive/` | Seven files nothing reads | See `data/clean/archive/README.md`. |

Two consequences worth knowing before touching it:

- **Anything in `derived/` can be deleted and regenerated; nothing in `aep/` or
  `decisions/` can.** That is the whole point of the split. Before 2026-08-06 a
  hand-scored AEP node file and a script output sat side by side with nothing
  distinguishing them.
- **Moving a file here invalidates its `format = "file"` target and everything
  downstream.** The 2026-08-06 move left 85 of 106 targets outdated, roughly a
  35 minute rebuild, because the Vannmiljø lookups feed the whole chain. Budget
  for it, or move only files outside that chain.

`archive/` holds files no code path reads, verified by grep at the time of the
move. They are archived rather than deleted because "unreferenced" is not
"unwanted": several are raw exports, and one is the input to a step that was
never wired up. Its README lists each with a date and a reason.

### 2.2 The pipeline

`targets` + `tarchetypes`, with `crew` for parallelism and `pointblank` for
validation. Roughly:

```
Vannmiljø raw CSVs ─┐
  + lookup tables   ├─> join ─> filter ─> resolve conflicts ─> split sites ─> vm_edata_* tables ─┐
                    ┘                                                                            │
eData literature files ─> read per module ─> validate ─> join ─────────────────────────────────> literature_joined
                                                                                                 │
                                    literature_clean ─> literature_clean_standardised ─> load_literature_pqt
                                                                                                 │
              ┌──────────────────────────────┬───────────────────────────────┬──────────────────┘
              v                              v                               v
    summarise_literature_data       outlier_* (tar_map factory)      maps / thresholds / QC report
                                              v
                                docs/NBXX-Distributions-*.qmd
```

`load_literature_pqt` is the hub target. Almost every notebook starts with
`tar_read(load_literature_pqt)` or `tar_read(summarise_literature_data)`.

Key conventions:

- Section headers in `_targets.R` use `## # Name ----` / `### # Name ----` so
  they fold in the editor outline. Depth of `#` tracks nesting.
- Unit standardisation, LOD/LOQ imputation (`x / sqrt(2)`), and country/ocean
  merging all happen in `literature_clean_standardised`.
- Notebooks are rendered *by* the pipeline via `tar_quarto()` targets, so
  rendering is itself dependency-tracked.

### 2.2.1 Rendering is deliberately minimal (from 2026-07-29)

Quarto rendering is the slowest part of this project, so it is now opt-in on
**two** independent switches. To make a document build, you must add it to
**both**:

1. `_targets.R`, as a `tar_quarto()` target. Only `render_index` and
   `render_nbxx_sample_groups` exist. This controls the pipeline.
2. `_quarto.yml`, in the `project: render:` list. Without this, `quarto render`
   walks and rebuilds every `.qmd` in the tree regardless of targets.

Everything else is **parked**: the `.qmd` files still exist and carry
`execute: freeze: true` in their frontmatter, and can still be built by hand
with `quarto render docs/<file>.qmd`, but no project build touches them. The
sidebar in `_quarto.yml` is trimmed to match the render list, because entries
pointing at unbuilt files render as dead links.

Outputs: `index.qmd` produces **html** (for Sam's own review) and **docx** (for
sharing, via `custom-reference-doc.docx`). The docx format is declared in
`index.qmd`'s own frontmatter, not project-level, so notebooks do not also emit
Word files. Project-level html settings still merge into it; verified with
`quarto inspect`.

### 2.2.2 targets must be told about the package (fixed 2026-07-29)

`tar_option_set(imports = "STOPAEP")` is **load-bearing**. Project functions
live in the `STOPAEP` package namespace, because `_targets.R` calls
`pkgload::load_all()` rather than sourcing files into the global environment.
targets only hashes objects in its own environment, so without `imports` it
never sees those functions change: editing any `R/fct_*.R` file invalidated
**nothing**, and `tar_make()` silently reused stale results.

Verified: before the fix, changing `sample_triage_groups()` left
`tar_outdated()` reporting `(none)`. This is almost certainly the cause of the
`load_literature_pqt` "doesn't properly update" note in section 3.7.

If results ever look stale again, check this line first.

### 2.3 The outlier / distributions machinery

This is the newest and most intricate part, and the epicentre of the current
mess. Three moving pieces:

1. **`R/fct_outlier_groups.R`** derives group tables from the data:
   `get_compartment_groups()` (compartment x subcompartment) and
   `get_biota_groups()` (species group x species x tissue), plus
   `slugify_name()` which defines the naming scheme everything else keys on.
2. **`_targets.R`** builds one target per group with `tar_map()`
   (`outlier_compartment_<slug>`, `outlier_biota_<slug>`), each running
   `outlier_group_analysis()`: Tukey fences on log10, robust modified z-score,
   Hartigan's dip test, Winsorization comparison. Groups with `n < 10`
   (`outlier_min_n`) get a distribution but no flags.
3. **`scripts/generate_distribution_notebooks.R`** scaffolds
   `docs/NBXX-Distributions-<Name>.qmd`, one file per compartment or species
   group, one section per target. It is idempotent and append-only: it never
   rewrites hand-written prose, it only appends sections for groups not yet
   present (detected by `#| label: read-<group_name>` chunk labels).

The intended loop is: rebuild data, run the generator, hand-edit the new
sections, render. `docs/NBXX-Outliers.qmd` is the hand-authored *G. morhua*
liver case study the whole thing was generalised from, and the generator
deliberately leaves it alone.

**There is a documented two-pass (really three-pass) caveat**, explained in
comments at `_targets.R:84-98`: `tar_map()` needs its `values` at pipeline
*definition* time, but those values are read out of the target store with
`tar_read(load_literature_pqt)`. So a genuinely new group requires
`tar_make()` (rebuild data) then `tar_make()` again (pick up the new branch)
then run the generator (scaffold the file) then `tar_make()` once more (define
its render target). This is inherent to data-dependent static branching.

### 2.3.1 Sam cannot see inline images (noted 2026-08-05)

He works in VSCode / Positron through an unofficial Claude Code extension that
does not render images. **Anything visual is invisible to him unless it is a
file he can open himself.**

Practical consequences, and they apply to every plotting task in this repo:

- **Write visual output to a stable path inside the project**, not to a scratch
  or temp directory, and **say the path**. A plot rendered only into a scratchpad
  effectively does not exist.
- **Describe what the image shows in words.** "N005's boxplot spans four orders
  with two clusters and a void between them" is the part that survives; the
  picture is not.
- Reviewing a plot by eye is *my* job before handing it over, since he cannot do
  it from the conversation. Render it, look at it, fix what is wrong, then report
  what it looks like.
- **My own eyeballing of a screenshot is not reliable for subtle layout bugs**
  (text overlap, spacing, precise alignment), learned the hard way 2026-08-08
  fixing a node-card corner-id marker: repeated "looks fine now" verdicts from
  reading rendered PNGs turned out wrong, several times in a row, wasting many
  fix-render-look iterations before the actual cause was found. For that class
  of bug, verify structurally instead of by eye where possible -- read back
  actual coordinates (`ggplot_build()` panel params, `grid::current.viewport()`),
  diff pixel regions programmatically, or render a debug marker at the exact
  intended bounds and check it numerically. If a screenshot is genuinely the
  only check available, say so and flag the result as an impression, not a
  verified fix.

`figures/` is the conventional destination: `figures/aep.png` for the diagram,
`figures/node_cards/*.png` for the per-node cards, and `triage/` for the triage
panels (which the notebooks link rather than embed).

**`figures/dev/` is where scratch renders go** (added 2026-08-10, gitignored).
Before/after pairs for a styling change, variant sweeps, anything drawn to be
looked at once and then thrown away. It exists because the two obvious
alternatives are both wrong: the scratchpad is invisible to Sam, and
`figures/` proper is where the pipeline's real artefacts live, so a comparison
render dropped there is indistinguishable from one. Name files so the pairing
is obvious (`before-N004.png` / `after-N004.png`) and say the path in chat.

It earned its place immediately: a spacing change I had measured as real
(5px to 12px of violin clearance) turned out to be invisible to the eye on the
rendered card, which no amount of pixel arithmetic would have told me. Render
the pair, hand over both paths, let Sam judge.

### 2.4 Environment quirks

- `here::i_am("Readme.md")` anchors the project root in `_targets.R` and in the
  generator script. Note the file on disk is `README.md`. This works on Windows
  because the filesystem is case-insensitive.
- `_targets.yaml` **and** `docs/_targets.yaml` both exist and both hardcode an
  absolute store path (`C:/Users/SAW/Local Documents/...`). The duplicate in
  `docs/` exists so notebooks can find the store when Quarto renders with
  `docs/` as the working directory.
- `renv.lock` is present but **renv is switched off**: `.Rprofile` has
  `source("renv/activate.R")` commented out. Dependencies are effectively
  whatever is installed in the user library.
- `_quarto.yml` sets project-wide `execute: freeze: auto`, `echo: false`,
  `warning: false`, `message: false`, plus `embed-resources: true` and the
  `flextable-qmd` filter (needed or flextables render wrong).

## 3. Flaws and current disarray

Listed roughly by how much they will bite. I have **not** fixed any of these.

> **Status 2026-07-29:** 3.1 and 3.2 are resolved by `PLAN.md` Phase 0. The
> outlier factory and the 14 generated distributions notebooks are to be
> **deleted**, not repaired. The rest of section 3 is deferred until after
> submission (`PLAN.md` section 10).
>
> **Re-audited 2026-08-07** against the current tree: 3.1, 3.2, 3.3 and 3.6 are
> all confirmed resolved and the detail below is now historical, kept for
> context rather than as an open flaw. 3.4, 3.5 and 3.7 were spot-checked and
> are still accurate. Package Imports (3.8) got a follow-up correction the same
> day.

### 3.1 The outlier factory is commented out of the pipeline — **resolved**

No longer applicable: `outlier_targets_compartment`, `outlier_targets_biota`
and `outlier_notebook_targets` are gone from `_targets.R` entirely, not just
commented out, and no `docs/NBXX-Distributions-*.qmd` files remain in the
tree. (Original text below, for context.)

`_targets.R:1158-1160` and `:1262` have `outlier_targets_compartment`,
`outlier_targets_biota`, and `outlier_notebook_targets` commented out of the
returned `list()`. The targets are still *defined* above, and their objects are
still sitting in `_targets/objects/` from an earlier run.

Consequence: every `docs/NBXX-Distributions-*.qmd` calls
`tar_read(outlier_compartment_...)` / `tar_read(outlier_biota_...)` and
currently succeeds only because those stale objects survive. `targets` regards
them as orphans. A `tar_prune()`, or a store rebuild, deletes them and every
distributions notebook breaks at once. This is the single most fragile thing in
the repo right now.

There are also **no `render_*` objects in the store at all**, so the Quarto
render targets have not completed since the store was last built.

### 3.2 Duplicated and stray files in `docs/` — **resolved**

`docs/NBXX-Distributions-Aquatic copy.qmd` and
`docs/NBXX-Distributions-Aquatic-Sediment.rmarkdown` no longer exist in the
tree. Both root `.nojekyll` and `docs/.nojekyll` were checked 2026-08-07: root
`.nojekyll` is present (0 bytes, tracked); `docs/.nojekyll` is absent, but
since the distributions notebooks it would have served are also gone, this no
longer looks like an open question. (Original text below, for context.)

- `docs/NBXX-Distributions-Aquatic copy.qmd` is **byte-identical** to
  `docs/NBXX-Distributions-Aquatic.qmd`, and both are committed.
- `docs/NBXX-Distributions-Aquatic-Sediment.rmarkdown` is Quarto knit detritus
  that got committed. It is deleted in the working tree but not yet staged.
- `docs/.nojekyll` deleted in the working tree, alongside a root `.nojekyll`.
  **UNCERTAIN** whether that deletion was deliberate; it matters for GitHub
  Pages if that is a publishing route.

### 3.3 The sidebar has fallen far behind the file tree — **resolved**

`_quarto.yml`'s sidebar now lists only three entries (`index.qmd`,
`docs/NBXX-Sample-Groups.qmd`, `docs/AP04-unit-corrections.qmd`), consistent with the
deliberately-minimal rendering regime in 2.2.1. The mismatch described below no
longer exists. (Original text below, for context.)

`_quarto.yml` lists NB01 to NB08, `NBXX-reparfjorden`, AP01 and AP02. Not
listed, but present and rendering: all 14 `NBXX-Distributions-*`,
`NBXX-Outliers`, `NBXX-REACH`, `NBXX-algae`, `NBXX-fish`,
`NBXX-norske-utslipp`, `NBXX-Sample-Groups`, `AP03-creed-criteria`,
`AP03-project`, `NB07-aep-review`. In a Quarto website every `.qmd` in the
project renders whether or not it is in the nav, so these are being built and
then are unreachable from the site.

### 3.4 Notebook numbering has collapsed

Filename prefixes, sidebar labels, and YAML titles disagree three ways:

- `NB03-qc.qmd` is titled "Notebook 01 - Quality Control".
- `NB07-emissions.qmd` is titled "Notebook 08 - Emissions Data".
- `NB02-vannmiljo-qc.qmd` is titled "NB02 3: Vannmiljø QC" and its body says
  "I am also confused over notebook numbering."
- Two files claim `AP03`; two claim `NB07`.
- Everything newer just uses `NBXX`, which has become a permanent prefix rather
  than a placeholder.

### 3.5 Frontmatter churn

Commit `9134762` moved shared NBXX frontmatter into `_quarto.yml`; `8ad2d1f`
("Lots of frontmatter modification. Generally a Big Mess.") appears to have
partly undone it. The current `_quarto.yml` has no per-notebook frontmatter
block, and individual notebooks carry their own `format:` / `execute:` keys
again (for example `NBXX-Distributions-Aquatic-Sediment.qmd` sets
`lightbox: true` and `fig-column: screen-inset-shaded` locally). The intended
end state is **UNCERTAIN** and worth deciding explicitly before touching it.

### 3.6 Live bug in the new `NBXX-Sample-Groups.qmd` — **resolved**

Checked 2026-08-07: no `multimodal` column or casing mismatch remains anywhere
in the file. Either fixed directly or superseded by a rewrite. (Original text
below, for context.)

Untracked, work in progress. The column is created as `` `multimodal (p)` ``
(lowercase) at line 58 but referenced as `"Multimodal (p)"` (capitalised) in
the `color()` call at line 81. flextable will error on the unknown column.

### 3.7 Known-suspect data handling

Flagged in comments by Sam, carried here so they do not get lost:

- `_targets.R:1038` "I believe something I've done somewhere means that
  [`load_literature_pqt`] doesn't properly update." The target is a pass-through
  of `literature_clean_standardised` with a comment describing a dependency on
  a `save_literature_pqt` target that is not actually referenced. Invalidation
  is therefore not doing what the comment claims.
- LOD/LOQ imputation is `x / sqrt(2)`, self-described in the code as "very
  basic, rather bad imputation".
- `summarise_literature_data` has a TODO for weighted means, and its `filter()`
  sits *after* `group_by()` and *before* the outlier `mutate()`, which is legal
  but easy to misread.
- 24 rows in Aquatic Sediment have `MEASURED_VALUE == 0` with no LOD/LOQ or
  censoring flag; currently patched to `NA` inline inside the notebook rather
  than upstream in the pipeline.
- `NB02-vannmiljo-qc.qmd` opens with "FIXME: Doesn't work rn because we're
  having an issue with the validation function."

### 3.8 Reproducibility and packaging

- renv disabled (3.4 above), so the lockfile is decorative.
- This is a pseudo-package: real enough to need `Imports:` kept honest, not a
  real package otherwise. `Description:` is literally "A bit of a mess so far."
  and `License:` is still the `use_mit_license()` placeholder, and that is fine
  for now. **Updated 2026-08-07:** `qs2`, `crew`, `quarto`, `pkgload` were
  checked and are all present in `Imports:`, contrary to what this section used
  to say. The actual gap found by auditing every `pkg::` call in `R/*.R` and
  `_targets.R` against `Imports:` was `ggimage` (used in
  `R/fct_aep_edges.R:533` for the node-card image inset), which has been added.
- ~~`tests/testthat/` contains one file, `test-fct_imputation.R`, against 30
  function files.~~ Checked 2026-08-07: 29 test files exist against 49
  function files in `R/`, so coverage is much healthier than this line
  claimed. Left unstruck how current that ratio is; worth a proper coverage
  check rather than a file count if it matters later.
- Absolute Windows paths in both `_targets.yaml` files make the repo
  single-machine.
- `references.bib` is 4.6 MB and `manifest.json` is 770 KB, both committed.
- ~~`.quarto/` holds ~190 orphaned `quarto-session-temp*` directories.~~
  Cleaned 2026-08-07 (197 dirs removed, gitignored so nothing to commit).

## 4. Where the project is going

### 4.1 The prototype that matters

`docs/NBXX-algae.qmd` is the most important file in the repo for understanding
intent. It runs the full intended workflow end to end, by hand, for one system:

1. Subset to a group (algae, shoot tips, wet weight).
2. Compute `mean`, `sd`, `n`, `n_lines_of_evidence` (distinct `REFERENCE_ID`).
3. Score the group as an AEP **node** on four EPEQ criteria adapted from
   Peng et al. 2022: `essentiality_score`, `plausibility_score`,
   `evidence_score`, `quantification_score`, each 1-3, each with a written
   justification.
4. Hypothesise **edges** (`tbl-edges-epeq`) with the same four scores plus
   `from`, `to`, `magnitude`, `unit`, `n`, `sd`, and a comment.
5. Assemble a manual AEP diagram from those nodes and edges.

Everything else in the project is machinery for doing step 1-3 credibly across
many groups. The paper is roughly "do this 4-5 times for chosen systems, plus
one holistic low-detail AEP".

### 4.2 The intended data structures

Not yet built. The direction agreed 2026-07-29:

- **`data/clean/decisions/group_decisions.csv`** (hand-edited). The human judgement layer.
  One row per group from `summarise_literature_data`, with a `decision` of
  `own_notebook` / `lump` / `split` / `drop`, a `lump_into` key, and free-text
  notes. The pipeline reads it; it never writes it. Grouping becomes reviewable
  data rather than logic buried in code.
- **`data/clean/aep/aep_nodes.csv`** (hand-edited). One row per AEP node: the group
  key, the four EPEQ scores plus justifications, and manual `x` / `y` layout
  coordinates.
- **`data/clean/aep/aep_edges.csv`** (hand-edited). One row per edge: `from`, `to`,
  four EPEQ scores plus justifications, `magnitude` / `unit` where known, and a
  `status` of `empirical` or `putative`.

The AEP figure is then a pure rendering of two CSVs, and re-scoring a node is a
spreadsheet edit rather than a code change.

### 4.3 The "report card"

The target artefact for a node. Should carry, compactly: mean, median, n, unit,
n references, a visual distribution, geographic range, temporal range, and the
four EPEQ scores. It has to be small enough to sit on a graph node.

**Layout note:** AEP node positions are semantically meaningful (source at top,
target site exposure at bottom), so an automatic graph layout is actively wrong
here. Use manual coordinates from `aep_nodes.csv`. For the distribution inset,
pre-render a small PNG per node and place it with `ggimage::geom_image`, reusing
the file-target machinery from the triage panels.

### 4.4 Plot rules for group-level figures

Learned the hard way (see `PLAN.md` section 1 for the full post-mortem):

- **Summarising geoms only** at group level. `geom_bin2d`, `stat_summary_hex`,
  `geom_density`, `geom_boxplot`. Never `geom_point` / `geom_jitter` /
  `geom_dotplot` on a group that might have 40,000 rows.
- **Switch on n.** Below roughly 30 points a density or hex plot is
  meaningless; show the points. Above it, always bin. One helper should own
  this decision so it is consistent everywhere.
- **Do not store ggplot objects in targets.** A ggplot captures its entire
  input data, so the serialised object is huge, and drawing happens at print
  time anyway so nothing is saved. Use `tar_target(format = "file")` returning
  the path to a written PNG. targets then caches the *image*.
- **No patchwork in exploratory work.** Every triage plot is written as its own
  PNG; layout is Quarto's job (`layout-ncol`). Composing panels is reserved for
  manuscript figures at submission prep (`PLAN.md` Phase 5). The sediment
  notebook already reached this conclusion in a comment before abandoning its
  `pw <-` line. One-plot-per-file also means editing one plot function
  invalidates only that plot, not a whole composed panel.
- The plots at the top of `docs/NBXX-Distributions-Aquatic-Sediment.qmd` are the
  reference implementation and follow these rules already.

### 4.4.-2 Two data traps, learned 2026-08-05

Both cost a day between them. Both are cheap to check and expensive to miss.

**Never write a micro sign you do not have to.** `µ` reaches this project as at
least four things: `U+03BC` (Greek mu), `U+00B5` (micro sign), plain `u`, and
`U+FFFD` (a micro sign already destroyed by an encoding round-trip). CSV plus
Windows plus Excel will mangle it eventually, and the failure is silent: an
unmatched unit becomes `NA`, the row is dropped, and a whole reference can vanish
without a message. 18 rows of `2000JulshamnTraceElementLevels` were lost this way
for months.

Prefer `ug` in anything hand-entered. Where a micro sign is unavoidable,
`normalise_unit_string()` maps every variant onto `u` before matching, and
`standardise_measured_units()` warns rather than dropping silently. Do not add a
new unit-matching regex that assumes one spelling.

**A foothill three orders of magnitude off the main mode is a unit error until
proven otherwise.** Sam's own note on the sediment distribution ("many samples at
extremely high concentrations, which suggests unit errors") was right, and the
same reasoning finds the low tail. Copper in a given matrix spans maybe two to
three orders in reality; anything sitting a clean 10^3 away from the mode is
almost always mg/kg against ug/kg, or ug/g against ug/kg. Check the unit before
reaching for a statistical explanation. Two separate 1000x faults were found this
way, one in the extraction and one in the pipeline (`PLAN.md` section 9b).

### 4.4.-1.5 `row_id` and the corrections layer (built 2026-08-06)

**Read `docs/AP04-unit-correction.qmd` before touching anything unit-related.** It is the
how-to, it is built rather than parked, and it reads the corrections file live.

Three things that are easy to get wrong from cold:

- **`row_id` is lower case, and so is everything the corrections layer adds.**
  SCREAMING_SNAKE in this project means "column of the eData schema". `row_id`,
  `unit_correction_id` and `unit_correction_factor` are administrative columns of
  ours and are not in the schema. The casing is the only cue.
- **`row_id` is not a counter and must never become one.** It is `SAMPLE_ID`
  (itself content-derived), with `SUBSAMPLE` appended only where rows collide,
  and the build aborts rather than falling back to a suffix. A positional id
  shifts under insertion and would let a hand-edited correction silently
  overwrite a different measurement.
- **`data/clean/decisions/unit_corrections.csv` overrides measured values.** Pipeline reads,
  never writes. Every failure is an abort, not a warning. Each correction carries
  both a selector and the `row_id`s it matched, and the two must agree; that is
  what makes drift loud instead of silent. `scripts/scaffold_unit_corrections.R`
  records the ids and is deliberately not a target.

**Validate a correction factor by whether the corrected rows land on top of their
uncorrected campaign-mates.** Where a campaign is only partly affected this is
available for free and it is far stronger than any comment or heuristic. It is
what confirmed C001 (five species within 25%) and what exposed the separate
*G. morhua* muscle fault (ratio 20.3). See PLAN.md 9d and 9e.

**A mode that is 45% of the data is a provenance problem, not a statistical one.**
`drop_outliers` cannot reach it and should not be asked to. Exclude the affected
rows with a documented reason (`exclude_campaigns` / `exclude_references` on
`aep_nodes.csv`), then score what survives. Averaging over rows you believe are
mislabelled and calling the result low-quality evidence gives a wrong number
wearing an honest label.

### 4.4.-1 Sample size means `MEASURED_N` (rule set 2026-08-05)

Project-wide, and it is a rule because breaking it is invisible rather than
noisy:

- **Anywhere a sample size is reported, it is `sum(MEASURED_N)`**, a count of
  measurements.
- **Anywhere rows are counted, the label says so** ("n rows", "Rows").

A Vannmiljø row carries one measurement; a literature row can report an
aggregate of fifty. So the two counts diverge by whatever mix of sources a group
happens to have, and a figure reporting one next to a heading reporting the other
looks like a bug in the data rather than a difference in definition. That is
exactly what happened on the fish overview, where a heading said 2,374 and the
panel beside it said 500-odd, with nothing on the page explaining the gap.

Where a count is weighted, **anything divided by it must be weighted too**. The
outlier counts in the panel margins and `n_double_outliers` in
`summarise_literature_data` are both `sum(flag * MEASURED_N)` for this reason
(PLAN.md P1.5).

The deliberate exceptions are counts of *marks drawn*: the categorical heatmap
fill counts rows in a bin, and the outlier ticks are one per flagged row. Both
say "rows" where they are named.

### 4.4.0 Group keys and plot scope

`triage_group_cols()` defines a sample group on eight columns, including
`MEASURED_UNIT_STANDARD` and `SITE_GEOGRAPHIC_FEATURE(_SUB)`. That has a
consequence worth remembering: **any plot that facets on a column in the group
key is degenerate**, because that column is constant within a group.

Two triage panels therefore deliberately relax the key via
`filter_to_group(..., exclude_cols = ...)`:

- **(a) overall distribution** relaxes the unit, so dry and wet weight can be
  compared. This is the entire point of the panel.
- **(d) by site type** relaxes geography, so the same species/compartment/unit
  can be compared across site types.

The other three stay strictly within the group. Check this before adding any
new faceted view.

### 4.4.2 Two traps in the triage layer

**Group labels must carry every group-key column.** `slugify_name()` ends with
`make.unique()`, so any two groups sharing a label get `_1` / `_2` suffixes.
That is bad twice over: the notebook shows two identically titled headings, and
the unsuffixed slug becomes a string *prefix* of the suffixed one, so any
`startsWith()` matching on filenames silently grabs the wrong files. If a new
column joins `triage_group_cols()`, add it to `triage_group_label()` too.

**A notebook chunk's `tar_read()` calls are its dependency declaration.**
`tar_quarto()` scans the `.qmd` for `tar_read()` / `tar_load()` to work out what
the render target depends on. Constructing file paths by hand instead of reading
the file-target removes that link, and the notebook then renders *before* its
inputs are built. If a chunk uses a target's output, read the target, even when
you could derive the paths yourself.

### 4.4.1 Data hygiene

`literature_analysis_ready` (to be built, `PLAN.md` P1.0) sits between
`load_literature_pqt` and everything downstream. It drops rows where
`MEASURED_VALUE_STANDARD` is `NA` or `<= 0`, and reports the counts dropped per
group rather than doing it silently.

A stored `0` is a non-detect that lost its censoring flag; true zero copper is
implausible in any environmental matrix. Confirmed for 24 Aquatic Sediment rows
from `Vm_2010_2025` campaigns. **This drops on the measured value column only.**
A whole-row `tidyr::drop_na()` would gut the dataset, because many eData columns
are legitimately sparse.

Nothing more exotic than this before submission. No censored-data modelling, no
LOD reconstruction.

### 4.5 Deadlines and framing

- Submission target **Monday 2026-09-14**. This governs; the 2026-08-31
  contract end mentioned in `docs/_planning.qmd` does not.
- **~26 working days available**, not the ~15 originally assumed. Holiday is
  **Mon 17 – Tue 25 Aug** (laptop available, but costed at zero in the plan).
  Full calendar in `PLAN.md` section "Actual capacity".
- `docs/_planning.qmd` is **hopelessly out of date**: written in April, plans a
  late-July submission. It still renders into the site via the `_quarto.yml`
  sidebar. Treat `PLAN.md` as the only current plan.
- Triage cutoff is **`n >= 100`** to start. The 30-100 band is decided from the
  summary table alone and only gets panels if buffer survives.
- The literature review yielded ~35 sources. **Scale in this paper comes from
  Vannmiljø, not from the review.** `AP01-review-protocol.qmd` currently
  presents the review as systematic, which invites assessment against PRISMA.
  It should be reframed as targeted evidence-gathering complementing a national
  monitoring database.
- Sparse edges are a **finding**, not a shortfall, provided empirically
  supported edges are visually distinguished from putative ones. This framing
  is also what bounds the edge gap-filling work.

## 5. Working agreements

- **Do not make sweeping independent changes.** Sam has asked for collaborative,
  step-at-a-time work. Propose, confirm, then act.
- Never use em-dashes in generated content or in chat.
- Before changing anything in the outlier/distributions chain, check whether the
  change forces a store rebuild. Given 3.1, a rebuild currently destroys the
  data the distribution notebooks read.
- `scripts/generate_distribution_notebooks.R` is append-only by design. Preserve
  that property. Hand-written prose in the generated notebooks is not
  reproducible from anywhere else.
- Prefer fixing data problems in `_targets.R` over patching them inline in a
  notebook, but note that the reverse is currently common in this repo.
- **Test every new function before handing it over.** Not "it parses", not "the
  target is registered": actually call it. Two levels, both required:
  1. **A `testthat` file** in `tests/testthat/`, built on small synthetic
     fixtures rather than the target store, so it runs in seconds and does not
     break when the pipeline is rebuilt. Cover the degenerate cases, since
     those are what a heterogeneous dataset supplies: empty groups, `NA`
     grouping values, n below a switch threshold, all-missing columns.
  2. **A smoke run against real data** for anything that touches the pipeline.
     For plots this means forcing an actual draw (`ggplot_build()`) and an
     actual write, because a ggplot object constructs fine and only fails when
     rendered. `ggplot_build()` still does not exercise the device, so test the
     `ggsave()` path too.

  Run with:
  `Rscript -e 'pkgload::load_all(quiet=TRUE); testthat::test_dir("tests/testthat")'`

  Do not use `skip_on_cran()` here. This is a research compendium, never going
  to CRAN, and `NOT_CRAN` is unset under `Rscript`, so the skip silently
  disables the test in the only place it would ever run.
- **Sam writes the paper.** Code and advice from an LLM are in scope; prose for
  the manuscript is not, beyond drafting bullet points when explicitly asked.
- **Scientific judgement stays manual.** Automating the ranking of which groups
  deserve scrutiny is welcome. Automating the decision about what to lump,
  split, or drop is not.
- Time is the binding constraint, not code quality. Prefer the boring fix that
  ships. Flag tempting refactors rather than doing them.
