# Plan: rationalise target names

**Status: deferred, not scheduled.** Written 2026-08-06 from an audit of all 100
targets in `_targets.R`. This is a self-contained job that can be run in one
sitting whenever there is a natural full rebuild, and should not be started
before submission (2026-09-14) unless the pipeline is being rebuilt anyway.

Nothing in this document has been applied.

------------------------------------------------------------------------

## 1. The principle

### Targets are nouns

The starting proposal was `verb_adj_noun`. That is the right grammar for
**functions**, and `R/fct_*.R` already follows it. It is the wrong grammar for
targets, for two reasons.

**A target is not an action, it is the artefact the action produced.** This is
the split the `targets` manual is built on: functions are verbs, the pipeline is
a graph of nouns. Naming a target for its verb records *how* it was made, which
the ten lines of code beside it already say, and hides *what it is*, which is
the thing you need when you meet `tar_read(...)` at the top of a notebook three
weeks later.

**Sort order is the API.** `tar_make(names = starts_with("vm_"))`,
`tar_visnetwork(names = ...)`, `tar_prune()`, `tar_manifest()` and the
`## # Name ----` editor outline all key on the *leading* substring of the name.
Verb-first scatters related artefacts across the alphabet:
`summarise_literature_data` and `load_literature_pqt` are adjacent in the DAG
and eleven letters apart in every listing. Noun-first clusters them, and the
cluster is what you select on.

### The grammar

General to specific, always:

```
<source>_<entity>_<state>

vm_sites_filtered          not  vm_filtered_sites
lit_measurements
aep_node_card_files
```

State suffixes are past participles (`_filtered`, `_resolved`, `_split`,
`_standardised`, `_clean`, `_dropped`), which are adjectival, so the adjective
you wanted is still there, just on the side of the noun that sorts correctly.

### The four rules that follow

1. **Source prefix first, and every target has one.** Currently `vm_`,
   `lit_`, `triage_`, `aep_`, plus a small unprefixed tail (maps, thresholds,
   reports, renders) that is genuinely cross-cutting.
2. **File and object targets pair by suffix.** `x_file` (or `x_files` where
   there are genuinely many) is the tracked path; bare `x` is the parsed
   object. `_data` is a noise suffix in a pipeline where everything is data.
3. **Verbs only where the action *is* the product.** The `render_*` family is
   the whole of this exception and stays as it is.
4. **Keep base names short where `tar_map()` may branch.** Static branching
   appends `_<slug>`, and a long base plus a long slug produces target names no
   one can read or type.

### What this is not

Not a rename of functions in `R/`, not a change to column names, not a change
to file names on disk. Targets only.

------------------------------------------------------------------------

## 2. What is actually inconsistent

Ranked by how much confusion each one causes, which is not the same as how many
targets it touches.

### Tier 1: actively misleading, worth the churn

**T1.1 The literature eData reads carry no source prefix at all.**
`campaign_files`, `measurements_data`, `biota_data`, `sites_data` and their
seven siblings sit unprefixed next to fully-prefixed Vannmiljø twins. So
`sites_data` and `vm_edata_sites` are the same entity from two sources and only
one of them says which. This is the single highest-value fix: 22 targets, and
it is the reason the literature half of the pipeline is hard to select on.

**T1.2 `aep_node_cards` and `node_cards` are different targets.**
[_targets.R:1467](_targets.R#L1467) is the card *table*;
[_targets.R:1522](_targets.R#L1522) writes the *PNGs*. Two names one word apart
for two unrelated types, in the newest and least settled part of the pipeline.

**T1.3 Two different file/object pair conventions.** `unit_corrections_file` /
`unit_corrections` (path, then parsed) against `campaign_files` /
`campaign_data` (path, then parsed). Both readable, but you cannot guess which
one a given target uses.

**T1.4 `API_biota_common_names`** is the only SCREAMING prefix in the pipeline,
so it sorts above everything and belongs to no family.

### Tier 2: cosmetic, do only if Tier 1 is already being done

**T2.1 Validation targets disagree on word order.** `data_validation`,
`methods_data_validation`, `vm_edata_validation`.

**T2.2 `vm_filtered_*` is the only adjective-first family**, against
`vm_sites_split` and `vm_compartment_conflicts_resolved` elsewhere in the same
block. Flipping the three makes all `vm_sites_*` sort together.

**T2.3 Two 44-character names.**
`vm_compartment_geo_conflicts_resolved_removed` and its sibling. The
`_resolved` / `_resolved_removed` pair reads as "resolved, then removed", when
what is meant is "kept" and "discarded".

**T2.4 Geography and map targets are adjective-first**, `wgs84_geography` /
`polar_geography` / `wgs84_map` / `polar_map`, so the two geographies and the
two maps do not cluster.

**T2.5 `summarise_literature_data` and `load_literature_pqt` are verbs.**
`load_literature_pqt` is also a pass-through whose comment claims a dependency
on a `save_literature_pqt` target that is not referenced (`CLAUDE.md` 3.7,
`PLAN.md` section 10). Renaming it is the right moment to decide whether it
should exist at all, which makes this a slightly larger job than a rename.

------------------------------------------------------------------------

## 3. The mapping

`keep` means the name already follows the scheme.

### Vannmiljø

| Current | Proposed | Tier |
|---|---|---|
| `vm_raw_copper`, `vm_raw_sites` | keep | |
| `vm_lookup_*` (6 targets) | keep | |
| `vm_join_sites_measurements` | keep | |
| `vm_join_sites_measurements_lookup` | keep | |
| `vm_filtered_compartments` | `vm_compartments_filtered` | T2.2 |
| `vm_filtered_sites` | `vm_sites_filtered` | T2.2 |
| `vm_filtered_dates` | `vm_dates_filtered` | T2.2 |
| `vm_compartment_conflicts_resolved` | `vm_compartment_conflicts` | T2.3 |
| `vm_compartment_conflicts_resolved_removed` | `vm_compartment_conflicts_dropped` | T2.3 |
| `vm_compartment_geo_conflicts_resolved` | `vm_compartment_geo_conflicts` | T2.3 |
| `vm_compartment_geo_conflicts_resolved_removed` | `vm_compartment_geo_conflicts_dropped` | T2.3 |
| `vm_sites_split`, `vm_sites_split_clean` | keep | |
| `vm_edata_*` (8 targets) | keep | |
| `vm_edata_validation` | keep | |

### Literature (eData)

| Current | Proposed | Tier |
|---|---|---|
| `campaign_files` | `lit_campaign_files` | T1.1 |
| `campaign_data` | `lit_campaign` | T1.1, T1.3 |
| `samples_files` / `samples_data` | `lit_samples_files` / `lit_samples` | T1.1, T1.3 |
| `biota_files` / `biota_data` | `lit_biota_files` / `lit_biota` | T1.1, T1.3 |
| `compartments_files` / `compartments_data` | `lit_compartments_files` / `lit_compartments` | T1.1, T1.3 |
| `measurements_files` / `measurements_data` | `lit_measurements_files` / `lit_measurements` | T1.1, T1.3 |
| `methods_files` / `methods_data` | `lit_methods_files` / `lit_methods` | T1.1, T1.3 |
| `parameters_files` / `parameters_data` | `lit_parameters_files` / `lit_parameters` | T1.1, T1.3 |
| `reference_files` / `reference_data` | `lit_reference_files` / `lit_reference` | T1.1, T1.3 |
| `sites_files` / `sites_data` | `lit_sites_files` / `lit_sites` | T1.1, T1.3 |
| `creed_scores_files` / `creed_scores_data` | `lit_creed_scores_files` / `lit_creed_scores` | T1.1, T1.3 |
| `methods_data_validation` | `lit_methods_validation` | T2.1 |
| `data_validation` | `lit_validation` | T2.1 |
| `API_biota_common_names` | `lit_biota_common_names` | T1.4 |
| `literature_joined` | `lit_joined` | see 6.1 |
| `literature_clean` | `lit_clean` | see 6.1 |
| `literature_clean_standardised` | `lit_standardised` | see 6.1 |
| `load_literature_pqt` | `lit_pqt`, or delete | T2.5 |
| `row_id_collisions` | `lit_row_id_collisions` | T1.1 |
| `unit_corrections_file` / `unit_corrections` | `lit_unit_corrections_file` / `lit_unit_corrections` | T1.1 |
| `literature_corrected` | `lit_corrected` | see 6.1 |
| `unit_correction_report` | `lit_unit_correction_report` | T1.1 |
| `literature_analysis_ready` | `lit_analysis_ready` | see 6.1 |
| `literature_dropped_report` | `lit_dropped_report` | see 6.1 |
| `summarise_literature_data` | `lit_group_summary` | T2.5 |

### Triage

| Current | Proposed | Tier |
|---|---|---|
| `sample_groups_table` | `triage_groups_table` (see 6.2) | T2 |
| `triage_pilot_groups`, `triage_scale_limits`, `triage_group_slices`, `triage_pilot_plots` | keep | |
| `triage_overview_node_table`, `triage_overview_plots` | keep | |
| `triage_species_node_table`, `triage_species_plots` | keep | |

### Decisions and AEP

| Current | Proposed | Tier |
|---|---|---|
| `group_ids_file` / `group_ids` | keep | |
| `group_decisions_file` / `group_decisions` | keep | |
| `aep_nodes_file` / `aep_nodes` | keep | |
| `aep_node_members_file` / `aep_node_members` | keep | |
| `aep_node_groups_file` / `aep_node_groups` | keep | |
| `aep_node_cards` | keep (the table) | T1.2 |
| `node_cards` | `aep_node_card_files` | T1.2 |
| `node_cards_compact` | `aep_node_card_files_compact` | T1.2 |
| `aep_node_coverage` | keep | |
| `aep_edges_file` / `aep_edges` | keep | |
| `aep_diagram` | `aep_diagram_file` **if** it is `format = "file"`; verify first | T1.3 |

### Maps, reports, renders

| Current | Proposed | Tier |
|---|---|---|
| `wgs84_geography` | `geography_wgs84` | T2.4 |
| `polar_geography` | `geography_polar` | T2.4 |
| `wgs84_map` | `map_wgs84` | T2.4 |
| `polar_map` | `map_polar` | T2.4 |
| `copper_toxicity_thresholds` | keep | |
| `unit_anomaly_report`, `data_quality_report` | keep | |
| `render_index`, `render_nbxx_sample_groups`, `render_ap04_units` | keep (rule 3) | |

------------------------------------------------------------------------

## 4. Cost, and why this is deferred

**Renaming a target invalidates it, and everything downstream.** `targets` keys
the store on the name, so a renamed target is a new target with no cached
result. Renaming the `vm_` and `lit_` roots is a **full pipeline rebuild**, and
per `CLAUDE.md` 3.1 a store rebuild currently destroys the orphaned outlier
objects that the parked distributions notebooks still read.

Blast radius outside `_targets.R`, measured 2026-08-06:

- **~35 `tar_read()` / `tar_load()` call sites** across `docs/*.qmd` and
  `index.qmd`. `load_literature_pqt` alone accounts for 9.
- **9 files in `scripts/`** reading targets directly.
- **A handful of mentions in `R/*.R`**, all in roxygen comments and error
  messages rather than code.
- **Prose references in `PLAN.md` and `CLAUDE.md`**, which should be updated in
  the same commit or they become misleading.

Each notebook call site is also a **dependency declaration** (`CLAUDE.md` 4.4.2):
`tar_quarto()` scans the `.qmd` for `tar_read()` to work out what the render
target depends on. A missed rename does not just fail, it can silently render a
notebook before its input exists.

**Estimated effort:** 2 to 3 hours for Tier 1 and Tier 2 together, dominated by
verification rather than editing, plus whatever a full `tar_make()` costs.

**When to run it:** at a point where the store is being rebuilt anyway. Do not
run it incrementally. Half-renamed is worse than either end state, because then
neither convention predicts anything.

------------------------------------------------------------------------

## 5. Procedure, when the time comes

1. **Branch.** `git switch -c rename-targets`, with a clean working tree, so
   the whole thing is one revertible commit.
2. **Snapshot the current DAG.** `tar_manifest()` written to CSV, before any
   edit. This is the checklist and the diff base.
3. **Edit `_targets.R` names only**, in mapping order, one family at a time.
4. **Trap: target names appear as named arguments.** `_targets.R` has ~167
   lines of the form `arg = value`, many of them `vm_lookup_methods =
   vm_lookup_methods`. The **left** side is the function's parameter name and
   must not change; only the **right** side is the target symbol. Blind
   find-and-replace across the file will silently rename function parameters and
   break the call. Rename by symbol, and re-read each hit.
5. **Sweep the call sites** in `docs/*.qmd`, `index.qmd`, `scripts/*.R`,
   `R/*.R`, `PLAN.md`, `CLAUDE.md`.
6. **Verify the graph before building it.** `tar_manifest()` again and diff
   against step 2: the row count must be identical and every dependency edge
   must have survived. `tar_validate()` catches undefined symbols without
   running anything.
7. **Grep for orphans**: any remaining occurrence of an old name anywhere in
   the tree is either a miss or a comment that needs updating.
8. **Rebuild.** `tar_make()`, expecting everything to run.
9. **Render.** The three live `tar_quarto()` targets, to confirm no notebook
   lost its dependency link.
10. **Record the scheme** in `CLAUDE.md` section 2.2 as a convention, so it
    does not decay again. This step is the point of the exercise; without it
    the next twenty targets are named ad hoc and the drift restarts.

------------------------------------------------------------------------

## 6. Open questions, for Sam

**6.1 `lit_` or `literature_`?** The mapping above uses `lit_`, on the grounds
that a prefix typed constantly in `tar_read()` and tidyselect should be short,
and that it makes the literature family exactly as wide as `vm_`. The
conservative alternative is `literature_` throughout, which abbreviates nothing
and matches the four existing `literature_*` targets, at the cost of six
characters on 22 names. **Recommendation: `lit_`.** Sam's call.

**6.2 Should the literature reads mirror `vm_edata_*` exactly**, i.e.
`lit_edata_campaign` rather than `lit_campaign`? Argument for: perfect
symmetry. Argument against, and the reason the mapping does not do it: on the
Vannmiljø side `_edata_` marks a real transformation boundary (raw Vannmiljø
reshaped into eData tables), whereas the literature data *arrives* as eData, so
the infix would mark nothing. **Recommendation: `lit_*`.**

**6.3 `sample_groups_table`.** Proposed as `triage_groups_table`, but it may be
a general group inventory rather than a triage artefact, in which case it should
keep a neutral name. Needs 30 seconds of looking at what reads it.

**6.4 Tier 2 at all?** Tier 1 is 25 targets and removes real ambiguity. Tier 2
is another 12 and is aesthetic. Doing Tier 1 alone is a coherent stopping point;
doing Tier 2 alone is not.

**6.5 Fold `load_literature_pqt` away entirely** rather than rename it, given it
is a pass-through with a broken dependency comment. That is a pipeline change,
not a rename, and would need its own decision.

------------------------------------------------------------------------

## 7. Resources

There is no canonical naming specification for `targets`, which is why this
felt underspecified. What exists:

- **targets user manual**, https://books.ropensci.org/targets/ . The
  "Functions" chapter is where the noun/verb split is argued. The hard
  constraints (valid R symbols, unique, no leading dot) are in `?tar_target`.
- **targets GitHub Discussions**, https://github.com/ropensci/targets/discussions .
  The best prior art. Landau answers naming questions directly there, and it is
  where practical conventions get hashed out rather than in the manual.
- **dbt model naming conventions**, docs.getdbt.com, "How we name our models".
  The non-obvious one and the most directly useful. dbt solves exactly this
  problem, a large DAG of named data artefacts read by humans, and has converged
  on layer-prefix-first (`stg_` to `int_` to `fct_`/`dim_`). Our
  `vm_raw_` to `vm_join_` to `vm_edata_` to `lit_` to `aep_` progression is
  already halfway there; dbt supplies the argument for why prefix-first beats
  grammar.
- **tidyverse style guide**, https://style.tidyverse.org/syntax.html#object-names .
  Nouns for objects, verbs for functions, snake_case.
- **tidyverse design guide**, https://design.tidyverse.org/function-names.html .
  About functions, but the noun/verb distinction is the useful part.
- **Bruno Rodrigues, *Building Reproducible Analytical Pipelines with R***,
  https://raps-with-r.dev . The `targets` chapters, written from a
  practitioner's angle rather than a reference manual's.
- `tar_manifest()`, `tar_glimpse()`, `tar_visnetwork()` for auditing the set
  before and after.
