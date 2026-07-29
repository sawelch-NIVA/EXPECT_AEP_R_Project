# Plan: from 300 messy groups to a submitted AEP paper

Written 2026-07-29. Supersedes `docs/_planning.qmd`, which is stale (April, plans a late-July submission).

**Submission target: Monday 2026-09-14.**

### Actual capacity (recount, 2026-07-29)

You estimated ~3 working weeks. The calendar says otherwise:

| Block | Dates | Working days |
|---|---|---|
| This week | Wed 29 – Fri 31 Jul | 3 |
| Week 2 | Mon 3 – Fri 7 Aug | 5 |
| Week 3 | Mon 10 – Fri 14 Aug | 5 |
| **Holiday** | **Mon 17 – Tue 25 Aug** | **0 planned** |
| Week 4 | Wed 26 – Fri 28 Aug | 3 |
| Week 5 | Mon 31 Aug – Fri 4 Sep | 5 |
| Week 6 | Mon 7 – Fri 11 Sep | 5 |
| Submit | Mon 14 Sep | — |
| | | **26 days** |

**You have roughly 26 working days, not 15.** The holiday is 7 working days, not half the remaining period. Laptop availability over Aug 17-25 is deliberately costed at **zero**: anything achieved there is a bonus, and nothing critical is scheduled into it.

Given the stated tendency to overrun, the recommendation is to **bank the slack rather than expand scope.** Week 6 is left as buffer.

**Immediate goal: meaningful progress on the data analysis by Friday 2026-07-31.** Concretely, that means grouping decisions made for the groups that carry most of the data. It does not mean touching the AEP.

------------------------------------------------------------------------

## 1. Post-mortem: why attempt one failed

Recorded so we design against it rather than rediscovering it.

| \# | What happened | Root cause | Design response |
|------------------|------------------|------------------|------------------|
| 1 | Some plots meaningless (n too small), some crashed the render (n too large) | One plot spec applied across a 4-order-of-magnitude range of n | Summarising geoms only; switch presentation on n at a single chokepoint |
| 2 | Plots took forever to resolve through targets | Non-summarising aesthetics, and plot objects rebuilt/redrawn per render | Write PNGs from `format = "file"` targets; notebooks embed, never draw |
| 3 | *G. morhua* liver structure did not generalise | Depth-first on one group, then assumed reuse | Breadth-first triage first; depth only for groups that earn it |

The deeper pattern: attempt one tried to produce **comprehensive coverage** and then read it. Attempt two produces a **ranked shortlist**, reads that, and spends the remaining effort only where the reading says it is worth it.

------------------------------------------------------------------------

## 2. Architecture

Four thin layers. Each is small, and each hands off to the next through a hand-edited CSV, so human judgement is always the interface.

```         
summarise_literature_data  (exists, needs a triage score)
        │
        v
  drop_nonpositive_values (new: strips NA and 0 measured values, counts losses)
        │
        v
  triage panels           one PNG *per plot* per group, n >= 100, format = "file"
        │
        v
  docs/NBXX-Triage.qmd    contact sheet, ranked. YOU READ THIS.
        │
        v
  data/clean/group_decisions.csv     <-- hand-edited. THE decision layer.
        │
        v
  report cards / nodes  ──> data/clean/aep_nodes.csv   <-- hand-edited EPEQ + x/y
        │
        v
  data/clean/aep_edges.csv                             <-- hand-edited EPEQ + status
        │
        v
  plot_aep()              ggraph, manual layout, node cards as images
```

Nothing downstream of `group_decisions.csv` runs until the decisions exist. That is deliberate: it stops the pipeline from generating work you have not asked for yet.

------------------------------------------------------------------------

## 3. Phase 0 — Unblock ✅ DONE 2026-07-29

These are cheap and they are blocking. Do not extend this phase.

- [x] **P0.1** Fix [NBXX-Sample-Groups.qmd:81](docs/NBXX-Sample-Groups.qmd#L81): column built as `` `multimodal (p)` ``, referenced as `"Multimodal (p)"`. flextable will error. One-character fix.
- [x] **P0.2** **DECIDED 2026-07-29: delete.** Remove the outlier `tar_map` factory (`_targets.R:78-185`, and the commented list entries at `:1158-1160` and `:1262`), the 14 generated `docs/NBXX-Distributions-*.qmd` files, and `scripts/generate_distribution_notebooks.R`. The triage layer replaces them. **Keep:** `docs/NBXX-Outliers.qmd` (hand-authored *G. morhua* case study), `R/fct_outlier_detection.R` and `R/fct_statistics.R` (statistics reused by the triage panel), and `R/fct_outlier_groups.R` (`slugify_name()`, `get_compartment_groups()`, `get_biota_groups()` are all still needed to enumerate groups). **Salvage first:** the hand-written prose in `NBXX-Distributions-Aquatic-Sediment.qmd` is the reference implementation for P1.1 and the zero-value note at its lines 53-57 is a real data finding. Lift both out before deleting anything. Also drop `~200` orphaned `outlier_*` objects from the store via `tar_prune()` once the factory is gone.
- [x] **P0.3** Delete `docs/NBXX-Distributions-Aquatic copy.qmd` (byte-identical duplicate). Stage the already-deleted `.rmarkdown` file.
- [x] **P0.4** Confirm whether deleting `docs/.nojekyll` was deliberate.

**Outcome:**

- `multimodal` replaces `bimodal` throughout (`R/fct_outlier_detection.R`, `R/fct_outlier_report.R`, `_targets.R`, `docs/NBXX-Outliers.qmd`). Hartigan's dip test rejects unimodality; it does not count modes, so the old name asserted more than the test supports.
- `docs/NBXX-Sample-Groups.qmd` renders. Verified in the output: red text on exactly 15 cells (the 15 multimodal groups), yellow background on 147 cells (20 flagged rows). Also made both flags NA-safe, so untested groups (n below `min_n`) no longer leak into the highlight sets.
- 16 `.qmd` files deleted (14 distributions + duplicate + `.rmarkdown`), plus `scripts/generate_distribution_notebooks.R` and 108 lines of `tar_map` factory from `_targets.R`. Plot specs salvaged to `scripts/reference_triage_plots.R`, which is never rendered.
- Render targets cut from **16 to 2** (`render_index`, `render_nbxx_sample_groups`). `_quarto.yml` gained an explicit `project: render:` list, so `quarto render` no longer walks every `.qmd` in the tree. Sidebar trimmed to match, avoiding dead links.
- All 22 parked notebooks got `execute: freeze: true`; all 22 frontmatter blocks verified as valid YAML afterwards.
- `index.qmd` now emits **html + docx** (docx via `custom-reference-doc.docx`, png/8x6/300dpi). Confirmed via `quarto inspect` that project-level html settings still merge through: svg, 12x8, code-fold, embed-resources all survive.
- Deployment cruft removed: `rsconnect/`, `.posit/`, `manifest.json`, stale `docs/search.json`, and the commented Posit Connect target. ~2.9 MB.
- `tar_prune()` removed **206 orphaned `outlier_*` objects**; verified beforehand that the prune list contained nothing else. 53 real objects remain.
- Pipeline validates: 65 targets defined.

------------------------------------------------------------------------

## 4. Phase 1 — Triage infrastructure (Wed PM – Thu AM)

The goal is a contact sheet you can read in one sitting.

- [x] **P1.0** ✅ **Built 2026-07-29, untested.** `R/fct_analysis_ready.R` provides `drop_nonpositive_measurements()` (filters `MEASURED_VALUE_STANDARD` for `NA` / zero / negative, on that column only) and `report_dropped_measurements()` (per-group loss counts: `n_na`, `n_zero`, `n_negative`, `n_dropped`, `prop_dropped`, worst first). Two targets added: `literature_analysis_ready` and `literature_dropped_report`. `summarise_literature_data` now reads from the former, and its redundant internal `filter(!is.na(...))` is removed.
      **Not run.** `tar_make()` is yours. Expect `summarise_literature_data` and everything downstream to invalidate; group stats may shift slightly where zeros were previously included in `mean` / `sd` / `median`.
- [~] **P1.1** 🔧 **Pilot built 2026-07-29, untested.** `R/fct_group_triage.R`, one function per plot: `triage_plot_density()`, `triage_plot_by_date()`, `triage_plot_by_category()` (shared by the campaign and site-type views), `triage_plot_spatial()`. No patchwork. Each writes its own PNG at a fixed 8x5in / 150dpi canvas.
      `triage_use_points()` owns the single n-switch: under 30 observations a view draws points, over it draws 2D bins or a density. `triage_empty_plot()` returns a labelled blank where a view cannot apply (no coordinates, no category clearing `min_facet_n`), so one awkward group cannot kill a whole batch and the gap stays visible on the contact sheet.
      **Pilot scope:** targets `triage_pilot_groups` (5 groups sampled from the 27 with `n >= 100`, `seed = 20260729` so the selection is stable) and `triage_pilot_plots` (`format = "file"`, writes to `_triage/`). Widen by raising `n_sample`, or set it to `Inf` for all 27.
      **Not run.** Aesthetics are expected not to fit every group yet; that is what the pilot is for.
- [x] **P1.1a** ✅ **Tested 2026-07-29.** 73 tests across `tests/testthat/test-fct_analysis_ready.R`, `test-fct_group_triage.R`, `test-fct_sample_groups_table.R`, all on synthetic fixtures so they do not depend on the target store. Plus a smoke run over all 5 pilot groups with real data, forcing both `ggplot_build()` and `ggsave()`.
      Bugs found and fixed: (1) `map_data()` is exported by **ggplot2**, not `maps` — passing a `maps::map()` object to `geom_polygon()` routes through the deprecated `fortify.map()` and dies in `names[df$group, 1]`; (2) `.keep = "none"` left `n` and `median` in their original input positions, so the table rendered as N, Median, Group, Location instead of Group first — now ordered explicitly.
      **Measured cost: 0.35s per plot**, not the 2-3s assumed. All 27 groups with `n >= 100` would take under a minute, so widening the pilot is cheap.
- [x] **P1.1b** ✅ **Pilot feedback applied 2026-07-29.** (1) `docs/NBXX-Sample-Groups.qmd` gained a Triage plots section: one `##` heading per group, its five views in a `layout-ncol=5` row, lightbox enabled, generated `asis` from the targets so it scales past the pilot. (2) Files renamed `{group}_{a-e}_{view}.png` so a file browser sorts them into reading order. (3) Plot (a) is now unit-agnostic: it receives a subset built with `filter_to_group(..., exclude_cols = "MEASURED_UNIT_STANDARD")` and colours by unit, so dry vs wet divergence is visible (G. morhua liver goes from 218 rows/1 unit to 3,014 rows/2 units, 2 density curves). (4) `prettify_campaign_name()` extracted as its own function and wired in via a new `label_fn` argument. (5) The points-vs-bins switch is now documented in a callout in the notebook.
      **Watch out:** the `case_when()` in `docs/NBXX-Outliers.qmd` and `scripts/reference_triage_plots.R` has no `.default`, so it turns every non-Vannmiljø campaign into `NA` — 28 of 72 campaigns. `prettify_campaign_name()` fixes this; the notebook copies still have the bug.
      87 tests passing. Notebook renders: 25 base64-embedded images, 5 layout rows, no broken paths.
- [ ] **P1.2** `write_triage_plot(plot, group_slug, plot_name, dir)` returning a file path. Fixed canvas size, PNG via ragg, so a 40,000-row group and a 150-row group produce the same-sized artefact.
- [ ] **P1.3** `tar_map()` over groups with **`n >= 100`**, `format = "file"`, writing `_triage/<slug>-<plot_name>.png`. Groups below 100 are decided from the summary table alone and never get a panel.
- [ ] **P1.4** Add a triage ranking to `summarise_literature_data`. Keep it dumb: **sort by `n` descending**, and show flag columns alongside (`n_references == 1`, outlier fraction \> 5%, `multimodal`, CV, number of distinct units, plus the P1.0 drop count). Do not build a composite score. The flags are for your eye, not for arithmetic.
- [ ] **P1.5** **Fix the outlier-fraction denominator.** In `summarise_literature_data`, `n = sum(MEASURED_N)` (total measurements) but `n_double_outliers = sum(outlier_RMZ & outlier_IQR)` (count of **rows**). The ratio `n_double_outliers / n` therefore divides a row count by a measurement count, so the flag systematically under-fires wherever `MEASURED_N > 1`. It currently trips on 20 of 245 groups; the true figure is higher. **Not fixed in Phase 0 because it changes a reported statistic, which is your call, not mine.** Options: divide by row count instead, or weight the outlier count by `MEASURED_N`. Cheap either way, but decide before the Phase 2 decisions rest on it.

**Budget check:** at `n >= 100`, expect roughly **30-50 groups** rather than the 60-100 at `n >= 30`. At 2-3s x 5 plots that is 8-13 min single-threaded, a few minutes across crew workers. Runs once, then targets caches each PNG independently, so editing one plot function only invalidates that plot across groups.

**Deferred by choice:** groups in the 30-100 range get panels only if Week 6 buffer survives. Note in `group_decisions.csv` that they were decided from the table alone, so it is visible later which decisions had thinner support.

**Cut line (3pm Thursday):** if the `tar_map` file-target factory is fighting you, abandon it and generate the panels from a plain loop in a script writing to `_triage/`. Losing the dependency tracking costs you one manual re-run. Losing Friday costs you the week.

------------------------------------------------------------------------

## 5. Phase 2 — Make the decisions (Thu PM – Fri)

**This is the Friday deliverable, and it is the only thing that matters this week.**

- [ ] **P2.1** `docs/NBXX-Triage.qmd`: a contact sheet embedding the panels as images in rank order, each above its summary-table row. This is a working document, not a publication artefact. It replaces all 14 distributions notebooks for now.
- [ ] **P2.2** Scaffold `data/clean/group_decisions.csv` with one row per group, pre-filled with the group key, `n`, and flags, and an empty `decision` column.
- [ ] **P2.3** **You read the contact sheet and fill in the decisions.** No automation. Work down from largest n.

**Definition of done for Friday:** every group covering the top 90% of total measurements has a `decision`. In practice that is probably the top 30-50 rows, not all 300. Everything below the line defaults to `lump` or `drop` and can be revisited if a specific system needs it.

**Explicitly not this week:** report cards, EPEQ scoring, edges, any AEP rendering, any repo cleanup beyond Phase 0.

------------------------------------------------------------------------

## 6. Phase 3 — Report cards and nodes (Mon 3 – Fri 7 Aug)

- [ ] **P3.1** `node_report_card(group)` returning a one-row tibble: mean, median, n, unit, n references, geographic range, temporal range.
- [ ] **P3.2** Mini distribution PNG per node. Reuses P1.2 at a smaller canvas.
- [ ] **P3.3** Scaffold `data/clean/aep_nodes.csv` from the groups marked `own_notebook`, with empty EPEQ score and justification columns.
- [ ] **P3.4** **You score the nodes.** Copy the reasoning style already in `NBXX-algae.qmd:tbl-epeq-algae`; it is good and it is yours.
- [ ] **P3.5** Pick the systems to model. Target 3-5, likely fish, algae, shellfish, sediment, water. Driven by the Phase 2 decisions, not chosen in advance.

------------------------------------------------------------------------

## 7. Phase 4 — Edges (Mon 10 – Fri 14 Aug, hard time-box)

**Your stated highest risk. The mitigation is scope, not effort.**

- [ ] **P4.1** Scaffold `data/clean/aep_edges.csv` for every plausible flow between chosen nodes, all defaulting to `status = "putative"`.
- [ ] **P4.2** **Time-box gap-filling to 3 working days.** For each edge, spend at most \~30 minutes looking for empirical support. Found: score it, mark `empirical`, cite. Not found: leave `putative`, write one sentence on what evidence *would* settle it, move on.
- [ ] **P4.3** Make `putative` visually distinct in the diagram (dashed, greyed, unlabelled magnitude).

The unfilled edges are a result. A regional-scale AEP that honestly marks its own gaps is a more defensible contribution than one that quietly implies completeness. This is also the argument that lets you stop on schedule.

------------------------------------------------------------------------

## 8. Phase 5 — Assemble and render (Wed 26 – Fri 28 Aug)

- [ ] **P5.1** `plot_aep(nodes, edges)`: ggraph with **manual** coordinates from `aep_nodes.csv`. Automatic layouts are wrong here because vertical position carries source-to-TSE meaning.
- [ ] **P5.2** Node cards placed with `ggimage::geom_image`.
- [ ] **P5.4** **Composed multi-panel figures.** This is where patchwork comes back, deliberately and only for figures actually going into the manuscript. Triage plots stay as individual PNGs forever.
- [ ] **P5.3** 3-5 focused system AEPs plus one holistic low-detail AEP (matching the `docs/_planning.qmd` Materials/Methods items 1 and 2, which are still the right targets even though its schedule is dead).

------------------------------------------------------------------------

## 9. Phase 6 — Writing (Mon 31 Aug – Fri 11 Sep, buffer included)

You write. Noted as scheduled work so it is not treated as free.

- [ ] **P6.1** Rewrite `AP01-review-protocol.qmd` to present the review as targeted evidence-gathering complementing Vannmiljø, not as a systematic review. Cheap, and removes a reviewer attack surface.
- [ ] **P6.2** Move introduction material to discussion, as anticipated in `_planning.qmd` on the basis of the eData paper experience.
- [ ] **P6.3** Resolve the `**Comment:**` and `**KET:**` co-author prompts left inline in `index.qmd`.
- [ ] **P6.4** Results and discussion from the AEPs.

------------------------------------------------------------------------

## 10. Deferred, deliberately

Real problems (see `CLAUDE.md` section 3) that must not eat working days before 2026-09-14. Revisit after submission.

- renv disabled; `renv.lock` decorative.
- Absolute Windows paths in both `_targets.yaml` files.
- Notebook numbering collapse (three-way disagreement between filename, title, and sidebar); duplicate `AP03` and `NB07` prefixes.
- `_quarto.yml` sidebar \~20 files behind the tree.
- Half-migrated frontmatter between commits `9134762` and `8ad2d1f`.
- `DESCRIPTION` placeholder license, missing imports, one test file.
- 4.6 MB `references.bib`, 770 KB `manifest.json`, \~190 orphaned `.quarto/quarto-session-temp*` directories.

**One exception worth doing if the pipeline is genuinely costing you time:** cut the number of rendered notebooks. Roughly 34 `.qmd` files currently render on every full build, of which \~20 are unreachable from the sidebar anyway. Phase 0.2 plus Phase 2.1 removes 14 of them on their own. Render cost is the main pipeline slowness; repo size is mostly not.

Also worth an hour at some point, but not before Friday: `load_literature_pqt` is a pass-through whose comment claims a dependency on a `save_literature_pqt` target that is not actually referenced, which is very likely the cause of the "doesn't properly update" note at `_targets.R:1038`.

------------------------------------------------------------------------

## 11. Open questions

1.  ~~**P0.2**: delete or re-enable the outlier factory?~~ **Answered 2026-07-29: delete.** See Phase 0.
2.  ~~**When exactly is the holiday?**~~ **Answered: Mon 17 – Tue 25 Aug**, laptop available but costed at zero. Real dates now pinned throughout.
3.  ~~**Is `n >= 30` the right triage cutoff?**~~ **Answered: start at `n >= 100`.** Revisit the 30-100 band only if Week 6 buffer survives.
4.  ~~`docs/_planning.qmd` contract-end vs deadline?~~ **Answered: 2026-09-14 governs.** `docs/_planning.qmd` is hopelessly out of date and should be either deleted or replaced with a pointer to this file (it is still linked from the `_quarto.yml` sidebar, so it renders into the site as-is).
5.  **P1.0 scope:** confirm that dropping `NA`/`0` means the measured value column only, not a whole-row `drop_na()`. Proceeding on the narrow reading.