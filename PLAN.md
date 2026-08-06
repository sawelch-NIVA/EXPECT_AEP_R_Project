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
- [x] **P1.1c** ✅ **2026-07-29.** Facet minimum removed from panels (c) and (d): they answer a coverage question, not a statistical one. Safe because cardinality is bounded (max 31 campaigns, 5 site types per group). This restores **62 of 204** campaign facets that the `n >= 10` filter had been hiding.
      **Panel (d) was degenerate and is now fixed.** `SITE_GEOGRAPHIC_FEATURE(_SUB)` are part of the group key, so within a strict group there was always exactly one site type and the panel was a single row. It now uses a geography-relaxed subset (same trick as panel (a) with units), showing 3-5 site types where the data supports it.
      `case_when()` bug fixed at both remaining sites (`docs/NBXX-Outliers.qmd`, `scripts/reference_triage_plots.R`), both now calling `prettify_campaign_name()`. In the Outliers notebook the `.default =` was additionally being passed to `mutate()` rather than `case_when()`, silently creating a spurious `.default` column.
      90 tests passing.
- [x] **P1.1d** ✅ **2026-07-29.** Shared scales via a new `triage_scale_limits` target (`compute_triage_scale_limits()`, grouped by `ENVIRON_COMPARTMENT`, `by =` parameterised). Every panel in a group and every group in a compartment now share a value axis. Panel (e) colour scale is binned on **both** branches with shared limits (it was continuous for points, binned for hex). Group headings report `n_sources`. Embedding disabled and `_triage/` renamed `triage/`: page went **3.3 MB to 0.24 MB** with 25 linked PNGs.
      **Measured, so you can judge the squeeze:** Aquatic spans **12.3 orders of magnitude**, which is the entire global range, so per-compartment limits are effectively global for ~90% of rows. Adding unit to `by` only narrows Aquatic/mg-L to 9.8. The spread is genuinely within-unit.
      **Data flag:** max Aquatic sediment value is 270,000 mg/kg dry = 27% Cu by mass. Ore grade, not sediment. Worth chasing.
      99 tests passing.

- [x] **P1.1e** ⚠️ **Root cause found for the long-standing invalidation bug.** `tar_option_set(imports = "STOPAEP")` added. Functions live in the package namespace via `pkgload::load_all()`, and without `imports` targets does not hash them, so **editing any `R/fct_*.R` function invalidated nothing**. Confirmed directly: after changing `sample_triage_groups()`, `tar_outdated()` reported `(none)` and `tar_make()` reused the stale target. This is almost certainly the cause of the `load_literature_pqt` "doesn't properly update" note (CLAUDE.md 3.7).
      Enabling it invalidated 60 targets once, as expected. Full rebuild run; store now clean (`tar_outdated()` = 0).
- [x] **P1.1f** ✅ **Widened to 25 groups 2026-07-29.** Date axis is now global and never grouped (`compute_triage_scale_limits()` carries `date_min`/`date_max` spanning 1988-07-01 to 2025-11-18); rug lines thickened. 125 PNGs, full pipeline 1.7 min, page 0.33 MB with linked images.
      **Two bugs found by widening, both invisible at 5 groups:**
      1. `slugify_name()` calls `make.unique()`, so groups sharing a label got `_1`/`_2` suffixes. `triage_group_label()` omitted `SITE_GEOGRAPHIC_FEATURE_SUB` even though it is part of the group key, so distinct groups collided. That produced **duplicate notebook headings** and left unsuffixed slugs as string prefixes of suffixed ones, breaking filename matching. Label now includes the sub-site; 25/25 slugs unique, no suffixes.
      2. Constructing filenames by hand removed the chunk's `tar_read(triage_pilot_plots)` call, which was the **only thing giving the render target its dependency on the plots** — so Quarto rendered before the PNGs existed and every image came out "not available". The chunk now validates against the target's own output, keeping the dependency live.
      112 tests passing.

- [x] **P1.1g** ✅ **2026-07-30. Thresholds, headings, table links, layout.**
      `R/fct_threshold_match.R` (new) matches `copper_toxicity_thresholds` to a
      group by compartment, unit and taxon. Read its header before interpreting
      any line: the comparators are borrowed across compartments, species and
      tissues, deliberately, and are a sanity check rather than an assessment.
      Mapping: all non-marine water to the freshwater classes, marine and
      brackish to coastal, sediment to (marine) sediment, terrestrial blank;
      vertebrates to *G. morhua*, invertebrates to *M. edulis*, algae and the
      catch-all groups blank. Units are **matched, never converted**, so
      wet-weight biota get PROREF, dry-weight invertebrates fall through to the
      ICES BAC, and dry-weight vertebrates get nothing.
      **Corrected the threshold data itself.** Freshwater and sediment boundaries
      were misaligned by one row against their own comments (sediment had two
      classes sharing an upper boundary of 20). Copper has no Class III in
      either: M-608 runs Class II into Class IV. Now
      `c(0.3, 7.8, 15.6, NA)` and `c(20, 84, 147, NA)` over four classes.
      **Presentation, second attempt.** The first put rotated labels beside each
      line; with the shared axis spanning up to 12.3 orders and the boundaries
      inside about one, three labels landed within 7% of the panel width and
      stacked illegibly. Class names now sit on a **secondary axis** and the
      panels carry no text at all. Colour and linetype are keyed on the class
      *number* (muted MD blue-green-ochre-orange-red, dotted through to solid),
      so Poor stays orange despite Class III being absent, and a dataset that
      does have a Class III picks up the third style with nothing reordered.
      **Two bugs worth remembering:** (1) `stat_bin2d()` takes its binning range
      from the *shared scale*, not its own layer, so a label at `y = Inf` pushed
      that range to infinity, the stat asked for over a million bins and failed
      outright, drawing no heatmap. Panels (c) and (d) now count explicitly and
      draw `geom_tile(height = 1)`, which also fixed the band-height gaps (0.179
      of a row pitch of 1.0). (2) A class-number regex `(I{1,3}|IV|V)` matched the
      bare `I` inside `(IV)`, styling Poor as Background. Ordered alternation is
      load-bearing. Caught by eye on a prototype, not by any numeric check.
      Also: hierarchical headings (six levels, unit not a level, unit variants
      share a heading and an anchor), siblings ordered by summed `MEASURED_N`,
      summary-table hyperlinks into the 24 sections (opt-in per document, so the
      manuscript does not carry dead anchors into its docx), `theme_minimal()` via
      a shared `triage_theme()`, dotted grey60 trendline over a solid white halo,
      and `docs/_metadata.yml` for notebook-wide `page-layout: full` /
      `toc-depth: 6`. 237 tests passing, up from 112.
      **Come back to:** panel (b) draws threshold lines but omits the class axis,
      because on a vertical axis the numerals collide (II and IV are 0.24 orders
      apart on a 10.7-order axis) and the rotated title crowds the legend. No
      better idea yet, and these are triage plots.

- [x] **P1.2** ✅ **2026-07-30.** Delivered as `write_triage_plots_for_group()`: fixed 8x5in / 150dpi canvas, PNG via `ragg::agg_png`, returning the written paths.
- [x] **P1.3** ✅ **2026-07-30, by a different design.** One `triage_pilot_plots` target with `format = "file"` writing all 125 PNGs to `triage/`, rather than a `tar_map()` over groups. Ticked off as-is on Sam's call.
      **Known cost, so it is not a surprise later:** invalidation is all-or-nothing. Editing any plot function redraws all 125 PNGs, measured at 1m 20s to 2m 20s. A `tar_map()` would redraw only the affected group's five. Revisit only if that wait starts to bite.
      Note the directory is `triage/`, not `_triage/` as this item originally said: Quarto skips underscore-prefixed directories as project resources, which broke every linked image.
- [x] **P1.4** ✅ **2026-07-30.** Ranked by `n` descending, no composite score.
      `R/fct_triage_flags.R` holds `add_triage_flags()`, which the
      `summarise_literature_data` target now ends with, plus `group_flag_text()`
      and `group_summary_line()`. The summary table and the per-group text under
      each triage heading read the **same** function, and a test walks every
      group asserting the highlighting and the prose agree.
      **Scope pulled back from what this item originally listed**, on Sam's call
      2026-07-30, and the reasoning is worth keeping because it will recur:
      - `n_references == 1` **dropped as a flag.** It fired on 234 of 245 groups.
        Vannmiljø is one `REFERENCE_ID` covering monitoring for the whole of
        Norway, so a single source is the normal state of this dataset, not an
        exception. Flagging the baseline left 236 of 245 groups flagged and
        buried the signals that discriminate. `n_sources` is still reported.
      - **CV removed entirely, and no replacement added.** Measured on this data,
        CV correlated **0.96** (Spearman) with max/median across the 51 groups
        with `n >= 20`: it tracked the single largest value, not the spread.
        Dropping one row of 4,969 from Marine/Salt Water moved CV from 40.7 to
        4.7, while a log-scale spread measure moved 2.9 to 2.8. It was also
        redundant, being exactly `sd / mean` with both already reported.
        Geometric SD and the interquartile ratio were offered as defensible
        log-scale alternatives and **declined**: a spread statistic that cannot be
        justified in the methods section is worse than none.
      - Drop proportion and unit count are **columns, not flags**, which is what
        this item asked for. A 90% drop rate is reported and not warned about.
      **The flag set is now exactly two**, both predating this work: outlier
      fraction > 5% and the dip test. `triage_flag_limits()` carries a note that
      adding a third is Sam's call, and a test asserts the set has not grown.
      287 tests passing.
- [x] **P1.5** ✅ **2026-07-30. Weighted by `MEASURED_N`**, per Sam's decision.
      `n_double_outliers` is now `sum((outlier_RMZ & outlier_IQR) * MEASURED_N)`,
      so numerator and denominator are both measurement counts. The old row count
      is retained as `n_outlier_rows` so the change stays auditable.
      **Measured effect:** 20 groups flagged before, **22** after; nothing
      un-flagged. Small in count because most groups have `MEASURED_N == 1`
      throughout, but large where it bites: an Aquatic Sediment group went 3.7% to
      **15.9%** (6 outlier rows carrying 26 measurements) and a *G. morhua* group
      3.9% to **11.8%**. `na.rm = TRUE` because `robust_modified_z_score()`
      returns `NA` where the MAD is zero, so untested rows count as
      non-outliers, which is the conservative direction.

- [ ] ~~**P1.5** **Fix the outlier-fraction denominator.**~~ Superseded above. In `summarise_literature_data`, `n = sum(MEASURED_N)` (total measurements) but `n_double_outliers = sum(outlier_RMZ & outlier_IQR)` (count of **rows**). The ratio `n_double_outliers / n` therefore divides a row count by a measurement count, so the flag systematically under-fires wherever `MEASURED_N > 1`. It currently trips on 20 of 245 groups; the true figure is higher. **Not fixed in Phase 0 because it changes a reported statistic, which is your call, not mine.** Options: divide by row count instead, or weight the outlier count by `MEASURED_N`. Cheap either way, but decide before the Phase 2 decisions rest on it.

**Budget check:** at `n >= 100`, expect roughly **30-50 groups** rather than the 60-100 at `n >= 30`. At 2-3s x 5 plots that is 8-13 min single-threaded, a few minutes across crew workers. Runs once, then targets caches each PNG independently, so editing one plot function only invalidates that plot across groups.

**Deferred by choice:** groups in the 30-100 range get panels only if Week 6 buffer survives. Note in `group_decisions.csv` that they were decided from the table alone, so it is visible later which decisions had thinner support.

**Cut line (3pm Thursday):** if the `tar_map` file-target factory is fighting you, abandon it and generate the panels from a plain loop in a script writing to `_triage/`. Losing the dependency tracking costs you one manual re-run. Losing Friday costs you the week.

------------------------------------------------------------------------

## 5. Phase 2 — Make the decisions (Thu PM – Fri)

**This is the Friday deliverable, and it is the only thing that matters this week.**

- [x] **P2.1** ✅ **2026-07-30, satisfied by `docs/NBXX-Sample-Groups.qmd`** rather than by a new file. It already embeds the panels as linked images, one row of five views per group, under a hierarchical heading tree, above a summary table that links into each section. A second near-identical notebook would be duplication, and the 14 distributions notebooks it was meant to replace are already gone (P0.2).
      Also widened from a 25-group sample to **all 27 groups with `n >= 100`** (`n_sample = Inf`). At 25 there were two eligible groups with no panels and nothing on the page to say they were missing rather than absent.
      **Difference from this item as written:** ordering is hierarchical with siblings ranked by `sum(MEASURED_N)`, not flat rank order. Nesting is what makes 27 groups navigable, and the summary table above is already strictly rank-ordered, so rank order is available where it is useful.
- [x] **P2.2** ✅ **2026-07-30.** `R/fct_group_decisions.R` plus `scripts/scaffold_group_decisions.R`, which writes `data/clean/group_decisions.csv`: 245 rows, the full group key, `species_common_name`, `rank`, `n`, `n_sources`, `cum_pct`, `tier`, both flags, and empty `decision` / `lump_into` / `notes`.
      **Scaffolding is a hand-run script, not a target.** The pipeline reads the file (`group_decisions` target, which also warns when groups in the data are absent from the file) and never writes it. Writing a hand-edited file from a target is how an afternoon of judgement gets silently overwritten by a rebuild.
      **The scaffold is an idempotent merge**, so it is safe to re-run whenever new data arrives: machine context (`n`, coverage, flags) refreshes, `decision` / `lump_into` / `notes` are never touched, new groups append as undecided, and a *decided* group that has vanished warns rather than disappearing quietly. Same cache-versus-curation split as section 10. `read_group_decisions()` validates the vocabulary and warns on a `lump` with no `lump_into`. 24 tests.
- [ ] **P2.3** **You read the contact sheet and fill in the decisions.** No automation. Work down from largest n.

      **Revised 2026-07-30: the CSV is not enough, and the working documents are the answer.** `group_decisions.csv` is fine for a first pass, but a comparison between groups does not fit in a `notes` cell, and the verdict needs to sit next to the plot it is about. `scripts/generate_group_notebooks.R` scaffolds one markdown document per notebook into `docs/groups/`: 11 files, 245 group sections, a glance table per notebook for comparing its groups, and a `**Verdict:**` prompt under each group.
      **The generator is append-only and that is the entire design.** Its predecessor, `scripts/generate_distribution_notebooks.R`, was deleted in P0.2 along with fourteen generated notebooks, and the lesson recorded in CLAUDE.md is that hand-written prose in generated files is reproducible from nowhere else. So it creates a file only if absent, appends sections only for groups not already present (detected by `{#grp-Gnnn}` anchor, so retitling a heading is safe), and never rewrites, reorders or removes anything. Verified byte-identical on re-run, and there is a test that writes prose, forces an append, and asserts the prose survives.
      Output is deliberately **static**: counts are written into the markdown rather than computed by chunks, because these are documents to write in, not reports to re-run. The cost is that figures go stale if the data change, which is why every file is date-stamped and every group carries its stable id.
      Not in the `_quarto.yml` render list, so a project build ignores them. Render one by hand with `quarto render docs/groups/<file>.qmd`.
      Only 29 of 245 groups have triage panels; the rest carry a note naming the `must_include` remedy and their id.

**Definition of done, revised 2026-07-30 after measuring it.** The original wording was "every group covering the top 90% of total measurements", guessed at "probably the top 30-50 rows". The real distribution is far more skewed than that:

| Coverage | Groups needed |
|---|---|
| 50% | **2** |
| 75% | 3 |
| 90% | **7** |
| 95% | 17 |
| 99% | 85 |

183 of 245 groups have `n < 30`. So the literal Friday criterion is **7 decisions**, which is an hour, not two days. All 7 already have panels, as do all 17 of the top 95%.

**The criterion is also aimed at the wrong thing, and this matters more than the count.** Measurement volume is dominated by Vannmiljø water and sediment monitoring, so the top 90% is 4 abiotic groups plus 3 biota groups (two of them the same *Mytilus edulis* / soft tissue group in different site types). That gives almost no biota diversity. The biota groups an AEP needs as nodes are nearly all small: **62 of the 68 `top99` groups are Biota**, sitting between `n = 20` and `n = 218`. Algae, one of the systems P3.5 names, does not appear until well below the 95% line (*Ascophyllum nodosum* 75 rows, *Fucus vesiculosus* 71).

**Recommended replacement:** decide all **17** groups down to 95% coverage, *plus* every group needed for AEP node coverage regardless of `n`, driven by the systems chosen in P3.5. Ranking by `n` is the right way to order the work; it is the wrong way to decide where to stop.

**Explicitly not this week:** report cards, EPEQ scoring, edges, any AEP rendering, any repo cleanup beyond Phase 0.

------------------------------------------------------------------------

## 6. Phase 3 — Report cards and nodes (Mon 3 – Fri 7 Aug)

> **Process change, 2026-08-05.** Sam abandoned sequential review of all 245
> groups: "going through all the notebooks sequentially is not a good idea.
> Instead I need to start picking groups of interest and expand from there."
> The 11 documents in `docs/groups/` become reference rather than the spine.
> Sequential review optimises for coverage; selection needs the complement, a
> ranked record of what has *not* been picked, which is `aep_node_coverage`.

- [x] **P3.1** ✅ **2026-08-05.** `node_report_card()` in `R/fct_aep_nodes.R`:
      measurements and rows separately, sources, unit, arithmetic and geometric
      mean, SD and GSD, median, spatial and temporal range, plus `n_arctic` /
      `pct_arctic`.
- [x] **P3.2** ✅ **2026-08-05/06.** `write_node_cards()` writes one PNG per node
      to `figures/node_cards/` and a smaller variant to
      `figures/node_cards_compact/`, both `format = "file"` targets. The compact
      variant is what the diagram places, so the card and the node cannot drift.
- [x] **P3.3** ✅ **2026-08-05, by a different design.** Not scaffolded from the
      `notebook` assignment: a node is **not** a sampling group, and Sam's own
      prototype proves it. `docs/NBXX-algae.qmd` defines its marine node with
      `LATITUDE >= 66.5`, which is not in `triage_group_cols()` at all, and drops
      outliers a few lines later.
      So the layer is **two hand-edited files**: `data/clean/aep_nodes.csv`
      (identity, level, x/y, restrictions, EPEQ scores and justifications) and
      `data/clean/aep_node_members.csv` (one row per node x group). One node can
      be one group, several groups, or a restricted slice of either.
      **Restrictions are fixed columns, not a filter expression in a cell**:
      `lat_min`, `lat_max`, `date_min`, `date_max`, `exclude_references`,
      `drop_outliers`. Arbitrary R in a spreadsheet cannot be validated, fails at
      pipeline runtime rather than read time, and does not diff usefully. Anything
      beyond these should become a named column, not an escape hatch.
      `node_type` separates `empirical` (resolved from data) from `external`
      (magnitude typed in from an assessment made elsewhere, which is what P3.6
      needs for the emissions and REACH nodes).
      **Mixed units are refused, not averaged.** Scaffolding is
      `scripts/scaffold_aep_nodes.R`, append-only, and it does **not** propose
      nodes. 47 tests.

      **Arctic coverage is reported, never filtered** (Sam's call 2026-08-05).
      Measured first: a global `LATITUDE >= 66.5` would drop **81% of
      measurements** (95,816 to 17,900) and leave Marine/Salt Water on 258. So
      the AEP is Norwegian, and Arctic representativeness is a stated property of
      each node in the same spirit as `n_sources`. If the framing survives to
      submission it needs a sentence in the methods.
- [ ] **P3.4** **You score the nodes.** Copy the reasoning style already in `NBXX-algae.qmd:tbl-epeq-algae`; it is good and it is yours.
- [ ] **P3.5** Pick the systems to model. Target 3-5, likely fish, algae, shellfish, sediment, water. Driven by the Phase 2 decisions, not chosen in advance.

      **A notebook is not a node** (clarified 2026-07-30). A notebook is a place to do exploratory work; **one or more nodes are an outcome of that work**. So the 11 notebooks from the P2.3 scheme and the 3-5 focused AEPs here were never in tension, and the question of "11 notebooks against a target of 3-5" does not need answering. The count to watch is nodes, and it is not known until the notebooks have been worked through.

      Corollary for the Fish notebook, which at 89 groups is an order of magnitude larger than any other: **that is not a problem to solve before writing it.** The scrutiny (compare across tissues, then across species) is what the notebook is *for*, and it needs the notebook to exist first. Panels f and g already do exactly those two comparisons at the sub-compartment and species-group headings.

- [ ] **P3.6** **Transcribe one source node and one source edge by hand, from assessments that already exist.** Added 2026-07-30.

      A diagram whose top level is "marine water" is an occurrence summary with arrows on it. What makes it an *aggregate exposure* pathway is that it reaches from a release to a target site exposure. The minimum that is recognisably an AEP is roughly three levels: source, medium, organism.

      **This is typing, not infrastructure, and that distinction is the whole point of this item.** `docs/NBXX-norske-utslipp.qmd` already carries a complete written WoE assessment under "Line of Evidence Assessment": Essentiality 1, Theoretical Plausibility 3, Empirical Support 1, Quantitative Understanding 1, each with a justification. `docs/NBXX-REACH.qmd` has the same for the tonnage side. `aep_nodes.csv` and `aep_edges.csv` are hand-edited CSVs, so those scores become rows in about half an hour with no code at all.

      **Do not integrate the emissions data into the pipeline to do this.** See section 10.

      Useful fact for when you get there: `copper_emissions_by_source_summarised` in the utslipp notebook is already `source_category × medium × emission_kg`, i.e. `from` / `to` / `magnitude` with a real unit. The Sankey chunk that consumes it is `eval: false`, but the table itself is an edge list in all but name. These are also among the few edges that can carry an **empirical magnitude in kg**, as opposed to the water-to-biota edges, which need paired observations that may not exist. The source end of the diagram may therefore end up better evidenced than the middle, which is a finding rather than an embarrassment.

------------------------------------------------------------------------

## 7. Phase 4 — Edges (Mon 10 – Fri 14 Aug, hard time-box)

**Your stated highest risk. The mitigation is scope, not effort.**

- [x] **P4.1** ✅ **2026-08-05.** `scripts/scaffold_aep_edges.R` proposes every
      **downward** flow between placed nodes, ordered by `level`
      (source → medium → organism → tse), all `putative`. Upward flows (biota
      returning copper to sediment) are real but are a judgement to make
      deliberately, not to find pre-typed. Append-only: it never removes or
      reverses anything.
      Deliberately over-generous, because the time-box in P4.2 works by having
      the candidate list in front of you and crossing edges off. **16 edges
      proposed across your 7 nodes; expect to delete most of them.**
      `read_aep_edges()` refuses unknown node ids, self-loops, duplicate ids and
      out-of-range scores; `validate_aep_edges()` warns on an empirical edge with
      no `evidence_justification`, a putative edge carrying a magnitude, a
      magnitude with no unit, and nodes with no edges at all.
- [ ] **P4.2** **Time-box gap-filling to 3 working days.** For each edge, spend at most \~30 minutes looking for empirical support. Found: score it, mark `empirical`, cite. Not found: leave `putative`, write one sentence on what evidence *would* settle it, move on.
- [x] **P4.3** ✅ **2026-08-05.** `aep_edge_styles()` separates the two in
      **four** channels at once (linetype, colour, linewidth, alpha), and a test
      asserts they never collapse to the same styling. Magnitudes are labelled on
      empirical edges only. This is the property the phase's whole argument rests
      on, so it is enforced rather than trusted.

The unfilled edges are a result. A regional-scale AEP that honestly marks its own gaps is a more defensible contribution than one that quietly implies completeness. This is also the argument that lets you stop on schedule.

------------------------------------------------------------------------

## 8. Phase 5 — Assemble and render (Wed 26 – Fri 28 Aug)

- [x] **P5.1** ✅ **2026-08-05, first working version.** `plot_aep()` with manual
      coordinates, as specified. **Plain ggplot2 rather than ggraph**: with
      coordinates already fixed there is no layout algorithm left to want, and a
      graph package would be a dependency added to draw segments and text. The
      `aep_diagram` target writes `figures/aep.png` as a `format = "file"`
      target, per CLAUDE.md 4.4.
      **Known rough edge:** arrow ends are trimmed by a fixed fraction of the
      segment, so against a wide node label they can stop visibly short. Fine for
      a working diagram, wants per-label geometry for a manuscript figure. Revisit
      at P5.4, not before.
- [x] **P5.2** ✅ **2026-08-06.** `plot_aep()` places `node_cards_compact` with
      `ggimage::geom_image` (`R/fct_aep_edges.R:396-401`), falling back to a text
      label for any node without an image, so an unplaced card degrades rather
      than blanking the node.
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

## 9a. Notebook-review fixes, 2026-08-05

Worked from the TODOs Sam left in `docs/groups/` while reading the panels. Each
is answered in place in the notebook it came from; recorded here so the reasoning
survives the notebooks going stale. **690 tests passing, up from 600.**

- **Outlier criteria both on log10.** The complaint was "outliers aren't using
  abs, so only highlighted on the right". The `abs()` was there and the Tukey
  fences were always two-sided; the RMZ ran on the **raw** scale, where MAD is
  set by the bulk near the median, so nothing below the median could reach 3.5.
  Measured over the 74 groups with n >= 10 rows, double-flagged rows split
  low/high: **5 / 2,525** before, **359 / 1,637** after, and 2.2% of rows flagged
  against 2.8%. Sam's literal instruction was to move to non-logged data; that
  was measured too (**5 / 8,876**, 9.9% of rows) and rejected on the numbers,
  since it worsens the exact symptom being fixed.
  Group flags went **22 to 14**, of which 6 of the 11 losses were groups under 10
  rows that `summarise_literature_data` had been flagging without gating.
  That inline copy of the logic in `_targets.R` is gone; it now calls
  `flag_outliers()` like everything else, so the summary table cannot drift from
  the panels again. Added a MAD-zero guard: the criterion abstains rather than
  flagging every value that is not exactly the median.
- **Threshold lines named by lower bound.** `THRESHOLD_VALUE` is an upper
  boundary in the source, so lines read one class low and Class V never appeared
  anywhere. Now named for the class each boundary **opens**
  (`add_threshold_boundary_class()`), so sediment reads II / IV / V up 20 / 84 /
  147. The successor is taken from the next rung present, not by adding one to
  the numeral, because copper has no Class III. Coastal water gained an
  open-ended Class V row so its top line has a name.
- **Sample-size rule.** A sample size is `sum(MEASURED_N)`; anything counting
  rows says "rows". The category count labels were rows while the heading above
  them was measurements, which is what made the fish overview unreadable. Both
  the label and its bracketed outlier count are now measurement-weighted; the
  heatmap legend is renamed "Rows". Panels carry an in-plot `n (n outliers)`
  header.
- **Count labels moved off the data** into a reserved right-hand margin, rather
  than haloed in place. Fixes "polluted seabed's very high right conc covers up
  sample size" and stops the label hiding what it counts.
  **Regression worth remembering:** the first attempt anchored the labels beyond
  `limits[2]`, and a continuous scale with explicit limits censors out-of-bounds
  values, so every label silently vanished behind a "removed N rows" warning.
  Expansion adds drawing room; it does not widen limits.
- **Panel (e) rebinned** to one colour step per order of magnitude with the M-608
  boundaries inserted and named in the key. Thresholds cannot be lines on that
  panel: its axes are lon/lat and concentration is the fill. The finer binning
  needed the legend bar stretched to most of the panel height, or 15 labels
  overlap into an unreadable block.
- **Figure-ref bullets in the scaffold**, append-safe by construction and
  deliberately not backfilled into sections that already have prose.

### 9b. The unit-standardisation audit, 2026-08-05

Sam asked whether the Coteur discrepancy was his extraction or the pipeline,
having found the paper reports μg g⁻¹. **It was both, and they are separate
faults that happen to share the same 1000x signature.**

**Fault 1, the pipeline, and the worse of the two.**
`standardise_measured_units()` derived the standard unit from one rule (does the
string contain dry / wet / L) and the conversion factor from an independent one
(does it contain "ug"). Nothing checked that the two agreed. Reading only the
numerator prefix means assuming the denominator is kg or L, and for **μg/g that
assumption is wrong by exactly the factor that hides it**: μg/g *is* mg/kg, a 1:1
conversion, but the code divided by 1000.

**93 rows across 11 references** came out a thousandfold low. All biota and
sediment: Ervik, Routti, Leung, Kryukova, Olsvik, Verjordet, Schaanning,
Pempkowiak, Gillan, Sonne, Brooks. Corrected values are now 0.3 to 243 mg/kg,
which is ordinary; before, they were 0.0003 to 0.243.

Units are now **parsed**: numerator and denominator read separately, factor
derived from both, standard unit derived from the denominator rather than
guessed at a second time. Anything unparseable is reported by name and row count
instead of silently becoming `NA`, because silence is how this survived.

**Fault 2, the extraction.** Coteur itself is *not* in that 93. Its rows carry
`μg/kg (dry)` in the source file, so the pipeline converted them correctly from a
unit that was wrong on the way in. That one is a transcription fix in
`data/raw/eData/`, and it is still outstanding.

**Recovered as a side effect:** 18 rows of `2000JulshamnTraceElementLevels` whose
micro sign had been destroyed by an encoding round-trip (`U+FFFD`). They matched
neither μ nor µ, so they converted to `NA` and the whole reference was dropped in
silence. Now repaired, with a warning naming the assumption. **This added six new
groups** (hooded and harp seal, kidney/liver/muscle), which is why
`group_ids.csv` and `group_decisions.csv` gained rows G246-G251.

**Still dropped, and correctly:** `µM` (42 rows) needs a molar mass to convert
and is a real conversion Sam could do for copper if those rows matter; `%`
(11 rows); a bare `mg/kg` (1 row) refused rather than guessed, since dry and wet
differ by a factor of four or five in biota.

### 9c. The Vannmiljø unit review, 2026-08-05

Asked for after G047 turned out to be the known Urban Fjord 1000x problem.

**Our Vannmiljø conversion code is correct.** `vm_convert_unit()` maps the three
units that reach it (`µg/l`, `mg/kg t.v.`, `mg/kg v.v.`) onto the right standard
units, and **aborts on anything unrecognised** rather than guessing, which is
better than the literature path was before today. Those then flow through the
rewritten `standardise_measured_units()`, so µg/L to mg/L is a correct 1e-3 and
the two mass units pass through at 1. LOQ and LOD take the measurement's unit,
which is forced rather than assumed: the raw export has one `Enhet_id` per row
and no separate limit-unit column.

Two things worth knowing rather than fixing:

- The raw copper export carries a **fourth** unit, `ng/m3` (1,123 rows, air
  deposition). It never reaches `vm_convert_unit()` because
  `vm_filter_compartments()` drops it first. So the abort is narrower than it
  looks: it can only see units surviving the compartment filter. Acceptable,
  since those rows are unwanted anyway, but the abort is not the whole guard it
  appears to be.
- `known_units` hardcodes `U+00B5` and the lookup supplies `U+00B5`, so they
  agree today. If the export encoding ever shifts to `U+03BC`, the pipeline
  aborts. Loud, which is right.

**The error is in the source, and the source says so.** 33 rows across seven
biota groups carry the comment *"Verdier oppgitt i µg/g (w.w.) og multiplisert
med 1000"*: values given in µg/g wet weight and multiplied by 1000. **µg/g is
mg/kg**, so the multiplication is the error. All are Urban Fjord Contaminants,
sampled 2017-10-15.

| Group | rows | geo. mean | ÷ 1000 |
|---|---:|---:|---:|
| *Gadus morhua* muscle | 15 | 3,670 | 3.67 |
| *Euphausiacea* whole body | 3 | 18,600 | 18.6 |
| *Polychaeta* whole body | 3 | 17,800 | 17.8 |
| *Gadus morhua* liver | 3 | 14,400 | 14.4 |
| *Pandalus borealis* soft tissue | 3 | 7,460 | 7.46 |
| *Clupea harengus* muscle | 3 | 799 | 0.80 |
| *Mytilus edulis* soft tissue | 5 | 66.7 | 0.067 |

Every one is ordinary once divided. This is the **third** instance in one day of
the same µg/g-is-mg/kg misconception, after Sam's Coteur extraction and the
`standardise_measured_units()` fault (section 9b).

**Detectors added** (`R/fct_unit_anomalies.R`, target `unit_anomaly_report`),
because this class of error cannot be prevented, only found:

- `scan_comment_unit_flags()` reads the comment field for submitters describing
  their own unit arithmetic. Certain rather than inferential.
- `scan_group_scale_offsets()` compares each campaign's geometric mean against
  the **median of the other campaigns in the same sampling group**. Two earlier
  reference statistics were wrong and the reasoning is recorded in the function:
  a pooled-row reference let the 18 bad rows outvote the correct ones and flagged
  the four **correct** campaigns instead.

Both **report and never correct**. Rewriting a measured value on the strength of
a free-text comment is Sam's judgement, not the pipeline's.

**Deliberately not acted on, 2026-08-05 (Sam: "we won't touch those yet").**
Logged here so they are not lost:

1. **The 33 Urban Fjord rows.** Correct, exclude, or leave to the outlier
   machinery. Correcting is well evidenced but means this pipeline overriding a
   national database, which wants a methods sentence. Note one of them
   (*M. edulis* at 1,420 mg/kg wet) sits inside AEP node N003, so the decision
   reaches the AEP and not just the triage plots.
2. **_Salmo trutta_ muscle**, flagged in three campaigns at 2.3 to 2.7 orders
   apart (Measures Monitoring, Urban Fjord, MilFersk). Uninvestigated; looks like
   the same family of problem.
3. **Aquatic Sediment / Screening New Contaminants**, 2.0 orders low over 19
   rows. Uninvestigated.

`unit_anomaly_report` re-derives all three on every build, so they cannot go
stale or be forgotten.

### 9d. `row_id` and the corrections layer, 2026-08-06

Detection existed; correcting was still a thing done by hand in a notebook, or
not at all. Two files close that, and the first exists only to make the second
safe.

**`row_id` (`R/fct_row_ids.R`).** A stable per-measurement key, assigned in
`load_literature_pqt`. Lower case deliberately: SCREAMING_SNAKE in this project
means "column of the eData schema", and this is an administrative key of ours
that is not in it. Same convention holds across the corrections file.

**A sequential `R00001` counter was proposed and rejected.** It is *positional*:
Vannmiljø is re-exported periodically and the eData files are edited whenever an
extraction fault is found, so one inserted row shifts every id after it, and a
hand-edited correction keyed on `R01234` then silently overwrites a different
measurement. That is the same silent-success failure as the missing
`imports = "STOPAEP"` and the untracked `group_decisions.csv`, and the worst
possible place to introduce it deliberately.

`SAMPLE_ID` is already content-derived (`generate_sample_id_with_components()`),
so it has no such mode, and it sorts by site then parameter then date as a
property of what it is made of. `row_id` is therefore `SAMPLE_ID`, with
`SUBSAMPLE` appended only where rows genuinely collide, and an **abort** if a tie
survives every disambiguator. No counter fallback: a silently-suffixed id is a
positional id in disguise.

Measured: 90,221 rows, all unique. Vannmiljø was already clean at 89,631/89,631;
29 literature rows across 11 shared ids were extraction defects (`SAMPLE_ID`s
written before `SUBSAMPLE` was refined, or never carrying it), fixed at source.
`row_id_collisions` reports any that return.

**Unit corrections (`R/fct_unit_corrections.R`).** `data/clean/unit_corrections.csv`,
hand-edited, read and never written, applied by `literature_corrected` between
`load_literature_pqt` and `literature_analysis_ready`. Above the hygiene step so
corrections land before anything is dropped or summarised; below
`literature_clean_standardised` so *our* conversions and *their* errors stay
separate concerns.

**Correcting per AEP node was considered and rejected.** The same bad rows feed
the triage panels, the summary table and `group_decisions.csv`; correcting only
at the node leaves all of those lying while the AEP tells the truth.

Three design points worth keeping:

1. **Both a selector and a `row_ids` list, required to agree.** Selector alone
   silently widens when a re-export adds matching rows; `row_ids` alone silently
   narrows and records no reasoning. Requiring both turns a change of extent into
   a build failure that names the drift in both directions. `row_id` exists for
   this. Resolution is `scripts/scaffold_unit_corrections.R`, hand-run and never
   a target, because resolving in the pipeline would make the ids track the data
   and destroy the check.
2. **Matching happens in a pass of its own, before anything is scaled.** Matching
   and scaling in one loop tests `value_min`/`value_max` against values an
   earlier correction already multiplied, so the row order of the CSV would
   change the numbers. Caught by a test, not by review.
3. **`comment_match` normalises the micro sign on both sides.** The comment that
   identifies the Urban Fjord fault carries a real `µ`. Requiring one to be typed
   into a spreadsheet, on Windows, to select rows for overwriting, invites 4.4.-2
   into the least forgiving place in the pipeline. Write `ug`.

Corrections scale `MEASURED_VALUE_STANDARD` and both LOD/LOQ standard and imputed
columns (a submitter who multiplied their values multiplied their limits).
`MEASURED_VALUE` is untouched as the audit trail. Every failure is an abort, not
a warning: stale correction, drifted row ids, a row matched twice, a missing
`reason` or `evidence`, no selector at all.

`unit_anomaly_report` now reads corrected data, which changes what it means: it
is a **shrinking to-do list**, not a static record. A group still flagged after a
correction means the correction was insufficient.

C001 written by Sam 2026-08-06: Urban Fjord, `comment_match`, factor 0.001,
33 rows across 7 groups.

**The factor is cross-validated five ways, and that method is reusable.** The
campaign is only partly affected, so each species has both corrected and
uncorrected rows and they must agree afterwards. Post-correction medians against
their uncorrected campaign-mates: *C. harengus* muscle 1.10, Euphausiacea 1.13,
Polychaeta 1.15, *M. edulis* 1.24, *P. borealis* 1.24. Five species inside 25%
is far stronger than the comment alone. **Use this check on every future
correction**: a right factor lands the corrected rows on top of their peers.

### 9e. `exclude_campaigns`, and why G047 needed it, 2026-08-06

The same check found a **second, unrelated fault** that no factor can fix.
*G. morhua* muscle came out at ratio 20.3, against 1.10-1.24 for everything else.

Diagnosis, at the single site `Vannmiljø_01.01-82497`:

| | year | n | median |
|---|---|---|---|
| Cod muscle (corrected) | 2017 | 15 | 3.509 |
| Cod muscle (uncorrected) | 2022 | 3 | 0.173 |
| Cod liver | 2022 | 3 | 5.08 |
| Cod liver | 2023 | 3 | 4.22 |

Four things say this is a **tissue-labelling fault, not contamination**:

1. A 20x fall in five years at one site is not an exposure signal.
2. The high muscle values sit inside the cod *liver* range at that same site.
   So does Measures Monitoring 2010 muscle (6.34).
3. Cod muscle across all campaigns is either 0.10-0.35 or 3.5-7.7, with
   **nothing in between**. Contamination gives a continuum; a labelling fault
   gives exactly this gap.
4. Liver is flat while muscle moves 20x. That is backwards: copper is essential
   and homeostatically regulated in muscle, and liver is the accumulating organ.

**Why not just flag the node as low-quality evidence** (Sam's first instinct,
and a reasonable one). Low quality is the right label for genuine uncertainty.
This is a positive diagnosis of a defect in a known, enumerable set of rows.
Averaging over rows believed to be mislabelled and calling the mean low-quality
gives a wrong number wearing an honest label, and the EPEQ score then does not
mean what it says. Exclude first, then score what survives.

**Why not `drop_outliers`.** 20 of 44 rows. Tukey fences cannot reach a mode
that size, and a mode that size is not an outlier in any statistical sense. This
is a provenance judgement, not a statistical one.

So `exclude_campaigns` joins `exclude_references` as a fixed restriction column
on `aep_nodes.csv`, sharing `apply_node_exclusion()` so the two cannot drift.
**An exclusion matching nothing warns**, because the silent version is
particularly nasty here: the node still resolves, still produces a mean, and the
rows you believed you removed are still in it. Campaign names carry spaces and
parentheses, so a typo is easy and otherwise invisible.

Excluding both affected campaigns leaves N005 on **21 rows spanning 0.10-0.50
mg/kg wet**, across four campaigns. It also drops the 3 clean 2022 Urban Fjord
rows, which is the deliberate conservative reading: a campaign that mislabels
tissue in one year is suspect throughout, and "looks fine" is not evidence.

Still outstanding from 9c: anomalies 2 (*S. trutta* muscle) and 3 (Aquatic
Sediment / Screening), and the Coteur transcription fix, which stays a raw-data
edit. If corrections are applied before submission they need a methods sentence,
since this is the pipeline overriding a national database.

**Anomaly 2 is worse than 9c recorded.** *S. trutta* muscle, Urban Fjord, 6 rows,
median **33,571 mg/kg wet**, max **76,815**, i.e. 7.7% copper by mass. Its
comment reads `ICP-MS`, not unit arithmetic, so C001 correctly did not touch it.
Next correction candidate; use the cross-validation check to pick the factor
(other trout muscle campaigns sit at 0.15-0.64).

### 9f. State of play, end of 2026-08-06

Pipeline fully built, `tar_outdated()` reports 0. Test suite 1115 passing.
**Nothing committed yet** (~30 modified, ~7 new files).

#### Do this first

`aep_nodes.csv` has the `exclude_campaigns` column but **N005's cell is still
blank**, so the node is still on all 44 rows (mean 2.23, median 0.235, GSD 5.15,
still bimodal). Paste:

```
Vm_2010_2025 (Urban Fjord Contaminants); Vm_2010_2025 (Measures Monitoring)
```

then `tar_make()`. N005 lands on 21 rows at 0.10-0.50 mg/kg wet. This is the
last step of 9e and everything in that section assumes it.

#### Open, ordered by how much they bite

1. **C002 for *S. trutta*.** See 9e. Biggest remaining data fault.
2. **The comment detector never shrinks.** `scan_comment_unit_flags()` matches on
   comment text, and the text does not change when a row is corrected, so it
   still reports 8 groups including the 33 rows C001 already fixed. Only the
   offsets half of `unit_anomaly_report` is a shrinking to-do list (5 down to 4);
   the comment half is not, and it will mislead. Fix is one filter on
   `unit_correction_id`. Cheap, do it before it costs someone an hour.
3. **C001's `evidence` cell** records only the comment. Add the five-species
   cross-validation from 9d; it is the stronger argument and it is the one that
   belongs in the methods.
4. **5 failing tests in `test-fct_node_cards.R`**, pre-existing, from the compact
   card work of 2026-08-05: `title_size` 2.6 against headline 3.7, and the
   compact strip drawing an x axis the test expects blank. Not caused by the
   units work; unpicked.
5. **9c anomalies 2 and 3, and the Coteur transcription fix.** Unchanged.

#### Then the actual AEP

**P3.4 and P4.2 are what is left, and they are both Sam's judgement, not
infrastructure.** Every node carries only `plausibility_score`, so three of the
four EPEQ chips render grey on every card, and all 16 edges are still `putative`.
The machinery to draw a fully scored, empirically-supported AEP has existed since
2026-08-05 and is waiting on the scoring.

That is worth stating plainly, because two full days have now gone into data
faults found while looking at the AEP rather than into the AEP itself. The faults
were real and worth fixing, and the correction layer will keep paying off. But
the deadline is bought with P3.4 and P4.2, not with more detection.

### The decisions file was not tracked at all

Found while re-running after the scaffold. `group_decisions` and `group_ids`
took their paths as literal strings, so targets hashed the **command** and never
the file. **Editing `group_decisions.csv` by hand invalidated nothing.**
Confirmed: six rows appended, `tar_outdated()` reported `(none)`.

Same class as the missing `imports = "STOPAEP"` (P1.1e) and the same symptom,
work that appears done and silently is not. It would have bitten hardest in
Phase 2, where a day of judgement goes into that file and every downstream figure
needs to see it. Both paths are now `format = "file"` targets.

### Open, and Sam's call

- ~~**Panel (b) still has no threshold class axis.**~~ **Added 2026-08-05** on
  Sam's call: "just print the numerals even if they collide for now." They do
  collide where boundaries are close, and that is accepted. Revisit at
  figure-preparation time (P5.4), not before.
- **Panel (e) resolution is mostly a data problem.** Limits shared per
  compartment span 12.3 orders; per compartment *and unit* would be 8.3 for
  sediment mg/kg (dry). But **99% of sediment rows sit within 2.8 orders** and
  the rest of the span is two tails of bad data: the Coteur unit error at the
  bottom and the 270,000 mg/kg ore-grade value at the top. Chasing those two
  beats any scale change, and they distort means and threshold comparisons as
  well as this panel. Splitting limits by unit also costs the dry-vs-wet
  comparison that panel (a) exists for.
- **Coteur 2003: confirmed by Sam against the paper**, which reports μg g⁻¹ dry
  weight. The source file records `μg/kg (dry)`, so this one is a transcription
  fix in `data/raw/eData/`, not a pipeline fix, and it is **still outstanding**.
  See section 9b for the separate pipeline fault it led to.
- **Molar units are excluded, and this is a standing decision (2026-08-05).**
  42 rows carry `µM` and are dropped. They *are* convertible (1 µM Cu is
  63.55 µg/L), so this is a choice rather than a limitation: a molar measurement
  is not obviously comparable with the total/dissolved mix already in the data,
  and 42 rows do not justify the methods-section paragraph that defending the
  conversion would need. **Revisit only if a chosen AEP node depends on them.**
  If the exclusion survives to submission it belongs in the methods as one
  sentence, since it is a deliberate exclusion rather than an absence.

### Nice to have, not before submission

- **Correlation matrices per group** (`docs/groups/aquatic-sediment.qmd`, G013).
  Agreed 2026-08-05 to defer: it is a new analysis rather than a fix to an
  existing panel, and nothing in Phase 3 or 4 depends on it.

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

### Rationalise target names

Noted 2026-08-06. Full audit and mapping in **`PLAN-target-naming.md`**; nothing applied.

All 100 targets in `_targets.R` audited against a `<source>_<entity>_<state>` scheme. The load-bearing problems are that the literature eData reads carry no source prefix at all (`sites_data` beside `vm_edata_sites`, 22 targets), and that `aep_node_cards` (a table) and `node_cards` (the PNGs) are one word apart. Tier 1 is 25 targets and removes real ambiguity; Tier 2 is another 12 and is aesthetic.

**Cost: 2-3 hours plus a full rebuild**, because `targets` keys the store on the name, so every rename is a cache miss. Blast radius outside `_targets.R` is \~35 `tar_read()` call sites in notebooks (each one a dependency declaration, `CLAUDE.md` 4.4.2) and 9 scripts. Run it in one commit at a point where the store is being rebuilt anyway, never incrementally: half-renamed is worse than either end state.

Three decisions wait on Sam (`lit_` vs `literature_`; Tier 2 at all; whether `load_literature_pqt` should be folded away rather than renamed).

### Integrate the emissions and REACH data into the pipeline

Deferred 2026-07-30, deliberately and with a reason, after Sam asked whether to
switch to this before finishing the concentration work. **His instinct to stay
with concentrations was right**, and the earlier steer towards building this
infrastructure now overstated what a complete AEP actually requires.

Current state: neither `docs/NBXX-REACH.qmd` nor `docs/NBXX-norske-utslipp.qmd`
is in the pipeline. Both are parked, and all the Excel reading happens inside the
`.qmd`. Bringing them in properly means the two TODOs sitting in the utslipp
notebook right now:

```
# Match emissions to fylke/site coordinates
# Translate medium to ENVIRON_COMPARTMENT
```

The second is the compartment mismatch: emission media do not map cleanly onto
`ENVIRON_COMPARTMENT_SUB`, and reconciling them is a judgement call per medium.

**Why later is strictly better here.** Doing it now means mapping *every*
medium. Doing it after the AEP exists means mapping the two or three the AEP
actually uses. Same argument as the coverage tiers in P2.3: ranking orders the
work, it does not decide where to stop.

The thing that unblocks the first AEP is P3.6, which needs no pipeline work at
all: the WoE assessments are already written as prose tables, and the AEP node
and edge files are hand-edited CSVs. Transcription, not integration.

Revisit once the AEP structure has proven itself and it is clear which media
matter.

### Split the species-name cache from the curation layer

Noted 2026-07-30 while the reasoning was fresh. **An important pattern, not an
important task right now.** Roughly 30-45 minutes including tests, and the
current state works (94 of 128 species named, 99% of biota rows by volume).

`data/clean/species_common_names_cache.csv` currently does two jobs with
different lifecycles: it stores what the API said (reproducible, disposable,
regenerable) *and* it is the only place a hand correction could live
(irreplaceable). Because they share one file, the cache cannot safely be deleted
to force a refresh, which is the opposite of what a cache is for. Hand edits
survive today only because `get_common_names()` never re-queries a species
already present, i.e. by accident rather than by design.

The pattern, if and when it is worth doing:

1. **Machine cache**, append-only, never hand-edited, safe to delete. One row per
   (query, source) **including negative results**, so a species with genuinely no
   English name is not re-queried forever and "never asked" stays distinguishable
   from "asked, no answer".
2. **Override file**, hand-written, never machine-written, committed, with a
   `reason` column so a decision is reviewable later and defensible in the
   methods.
3. **One resolver** owning the precedence `override > source[1] > source[2] > NA`,
   emitting a `name_source` column.

Plus the piece that actually closes the loop: a **curation-todo target** listing
unresolved species by data volume, *with the candidate names the APIs returned*.
New data repopulates it automatically; the negative cache stops it re-proposing
what has already been rejected. Same shape as `group_decisions.csv`: pipeline
proposes, human decides in a CSV, pipeline reads and never writes.

Two traps worth recording:

- **Name resolution and attribute lookup are different problems.** `Eukronia
  hamata` in the data is a misspelling of `Eukrohnia hamata`, which resolves
  fine. Fuzzy name resolution belongs upstream, with its own overrides;
  otherwise every typo looks like a missing common name and the same organism
  gets curated twice.
- **Not everything unnamed is a species.** `Bunndyr` (Norwegian for benthic
  fauna) and `Zooplankton epilimnion` should fail validation on the way in, not
  sit in a curation queue. Wants a `not_a_species` bucket.

Rejected alternatives: a database (loses reviewable diffs, which is the whole
point at this scale); `memoise`/`cachem` (opaque binary, cannot be a curation
surface); the `targets` store (disposable by design, so human decisions must not
live there).

**One exception worth doing if the pipeline is genuinely costing you time:** cut the number of rendered notebooks. Roughly 34 `.qmd` files currently render on every full build, of which \~20 are unreachable from the sidebar anyway. Phase 0.2 plus Phase 2.1 removes 14 of them on their own. Render cost is the main pipeline slowness; repo size is mostly not.

Also worth an hour at some point, but not before Friday: `load_literature_pqt` is a pass-through whose comment claims a dependency on a `save_literature_pqt` target that is not actually referenced, which is very likely the cause of the "doesn't properly update" note at `_targets.R:1038`.

------------------------------------------------------------------------

## 11. Open questions

1.  ~~**P0.2**: delete or re-enable the outlier factory?~~ **Answered 2026-07-29: delete.** See Phase 0.
2.  ~~**When exactly is the holiday?**~~ **Answered: Mon 17 – Tue 25 Aug**, laptop available but costed at zero. Real dates now pinned throughout.
3.  ~~**Is `n >= 30` the right triage cutoff?**~~ **Answered: start at `n >= 100`.** Revisit the 30-100 band only if Week 6 buffer survives.
4.  ~~`docs/_planning.qmd` contract-end vs deadline?~~ **Answered: 2026-09-14 governs.** `docs/_planning.qmd` is hopelessly out of date and should be either deleted or replaced with a pointer to this file (it is still linked from the `_quarto.yml` sidebar, so it renders into the site as-is).
5.  **P1.0 scope:** confirm that dropping `NA`/`0` means the measured value column only, not a whole-row `drop_na()`. Proceeding on the narrow reading.