# One-off: wrap the hand-written areas of the ALREADY-EXISTING
# docs/groups/*.qmd files in callouts (2026-08-08), matching what
# group_section_markdown()/notebook_header_markdown() now do for freshly
# generated content -- see their headers in R/fct_group_notebooks.R. Only
# needed once, for files written before the callout change; new
# files/sections get it automatically from the generator from here on.
#
#   Rscript scripts/wrap_group_qmd_callouts.R

files <- list.files("docs/groups", pattern = "\\.qmd$", full.names = TRUE)

wrap_verdicts <- function(lines) {
  verdict_idx <- grep("^\\*\\*Verdict:\\*\\*", lines)
  heading_idx <- grep("^## ", lines)
  out <- character(0)
  cursor <- 1
  for (v in verdict_idx) {
    # End of this verdict block: just before the next "## " heading, or EOF.
    next_heading <- heading_idx[heading_idx > v]
    end <- if (length(next_heading) > 0) min(next_heading) - 1 else length(lines)
    # Trim trailing blank lines so ":::" sits right after the content.
    while (end > v && lines[end] == "") end <- end - 1

    out <- c(out, lines[cursor:(v - 1)], "::: {.callout-note}", lines[v:end], ":::", "")
    cursor <- end + 1
    while (cursor <= length(lines) && lines[cursor] == "") cursor <- cursor + 1
  }
  c(out, if (cursor <= length(lines)) lines[cursor:length(lines)] else character(0))
}

wrap_comparison <- function(lines) {
  start <- grep("^# Comparison$", lines)
  end_marker <- grep("^# Groups$", lines)
  if (length(start) == 0 || length(end_marker) == 0) {
    return(lines)
  }
  start <- start[1]
  end <- end_marker[1] - 1
  while (end > start && lines[end] == "") end <- end - 1
  body_start <- start + 1
  while (body_start <= end && lines[body_start] == "") body_start <- body_start + 1
  if (body_start > end) {
    return(lines)
  }
  c(
    lines[seq_len(start)], "",
    "::: {.callout-note}", lines[body_start:end], ":::", "",
    lines[end_marker[1]:length(lines)]
  )
}

for (path in files) {
  lines <- readr::read_lines(path)
  lines <- wrap_comparison(lines)
  lines <- wrap_verdicts(lines)
  readr::write_lines(lines, path)
}

for (path in files) {
  lines <- readr::read_lines(path)
  v <- grep("^\\*\\*Verdict:\\*\\*", lines)
  bad <- v[lines[v - 1] != "::: {.callout-note}"]
  if (length(bad) > 0) cat("PROBLEM in", path, "at lines", bad, "\n")
}
message("Done wrapping callouts.")
