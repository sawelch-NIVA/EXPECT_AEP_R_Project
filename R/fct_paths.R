# Project-root-anchored paths that stay portable across machines.
#
# The problem, open since 2019 (r-lib/here#36): here::here() always returns an
# ABSOLUTE path. Anything targets records verbatim then becomes machine-specific
# -- the `path` column of tar_meta for a `format = "file"` target, and the source
# and output paths a tar_quarto() target stores. Move the repo, or open it on a
# second machine, and every such target invalidates because the recorded path no
# longer matches, even though the file is byte-identical.
#
# here has no relative-path mode and is not going to grow one. fs::path_rel()
# composed on top is the accepted workaround.
#
# Verified on this repo 2026-07-31: the existing `format = "file"` targets happen
# to record relative paths already, because get_literature_csv_paths() and
# write_triage_plots() build them with plain relative strings rather than here().
# There were no absolute paths in tar_meta. So this is a guard against the next
# file target, not a repair of a live break.

#' Project-Relative Path
#'
#' [here::here()] with the answer expressed relative to the working directory,
#' so it is safe to store in `tar_meta` or hand to a `format = "file"` target.
#'
#' Anchoring is unchanged: resolution still goes through here, so
#' `here::i_am("Readme.md")` in `_targets.R` still defines the root. Only the
#' representation differs.
#'
#' Relative to *the working directory*, which under `tar_make()` is the project
#' root. That is also the right answer when the working directory is somewhere
#' else: called during a Quarto render rooted in `docs/`, `here_rel("data/x")`
#' gives `"../data/x"`, which resolves correctly from there.
#'
#' Returns a plain character vector, not the `fs_path` that [fs::path_rel()]
#' hands back. targets accepts `fs_path` (it inherits from character) but hashes
#' it differently, so a bare character keeps target invalidation predictable.
#'
#' @param ... Path components, passed to [here::here()].
#' @return A character vector of paths relative to the working directory.
#' @examples
#' \dontrun{
#' here_rel("data/clean/group_decisions.csv")
#' #> "data/clean/group_decisions.csv"      (not "C:/Users/.../data/clean/...")
#' }
#' @export
here_rel <- function(...) {
  as.character(fs::path_rel(here::here(...)))
}
