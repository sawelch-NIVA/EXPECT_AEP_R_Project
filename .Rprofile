# source("renv/activate.R")
library(targets)

# crew workers are fresh R processes that cannot library(STOPAEP), because
# pkgload::load_all() attaches the package without ever installing it. Load it
# from source instead. Guarded on the worker command line so ordinary sessions
# (and the tar_make() process itself, which load_all()s in _targets.R) are
# unaffected. See targets.qmd section 2 for why the other two candidate fixes
# do not work.
if (any(grepl("crew::crew_worker", commandArgs(), fixed = TRUE))) {
  pkgload::load_all(quiet = TRUE)
}
