# Source targets
source("_targets.R")
# Run the pipeline in a fresh R session
tar_make(
  callr_function = callr::r,
  names = !matches("deploy_posit_connect_cloud") # don't deploy
)
