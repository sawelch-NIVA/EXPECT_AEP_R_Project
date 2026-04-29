# From https://books.ropensci.org/targets/performance.html
results <- profvis::profvis(
  targets::tar_make(
    callr_function = NULL, # Do not run the pipline behind a callr::r() process.
    use_crew = FALSE, # Disable parallel computing with crew (optional)
    as_job = FALSE # Do not run the pipeline in a Posit Workbench / RStudio background job.
  )
)
print(results, aggregate = TRUE) # aggregate = TRUE is crucial for interpretable flame graphs.
