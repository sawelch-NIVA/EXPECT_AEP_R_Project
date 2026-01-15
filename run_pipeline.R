run_pipeline <- function(
  destroy_all = FALSE,
  render_quarto = TRUE,
  deploy = FALSE,
  names = NULL,
  reporter = NULL, # FIXME: this doesn't work rn
  callr_function = NULL
) {
  # Setup Pushover credentials ----
  pushoverr::set_pushover_user(user = Sys.getenv("PUSHOVER_USER"))
  pushoverr::set_pushover_app(token = Sys.getenv("PUSHOVER_APP"))
  if (destroy_all) {
    targets::tar_destroy("all", ask = FALSE)
  }

  tryCatch(
    {
      # Determine targets to run ----
      if (!is.null(names)) {
        # If names is specified, use it directly and ignore render_quarto/deploy
        if (render_quarto || !deploy) {
          warning_msg <- "names argument provided: ignoring render_quarto and deploy arguments"
          warning(warning_msg)
          pushoverr::pushover_high(
            message = warning_msg,
            title = "Pipeline Warning"
          )
        }
        targets::tar_make(names = names, callr_function = callr_function)
      } else if (render_quarto && deploy) {
        # Run all targets
        targets::tar_make(callr_function = callr_function)
      } else if (render_quarto) {
        # Run only rendering targets
        targets::tar_make(
          names = starts_with("render_"),
          callr_function = callr_function,
          reporter = reporter
        )
      } else if (deploy) {
        # Run only deployment targets
        targets::tar_make(
          names = starts_with("deploy_"),
          callr_function = callr_function,
          reporter = reporter
        )
      } else {
        # Run everything except render and deploy targets
        targets::tar_make(
          names = !starts_with("render_") & !starts_with("deploy_"),
          callr_function = callr_function,
          reporter = reporter
        )
      }

      # Success notification ----
      pushoverr::pushover_high(
        message = "Pipeline completed successfully! ✓",
        title = "Pipeline Success"
      )
    },
    error = function(e) {
      print(e$message)

      # Failure notification ----
      pushoverr::pushover_high(
        message = stringr::str_sub(
          paste(
            "Pipeline failed with error:",
            e$message
          ),
          start = 1,
          end = 1000
        ),
        title = "Pipeline Failed"
      )
      stop(e) # Re-throw the error
    }
  )
}

#  preferred: don't start from scratch, don't render documents, don't update website
run_pipeline(
  destroy_all = FALSE,
  render_quarto = FALSE,
  deploy = FALSE,
  reporter = "terse"
)

# # Reporters:
# "balanced": a reporter that balances efficiency with informative detail. Uses a cli progress bar instead of printing messages for individual dynamic branches. To the right of the progress bar is a text string like "22.6s, 4510+, 124-" (22.6 seconds elapsed, 4510 targets completed successfully so far, 124 targets skipped so far).

# For best results with the balanced reporter, you may need to adjust your cli settings. See global options cli.num_colors and cli.dynamic at https://cli.r-lib.org/reference/cli-config.html. On that page is also the CLI_TICK_TIME environment variable which controls the time delay between progress bar updates. If the delay is too low, then overhead from printing to the console may slow down the pipeline.

# "terse": like the "balanced" reporter, but without a progress bar.

# "silent": print nothing.

# "timestamp": same as the "verbose" reporter except that each message begins with a time stamp.

# "verbose": print messages for individual targets as they dispatch or complete. Each individual target-specific time (e.g. "3.487 seconds") is strictly the elapsed runtime of the target and does not include steps like data retrieval and output storage.
