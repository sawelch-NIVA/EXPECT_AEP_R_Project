run_pipeline <- function(
  render_quarto = TRUE,
  deploy = FALSE,
  names = NULL,
  callr_function = NULL
) {
  # Setup Pushover credentials ----
  pushoverr::set_pushover_user(user = Sys.getenv("PUSHOVER_USER"))
  pushoverr::set_pushover_app(token = Sys.getenv("PUSHOVER_APP"))

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
          callr_function = callr_function
        )
      } else if (deploy) {
        # Run only deployment targets
        targets::tar_make(
          names = starts_with("deploy_"),
          callr_function = callr_function
        )
      } else {
        # Run everything except render and deploy targets
        targets::tar_make(
          names = !starts_with("render_") & !starts_with("deploy_"),
          callr_function = callr_function
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
