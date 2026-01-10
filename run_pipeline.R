run_pipeline <- function(
  render_quarto = TRUE,
  deploy = TRUE,
  callr_function = NULL
) {
  pushoverr::set_pushover_user(user = Sys.getenv("PUSHOVER_USER"))
  pushoverr::set_pushover_app(token = Sys.getenv("PUSHOVER_APP"))
  tryCatch(
    {
      if (render_quarto && deploy) {
        targets::tar_make(callr_function = callr_function)
      } else if (render_quarto) {
        targets::tar_make(
          names = starts_with("render_"),
          callr_function = callr_function
        )
      } else if (deploy) {
        targets::tar_make(
          names = starts_with("deploy_"),
          callr_function = callr_function
        )
      } else {
        targets::tar_make(
          names = !starts_with("render_") & !starts_with("deploy_"),
          callr_function = callr_function
        )
      }

      # # Success notification
      pushoverr::pushover_high(
        message = "Pipeline completed successfully! ✓",
        title = "Pipeline Success"
      )
    },
    error = function(e) {
      print(e$message)
      # Failure notification
      pushoverr::pushover_high(
        message = paste(
          "Pipeline failed with error:",
          substr(e$message, start = 1, stop = 1000)
        ),
        title = "Pipeline Failed"
      )
      stop(e) # Re-throw the error
    }
  )
}
