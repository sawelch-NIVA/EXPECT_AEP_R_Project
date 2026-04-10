run_pipeline <- function(
  destroy_all = FALSE,
  render_quarto = TRUE,
  deploy = FALSE,
  names = NULL,
  reporter = "balanced",
  load_workspace_on_error = FALSE
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
        targets::tar_make(
          names = names,
          reporter = reporter
        )
      } else if (render_quarto && deploy) {
        # Run all targets
        targets::tar_make(
          reporter = reporter
        )
      } else if (render_quarto) {
        # Run only rendering targets
        targets::tar_make(
          names = starts_with("render_"),
          reporter = reporter
        )
      } else if (deploy) {
        # Run only deployment targets
        targets::tar_make(
          names = starts_with("deploy_"),
          reporter = reporter
        )
      } else {
        # Run everything except render and deploy targets
        targets::tar_make(
          names = !starts_with("render_") & !starts_with("deploy_"),
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

      # Load workspace for failed target ----
      if (load_workspace_on_error) {
        failed_targets <- targets::tar_meta(fields = error) |>
          dplyr::filter(!is.na(error)) |>
          dplyr::pull(name)

        if (length(failed_targets) > 0) {
          message("Loading workspace for failed target: ", failed_targets[1])
          targets::tar_workspace(failed_targets[1])
        }
      }

      # Failure notification ----
      pushoverr::pushover_high(
        # trim really long errors to avoid surpassing pushoverr's limit
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


#  preferred
run_pipeline(
  destroy_all = FALSE, # start from scratch?
  render_quarto = FALSE, # render documents?
  deploy = FALSE, # update website?
  names = NULL, # run specific parts of pipeline?
  reporter = "balanced", # reasonable amount of metadata
  load_workspace_on_error = FALSE # load workspace for the failing target
)
