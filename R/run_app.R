#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(sqlite_base, cron_file, credentials) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui(), 
      server = app_server,
      onStart = function() {
        onStop(function() {
          cat("Releasing limer session key\n")
          release <- limer::release_session_key()
        })
      }
    ), 
    golem_opts = list(sqlite_base = sqlite_base, cron_file = cron_file, credentials = credentials)
  )
}
