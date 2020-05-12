#' Run the Shiny Application
#' 
#' @param sqlite_base SQLite base path
#' @param cron_responses CRON responses rda path
#' @param credentials User tibble
#' 
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(sqlite_base, cron_responses, credentials) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui(), 
      server = app_server
    ), 
    golem_opts = list(sqlite_base = sqlite_base, cron_responses = cron_responses, credentials = credentials)
  )
}
