# Module UI
  
#' @title   mod_config_limesurvey_ui and mod_config_limesurvey_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_config_limesurvey
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_config_limesurvey_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("input_text_lime_api")),
    uiOutput(ns("input_text_lime_username")),
    uiOutput(ns("input_text_lime_password")),
    actionButton(ns("save_lime_config"), "Save Limesurvey config", icon = icon("save"))
  )
}
    
# Module Server
    
#' @rdname mod_config_limesurvey
#' @export
#' @keywords internal
    
mod_config_limesurvey_server <- function(input, output, session, rv){
  ns <- session$ns
  
  output$input_text_lime_api <- renderUI({
    
    value <- rv$df_config %>% 
      dplyr::filter(.data$key == "lime_api") %>% 
      dplyr::pull(value)
    
    options(lime_api = value)
    
    textInput(
      ns("lime_api"), "Limesurvey API", value = value,
      placeholder = "https://{my_server}/index.php/admin/remotecontrol"
    )
    
  })
  
  output$input_text_lime_username <- renderUI({
    
    value <- rv$df_config %>% 
      dplyr::filter(.data$key == "lime_username") %>% 
      dplyr::pull(value)
    
    options(lime_username = value)
    
    textInput(
      ns("lime_username"),
      "Limesurvey username",
      value = value,
      placeholder = "username"
      )
    
  })
  
  output$input_text_lime_password <- renderUI({
    
    value <- rv$df_config %>% 
      dplyr::filter(.data$key == "lime_password") %>% 
      dplyr::pull(value)
    
    options(lime_password = value)
    
    textInput(
      ns("lime_password"),
      "Limesurvey password",
      value = value,
      placeholder = "password"
    )
    
  })
  
  observeEvent(input$save_lime_config, {
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("UPDATE config SET value = \"{input$lime_api}\" WHERE key = \"lime_api\";")
    )
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("UPDATE config SET value = \"{input$lime_username}\" WHERE key = \"lime_username\";")
    )
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("UPDATE config SET value = \"{input$lime_password}\" WHERE key = \"lime_password\";")
    )
    
    rv$df_config <- dplyr::tibble(
      key = c("lime_api", "lime_username", "lime_password"),
      value = c(input$lime_api, input$lime_username, input$lime_password)
    ) %>% 
      patchr::anti_join_bind(
        rv$df_config,
        by= "key"
      )
    
    options(lime_api = input$lime_api)
    options(lime_username = input$lime_username)
    options(lime_password = input$lime_password)
    
    try <- tryCatch(
      limer::get_session_key(),
      error = function(e) e
    )
    
    if ("error" %in% class(try)) {
      shinyalert::shinyalert(type = "error", text = "The limesurvey connection cannot be opened.")
    } else {
      release <- limer::release_session_key()
    }
    
  })
  
}
