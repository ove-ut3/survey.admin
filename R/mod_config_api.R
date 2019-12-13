# Module UI
  
#' @title   mod_config_api_ui and mod_config_api_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_config_api
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_config_api_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("input_text_api_key_bulkemailchecker")),
    uiOutput(ns("input_text_api_key_listflow")),
    uiOutput(ns("input_text_api_key_quickemailverification")),
    uiOutput(ns("input_text_api_key_emailmarker")),
    uiOutput(ns("input_text_api_key_spothit")),
    actionButton(ns("save_api_config"), "Save API config", icon = icon("save"))
  )
}
    
# Module Server
    
#' @rdname mod_config_api
#' @export
#' @keywords internal
    
mod_config_api_server <- function(input, output, session, rv){
  ns <- session$ns
  
  output$input_text_api_key_bulkemailchecker <- renderUI({
    
    value <- rv$dt_config %>% 
      dplyr::filter(key == "api_key_bulkemailchecker") %>% 
      dplyr::pull(value)
    
    textInput(ns("api_key_bulkemailchecker"), "API key bulkemailchecker.com", value = value)

  })
  
  output$input_text_api_key_listflow <- renderUI({
    
    value <- rv$dt_config %>% 
      dplyr::filter(key == "api_key_listflow") %>% 
      dplyr::pull(value)
    
    textInput(ns("api_key_listflow"), "API key listflow.io", value = value)
    
  })
  
  output$input_text_api_key_quickemailverification <- renderUI({
    
    value <- rv$dt_config %>% 
      dplyr::filter(key == "api_key_quickemailverification") %>% 
      dplyr::pull(value)
    
    
    textInput(ns("api_key_quickemailverification"), "API key quickemailverification.com", value = value)
    
  })
  
  output$input_text_api_key_emailmarker <- renderUI({
    
    value <- rv$dt_config %>% 
      dplyr::filter(key == "api_key_emailmarker") %>% 
      dplyr::pull(value)
    
    textInput(ns("api_key_emailmarker"), "API key emailmarker.com", value = value)
    
  })
  
  output$input_text_api_key_spothit <- renderUI({
    
    value <- rv$dt_config %>% 
      dplyr::filter(key == "api_key_spothit") %>% 
      dplyr::pull(value)
    
    textInput(ns("api_key_spothit"), "API key spot-hit.fr", value = value)
    
  })
  
  observeEvent(input$save_api_config, {
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      paste0('UPDATE config SET value = "', input$api_key_bulkemailchecker,'" WHERE key = "api_key_bulkemailchecker";')
    )
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      paste0('UPDATE config SET value = "', input$api_key_listflow,'" WHERE key = "api_key_listflow";')
    )
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      paste0('UPDATE config SET value = "', input$api_key_quickemailverification,'" WHERE key = "api_key_quickemailverification";')
    )
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      paste0('UPDATE config SET value = "', input$api_key_emailmarker,'" WHERE key = "api_key_emailmarker";')
    )
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      paste0('UPDATE config SET value = "', input$api_key_spothit,'" WHERE key = "api_key_spothit";')
    )
    
    rv$dt_config$value[which(rv$dt_config$key == "api_key_bulkemailchecker")] <- input$api_key_bulkemailchecker
    rv$dt_config$value[which(rv$dt_config$key == "api_key_listflow")] <- input$api_key_listflow
    rv$dt_config$value[which(rv$dt_config$key == "api_key_quickemailverification")] <- input$api_key_quickemailverification
    rv$dt_config$value[which(rv$dt_config$key == "api_key_emailmarker")] <- input$api_key_emailmarker
    rv$dt_config$value[which(rv$dt_config$key == "api_key_spothit")] <- input$api_key_spothit
    
  })

}
