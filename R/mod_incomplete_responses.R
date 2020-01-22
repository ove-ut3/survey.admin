# Module UI
  
#' @title   mod_incomplete_responses_ui and mod_incomplete_responses_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_incomplete_responses
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_incomplete_responses_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      width = 12,
      DT::DTOutput(ns("dt_incomplete_responses"))
    ),
    box(
      actionButton(ns("set_complete"), "Set as complete")
    )
  )
}
    
# Module Server
    
#' @rdname mod_incomplete_responses
#' @export
#' @keywords internal
    
mod_incomplete_responses_server <- function(input, output, session, rv){
  ns <- session$ns
  
  rv$df_incomplete_responses <- reactive({
    
    rv$df_participants_filter() %>% 
      tidyr::drop_na(lastpage_rate) %>% 
      dplyr::arrange(dplyr::desc(lastpage_rate)) %>% 
      dplyr::select(survey_id, token, firstname, lastname, datestamp, lastpage, group_order, lastpage_rate)
    
  })
  
  output$dt_incomplete_responses <- DT::renderDT({
    
    req(rv$df_participants_filter)
    
    rv$df_incomplete_responses() %>% 
      DT::datatable(
        rownames = FALSE
      ) %>%
      DT::formatPercentage("lastpage_rate", 1)

  })
  
  observeEvent(input$set_complete, {
    
    req(input$dt_incomplete_responses_rows_selected)
    
    set_complete_token <- rv$df_incomplete_responses() %>% 
      dplyr::filter(dplyr::row_number() %in% input$dt_incomplete_responses_rows_selected)
    
    set_finished_almost_complete(
      sqlite_base = golem::get_golem_options("sqlite_base"),
      cron_responses = golem::get_golem_options("cron_responses"), 
      almost_complete_group = c("123459" = 16, "123458" = 16, "123456" = 16),
      token = set_complete_token$token
    )
    
  })
  
  
}
