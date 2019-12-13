# Module UI
  
#' @title   mod_participants_ui and mod_participants_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param rv internal
#' @param get_filter_surveys internal
#'
#' @rdname mod_participants
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_participants_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      shinydashboardPlus::boxPlus(
        title = "Participants", width = 12,
        column(
          12, DT::dataTableOutput(ns("dt_participants"))
        ),
        enable_sidebar = TRUE,
        sidebar_icon = "columns",
        sidebar_title = "Add columns",
        sidebar_width = 30,
        sidebar_content = uiOutput(ns("select_attributes"))
      ),
      box(
        title = "Contacts", width = 7,
        rhandsontable::rHandsontableOutput(ns("hot_participants_contacts"))
      ),
      box(
        title = "Events", width = 5,
        DT::dataTableOutput(ns("dt_participants_events"))
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_participants
#' @export
#' @keywords internal
    
mod_participants_server <- function(input, output, session, rv){
  ns <- session$ns
  
  output$dt_participants <- DT::renderDT({
    
    rv$dt_participants_filter() %>% 
      patchr::rename(
        rv$dt_participants_attributes %>% 
          dplyr::mutate(column = patchr::str_normalise_colnames(description)) %>% 
          dplyr::select(column, rename = description),
        drop = FALSE
      ) %>% 
      dplyr::select(token, firstname, lastname, optout, completed, input[["picker_select_attributes"]]) %>% 
      DT::datatable(
        rownames = FALSE,
        selection = list(mode = "single", selected = 1),
        options = list(
          dom = "rftip",
          scrollY = '40vh',
          scrollX = '100%'
        )
      )

  })
  
  proxy <- DT::dataTableProxy("dt_participants")
  
  observeEvent(input$dt_participants_search, ignoreInit = TRUE, {
    
    if (!is.null(input$dt_participants_rows_current)) {
      
      DT::selectRows(proxy, input$dt_participants_rows_current[1])
      
    }
    
  })
  
  output$select_attributes <- renderUI({
    
    shinyWidgets::pickerInput(
      ns("picker_select_attributes"),
      label = "Additional fields",
      choices = rv$dt_participants_attributes$description,
      multiple = TRUE,
      options = list("showTick" = TRUE,
                     "actions-box" = TRUE,
                     "dropdown-align-right" = TRUE),
      choicesOpt = list(
        subtext = paste("- ", rv$dt_participants_attributes$attribute))
    )
    
  })
  
  rv$dt_participants_contacts_filter <- reactive({
    
    rv$dt_participants_contacts %>%
      dplyr::rename(date_source = date) %>% 
      dplyr::left_join(
        rv$dt_email_validation %>%
          dplyr::mutate(key = "email") %>% 
          dplyr::select(key, value = email, service, status, status_date = date),
        by = c("key", "value")
      ) %>% 
      dplyr::semi_join(
        rv$dt_participants_filter() %>%
          dplyr::filter(dplyr::row_number() == input[["dt_participants_rows_selected"]]),
        by = "token"
      )

  })
  
  output$hot_participants_contacts <- rhandsontable::renderRHandsontable({
    
    if (!is.null(rv[["dt_participants_contacts"]]) & !is.null(input[["dt_participants_rows_selected"]])) {
      
      rv$dt_participants_contacts_filter() %>% 
        dplyr::select(-token) %>% 
        rhandsontable::rhandsontable(rowHeaders = NULL, height = 233) %>%
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") %>%
        rhandsontable::hot_rows(rowHeights = 35) %>%
        rhandsontable::hot_cols(valign = "htMiddle")
      
    }
    
  })
  
  observeEvent(input$hot_participants_contacts, {
    
    if (!is.null(input$hot_participants_contacts)) {
      
      changes <- input$hot_participants_contacts$changes
      
      # remove-add | update
      if (!is.null(changes[["ind"]]) | !is.null(changes[["changes"]])) {
        
        update_contacts <- input$hot_participants_contacts %>% 
          rhandsontable::hot_to_r() %>% 
          dplyr::as_tibble() %>% 
          dplyr::mutate(token = rv$dt_participants_contacts_filter()$token[1]) %>% 
          dplyr::select(token, key, value, source, date = date_source)
        
        sqlite_contacts <- rv$dt_participants_contacts_filter() %>% 
          dplyr::select(token, key, value, source, date = date_source)
        
        if (!isTRUE(all.equal(update_contacts, sqlite_contacts))) {
          
          if (input$hot_participants_contacts$changes$event == "afterCreateRow") {
            update_contacts$token[changes$ind + 1] <- rv$dt_participants_contacts_filter()$token[1]
          }
          
          impexp::sqlite_execute_sql(
            golem::get_golem_options("sqlite_base"),
            paste0('DELETE FROM participants_contacts WHERE token = "', update_contacts$token[1], '";')
          )
          
          impexp::sqlite_append_rows(
            golem::get_golem_options("sqlite_base"),
            update_contacts,
            "participants_contacts"
          )
          
          rv$dt_participants_contacts <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "participants_contacts")
          
        }
        
        update_email_validation <- input$hot_participants_contacts %>% 
          rhandsontable::hot_to_r() %>% 
          dplyr::as_tibble() %>% 
          dplyr::mutate(token = rv$dt_participants_contacts_filter()$token[1]) %>% 
          dplyr::filter(key == "email") %>% 
          dplyr::select(email = value, service, status, date = status_date)
        
        sqlite_email_validation <- rv$dt_email_validation %>% 
          dplyr::semi_join(update_email_validation, by = "email")
        
        if (!isTRUE(all.equal(update_email_validation, sqlite_email_validation))) {
          
          if (input$hot_participants_contacts$changes$event == "afterCreateRow") {
            update_email_validation$token[changes$ind + 1] <- rv$dt_participants_contacts_filter()$token[1]
          }
          
          impexp::sqlite_execute_sql(
            golem::get_golem_options("sqlite_base"),
            glue::glue("DELETE FROM email_validation WHERE email = \"{update_email_validation$email}\";")
          )
          
          impexp::sqlite_append_rows(
            golem::get_golem_options("sqlite_base"),
            update_email_validation,
            "email_validation"
          )
          
          rv$dt_email_validation <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "email_validation")
          
        }
        
      }

    }
    
  })

  output$dt_participants_events <- DT::renderDT({
    
    if (!is.null(input[["dt_participants_rows_selected"]])) {
      
      impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "participants_events") %>% 
        dplyr::semi_join(
          rv$dt_participants_contacts_filter(),
          by = "token"
        ) %>% 
        dplyr::arrange(date) %>% 
        dplyr::select(-token) %>% 
        DT::datatable(
          rownames = FALSE,
          selection = list(mode = "none"),
          options = list(
            dom = "rt",
            scrollY = '20vh',
            scrollX = '100%'
          )
        )
      
    }
    
  })

  return(rv)
  
}
