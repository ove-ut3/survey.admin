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
  
  output$select_attributes <- renderUI({
    
    selected <- rv$df_config %>% 
      dplyr::filter(key == "participants_dt_attributes") %>% 
      tidyr::separate_rows(value, sep = ";") %>% 
      dplyr::pull(value)
    
    shinyWidgets::pickerInput(
      ns("picker_select_attributes"),
      label = "Additional fields",
      choices = rv$df_participants_attributes$description,
      selected = selected,
      multiple = TRUE,
      options = list(
        "showTick" = TRUE,
        "actions-box" = TRUE,
        "dropdown-align-right" = TRUE
      ),
      choicesOpt = list(
        subtext = paste("- ", rv$df_participants_attributes$attribute))
    )
    
  })
  
  output$dt_participants <- DT::renderDT({
    
    rv$df_participants_filter() %>% 
      patchr::rename(
        rv$df_participants_attributes %>% 
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
  
  observeEvent(input$picker_select_attributes, ignoreNULL = FALSE, ignoreInit = TRUE, {
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("UPDATE config SET value = \"{paste0(input$picker_select_attributes, collapse = ';')}\" WHERE key = \"participants_dt_attributes\";")
    )
    
    rv$df_config <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "config"
    )
    
  })
  
  observeEvent(input$dt_participants_search, ignoreInit = TRUE, {
    
    if (!is.null(input$dt_participants_rows_current)) {
      
      DT::selectRows(proxy, input$dt_participants_rows_current[1])
      
    }
    
  })
  
  rv$df_participants_contacts_filter <- reactive({
    
    rv$df_participants_contacts %>%
      dplyr::semi_join(
        rv$df_participants_filter() %>%
          dplyr::filter(dplyr::row_number() == input[["dt_participants_rows_selected"]]),
        by = "token"
      )

  })
  
  output$hot_participants_contacts <- rhandsontable::renderRHandsontable({
    
    req(
      rv$df_participants_contacts,
      input[["dt_participants_rows_selected"]]
    )
    
    rv$df_participants_contacts_filter() %>% 
      dplyr::select(-token) %>% 
      rhandsontable::rhandsontable(rowHeaders = NULL, height = 233) %>%
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") %>%
      rhandsontable::hot_rows(rowHeights = 35) %>%
      rhandsontable::hot_cols(valign = "htMiddle")

  })
  
  observeEvent(input$hot_participants_contacts, {
    
    req(input$hot_participants_contacts)
    
    changes <- input$hot_participants_contacts$changes
    
    # remove-add | update
    req(!is.null(changes[["ind"]]) | !is.null(changes[["changes"]]))
    
    sqlite_contacts <- rv$df_participants_contacts_filter()
    
    update_contacts <- input$hot_participants_contacts %>% 
      rhandsontable::hot_to_r() %>% 
      dplyr::as_tibble() %>% 
      dplyr::mutate(token = rv$df_participants_contacts_filter()$token[1]) %>% 
      dplyr::select(names(sqlite_contacts))

    if (!isTRUE(all.equal(update_contacts, sqlite_contacts))) {
      
      if (input$hot_participants_contacts$changes$event == "afterCreateRow") {
        update_contacts$token[changes$ind + 1] <- rv$df_participants_contacts_filter()$token[1]
      }
      
      impexp::sqlite_execute_sql(
        golem::get_golem_options("sqlite_base"),
        glue::glue("DELETE FROM participants_contacts WHERE token = \"{update_contacts$token[1]}\";")
      )
      
      impexp::sqlite_append_rows(
        golem::get_golem_options("sqlite_base"),
        update_contacts,
        "participants_contacts"
      )
      
      rv$df_participants_contacts <- impexp::sqlite_import(
        golem::get_golem_options("sqlite_base"),
        "participants_contacts"
      )
      
    }
    
  })

  output$dt_participants_events <- DT::renderDT({
    
    req(input[["dt_participants_rows_selected"]])
    
    impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "participants_events") %>% 
      dplyr::semi_join(
        rv$df_participants_contacts_filter(),
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

  })

}
