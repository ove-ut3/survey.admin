# Module UI
  
#' @title   mod_linkedin_ui and mod_linkedin_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_linkedin
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_linkedin_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        fluidRow(
            shinydashboardPlus::boxPlus(
            title = "Participants", width = 12,
            column(
              12, DT::DTOutput(ns("dt_participants"))
            ),
            enable_sidebar = TRUE,
            sidebar_icon = "columns",
            sidebar_title = "Add columns",
            sidebar_width = 30,
            sidebar_content = uiOutput(ns("select_attributes"))
          )
        )
      ),
      column(
        width = 6,
        fluidRow(
          box(
            width = 12,
            div(
              style = "display: inline-block;",
              uiOutput(ns("ui_search_button"))
            ),
            div(
              style = "display: inline-block;",
              uiOutput(ns("ui_search_suffix"))
            )
          ),
          box(
          title = "Invitation text", width = 12,
          selectInput(
            ns("invitation_text_language"),
            label = "Select language",
            choices = c("fr", "en")
          ),
          br(),
          uiOutput(ns("ui_invitation_text")),
          uiOutput(ns("ui_clipbutton_invitation_text"))
          )
        )
      ),
      column(
        width = 6,
        fluidRow(
          box(
            title = "Survey text", width = 12,
            selectInput(
              ns("survey_text_language"),
              label = "Select language",
              choices = c("fr", "en")
            ),
            br(),
            uiOutput(ns("ui_survey_text")),
            uiOutput(ns("ui_clipbutton_survey_text"))
          )
        )
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_linkedin
#' @export
#' @keywords internal
    
mod_linkedin_server <- function(input, output, session, rv){
  ns <- session$ns
  
  output$select_attributes <- renderUI({
    
    selected <- rv$df_linkedin %>% 
      dplyr::filter(key == "dt_attributes") %>% 
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
      dplyr::select(token, firstname, lastname, optout, completed, input[["picker_select_attributes"]]) %>%
      DT::datatable(
        selection = list(mode = 'single', selected = 1),
        rownames = FALSE,
        options = list(
          pageLength = -1,
          dom = 'rft',
          scrollY = '40vh'
        )
      )
    
  })
  
  observeEvent(input$picker_select_attributes, ignoreNULL = FALSE, ignoreInit = TRUE, {
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("UPDATE linkedin SET value = \"{paste0(input$picker_select_attributes, collapse = ';')}\" WHERE key = \"dt_attributes\";")
    )
    
    rv$df_linkedin <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "linkedin")
    
  })
  
  proxy <- DT::dataTableProxy("dt_participants")
  
  observeEvent(input$dt_participants_search, ignoreInit = TRUE, {
    
    req(input$dt_participants_rows_current)
      
    DT::selectRows(proxy, input$dt_participants_rows_current[1])
    
  })
  
  output$ui_search_button <- renderUI({
    
    req(input$dt_participants_rows_selected)
    
    linkedin <- rv$df_participants_filter() %>%
      dplyr::filter(dplyr::row_number() == input$dt_participants_rows_selected)
    
    rv$linkedin_search_suffix_text <- rv$df_linkedin %>%
      dplyr::filter(key == "search_text_input") %>% 
      dplyr::pull(value)
    
    actionButton(
      ns("button_search"),
      "Search",
      icon = icon("search"),
      onclick = paste0("window.open('", paste0("https://www.linkedin.com/search/results/all/?keywords=", linkedin$firstname, "%20", linkedin$lastname, "%20", input$search_suffix_text), "', '_blank')")
    )
    
  })
  
  output$ui_search_suffix <- renderUI({
    
    req(input$dt_participants_rows_selected)
    
    rv$linkedin_search_suffix_text <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "linkedin"
    ) %>% 
      dplyr::filter(key == "search_text_input") %>% 
      dplyr::pull(value)
    
    textInput(
      ns("search_suffix_text"),
      label = NULL,
      value = isolate(rv$linkedin_search_suffix_text),
      placeholder = "Search suffix (for example, a city to filter results)"
      
    )

  })
  
  observeEvent(input$dt_participants_rows_selected, {
    
    req(input$dt_participants_rows_selected)
    
    if (!is.null(input$search_suffix_text)) {
      
      updateTextInput(
        session,
        "search_suffix_text",
        value = rv$linkedin_search_suffix_text
      )
      
    }
    
    if (!is.null(input$invitation_text)) {
      
      updateTextAreaInput(
        session,
        "invitation_text",
        value = rv$linkedin_invitation_text
      )
      
    }
    
    if (!is.null(input$survey_text)) {
      
      updateTextAreaInput(
        session,
        "survey_text",
        value = rv$linkedin_survey_text
      )
      
    }
    
  })
  
  observeEvent(input$search_suffix_text, {

    req(input$search_suffix_text != rv$linkedin_search_suffix_text)

    rv$linkedin_search_suffix_text <- input$search_suffix_text
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("UPDATE linkedin SET value = \"{input$search_suffix_text}\" WHERE key = \"search_text_input\";")
    )

  })
  
  output$ui_invitation_text <- renderUI({
    
    req(
      input$dt_participants_rows_selected,
      input$invitation_text_language
    )
    
    rv$linkedin_invitation_text <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "linkedin"
    ) %>% 
      dplyr::filter(key == glue::glue("invitation_text_{input$invitation_text_language}")) %>% 
      dplyr::pull(value) %>% 
      stringr::str_replace_all("''", "'")

    tagList(
      column(
        width = 10,
        textAreaInput(
          ns("invitation_text"),
          label = NULL, 
          height = "150px", 
          value = isolate(rv$linkedin_invitation_text), 
          placeholder = "My invitation text"
        ),
        div(
          style = "display: inline-block; vertical-align: top;",
          fileInput(ns("import_invitation_text"), label = NULL, buttonLabel = "Import invitation text")
        ),
        div(
          style = "display: inline-block; vertical-align: top;",
          downloadButton(ns("export_invitation_text"), "Export invitation text", icon = icon("file-export"))
        )
      )
    )
    
  })
  
  output$ui_clipbutton_invitation_text <- renderUI({
    
    req(
      input$dt_participants_rows_selected,
      input$invitation_text
    )
    
    df_linkedin_fiter <- dplyr::filter(rv$df_participants_filter(), dplyr::row_number() == input$dt_participants_rows_selected)
    
    clipButton_text <- input$invitation_text %>%
      survey.admin::escape_space_glue(
        rv$df_participants_attributes %>% 
          tidyr::separate_rows(survey_id, sep = ";") %>% 
          dplyr::filter(survey_id == df_linkedin_fiter$survey_id)
      )
    
    try <- tryCatch(
      glue::glue_data(
        clipButton_text,
        .x = df_linkedin_fiter
      ),
      error = function(e) e
    )
    
    if ("error" %in% class(try)) {
      clipButton_text <- NULL
    } else {
      clipButton_text <- iconv(try, from = "UTF-8")
    }
    
    tagList(
      column(
        width = 2,
        rclipboard::rclipButton(ns("copy_invitation_text"), HTML("Copy<br/>invitation<br/>text"), clipButton_text, icon("clipboard"))
      )
    )
    
  })
  
  observeEvent(input$invitation_text, {
    
    req(input$invitation_text != rv$linkedin_invitation_text)
    
    rv$linkedin_invitation_text <- input$invitation_text
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("UPDATE linkedin SET value = \"{input$invitation_text}\" WHERE key = \"invitation_text_{input$invitation_text_language}\";")
    )

  })
  
  observeEvent(input$import_invitation_text, {
    
    req(
      input$import_invitation_text,
      input$invitation_text_language
    )
    
    invitation_text <- input$import_invitation_text$datapath %>% 
      readLines(encoding = "UTF-8") %>% 
      paste(collapse = "\n")
    
    rv$linkedin_invitation_text <- invitation_text
    
    updateTextAreaInput(
      session,
      "invitation_text",
      value = invitation_text
    )
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("UPDATE linkedin SET value = \"{invitation_text}\" WHERE key = \"invitation_text_{input$invitation_text_language}\";")
    )
    
  })
  
  output$export_invitation_text <- downloadHandler(
    filename = function() {
      "invitation_text.txt"
    },
    content = function(con) {
      input$invitation_text %>% 
        writeLines(con, useBytes = TRUE)
    }
  )
  
  output$ui_survey_text <- renderUI({
    
    req(
      input$dt_participants_rows_selected,
      input$survey_text_language
    )
     
    rv$linkedin_survey_text <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "linkedin"
    ) %>% 
      dplyr::filter(key == glue::glue("survey_text_{input$survey_text_language}")) %>% 
      dplyr::pull(value) %>% 
      stringr::str_replace_all("''", "'")
    
    tagList(
      column(
        width = 10,
        textAreaInput(
          ns("survey_text"),
          label = NULL,
          height = "250px",
          value = isolate(rv$linkedin_survey_text),
          placeholder = "My survey text"
        ),
        div(
          style = "display: inline-block; vertical-align: top;",
          fileInput(ns("import_survey_text"), label = NULL, buttonLabel = "Import survey text")
        ),
        div(
          style = "display: inline-block; vertical-align: top;",
          downloadButton(ns("export_survey_text"), "Export survey text", icon = icon("file-export"))
        )
      )
    )
    
  })
  
  output$ui_clipbutton_survey_text <- renderUI({
    
    req(
      input$dt_participants_rows_selected,
      input$survey_text
    )
    
    df_linkedin_fiter <- dplyr::filter(rv$df_participants_filter(), dplyr::row_number() == input$dt_participants_rows_selected)
    
    clipButton_text <- input$survey_text %>%
      survey.admin::escape_space_glue(
        rv$df_participants_attributes %>% 
          tidyr::separate_rows(survey_id, sep = ";") %>% 
          dplyr::filter(survey_id == df_linkedin_fiter$survey_id)
      )

    try <- tryCatch(
      glue::glue_data(
        clipButton_text,
        .x = df_linkedin_fiter
      ),
      error = function(e) e
    )

    if ("error" %in% class(try)) {
      clipButton_text <- NULL
    } else {
      clipButton_text <- iconv(try, from = "UTF-8")
    }
    
    tagList(
      column(
        width = 2,
        rclipboard::rclipButton(ns("copy_survey_text"), HTML("Copy<br/>survey<br/>text"), clipButton_text, icon("clipboard"))
      )
    )
    
  })
  
  observeEvent(input$survey_text, {
    
    req(input$survey_text != rv$linkedin_survey_text)
    
    rv$linkedin_survey_text <- input$survey_text
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("UPDATE linkedin SET value = \"{input$survey_text}\" WHERE key = \"survey_text_{input$survey_text_language}\";")
    )
    
  })
  
  observeEvent(input$import_survey_text, {
    
    req(
      input$import_survey_text,
      input$survey_text_language
    )
    
    survey_text <- input$import_survey_text$datapath %>% 
      readLines(encoding = "UTF-8") %>% 
      paste(collapse = "\n")
    
    rv$linkedin_survey_text <- survey_text
    
    updateTextAreaInput(
      session,
      "survey_text",
      value = survey_text
    )
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("UPDATE linkedin SET value = \"{survey_text}\" WHERE key = \"survey_text_{input$survey_text_language}\";")
    )
    
  })
  
  output$export_survey_text <- downloadHandler(
    filename = function() {
      "survey_text.txt"
    },
    content = function(con) {
      input$survey_text %>% 
        writeLines(con, useBytes = TRUE)
    }
  )
  
}
