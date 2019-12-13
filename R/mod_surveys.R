# Module UI
  
#' @title   mod_import_surveys_ui and mod_import_surveys_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal

#' @rdname mod_surveys
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_surveys_ui <- function(id){
  ns <- NS(id)
  
  uiOutput(ns("ui"))

}
    
# Module Server
    
#' @rdname mod_surveys
#' @export
#' @keywords internal
mod_surveys_server <- function(input, output, session, rv){
  ns <- session$ns
  
  output$ui <- renderUI({
    
    config_limesurvey <- rv$dt_config %>% 
      split(f = .$key, x = .$value)
    
    try <- tryCatch(
      limer::get_session_key(),
      error = function(e) e
    )
    
    if (
      !all(
        isTRUE(nchar(config_limesurvey[["lime_api"]]) >= 1),
        isTRUE(nchar(config_limesurvey[["lime_username"]]) >= 1),
        isTRUE(nchar(config_limesurvey[["lime_password"]]) >= 1),
        !"error" %in% class(try)
      )
    ) {
      
      tagList(
        box(
          title = "Limesurvey configuration", width = 12,
          mod_config_limesurvey_ui("config_limesurvey_ui_1")
        )
      )
      
    } else {
      
      tagList(
        box(
          title = "Surveys", width = 12,
          uiOutput(ns("surveys_ui"))
        ),
        box(
          title = "Attributes",
          DT::DTOutput(ns("dt_attributes"))
        )
      )
      
    }
    
  })
  
  list_surveys <- reactive({
    
    limer::call_limer("list_surveys") %>% 
      dplyr::select(survey_id = sid, survey_title = surveyls_title, expires)
    
  })
  
  output$picker_surveys <- renderUI({
    
    list_surveys_update <- list_surveys() %>% 
      dplyr::anti_join(rv$dt_surveys, by = "survey_id")
    
    shinyWidgets::pickerInput(
      ns("selected_survey_picker"), 
      label = NULL, 
      choices = list_surveys_update$survey_title, 
      multiple = TRUE,
      options = list(
        "showTick" = TRUE, 
        "actions-box" = TRUE,
        "title" = "No survey selected"
      ),
      choicesOpt = list(
        subtext = paste(
          "sid", 
          list_surveys_update$survey_id,
          sep = ": "
        )
      )
    )
  })
  
  observeEvent(input$import_surveys, {

    add_surveys <- list_surveys() %>%
      dplyr::anti_join(rv$dt_surveys, by = "survey_id") %>% 
      dplyr::filter(survey_title %in% input[["selected_survey_picker"]])

    if (nrow(add_surveys)) {
      
      rv$dt_surveys <- dplyr::bind_rows(rv$dt_surveys, add_surveys)
      
      impexp::sqlite_export(
        golem::get_golem_options("sqlite_base"), 
        rv$dt_surveys, 
        table_name = "surveys", 
        overwrite = TRUE
      )
      
      domain_limesurvey <- impexp::sqlite_import(
        golem::get_golem_options("sqlite_base"),
        "config"
      ) %>% 
        dplyr::filter(key == "lime_api") %>% 
        dplyr::pull(value) %>% 
        stringr::str_remove("admin/remotecontrol$")
      
      add_participants <- limer::get_participants(add_surveys$survey_id, all_attributes = TRUE) %>% 
        dplyr::select(-email) %>% 
        dplyr::mutate(
          surveyurl = glue::glue("{domain_limesurvey}{survey_id}?token={token}"),
          optouturl = glue::glue("{domain_limesurvey}optout/tokens/{survey_id}?token={token}")
        )
      
      rv$dt_participants <- dplyr::bind_rows(rv$dt_participants, add_participants)
      
      impexp::sqlite_export(
        golem::get_golem_options("sqlite_base"), 
        rv$dt_participants, 
        table_name = "participants", 
        overwrite = TRUE
      )
      
      rv$dt_participants_attributes <- dplyr::tibble(
        survey_id = add_surveys$survey_id,
        attribute = purrr::map(add_surveys$survey_id, ~ names(limer::get_attributes_descriptions(.))),
        description = purrr::map(add_surveys$survey_id, limer::get_attributes_descriptions)
      ) %>% 
        tidyr::unnest(c(attribute, description)) %>% 
        dplyr::bind_rows(
          rv$dt_participants_attributes %>% 
            tidyr::separate_rows(survey_id, sep = ";")
        ) %>% 
        dplyr::group_by(attribute, description) %>% 
        dplyr::summarise_at("survey_id", ~ paste(unique(.), collapse = ";")) %>% 
        dplyr::ungroup()
      
      impexp::sqlite_export(
        golem::get_golem_options("sqlite_base"), 
        rv$dt_participants_attributes, 
        table_name = "participants_attributes", 
        overwrite = TRUE
      )
      
      rv$dt_participants_attributes <- rv$dt_participants_attributes %>% 
        dplyr::mutate(num_attribute = as.integer(stringr::str_match(attribute, "ATTRIBUTE_(\\d+)")[, 2])) %>% 
        dplyr::arrange(num_attribute)
      
    }
    
  })

  output$dt_surveys <- DT::renderDT({
    
    data <- rv$dt_surveys
    
    if (nrow(data) >= 1) {
      
      data <- data %>% 
        dplyr::left_join(
          rv$dt_participants %>% 
            dplyr::count(survey_id) %>% 
            dplyr::rename(participants = n),
          by = "survey_id"
        ) %>% 
        dplyr::left_join(
          impexp::r_import(golem::get_golem_options("cron_file")) %>% 
            dplyr::group_by(survey_id) %>% 
            dplyr::summarise_at(c("optout", "completed"), sum) %>% 
            dplyr::ungroup(),
          by = "survey_id"
        ) %>% 
        dplyr::mutate(pct_completed = scales::percent(completed / (participants - optout), accuracy = 0.1, suffix = "\u202F%"))
      
    }
    
    data %>% 
      DT::datatable(
        options = list(
        dom = "rt",
        columnDefs = list(list(className = 'dt-right', targets = 6))
      ), 
      rownames = FALSE
    )
    
    
  })
  
  observeEvent(input$remove, {

    if (!is.null(input[["dt_surveys_rows_selected"]])) {

      selected_survey_id <- rv$dt_surveys$survey_id[as.numeric(input[["dt_surveys_rows_selected"]])]

      impexp::sqlite_execute_sql(
        golem::get_golem_options("sqlite_base"),
        glue::glue("DELETE FROM surveys WHERE survey_id IN ({paste0(selected_survey_id, collapse = ", ")});")
      )

      rv$dt_surveys <- impexp::sqlite_import(
        golem::get_golem_options("sqlite_base"), 
        "surveys"
      )
      
      rv$dt_participants_attributes <- rv$dt_participants_attributes %>% 
        tidyr::separate_rows(survey_id, sep = ";") %>% 
        dplyr::filter(!survey_id %in% !!selected_survey_id)
      
      impexp::sqlite_export(
        golem::get_golem_options("sqlite_base"),
        rv$dt_participants_attributes,
        "participants_attributes",
        overwrite = TRUE
      )
      
      rv$dt_participants <- rv$dt_participants %>% 
        dplyr::filter(!survey_id %in% !!selected_survey_id) %>% 
        dplyr::select(survey_id, tid, firstname, lastname, token, rv$dt_participants_attributes$description)
      
      impexp::sqlite_export(
        golem::get_golem_options("sqlite_base"),
        rv$dt_participants,
        "participants",
        overwrite = TRUE
      )

    }

  })

  output$surveys_ui <- renderUI({
  
    tagList(
      div(style="display: inline-block; vertical-align:top;", 
          uiOutput(ns("picker_surveys"))
      ),
      div(style="display: inline-block; vertical-align:top;", 
          actionButton(ns("import_surveys"), "Import selected surveys")
      ),
      DT::dataTableOutput(ns("dt_surveys")),
      div("", style = "height: 10px;"),
      actionButton(ns("remove"), "Remove selected surveys")
    )
    
  })
  
  output$dt_attributes <- DT::renderDT({
    
    rv$dt_participants_attributes %>% 
      DT::datatable(
        options = list(
          dom = "rtp"
        ), 
        rownames = FALSE
      )
    
  })
  
  return(rv)

}
