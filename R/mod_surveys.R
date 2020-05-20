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
    
    config_limesurvey <- rv$df_config %>% 
      split(f = .$key, x = .$value)
    
    try <- tryCatch(
      limer::get_session_key(),
      error = function(e) e
    )
    
    if (
      !all(
        isTRUE(nchar(config_limesurvey[["lime_api"]]) >= 1),
        isTRUE(nchar(config_limesurvey[["lime_username"]]) >= 1),
        isTRUE(nchar(config_limesurvey[["lime_password"]]) >= 1)
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
    
    list_surveys <- limer::call_limer("list_surveys")
    
    if (!is.data.frame(list_surveys)) {
      
      list_surveys <- dplyr::tibble(
        sid = character(0),
        surveyls_title = character(0),
        expires = character(0)
      )
      
    }
      
    list_surveys %>% 
      dplyr::select(survey_id = .data$sid, survey_title = .data$surveyls_title, .data$expires)
    
  })
  
  output$picker_surveys <- renderUI({
    
    list_surveys_update <- list_surveys() %>% 
      dplyr::anti_join(rv$df_surveys, by = "survey_id")
    
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
      dplyr::anti_join(rv$df_surveys, by = "survey_id") %>% 
      dplyr::filter(.data$survey_title %in% input[["selected_survey_picker"]])

    if (nrow(add_surveys) >= 1) {
      
      rv$df_surveys <- dplyr::bind_rows(
        rv$df_surveys, 
        add_surveys
      )
      
      impexp::sqlite_export(
        golem::get_golem_options("sqlite_base"), 
        rv$df_surveys, 
        table_name = "surveys", 
        overwrite = TRUE
      )
      
      domain_limesurvey <- impexp::sqlite_import(
        golem::get_golem_options("sqlite_base"),
        "config"
      ) %>% 
        dplyr::filter(.data$key == "lime_api") %>% 
        dplyr::pull(.data$value) %>% 
        stringr::str_remove("admin/remotecontrol$")
      
      add_participants <- limer::get_participants(
        add_surveys$survey_id,
        all_attributes = TRUE
      ) %>% 
        dplyr::select(-.data$email) %>% 
        dplyr::mutate(
          surveyurl = glue::glue("{domain_limesurvey}{survey_id}?token={token}") %>% 
            as.character(),
          optouturl = glue::glue("{domain_limesurvey}optout/tokens/{survey_id}?token={token}") %>% 
            as.character()
        )
      
      rv$df_participants <- dplyr::bind_rows(
        rv$df_participants,
        add_participants
      )
      
      impexp::sqlite_export(
        golem::get_golem_options("sqlite_base"), 
        rv$df_participants, 
        table_name = "participants", 
        overwrite = TRUE
      )
      
      rv$df_participants_attributes <- dplyr::tibble(
        survey_id = add_surveys$survey_id,
        attribute = purrr::map(add_surveys$survey_id, ~ names(limer::get_attributes_descriptions(.))),
        description = purrr::map(add_surveys$survey_id, limer::get_attributes_descriptions)
      ) %>% 
        tidyr::unnest(c(.data$attribute, .data$description)) %>% 
        dplyr::bind_rows(
          rv$df_participants_attributes %>% 
            tidyr::separate_rows(.data$survey_id, sep = ";") %>% 
            dplyr::mutate_at("survey_id",as.character)
        ) %>% 
        dplyr::group_by(.data$attribute, .data$description) %>% 
        dplyr::summarise_at("survey_id", ~ paste(unique(.), collapse = ";")) %>% 
        dplyr::ungroup()
      
      impexp::sqlite_export(
        golem::get_golem_options("sqlite_base"), 
        rv$df_participants_attributes, 
        table_name = "participants_attributes", 
        overwrite = TRUE
      )
      
      rv$df_participants_attributes <- rv$df_participants_attributes %>% 
        dplyr::mutate(num_attribute = as.integer(stringr::str_match(.data$attribute, "ATTRIBUTE_(\\d+)")[, 2])) %>% 
        dplyr::arrange(.data$num_attribute)
      
      cron_responses(operation = "add")
      
    }
    
  })

  output$dt_surveys <- DT::renderDT({
    
    data <- rv$df_surveys
    
    if (nrow(data) >= 1) {
      
      if (Sys.info()[["sysname"]] == "Windows" & !file.exists(golem::get_golem_options("cron_responses"))) {
        
        survey.admin::cron_responses_rda(
          sqlite_base = golem::get_golem_options("sqlite_base"),
          output_file = golem::get_golem_options("cron_responses"),
          session = FALSE
        )
        
      }
      
      data <- data %>% 
        dplyr::left_join(
          rv$df_participants %>% 
            dplyr::count(.data$survey_id) %>% 
            dplyr::rename(participants = .data$n),
          by = "survey_id") %>% 
          dplyr::left_join(
            impexp::r_import(golem::get_golem_options("cron_responses")) %>% 
              dplyr::group_by(.data$survey_id) %>% 
              dplyr::summarise_at(c("optout", "completed"), sum) %>% 
              dplyr::ungroup(),
            by = "survey_id"
          ) %>% 
          dplyr::mutate(pct_completed = scales::percent(.data$completed / (.data$participants - .data$optout), accuracy = 0.1, suffix = "\u202F%"))

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

    req(input$dt_surveys_rows_selected)
    
    selected_survey_id <- rv$df_surveys$survey_id[as.numeric(input[["dt_surveys_rows_selected"]])]

    paste_survey_id <- paste(selected_survey_id, collapse = "\", \"")
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("DELETE FROM surveys WHERE survey_id IN (\"{paste_survey_id}\");")
    )

    rv$df_surveys <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"), 
      "surveys"
    )
    
    if (nrow(rv$df_surveys) == 0) {
      cron_responses(operation = "remove")
    }
    
    rv$df_participants_attributes <- rv$df_participants_attributes %>% 
      tidyr::separate_rows(.data$survey_id, sep = ";") %>% 
      dplyr::filter(!.data$survey_id %in% !!selected_survey_id)
    
    impexp::sqlite_export(
      golem::get_golem_options("sqlite_base"),
      rv$df_participants_attributes,
      "participants_attributes",
      overwrite = TRUE
    )
    
    rv$df_participants <- rv$df_participants %>% 
      dplyr::filter(!.data$survey_id %in% !!selected_survey_id) %>% 
      dplyr::select(.data$survey_id, .data$tid, .data$firstname, .data$lastname, .data$token, rv$df_participants_attributes$description)
    
    impexp::sqlite_export(
      golem::get_golem_options("sqlite_base"),
      rv$df_participants,
      "participants",
      overwrite = TRUE
    )

  })

  output$surveys_ui <- renderUI({
  
    tagList(
      div(
        style = "display: inline-block; vertical-align:top;", 
        uiOutput(ns("picker_surveys"))
      ),
      div(
        style = "display: inline-block; vertical-align:top;", 
        actionButton(
          ns("import_surveys"),
          "Import selected surveys"
        )
      ),
      DT::dataTableOutput(ns("dt_surveys")),
      div("", style = "height: 10px;"),
      actionButton(
        ns("remove"),
        "Remove selected surveys"
      )
    )
    
  })
  
  output$dt_attributes <- DT::renderDT({
    
    rv$df_participants_attributes %>% 
      DT::datatable(
        options = list(
          dom = "rtp"
        ),
        rownames = FALSE
      )
    
  })

}
