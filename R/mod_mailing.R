# Module UI

#' @title   mod_mailing_ui and mod_mailing_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_mailing
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_mailing_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Email list", width = 4,
        uiOutput(ns("ui_select_max_emails")),
        numericInput(
          ns("select_latency_days"),
          "Minimum days before last email contact",
          value = 7,
          min = 0,
          step = 1
        ),
        DT::DTOutput(ns("dt_emails"))
      ),
      column(
        width = 8,
        box(
          title = "Mail template", width = 12,
          div(
            style = "display: inline-block; width: 49%;",
            uiOutput(ns("input_text_sender_email"))
          ),
          div(
            style = "display: inline-block; width: 50%;",
            uiOutput(ns("input_text_sender_alias"))
          ),
          uiOutput(ns("input_text_mail_subject")),
          uiOutput(ns("input_textarea_mail_body")),
          div(
            style = "display: inline-block; vertical-align: top;",
            fileInput(
              ns("import_mail"),
              label = NULL,
              buttonLabel = "Import mail configuration"
            )
          ),
          div(
            style = "display: inline-block; vertical-align: top;",
            downloadButton(
              ns("export_mail"),
              "Export mail configuration",
              icon = icon("file-export")
            )
          )
        ),
        box(
          title = "Send emails", width = 12,
          div(
            style = "display: inline-block; width: 50%;",
            numericInput(
              ns("mailing_sleep"),
              "Sleep time in seconds between each mail",
              value = 7,
              min = 0
            )
          ),
          div(
            style = "display: inline-block;",
            actionButton(
              ns("send_email"),
              "Send mails to selected emails",
              icon = icon("paper-plane")
            )
          )
          
        )
      )
    )
  )
}

# Module Server

#' @rdname mod_mailing
#' @export
#' @keywords internal

mod_mailing_server <- function(input, output, session, rv){
  ns <- session$ns
  
  output$ui_select_max_emails <- renderUI({
    
    max <- rv$df_participants_filter() %>% 
      dplyr::left_join(
        rv$df_participants_contacts %>% 
          dplyr::filter(
            .data$key == "email",
            .data$status %in% c("valid", "unknown")
          ) %>% 
          dplyr::select(.data$token, email = .data$value, .data$date) %>% 
          dplyr::mutate_at("date", lubridate::as_date),
        by = "token"
      ) %>% 
      tidyr::drop_na(.data$email) %>% 
      dplyr::count(.data$token) %>% 
      dplyr::pull(.data$n) %>% 
      max()
    
    if (max == -Inf) { # 0 emails
      
      selectInput(
        ns("select_max_email_per_token"), 
        "Maximum number of mails sent per token", 
        choices = 0
      )
      
    } else {
      
      selectInput(
        ns("select_max_email_per_token"), 
        "Maximum number of mails sent per token", 
        choices = 1:max,
        selected = 1
      )
      
    }
    
  })
  
  df_mailing_list <- reactive({
    
    req(
      input$select_max_email_per_token,
      input$select_latency_days
    )
    
    df_phoning_team_events <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "phoning_team_events"
    )
    
    rv$df_participants_filter() %>% 
      dplyr::left_join(
        rv$df_participants_contacts %>% 
          dplyr::filter(
            .data$key == "email",
            .data$status %in% c("valid", "unknown")
          ) %>% 
          dplyr::select(.data$token, email = .data$value, .data$date) %>% 
          dplyr::mutate_at("date", lubridate::as_date),
        by = "token"
      ) %>% 
      tidyr::drop_na(.data$email) %>% 
      dplyr::arrange(.data$token, dplyr::desc(.data$date)) %>% 
      dplyr::group_by(.data$token) %>% 
      dplyr::filter(dplyr::row_number() <= as.integer(input$select_max_email_per_token)) %>% 
      dplyr::ungroup() %>% 
      dplyr::anti_join(
        dplyr::bind_rows(rv$df_participants_events, df_phoning_team_events) %>% 
          dplyr::filter(.data$type %in% c("general mailing", "email")) %>% 
          dplyr::mutate(diff = lubridate::today() - lubridate::ymd(.data$date)) %>% 
          dplyr::filter(diff < input$select_latency_days),
        by = "token"
      )
    
  })
  
  output$dt_emails <- DT::renderDT({
    
    df_mailing_list() %>% 
      dplyr::select(.data$token, .data$email) %>% 
      DT::datatable(
        rownames = FALSE,
        options = list(
          dom = "rti",
          scrollY = '60vh',
          pageLength = -1
        )
      )
    
  })
  
  output$input_text_sender_email <- renderUI({
    
    rv$mailing_sender_email <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "mail_template"
    ) %>% 
      dplyr::filter(.data$key == "sender_email") %>% 
      dplyr::pull(.data$value)
    
    textInput(
      ns("sender_email"),
      label = "Sendind email",
      value = isolate(rv$mailing_sender_email),
      placeholder = "sender@domain.tld"
    )
    
  })
  
  observeEvent(input$sender_email, {
    
    req(input$sender_email != rv$mailing_sender_email)
    
    rv$mailing_sender_email <- input$sender_email
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("DELETE FROM mail_template WHERE key = \"sender_email\";")
    )
    
    impexp::sqlite_append_rows(
      golem::get_golem_options("sqlite_base"),
      dplyr::tibble(
        key = "sender_email",
        value = input$sender_email
      ),
      "mail_template"
    )
    
  })
  
  output$input_text_sender_alias <- renderUI({
    
    rv$mailing_sender_alias <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "mail_template"
    ) %>% 
      dplyr::filter(.data$key == "sender_alias") %>% 
      dplyr::pull(.data$value)
    
    textInput(
      ns("sender_alias"),
      label = "Sending alias",
      value = isolate(rv$mailing_sender_alias),
      placeholder = "Sender name"
    )
    
  })
  
  observeEvent(input$sender_alias, {
    
    req(input$sender_alias != rv$mailing_sender_alias)
    
    rv$mailing_sender_alias <- input$sender_alias
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("DELETE FROM mail_template WHERE key = \"sender_alias\";")
    )
    
    impexp::sqlite_append_rows(
      golem::get_golem_options("sqlite_base"),
      dplyr::tibble(
        key = "sender_alias",
        value = input$sender_alias
      ),
      "mail_template"
    )
    
  })
  
  output$input_text_mail_subject <- renderUI({
    
    rv$mailing_subject <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "mail_template"
    ) %>% 
      dplyr::filter(.data$key == "subject") %>% 
      dplyr::pull(.data$value)
    
    textInput(
      ns("mail_subject"),
      label = "Subject",
      value = isolate(rv$mailing_subject),
      placeholder = "My mail subject"
    )
    
  })
  
  observeEvent(input$mail_subject, {
    
    req(input$mail_subject != rv$mailing_subject)
    
    rv$mailing_subject <- input$mail_subject
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("DELETE FROM mail_template WHERE key = \"subject\";")
    )
    
    impexp::sqlite_append_rows(
      golem::get_golem_options("sqlite_base"),
      dplyr::tibble(
        key = "subject",
        value = input$mail_subject
      ),
      "mail_template"
    )
    
  })
  
  output$input_textarea_mail_body <- renderUI({
    
    rv$mailing_body <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "mail_template"
    ) %>% 
      dplyr::filter(.data$key == "body") %>% 
      dplyr::pull(.data$value)
    
    textAreaInput(
      ns("mail_body"),
      label = "Body",
      height = "370px",
      value = isolate(rv$mailing_body),
      placeholder = "My mail body"
    )
    
  })
  
  observeEvent(input$mail_body, {
    
    req(input$mail_body != rv$mailing_body)
    
    rv$mailing_body <- input$mail_body
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("DELETE FROM mail_template WHERE key = \"body\";")
    )
    
    impexp::sqlite_append_rows(
      golem::get_golem_options("sqlite_base"),
      dplyr::tibble(
        key = "body",
        value = input$mail_body
      ),
      "mail_template"
    )
    
  })
  
  observeEvent(input$import_mail, {
    
    req(input$import_mail)
    
    mail_template <- jsonlite::fromJSON(input$import_mail$datapath)
    
    # sender email
    updateTextInput(
      session,
      "sender_email",
      value = mail_template$sender_email
    )
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("DELETE FROM mail_template WHERE key = \"sender_email\";")
    )
    
    impexp::sqlite_append_rows(
      golem::get_golem_options("sqlite_base"),
      dplyr::tibble(
        key = "sender_email",
        value = input$sender_email
      ),
      "mail_template"
    )
    
    # sender alias
    updateTextInput(
      session,
      "sender_alias",
      value = mail_template$sender_alias
    )
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("DELETE FROM mail_template WHERE key = \"sender_alias\";")
    )
    
    impexp::sqlite_append_rows(
      golem::get_golem_options("sqlite_base"),
      dplyr::tibble(
        key = "sender_alias",
        value = input$sender_alias
      ),
      "mail_template"
    )
    
    # mail subject
    updateTextInput(
      session,
      "mail_subject",
      value = mail_template$subject
    )
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("DELETE FROM mail_template WHERE key = \"subject\";")
    )
    
    impexp::sqlite_append_rows(
      golem::get_golem_options("sqlite_base"),
      dplyr::tibble(
        key = "subject",
        value = input$mail_subject
      ),
      "mail_template"
    )
    
    # mail body
    updateTextAreaInput(
      session,
      "mail_body",
      value = mail_template$body
    )
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("DELETE FROM mail_template WHERE key = \"body\";")
    )
    
    impexp::sqlite_append_rows(
      golem::get_golem_options("sqlite_base"),
      dplyr::tibble(
        key = "body",
        value = input$mail_body
      ),
      "mail_template"
    )
    
  })
  
  output$export_mail <- downloadHandler(
    filename = function() {
      "mail_template.json"
    },
    content = function(con) {
      list(
        "sender_email" = input$sender_email,
        "sender_alias" = input$sender_alias,
        "subject" = input$mail_subject,
        "body" = input$mail_body
      ) %>% 
        jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE) %>% 
        writeLines(con, useBytes = TRUE)
    }
  )
  
  observeEvent(input$send_email, {
    
    if (nchar(input$sender_email) == 0) {
      shinyalert::shinyalert(type = "error", text = "Email sender must be filled.")
      return()
    }
    
    selected_emails <- df_mailing_list()
    
    if (!is.null(input[["dt_emails_rows_selected"]])) {
      selected_emails <- selected_emails %>% 
        dplyr::filter(dplyr::row_number() %in% input[["dt_emails_rows_selected"]])
    }
    
    participants_attributes <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "participants_attributes") %>% 
      tidyr::separate_rows(.data$survey_id, sep = ";") %>% 
      dplyr::filter(.data$survey_id %in% selected_emails$survey_id)
    
    survey.admin::mailing(
      rv,
      participants = selected_emails,
      participants_attributes = participants_attributes,
      from = list(
        "email" = input$sender_email,
        "alias" = input$sender_alias
      ),
      subject = input$mail_subject,
      body = input$mail_body,
      sleep = input$mailing_sleep,
      progress = TRUE
    )
    
  })
  
}
