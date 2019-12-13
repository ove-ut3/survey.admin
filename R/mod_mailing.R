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
        uiOutput(ns("select_max_emails")),
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
            actionButton(ns("save_mail"), "Save mail configuration", icon = icon("save"))
          ),
          div(
            style = "display: inline-block; vertical-align: top;",
            fileInput(ns("import_mail"), label = NULL, buttonLabel = "Import mail configuration")
          ),
          div(
            style = "display: inline-block; vertical-align: top;",
            downloadButton(ns("export_mail"), "Export mail configuration", icon = icon("file-export"))
          )
        ),
        box(
          title = "Send emails", width = 12,
          div(
            style = "display: inline-block; width: 50%;", # vertical-align: middle;
            numericInput(ns("mailing_sleep"), "Sleep time in seconds between each mail", value = 7, min = 0)
          ),
          div(
            style = "display: inline-block;", # vertical-align: middle;
            actionButton(ns("send_email"), "Send mails to selected emails", icon = icon("paper-plane"))
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

  dt_mailing_list <- reactive({
    
    rv$dt_participants_filter() %>% 
      dplyr::left_join(
        rv$dt_participants_contacts %>% 
          dplyr::filter(key == "email") %>% 
          dplyr::select(token, email = value, date_source = date) %>% 
          dplyr::mutate_at("date_source", lubridate::as_date),
        by = "token"
      ) %>% 
      dplyr::semi_join(
        rv$dt_email_validation %>% 
          dplyr::filter(status %in% c("valid", "unknown")),
        by = "email"
      )
    
  })
  
  output$select_max_emails <- renderUI({
    
    max <- dt_mailing_list() %>% 
      dplyr::count(token) %>% 
      dplyr::pull(n) %>% 
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
  
  output$dt_emails <- DT::renderDT({
    
    if (!is.null(input[["select_max_email_per_token"]])) {
      
      dt_mailing_list() %>% 
        dplyr::arrange(token, dplyr::desc(date_source)) %>% 
        dplyr::group_by(token) %>% 
        dplyr::filter(dplyr::row_number() <= as.integer(input[["select_max_email_per_token"]])) %>% 
        dplyr::ungroup() %>% 
        dplyr::select(token, email) %>% 
        DT::datatable(
          rownames = FALSE,
          options = list(
            dom = "rt",
            scrollY = '70vh',
            pageLength = -1
          )
        )
      
    }
    
  })
  
  output$input_text_sender_email <- renderUI({
    
    value <- rv$dt_mail_template %>% 
      dplyr::filter(key == "sender_email") %>% 
      dplyr::pull(value)
    
    textInput(ns("sender_email"), "Sendind email", value = value, placeholder = "sender@domain.tld")
    
  })
  
  output$input_text_sender_alias <- renderUI({
    
    value <- rv$dt_mail_template %>% 
      dplyr::filter(key == "sender_alias") %>% 
      dplyr::pull(value)
    
    textInput(ns("sender_alias"), "Sending alias", value = value, placeholder = "Sender name")
    
  })
  
  output$input_text_mail_subject <- renderUI({
    
    value <- rv$dt_mail_template %>% 
      dplyr::filter(key == "subject") %>% 
      dplyr::pull(value) %>% 
      stringr::str_replace_all("''", "'")
    
    textInput(ns("mail_subject"), "Subject", value = value, placeholder = "My mail subject")
    
  })
  
  output$input_textarea_mail_body <- renderUI({
    
    value <- rv$dt_mail_template %>% 
      dplyr::filter(key == "body") %>% 
      dplyr::pull(value) %>% 
      stringr::str_replace_all("''", "'")
    
    textAreaInput(ns("mail_body"), "Body", height = "350px", value = value, placeholder = "My mail body")
    
  })
  
  observeEvent(input$save_mail, {
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      paste0('UPDATE mail_template SET value = "', input$sender_email,'" WHERE key = "sender_email";')
    )
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      paste0('UPDATE mail_template SET value = "', input$sender_alias,'" WHERE key = "sender_alias";')
    )
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      paste0('UPDATE mail_template SET value = "', input$mail_subject,'" WHERE key = "subject";')
    )
    
    body <- input$mail_body %>% 
      stringr::str_replace_all('"', '""')
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      paste0('UPDATE mail_template SET value = "', body,'" WHERE key = "body";')
    )
    
    rv$dt_mail_template$value[which(rv$dt_mail_template$key == "sender_email")] <- input$sender_email
    rv$dt_mail_template$value[which(rv$dt_mail_template$key == "sender_alias")] <- input$sender_alias
    rv$dt_mail_template$value[which(rv$dt_mail_template$key == "subject")] <- input$mail_subject
    rv$dt_mail_template$value[which(rv$dt_mail_template$key == "body")] <- input$mail_body
    
  })
  
  observeEvent(input$import_mail, {
    
    if (!is.null(input$import_mail)) {
      
      #browser()
      
      mail_template <- jsonlite::fromJSON(input$import_mail$datapath)
      
      rv$dt_mail_template$value[which(rv$dt_mail_template$key == "sender_email")] <- mail_template$sender_email
      rv$dt_mail_template$value[which(rv$dt_mail_template$key == "sender_alias")] <- mail_template$sender_alias
      rv$dt_mail_template$value[which(rv$dt_mail_template$key == "subject")] <- mail_template$subject
      rv$dt_mail_template$value[which(rv$dt_mail_template$key == "body")] <- mail_template$body
      
    }
    
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
    
    selected_emails <- dt_mailing_list() %>% 
      dplyr::arrange(token, dplyr::desc(date_source)) %>% 
      dplyr::group_by(token) %>% 
      dplyr::filter(dplyr::row_number() <= as.integer(input[["select_max_email_per_token"]])) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(-date_source)
    
    if (!is.null(input[["dt_emails_rows_selected"]])) {
      selected_emails <- selected_emails %>% 
        dplyr::filter(dplyr::row_number() %in% input[["dt_emails_rows_selected"]])
    }
    
    attributes_body <- paste(input$mail_subject, input$mail_body) %>% 
      stringr::str_match_all("\\{([^\\}]+?)\\}") %>% 
      .[[1]] %>% 
      .[, 2] %>% 
      unique() %>% 
      dplyr::tibble(
        attribute_body = .
      )
    
    extra_attributes <- dplyr::tibble(
      attribute = c("TOKEN", "FIRSTNAME", "LASTNAME", paste0("ATTRIBUTE_", c(nrow(rv$dt_participants_attributes), nrow(rv$dt_participants_attributes) + 1))),
      description = c("token", "firstname", "lastname", "surveyurl", "optouturl"),
      attribute_body = c("TOKEN", "FIRSTNAME", "LASTNAME", "SURVEYURL", "OPTOUTURL")
    )
    
    rename <- dplyr::bind_rows(
      attributes_body %>% 
        dplyr::semi_join(
          rv$dt_participants_attributes,
          by = c("attribute_body" = "attribute")
        ) %>% 
        dplyr::left_join(
          rv$dt_participants_attributes,
          by = c("attribute_body" = "attribute")
        ) %>% 
        dplyr::mutate(attribute = attribute_body) %>% 
        dplyr::select(attribute, description, attribute_body),
      attributes_body %>% 
        dplyr::semi_join(
          rv$dt_participants_attributes,
          by = c("attribute_body" = "description")
        ) %>% 
        dplyr::left_join(
          rv$dt_participants_attributes,
          by = c("attribute_body" = "description")
        ) %>% 
        dplyr::mutate(description = attribute_body) %>% 
        dplyr::select(attribute, description, attribute_body),
      attributes_body %>% 
        dplyr::semi_join(extra_attributes, "attribute_body") %>% 
        dplyr::left_join(extra_attributes, "attribute_body"),
      attributes_body %>% 
        dplyr::semi_join(extra_attributes, by = c("attribute_body" = "description")) %>% 
        dplyr::left_join(extra_attributes %>% 
                           dplyr::select(-attribute_body),
                         by = c("attribute_body" = "description")) %>% 
        dplyr::mutate(description = attribute_body)
    ) %>% 
      dplyr::mutate_at("description", patchr::str_normalise_colnames) %>% 
      dplyr::rename(column = description, rename = attribute) %>% 
      dplyr::add_row(column = "email")

    to <- patchr::rename(selected_emails, rename) %>% 
      patchr::normalise_colnames()
    
    body_html <- input$mail_body %>% 
      stringr::str_replace_all("\n", "<br>")
    style <- "'font-family: calibri; font-size: 11pt;'"
    body_html <- glue::glue("<p style={style}>{body_html}</p>")
    
    recode_body <- rename %>% 
      dplyr::filter(attribute_body != rename)
    
    if (nrow(recode_body) >= 1) {
      for (num_attribute in 1:nrow(recode_body)) {
        body_html <- stringr::str_replace_all(
          body_html,
          paste0("\\{", recode_body$attribute_body[num_attribute], "\\}"),
          paste0("{", recode_body$rename[num_attribute], "}")
        )
      }
      
    }
    
    survey_id_tid <- limer::mailing_create_survey(
      from = list(
        "email" = input$sender_email,
        "alias" = input$sender_alias
      ),
      to = to,
      subject = input$mail_subject,
      body = body_html
    )
    survey_id <- survey_id_tid$survey_id
    tid <- survey_id_tid$tid
    
    withProgress(message = "Sending email :", value = 0, detail = "0%", {
      
      for (i in 1:length(tid)) {
        
        if (i != 1) Sys.sleep(input$mailing_sleep)
        
        mailing <- limer::mail_registered_participant(survey_id, tid = tid[i])
        
        if (stringr::str_detect(mailing$status, "\\d+ left to send$", negate = TRUE)) {
          
          key <- limer::get_session_key()
          mailing <- limer::mail_registered_participant(survey_id, tid = tid[i])
          
        }
        
        event <- dplyr::tibble(
          token = selected_emails$token[i],
          type = "general mailing",
          comment = to$email[i],
          date = as.character(lubridate::today())
        )
        
        impexp::sqlite_append_rows(
          golem::get_golem_options("sqlite_base"),
          event,
          "participants_events"
        )
        
        rv$dt_participants_events <- dplyr::bind_rows(
          rv$dt_participants_events,
          event
        )
        
        incProgress(
          1 / length(tid), 
          detail = paste0(
            round(i / length(tid) * 100, 1), "% - ", 
            to$email[i]
          )
        )
        
      }
      
    })
    
    suppression <- limer::call_limer("delete_survey", params = list("iSurveyID" = survey_id))
    
  })
 
}
