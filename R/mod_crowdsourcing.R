# Module UI
  
#' @title   mod_crowdsourcing_ui and mod_crowdsourcing_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_crowdsourcing
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_crowdsourcing_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns("ui"))
}
    
# Module Server
    
#' @rdname mod_crowdsourcing
#' @export
#' @keywords internal
    
mod_crowdsourcing_server <- function(input, output, session, rv){
  ns <- session$ns
  
  output$ui <- renderUI({
    
    validate(
      need("survey.crowdsourcing" %in% installed.packages(), "Package \"survey.crowdsourcing\" must be installed.")
    )
    
    tagList(
      fluidRow(
        box(
          title = "Crowdsourcing data", width = 12,
          rhandsontable::rHandsontableOutput(ns("hot_crowdsourcing_columns"))
        )
      ),
      fluidRow(
        box(
          title = "Contributors list", width = 5,
          fileInput(ns("import_contributors"), "Import and replace contributors"),
          DT::DTOutput(ns("dt_crowdsourcing_contributors"), height = 528),
          actionButton(ns("generate_passwords"), "Generate passwords")
        ),
        box(
          title = "Mail template", width = 7,
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
        )
      ),
      fluidRow(
        box(
          title = "Contributions moderation",
          width = 12
        )
      )
    )
    
  })
  
  output$hot_crowdsourcing_columns <- rhandsontable::renderRHandsontable({

    if (nrow(impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "crowdsourcing_columns")) == 0) {

      rv$dt_crowdsourcing_columns <- dplyr::tibble(
        description = names(rv$dt_participants)
      ) %>%
        dplyr::left_join(rv$dt_participants_attributes, by = "description") %>%
        tidyr::replace_na(list(num_attribute = 0L)) %>%
        dplyr::mutate(attribute = dplyr::if_else(is.na(attribute), description, attribute)) %>%
        dplyr::arrange(num_attribute) %>%
        dplyr::select(column = attribute, description) %>%
        dplyr::add_row(
          .before = 1,
          column = c("email", "phone", "survey_link"),
          description = c("Email", "Phone", "Survey link")
        ) %>%
        dplyr::mutate(
          description_new = description,
          display = FALSE,
          edit = FALSE,
          filter = FALSE,
          restriction = FALSE
        )

    }

    rv$dt_crowdsourcing_columns %>%
      rhandsontable::rhandsontable(rowHeaders = TRUE, height = 600, manualRowMove = TRUE) %>%
      rhandsontable::hot_table(contextMenu = FALSE, disableVisualSelection = TRUE, highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") %>%
      rhandsontable::hot_rows(rowHeights = 35) %>%
      rhandsontable::hot_col(c("column", "description"), readOnly = TRUE) %>%
      rhandsontable::hot_col(c("restriction", "display", "edit", "filter"), halign = "htCenter")

  })
  
  observeEvent(input$hot_crowdsourcing_columns, {
    
    if (!is.null(input$hot_crowdsourcing_columns)) {
      
      update <- input$hot_crowdsourcing_columns %>% 
        rhandsontable::hot_to_r() %>% 
        dplyr::as_tibble()
      
      if (!is.null(input$hot_crowdsourcing_columns$changes$changes) | !is.null(input$hot_crowdsourcing_columns$changes$ind)) {
        
        impexp::sqlite_export(
          golem::get_golem_options("sqlite_base"),
          update,
          "crowdsourcing_columns",
          overwrite = TRUE
        )
        
        if (input$hot_crowdsourcing_columns$changes$event != "afterRowMove") {
          rv$dt_crowdsourcing_columns <- update
        }
        
      }
      
    }
    
  })
  
  observeEvent(input$import_contributors, {
    
    if (!is.null(input$import_contributors)) {
      
      contributors <- read.csv(input$import_contributors$datapath, na.strings = "")

      impexp::sqlite_append_rows(
        golem::get_golem_options("sqlite_base"),
        contributors,
        "crowdsourcing_contributors"
      )

      rv$dt_crowdsourcing_contributors <- contributors
       
    }
    
  })
  
  output$dt_crowdsourcing_contributors <- DT::renderDT({
    
    contributor_restriction <- rv$dt_crowdsourcing_columns %>%
      dplyr::filter(restriction) %>% 
      dplyr::pull(description) %>% 
      stringr::str_replace_all(" ", ".")
    
    if (any(!contributor_restriction %in% names(rv$dt_crowdsourcing_contributors))) {
      
      list_mutate <- stats::setNames(list("NA_character"), contributor_restriction)
      
      rv$dt_crowdsourcing_contributors <- rv$dt_crowdsourcing_contributors %>% 
        dplyr::mutate(!!!list_mutate)
      
      impexp::sqlite_export(
        golem::get_golem_options("sqlite_base"),
        rv$dt_crowdsourcing_contributors,
        "crowdsourcing_contributors",
        overwrite = TRUE
      )
      
    }
    
    rv$dt_crowdsourcing_contributors %>% 
      DT::datatable(
        rownames = FALSE,
        options = list(
          dom = "rftp"
        )
      )
    
  })
  
  output$input_text_sender_email <- renderUI({
    
    value <- rv$dt_crowdsourcing_mail_template %>% 
      dplyr::filter(key == "sender_email") %>% 
      dplyr::pull(value)
    
    textInput(ns("sender_email"), "Sendind email", value = value, placeholder = "sender@domain.tld")
    
  })
  
  output$input_text_sender_alias <- renderUI({
    
    value <- rv$dt_crowdsourcing_mail_template %>% 
      dplyr::filter(key == "sender_alias") %>% 
      dplyr::pull(value)
    
    textInput(ns("sender_alias"), "Sending alias", value = value, placeholder = "Sender name")
    
  })
  
  output$input_text_mail_subject <- renderUI({
    
    value <- rv$dt_crowdsourcing_mail_template %>% 
      dplyr::filter(key == "subject") %>% 
      dplyr::pull(value) %>% 
      stringr::str_replace_all("''", "'")
    
    textInput(ns("mail_subject"), "Subject", value = value, placeholder = "My mail subject")
    
  })
  
  output$input_textarea_mail_body <- renderUI({
    
    value <- rv$dt_crowdsourcing_mail_template %>% 
      dplyr::filter(key == "body") %>% 
      dplyr::pull(value) %>% 
      stringr::str_replace_all("''", "'")
    
    textAreaInput(ns("mail_body"), "Body", height = "400px", value = value, placeholder = "My mail body")
    
  })
  
  observeEvent(input$send_email, {
    
    if (nchar(input$sender_email) == 0) {
      shinyalert::shinyalert(type = "error", text = "Email sender must be filled.")
      return()
    }
    
    selected_emails <- rv$dt_participants_filter() %>% 
      dplyr::left_join(rv$dt_participants_contacts %>% 
                         dplyr::filter(key == "email"),
                       by = "token") %>% 
      dplyr::rename(email = value)
    
    if (!is.null(input[["dt_emails_rows_selected"]])) {
      selected_emails <- selected_emails %>% 
        dplyr::filter(dplyr::row_number() %in% input[["dt_emails_rows_selected"]])
    }
    
    attributes_mail <- paste(input$mail_subject, input$mail_body) %>% 
      stringr::str_match_all("\\{([^\\}]+?)\\}") %>% 
      .[[1]] %>% 
      .[, 2] %>% 
      unique() %>% 
      tolower()
    
    rename <- rv$dt_participants_attributes %>% 
      dplyr::mutate_at("description", patchr::str_normalise_colnames) %>% 
      dplyr::bind_rows(
        dplyr::tibble(
          attribute = c("token", "firstname", "lastname"),
          description = c("token", "firstname", "lastname")
        )
      ) %>% 
      dplyr::filter(attribute %in% !!attributes_mail) %>% 
      dplyr::select(column = description, rename = attribute)
    
    to <- patchr::rename(selected_emails, rename)
    
    body_html <- input$mail_body %>% 
      stringr::str_replace_all("\n", "<br>")
    
    style <- "'font-family: calibri; font-size: 11pt;'"
    body_html <- glue::glue("<p style={style}>{body_html}</p>")
    
    if ("token" %in% names(to)) {
      
      attribute_token <- rv$dt_participants_attributes %>% 
        tail(1) %>% 
        dplyr::pull(num_attribute) %>% 
        sum(1) %>% 
        { paste0("attribute_", .) }
      
      names(to)[which(names(to) == "token")] <- attribute_token
      
      body_html <- stringr::str_replace_all(body_html, "\\{TOKEN\\}", paste0("{", toupper(attribute_token), "}"))
      
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
        
        if (length(mailing[[1]]$status) == 0) {
          
          key <- limer::get_session_key()
          mailing <- limer::mail_registered_participant(survey_id, tid = tid[i])
          
        }
        
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
  
  observeEvent(input$save_mail, {
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      paste0('UPDATE crowdsourcing_mail_template SET value = "', input$sender_email,'" WHERE key = "sender_email";')
    )
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      paste0('UPDATE crowdsourcing_mail_template SET value = "', input$sender_alias,'" WHERE key = "sender_alias";')
    )
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      paste0('UPDATE crowdsourcing_mail_template SET value = "', input$mail_subject,'" WHERE key = "subject";')
    )
    
    body <- input$mail_body %>% 
      stringr::str_replace_all('"', '""')
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      paste0('UPDATE crowdsourcing_mail_template SET value = "', body,'" WHERE key = "body";')
    )
    
    rv$dt_crowdsourcing_mail_template$value[which(rv$dt_crowdsourcing_mail_template$key == "sender_email")] <- input$sender_email
    rv$dt_crowdsourcing_mail_template$value[which(rv$dt_crowdsourcing_mail_template$key == "sender_alias")] <- input$sender_alias
    rv$dt_crowdsourcing_mail_template$value[which(rv$dt_crowdsourcing_mail_template$key == "subject")] <- input$mail_subject
    rv$dt_crowdsourcing_mail_template$value[which(rv$dt_crowdsourcing_mail_template$key == "body")] <- input$mail_body
    
  })
  
  observeEvent(input$import_mail, {
    
    if (!is.null(input$import_mail)) {
      
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
  
}
