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
          title = "Crowdsourcing columns", width = 12,
          rhandsontable::rHandsontableOutput(ns("hot_crowdsourcing_columns"))
        )
      ),
      fluidRow(
        box(
          title = "Contributors list", width = 5,
          fileInput(
            ns("import_contributors"),
            "Import and replace contributors"
          ),
          DT::DTOutput(ns("dt_crowdsourcing_contributors"), height = 528),
          actionButton(
            ns("generate_passwords"),
            "Generate passwords"
          )
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
            fileInput(ns("import_mail"), label = NULL, buttonLabel = "Import mail configuration")
          ),
          div(
            style = "display: inline-block; vertical-align: top;",
            downloadButton(ns("export_mail"), "Export mail configuration", icon = icon("file-export"))
          ),
          div(
            actionButton(
              ns("send_email"),
              "Send mails to selected emails",
              icon = icon("paper-plane")
            )
          )
        )
      ),
      fluidRow(
        box(
          title = "Contributions moderation",
          width = 12,
          DT::DTOutput(ns("dt_crowdsourcing_moderation"))
        )
      )
    )
    
  })
  
  output$hot_crowdsourcing_columns <- rhandsontable::renderRHandsontable({

    if (nrow(impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "crowdsourcing_columns")) == 0) {

      contacts_key <- rv$df_participants_contacts %>% 
        dplyr::pull(key) %>% 
        unique()
      
      rv$df_crowdsourcing_columns <- dplyr::tibble(
        description = names(rv$df_participants)
      ) %>%
        dplyr::filter(description != "tid") %>% 
        dplyr::left_join(rv$df_participants_attributes, by = "description") %>%
        tidyr::replace_na(list(num_attribute = 0L)) %>%
        dplyr::mutate(attribute = dplyr::if_else(is.na(attribute), description, attribute)) %>%
        dplyr::arrange(num_attribute) %>%
        dplyr::select(column = attribute, description) %>%
        dplyr::add_row(
          .after = 4,
          column = c("completed", "optout", "lastpage_rate"),
          description = c("completed", "optout", "lastpage_rate")
        ) %>%
        dplyr::add_row(
          .after = 9,
          column = c(
            contacts_key,
            glue::glue("{contacts_key}_invalid")
          ),
          description = c(
            contacts_key,
            glue::glue("{contacts_key}_invalid")
          )
        ) %>%
        dplyr::mutate(
          description_new = description,
          display = FALSE,
          order = NA_integer_,
          edit = FALSE,
          filter = FALSE,
          restriction = FALSE
        )

      impexp::sqlite_export(
        golem::get_golem_options("sqlite_base"),
        data = rv$df_crowdsourcing_columns,
        table_name = "crowdsourcing_columns",
        overwrite = TRUE
      )
      
    }

    rv$df_crowdsourcing_columns %>%
      rhandsontable::rhandsontable(rowHeaders = TRUE, height = 600) %>%
      rhandsontable::hot_table(contextMenu = FALSE, highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") %>%
      rhandsontable::hot_rows(rowHeights = 35) %>%
      rhandsontable::hot_col(c("column", "description"), readOnly = TRUE) %>%
      rhandsontable::hot_col("order", type = "numeric") %>%
      rhandsontable::hot_col(c("restriction", "display", "edit", "filter"), halign = "htCenter")

  })
  
  observeEvent(input$hot_crowdsourcing_columns, {
    
    req(input$hot_crowdsourcing_columns)
    
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
      
      rv$df_crowdsourcing_columns <- update

    }

  })
  
  observeEvent(input$import_contributors, {
    
    req(input$import_contributors)
    
    contributors <- read.csv(input$import_contributors$datapath, na.strings = "")

    impexp::sqlite_export(
      golem::get_golem_options("sqlite_base"),
      data = contributors,
      table_name = "crowdsourcing_contributors",
      overwrite = TRUE
    )

    rv$df_crowdsourcing_contributors <- contributors

  })
  
  output$dt_crowdsourcing_contributors <- DT::renderDT({
    
    contributor_restriction <- rv$df_crowdsourcing_columns %>%
      dplyr::filter(restriction) %>% 
      dplyr::pull(description) %>% 
      stringr::str_replace_all(" ", ".")
    
    if (any(!contributor_restriction %in% names(rv$df_crowdsourcing_contributors))) {
      
      list_mutate <- stats::setNames(list("NA_character_"), contributor_restriction)
      
      rv$df_crowdsourcing_contributors <- rv$df_crowdsourcing_contributors %>% 
        dplyr::mutate(!!!list_mutate)
      
      impexp::sqlite_export(
        golem::get_golem_options("sqlite_base"),
        rv$df_crowdsourcing_contributors,
        "crowdsourcing_contributors",
        overwrite = TRUE
      )
      
    }
    
    rv$df_crowdsourcing_contributors %>% 
      dplyr::select(user, password, contributor_restriction) %>% 
      DT::datatable(
        rownames = FALSE,
        options = list(
          dom = "rftp"
        )
      )
    
  })
  
  output$input_text_sender_email <- renderUI({
    
    rv$crowdsourcing_sender_email <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "crowdsourcing_mail_template"
    ) %>% 
      dplyr::filter(key == "sender_email") %>% 
      dplyr::pull(value)
    
    textInput(
      ns("sender_email"),
      label = "Sendind email",
      value = isolate(rv$crowdsourcing_sender_email),
      placeholder = "sender@domain.tld"
    )
    
  })
  
  observeEvent(input$sender_email, {
    
    req(input$sender_email != rv$crowdsourcing_sender_email)
    
    rv$crowdsourcing_sender_email <- input$sender_email
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("UPDATE crowdsourcing_mail_template SET value = \"{input$sender_email}\" WHERE key = \"sender_email\";")
    )
    
  })
  
  output$input_text_sender_alias <- renderUI({
    
    rv$crowdsourcing_sender_alias <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "crowdsourcing_mail_template"
    ) %>% 
      dplyr::filter(key == "sender_alias") %>% 
      dplyr::pull(value)
    
    textInput(
      ns("sender_alias"),
      label = "Sending alias",
      value = isolate(rv$crowdsourcing_sender_alias),
      placeholder = "Sender name"
    )
    
  })
  
  observeEvent(input$sender_alias, {
    
    req(input$sender_alias != rv$crowdsourcing_sender_alias)
    
    rv$crowdsourcing_sender_alias <- input$sender_alias
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("UPDATE crowdsourcing_mail_template SET value = \"{input$sender_alias}\" WHERE key = \"sender_alias\";")
    )
    
  })
  
  output$input_text_mail_subject <- renderUI({
    
    rv$crowdsourcing_mail_subject <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "crowdsourcing_mail_template"
    ) %>% 
      dplyr::filter(key == "subject") %>% 
      dplyr::pull(value) %>% 
      stringr::str_replace_all("''", "'")
    
    textInput(
      ns("mail_subject"),
      label = "Subject",
      value = isolate(rv$crowdsourcing_mail_subject),
      placeholder = "My mail subject"
    )
    
  })
  
  observeEvent(input$mail_subject, {
    
    req(input$mail_subject != rv$crowdsourcing_mail_subject)
    
    rv$crowdsourcing_mail_subject <- input$mail_subject
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("UPDATE crowdsourcing_mail_template SET value = \"{input$mail_subject}\" WHERE key = \"subject\";")
    )
    
  })
  
  output$input_textarea_mail_body <- renderUI({
    
    rv$crowdsourcing_mail_body <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "crowdsourcing_mail_template"
    ) %>% 
      dplyr::filter(key == "body") %>% 
      dplyr::pull(value) %>% 
      stringr::str_replace_all("''", "'")
    
    textAreaInput(
      ns("mail_body"),
      label = "Body",
      height = "400px",
      value = isolate(rv$crowdsourcing_mail_body),
      placeholder = "My mail body"
    )
    
  })
  
  observeEvent(input$mail_body, {
    
    req(input$mail_body != rv$crowdsourcing_mail_body)
    
    rv$crowdsourcing_mail_body <- input$mail_body
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("UPDATE crowdsourcing_mail_template SET value = \"{input$mail_body}\" WHERE key = \"body\";")
    )
    
  })
  
  observeEvent(input$import_mail, {
    
    req(input$import_mail)
    
    mail_template <- jsonlite::fromJSON(input$import_mail$datapath) %>% 
      purrr::map(stringr::str_replace_all, '"', '""')
    
    # sender email
    updateTextInput(
      session,
      "sender_email",
      value = mail_template$sender_email
    )
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("UPDATE crowdsourcing_mail_template SET value = \"{mail_template$sender_email}\" WHERE key = \"sender_email\";")
    )
    
    # sender alias
    updateTextInput(
      session,
      "sender_alias",
      value = mail_template$sender_alias
    )
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("UPDATE crowdsourcing_mail_template SET value = \"{mail_template$sender_alias}\" WHERE key = \"sender_alias\";")
    )
    
    # mail subject
    updateTextInput(
      session,
      "mail_subject",
      value = mail_template$subject
    )
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("UPDATE crowdsourcing_mail_template SET value = \"{mail_template$subject}\" WHERE key = \"subject\";")
    )
    
    # mail body
    updateTextAreaInput(
      session,
      "mail_body",
      value = mail_template$body
    )
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("UPDATE crowdsourcing_mail_template SET value = \"{mail_template$body}\" WHERE key = \"body\";")
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
    
    participants_mailing <- rv$df_crowdsourcing_contributors %>% 
      dplyr::filter(caractr::str_validate_email(user)) %>% 
      dplyr::mutate(email = user) %>% 
      dplyr::mutate(lib_diplome = apogee::lib_etape(`Code.diplôme`, prefixe = "diplome", suffixe = c("ville", "option", "particularite"))) %>% 
      dplyr::select(email = user, lib_diplome, password)

    copie_destinataire <- participants_mailing %>%
      dplyr::select(email, lib_diplome) %>%
      dplyr::full_join(participants_mailing %>%
                         dplyr::select(email_copie = email, lib_diplome),
                       by = "lib_diplome") %>%
      dplyr::filter(email != email_copie) %>%
      dplyr::group_by(email, lib_diplome) %>%
      dplyr::summarise(email_copie = paste0(email_copie, collapse = " ; ")) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(email_copie = glue::glue("(contacts ayant également accès à ce diplôme : {email_copie})"))
    
    participants_mailing <- participants_mailing %>%
      dplyr::left_join(copie_destinataire, by = c("email", "lib_diplome")) %>%
      dplyr::arrange(email, lib_diplome) %>%
      dplyr::mutate(liste = caractr::str_paste(lib_diplome, " ", email_copie, sep = ''),
                    liste = caractr::str_paste("<li>", liste, "</li>", sep = "")) %>%
      dplyr::group_by(email, password) %>%
      dplyr::mutate(
        liste = ifelse(dplyr::row_number() == 1, glue::glue("<ul>{liste}"), liste),
        liste = ifelse(dplyr::row_number() == dplyr::n(), glue::glue("{liste}</ul>"), liste)) %>%
      dplyr::summarise(liste_formations = paste0(liste, collapse = "")) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(user = email)
    
    participants_attributes <- dplyr::tibble(
      attribute = c("ATTRIBUTE_1", "ATTRIBUTE_2", "ATTRIBUTE_3"),
      description = c("user", "password", "liste_formations")
    )
    
    survey.admin::mailing(
      rv,
      participants = participants_mailing,
      participants_attributes = participants_attributes,
      from = list(
        "email" = input$sender_email,
        "alias" = input$sender_alias
      ),
      subject = input$mail_subject,
      body = input$mail_body,
      sleep = input$mailing_sleep,
      general == TRUE,
      crowdsourcing = TRUE
    )
    
  })
  
  output$dt_crowdsourcing_moderation <- DT::renderDT({
    
    impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "crowdsourcing_log"
    ) %>% 
      dplyr::filter(is.na(status)) %>% 
      DT::datatable()
    
  })
}
