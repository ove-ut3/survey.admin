# Module UI
  
#' @title   mod_sms_sending_ui and mod_sms_sending_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_sms_sending
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_sms_sending_ui <- function(id){
  ns <- NS(id)
  
  uiOutput(ns("ui_sms"))

}
    
# Module Server
    
#' @rdname mod_sms_sending
#' @export
#' @keywords internal
    
mod_sms_sending_server <- function(input, output, session, rv){
  ns <- session$ns
  
  output$ui_short_link <- renderUI({
    
    cuttly_api_key <- rv$df_config %>% 
      dplyr::filter(key == "api_key_spothit") %>% 
      dplyr::pull(value)
    
    validate(
      need(nchar(cuttly_api_key) >= 1, "A Spot-hit API Key must be defined to shorten URL.")
    )
    
    tagList(
      actionButton(ns("generate_short_link"), "Generate short links", icon = icon("link"))
    )
    
  })
  
  output$ui_sms <- renderUI({
    
    spothit_api_key <- rv$df_config %>% 
      dplyr::filter(key == "api_key_spothit") %>% 
      dplyr::pull(value)
    
    validate(
      need(nchar(spothit_api_key) >= 1, "A Spot-hit API Key must be defined to send SMS.")
    )

    tagList(
      column(
        width = 4,
        box(
          title = "Phone list", width = 12,
          uiOutput(ns("select_max_phones")),
          DT::DTOutput(ns("dt_phones"))
        )
      ),
      column(
        width = 8,
        box(
          title = "Generate shortened survey links", width = 12,
          uiOutput(ns("ui_short_link"))
        ),
        box(
          title = "SMS template", width = 12,
          uiOutput(ns("input_text_sender")),
          uiOutput(ns("input_textarea_body")),
          div(
            style = "display: inline-block; vertical-align: top;",
            fileInput(
              ns("import_sms"),
              label = NULL, 
              buttonLabel = "Import SMS configuration"
            )
          ),
          div(
            style = "display: inline-block; vertical-align: top;",
            downloadButton(
              ns("export_sms"),
              "Export SMS configuration",
              icon = icon("file-export")
            )
          )
        ),
        box(
          title = "Send SMS", width = 12,
          div(
            style = "display: inline-block; width: 50%;", # vertical-align: middle;
            numericInput(
              ns("sms_sleep"),
              "Sleep time in seconds between each SMS",
              value = 7,
              min = 0
            )
          ),
          div(
            style = "display: inline-block;", # vertical-align: middle;
            actionButton(
              ns("send_sms"),
              "Send SMS", 
              icon = icon("mobile")
            )
          )
        )
      )
    )
    
  })
  
  dt_sms_list <- reactive({
    
    rv$df_participants_filter() %>% 
      dplyr::left_join(
        rv$df_participants_contacts,
        by = "token"
      ) %>% 
      dplyr::filter(stringr::str_detect(value, "^0[67] ")) %>% 
      dplyr::arrange(token, dplyr::desc(date)) %>% 
      dplyr::rename(phone = value)
    
  })
  
  output$select_max_phones <- renderUI({
    
    max <- dt_sms_list() %>% 
      dplyr::count(token) %>% 
      dplyr::pull(n) %>% 
      max()
    
    if (max == -Inf) { # 0 emails
      
      selectInput(
        ns("select_max_phone_per_token"), 
        "Maximum number of phone sent per token", 
        choices = 0
      )
      
    } else {
      
      selectInput(
        ns("select_max_phone_per_token"), 
        "Maximum number of phone sent per token", 
        choices = 1:max,
        selected = 1
      )
      
    }
    
  })
  
  output$dt_phones <- DT::renderDT({
    
    req(input[["select_max_phone_per_token"]])
    
    dt_sms_list() %>% 
      dplyr::group_by(token) %>% 
      dplyr::filter(dplyr::row_number() <= as.integer(input[["select_max_phone_per_token"]])) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(token, phone) %>% 
      DT::datatable(
        rownames = FALSE,
        options = list(
          dom = "rti",
          scrollY = '65vh',
          pageLength = -1
        )
      )

  })
  
  observeEvent(input$generate_short_link, {
    
    selected_phones <- dt_sms_list() %>% 
      dplyr::group_by(token) %>% 
      dplyr::filter(dplyr::row_number() <= as.integer(input[["select_max_phone_per_token"]])) %>% 
      dplyr::ungroup()
    
    if (!is.null(input[["dt_phones_rows_selected"]])) {
      selected_phones <- selected_phones %>% 
        dplyr::filter(dplyr::row_number() %in% input[["dt_phones_rows_selected"]])
    }
    
    if ("surveyurl_cuttly" %in% names(rv$df_participants)) {
      
      selected_phones <- selected_phones %>% 
        dplyr::anti_join(
          tidyr::drop_na(rv$df_participants, "surveyurl_cuttly"),
          by = "token"
        )
      
    }
    
    if (nrow(selected_phones) >= 1) {
      
      api_key <- rv$df_config %>% 
        dplyr::filter(key == "api_key_cuttly") %>% 
        dplyr::pull(value)
      
      query <- selected_phones$surveyurl %>% 
        purrr::map(survey.api::cuttly, api_key = api_key)
      
      shortened_urls <- dplyr::tibble(
        token = selected_phones$token,
        new_shortened_url = purrr::map_chr(query,  ~ .$url$shortLink),
        status = purrr::map_chr(query ~ .$url$status)
      ) %>% 
        dplyr::filter(status == 7) %>% 
        dplyr::select(token, new_shortened_url)
      
      rv$df_participants <- rv$df_participants %>% 
        dplyr::left_join(shortened_urls, by = "token")
      
      if ("surveyurl_cuttly" %in% names(rv$df_participants)) {
        
        rv$df_participants <- rv$df_participants %>% 
          dplyr::mutate(
            new_shortened_url = dplyr::if_else(!is.na(new_shortened_url), new_shortened_url, surveyurl_cuttly)
          )
        
      }
      
      rv$df_participants <- rv$df_participants %>%
        dplyr::rename(surveyurl_cuttly = new_shortened_url)
      
      impexp::sqlite_execute_sql(
        golem::get_golem_options("sqlite_base"),
        "DROP table participants;"
      )
      
      impexp::sqlite_export(
        golem::get_golem_options("sqlite_base"),
        rv$df_participants,
        "participants"
      )
      
    }
    
  })
  
  output$input_text_sender <- renderUI({
    
    rv$sms_sender <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "sms_template"
    ) %>% 
      dplyr::filter(key == "sender") %>% 
      dplyr::pull(value)
    
    textInput(
      ns("sender"),
      label = "Sending alias", 
      value = isolate(rv$sms_sender), 
      placeholder = "Sender name", 
      width = "50%"
    )
    
  })
  
  observeEvent(input$sender, {
    
    req(input$sender != rv$sms_sender)
    
    rv$sms_sender <- input$sender
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("DELETE FROM sms_template WHERE key = \"sender\";")
    )
    
    impexp::sqlite_append_rows(
      golem::get_golem_options("sqlite_base"),
      dplyr::tibble(
        key = "sender",
        value = input$sender
      ),
      "sms_template"
    )

  })
  
  output$input_textarea_body <- renderUI({
    
    rv$sms_body <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "sms_template"
    ) %>% 
      dplyr::filter(key == "body") %>% 
      dplyr::pull(value) %>% 
      stringr::str_replace_all("''", "'")
    
    textAreaInput(
      ns("body"),
      label = "Body",
      height = "300px",
      value = isolate(rv$sms_body),
      placeholder = "My SMS body"
    )
    
  })
  
  observeEvent(input$body, {
    
    req(input$body != rv$sms_body)
    
    rv$sms_body <- input$body

    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("DELETE FROM sms_template WHERE key = \"body\";")
    )
    
    impexp::sqlite_append_rows(
      golem::get_golem_options("sqlite_base"),
      dplyr::tibble(
        key = "body",
        value = input$body
      ),
      "sms_template"
    )
    
  })
  
  observeEvent(input$import_sms, {
    
    req(input$import_sms)
    
    sms_template <- jsonlite::fromJSON(input$import_sms$datapath) %>% 
      purrr::map(stringr::str_replace_all, '"', '""')
    
    # sender
    updateTextInput(
      session,
      "sender",
      value = sms_template$sender
    )
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("DELETE FROM sms_template WHERE key = \"sender\";")
    )
    
    impexp::sqlite_append_rows(
      golem::get_golem_options("sqlite_base"),
      dplyr::tibble(
        key = "sender",
        value = input$sender
      ),
      "sms_template"
    )
    
    # body
    updateTextInput(
      session,
      "body",
      value = sms_template$body
    )
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("DELETE FROM sms_template WHERE key = \"body\";")
    )
    
    impexp::sqlite_append_rows(
      golem::get_golem_options("sqlite_base"),
      dplyr::tibble(
        key = "body",
        value = input$body
      ),
      "sms_template"
    )
    
  })
  
  output$export_sms <- downloadHandler(
    filename = function() {
      "sms_template.json"
    },
    content = function(con) {
      list(
        "sender" = input$sender,
        "body" = input$body
      ) %>% 
        jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE) %>% 
        writeLines(con, useBytes = TRUE)
    }
  )
  
  observeEvent(input$send_sms, {
    
    if (nchar(input$sender) == 0) {
      shinyalert::shinyalert(type = "error", text = "SMS sender must be filled.")
      return()
    }
    
    selected_phones <- dt_sms_list() %>% 
      dplyr::group_by(token) %>% 
      dplyr::filter(dplyr::row_number() <= as.integer(input[["select_max_phone_per_token"]])) %>% 
      dplyr::ungroup()

    if (!is.null(input[["dt_phones_rows_selected"]])) {
      selected_phones <- selected_phones %>% 
        dplyr::filter(dplyr::row_number() %in% input[["dt_phones_rows_selected"]])
    }
    
    attributes_body <- input$sms_body %>% 
      stringr::str_match_all("\\{([^\\}]+?)\\}") %>% 
      .[[1]] %>% 
      .[, 2] %>% 
      unique() %>% 
      dplyr::tibble(
        attribute_body = .
      )
    
    extra_attributes <- dplyr::tibble(
      attribute = c("FIRSTNAME", "LASTNAME", paste0("ATTRIBUTE_", nrow(rv$df_participants_attributes))),
      description = c("firstname", "lastname", "surveyurl"),
      attribute_body = c("FIRSTNAME", "LASTNAME", "SURVEYURL")
    )
    
    rename <- dplyr::bind_rows(
      attributes_body %>% 
        dplyr::semi_join(
          rv$df_participants_attributes,
          by = c("attribute_body" = "attribute")
        ) %>% 
        dplyr::left_join(
          rv$df_participants_attributes,
          by = c("attribute_body" = "attribute")
        ) %>% 
        dplyr::mutate(attribute = attribute_body) %>% 
        dplyr::select(attribute, description, attribute_body),
      attributes_body %>% 
        dplyr::semi_join(
          rv$df_participants_attributes,
          by = c("attribute_body" = "description")
        ) %>% 
        dplyr::left_join(
          rv$df_participants_attributes,
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
      dplyr::rename(column = description, rename = attribute_body) %>% 
      dplyr::add_row(column = "phone")
    
    to <- patchr::rename(selected_phones, rename)
    
    recode_body <- rename %>% 
      tidyr::drop_na(rename)
    
    api_key <- rv$df_config %>% 
      dplyr::filter(key == "api_key_spothit") %>% 
      dplyr::pull(value)
    
    name <- glue::glue("Shiny sending {lubridate::now()}")
    
    withProgress(message = "Sending SMS :", value = 0, detail = "0%", {
      
      for (i in 1:nrow(to)) {
        
        if (i != 1) Sys.sleep(input$sms_sleep)
        
        body <- input$sms_body
        
        if (nrow(recode_body) >= 1) {
          for (num_attribute in 1:nrow(recode_body)) {
            body <- stringr::str_replace_all(
              body,
              paste0("\\{", recode_body$rename[num_attribute], "\\}"),
              paste0(to[[recode_body$rename[num_attribute]]][i])
            )
          }
          
        }
        
        survey.api::spot_hit_send_sms(
          message = body,
          destinataires = to$phone[i],
          key = api_key,
          expediteur = input_text_sender,
          encodage = "ucs2",
          nom = name
        )

        event <- dplyr::tibble(
          token = selected_phones$token[i],
          type = "general sms",
          comment = to$email[i],
          date = as.character(lubridate::today())
        )

        impexp::sqlite_append_rows(
          golem::get_golem_options("sqlite_base"),
          event,
          "participants_events"
        )

        rv$df_participants_events <- dplyr::bind_rows(
          rv$df_participants_events,
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
    
  })
  
}
