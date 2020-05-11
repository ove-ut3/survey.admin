# Module UI
  
#' @title   mod_email_validation_ui and mod_email_validation_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_email_validation
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_email_validation_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 7,
        box(
          title = "Email", width = 12,
          DT::DTOutput(ns("dt_email"))
        )
      ),
      column(
        width = 5,
        uiOutput(ns("ui_email_malformed")),
        box(
          title = "Validation", width = 12,
          uiOutput(ns("ui_validation"))
        ),
        box(
          title = "Stats", width = 12,
          selectInput(
            ns("select_duplicate_token"),
            label = NULL,
            choices = c("All emails", "One email per token"),
            selected = "All emails"
          ),
          plotly::plotlyOutput(ns("stats"))
        )
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_email_validation
#' @export
#' @keywords internal
    
mod_email_validation_server <- function(input, output, session, rv){
  ns <- session$ns
  
  df_validation_email <- reactive({
    
    rv$df_participants_contacts %>% 
      dplyr::semi_join(
        rv$df_participants_filter(),
        by = "token"
      ) %>% 
      dplyr::filter(.data$key == "email") %>% 
      dplyr::select(.data$token, email = .data$value, .data$service, .data$status, date = .data$status_date) %>% 
      dplyr::mutate_at("service", as.factor) %>%
      tidyr::replace_na(list(status = "missing")) %>% 
      dplyr::mutate_at("status", factor, levels = c("valid", "unknown", "invalid", "missing"))

  })
  
  output$dt_email <- DT::renderDT({
    
    if (!is.null(input[["dt_email_domains_rows_selected"]])) {
      
      selected_domains <- rv$df_email_domains %>% 
        dplyr::filter(dplyr::row_number() %in% input[["dt_email_domains_rows_selected"]])
      
      selected_emails <- df_validation_email() %>% 
        dplyr::mutate(domain = stringr::str_match(.data$email, "@(.+)")[, 2]) %>%
        dplyr::semi_join(selected_domains, by = "domain") %>% 
        dplyr::select(-.data$domain)
      
    } else {
      
      selected_emails <- df_validation_email()
      
    }
    
    selected_emails %>% 
      DT::datatable(
        rownames = FALSE,
        filter = 'top',
        options = list(
          dom = "rti",
          scrollY = '72vh',
          pageLength = -1
        )
      )
    
  })
  
  output$ui_email_malformed <- renderUI({
    
    df <- rv$df_participants_contacts %>%
      dplyr::filter(.data$key == "email") %>%
      dplyr::inner_join(
        rv$df_participants,
        by = "token"
      ) %>%
      dplyr::select(.data$token, email = .data$value, .data$firstname, .data$lastname) %>%
      dplyr::filter(!str_validate_email(.data$email))
    
    if (nrow(df) >= 1) {
      tagList(
        box(
          title = "Malformed email or invalid domains", width = 12, collapsible = TRUE, collapsed = TRUE,
          DT::DTOutput(ns("dt_malformed_emails")),
          actionButton(ns("btn_validate_domains"), "Validate selected domains", icon = icon("check")),
          shinyWidgets::addSpinner(
            DT::DTOutput(ns("dt_email_domains"))
          )
        )
      )
    } else {
      tagList(
        box(
          title = "Malformed email or invalid domains", width = 12, collapsible = TRUE, collapsed = TRUE,
          uiOutput(ns("text_no_malformed_emails")),
          actionButton(ns("btn_validate_domains"), "Validate selected domains", icon = icon("check")),
          shinyWidgets::addSpinner(
            DT::DTOutput(ns("dt_email_domains"))
          )
        )
      )
    }

  })
  
  output$text_no_malformed_emails <- renderUI(HTML("No malformed emails<br><br>"))
  
  output$dt_malformed_emails <- DT::renderDT({
    
    df %>% 
      DT::datatable(
        rownames = FALSE,
        options = list(
          dom = "rti",
          scrollY = '20vh',
          pageLength = -1
        )
      )

  })
  
  observeEvent(input$btn_validate_domains, {

    selected_domains <- rv$df_email_domains
    
    if (!is.null(input[["dt_email_domains_rows_selected"]])) {
      selected_domains <- selected_domains %>% 
        dplyr::filter(dplyr::row_number() %in% input[["dt_email_domains_rows_selected"]])
    }
    
    withProgress(message = "Email domains validation :", value = 0, detail = "0%", {
      
      for (i in 1:nrow(selected_domains)) {
        
        selected_domains$status[i] <- test_url(selected_domains$domain[i])
        
        incProgress(
          1/nrow(selected_domains), 
          detail = paste0(
            round(i/nrow(selected_domains) * 100, 1), "% - ", 
            selected_domains$domain[i]
          )
        )
        
      }
      
    })
    
    rv$df_email_domains <- selected_domains %>%
      patchr::anti_join_bind(rv$df_email_domains, by = "domain", arrange = FALSE) %>%
      dplyr::arrange(-.data$n)
    
    if (nrow(impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "email_domains")) >= 1) {
      impexp::sqlite_execute_sql(
        golem::get_golem_options("sqlite_base"),
        "DELETE FROM email_domains;"
      )
    }
    
    impexp::sqlite_append_rows(golem::get_golem_options("sqlite_base"), rv$df_email_domains, "email_domains")
    
  })
  
  output$dt_email_domains <- DT::renderDT({
    
    rv$df_email_domains <- rv$df_participants_contacts %>% 
      dplyr::filter(.data$key == "email") %>%
      dplyr::inner_join(
        rv$df_participants_filter(),
        by = "token"
      ) %>% 
      dplyr::select(.data$token, email = .data$value, .data$firstname, .data$lastname) %>% 
      dplyr::filter(str_validate_email(.data$email)) %>% 
      dplyr::mutate(domain = stringr::str_match(.data$email, "@(.+)")[, 2]) %>%
      dplyr::group_by(.data$domain) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(-.data$n) %>% 
      dplyr::left_join(
        rv$df_email_domains %>% 
          dplyr::select(.data$domain, .data$status),
        by = "domain"
      )
    
    impexp::sqlite_export(
      golem::get_golem_options("sqlite_base"),
      rv$df_email_domains,
      "email_domains",
      overwrite = TRUE
    )
    
    rv$df_email_domains %>% 
      dplyr::mutate_at("status", as.factor) %>% 
      DT::datatable(
        rownames = FALSE,
        colnames = c("Domain" = "domain"),
        filter = 'top',
        options = list(
          dom = "rt",
          scrollY = '30vh',
          pageLength = -1
        )
      )
    
  })
  
  output$ui_validation <- renderUI({
    
    validate(
      need("survey.api" %in% utils::installed.packages()[, 1], "Package survey.api needs to be installed.")
    )
    
    tagList(
      div(
        style = "display: inline-block; width: 44%; vertical-align: top;",
        selectInput(
          ns("service_select"),
          "Service :",
          choices = c("bulkemailchecker.com", "listflow.io", "quickemailverification.com", "emailmarker.com")
        )
      ),
      div(
        style = "display: inline-block; width: 55%; vertical-align: top;",
        numericInput(
          ns("sleep_select"),
          "Sleep time in seconds between each validation:",
          value = 5,
          min = 0
        )
      ),
      div(
        tippy::with_tippy(
          actionButton(
            ns("validation"),
            label = "Validate selected emails",
            icon = icon("check")
          ),
          "No selection means all emails will be validated"
        )
      )
    )
    
  })
  
  observeEvent(input$validation, {
    
    dt_selected_emails <- df_validation_email()
    
    if (!is.null(input[["dt_email_rows_selected"]])) {
      dt_selected_emails <- dt_selected_emails %>% 
        dplyr::filter(dplyr::row_number() %in% input[["dt_email_rows_selected"]])
    }
    
    rv$df_selected_emails <- dt_selected_emails %>% 
      dplyr::select(.data$email) %>% 
      unique()
    
    shinyalert::shinyalert(title = "Do you confirm ?", type = "info", showCancelButton = TRUE, closeOnEsc = FALSE)

  })
  
  observeEvent(input$shinyalert, {
    
    if (input$shinyalert) {
      
      fn_validation <- switch(
        input$service_select,
        bulkemailchecker.com = survey.api::bulk_email_checker,
        listflow.io = survey.api::bulk_email_checker,
        quickemailverification.com = survey.api::bulk_email_checker,
        emailmarker.com = survey.api::bulk_email_checker
      )
      
      api_key_config <- switch(
        input$service_select,
        bulkemailchecker.com = "api_key_bulkemailchecker",
        listflow.io = "api_key_listflow",
        quickemailverification.com = "api_key_quickemailverification",
        emailmarker.com = "api_key_emailmarker"
      )
      
      api_key = rv$df_config %>% 
        dplyr::filter(.data$key == !!api_key_config) %>% 
        dplyr::pull(.data$value)
      
      output <- dplyr::tibble(
        email = rv$df_selected_emails$email,
        validation = list(nrow(rv$df_selected_emails))
      )
      
      withProgress(message = "Emails validation :", value = 0, detail = "0%", {
        
        for (i in 1:nrow(output)) {
          
          if (i == 1) {
            output$validation[[i]] <- fn_validation(output$email[i], key = api_key, sleep = 0)
          } else {
            output$validation[[i]] <- fn_validation(output$email[i], key = api_key, sleep = input$sleep_select)
          }
          
          incProgress(
            1 / nrow(output),
            detail = paste0(
              round(i / nrow(output) * 100, 1), "% - ",
              output$email[i]
            )
          )
          
        }
        
      })
      
      output <- output %>% 
        tidyr::unnest(.data$validation) %>% 
        dplyr::mutate(service = input$service_select)
      
      patch <- rv$df_participants_contacts %>% 
        dplyr::select(.data$token, .data$key, .data$value, .data$source, .data$date) %>% 
        dplyr::inner_join(
          output %>% 
            dplyr::select(value = .data$email, .data$status, .data$service, status_date = "time"),
          by = "value"
        )
        
      rv$df_participants_contacts <- patchr::df_update(
        rv$df_participants_contacts,
        patch,
        by = c("token", "key", "value")
      )

      impexp::sqlite_export(
        golem::get_golem_options("sqlite_base"),
        rv$df_participants_contacts,
        "participants_contacts",
        overwrite = TRUE
      )
      
    }
    
  })
  
  output$stats <- plotly::renderPlotly({
    
    df <- df_validation_email() %>% 
      tidyr::replace_na(list(status = "missing"))
    
    if (input$select_duplicate_token == "One email per token") {
     
      df <- df %>% 
        dplyr::arrange(.data$token, purrr::map_int(.data$status, ~ which(c("valid", "unknown", "invalid", "missing") %in% .))) %>% 
        dplyr::group_by(.data$token) %>% 
        dplyr::filter(dplyr::row_number() == 1) %>% 
        dplyr::ungroup()
       
    }
    
    df %>% 
      dplyr::pull(.data$status) %>% 
      graphr::shiny_pie(donut = TRUE)
    
  })
  
}
