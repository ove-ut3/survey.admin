# Module UI
  
#' @title   mod_phoning_ui and mod_phoning_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_phoning
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_phoning_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 4,
        fluidRow(
          box(
            title = "Team", width = 12,
            rhandsontable::rHandsontableOutput(ns("hot_users"), height = "400px"),
            div(
              style = "display: inline-block; vertical-align: top;",
              fileInput(
                ns("import_users"),
                label = NULL, 
                buttonLabel = "Import users"
              )
            ),
            div(
              style = "display: inline-block; vertical-align: top;",
              downloadButton(
                ns("export_users"),
                "Export users", 
                icon = icon("save")
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "survey.phoning configuration", width = 12,
            uiOutput(ns("picker_phoning_attributes_participants")),
            uiOutput(ns("ui_text_help"))
          )
        )
      ),
      column(
        width = 8,
        fluidRow(
          box(
            title = "Groups and team", width = 12,
            div(
              style = "display: inline-block; width: 33%;",
              uiOutput(ns("picker_attributes_groups"), inline = TRUE)
            ),
            div(
              style = "display: inline-block; width: 33%;",
              uiOutput(ns("ui_picker_user"), inline = TRUE)
            ),
            div(
              style = "display: inline-block; width: 33%; vertical-align: top;",
              uiOutput(ns("ui_picker_maximal_date"), inline = TRUE)
            ),
            br(),
            div(
              style = "display: inline-block; vertical-align: top;",
              actionButton(
                ns("affect_groups"),
                "Affect groups automatically"
              )
            ),
            div("Number of contacts per user", style = "display: inline-block; font-weight: bold; vertical-align: top;"),
            div(
              style = "display: inline-block; vertical-align: top;",
              shiny::numericInput(ns("to_contact"), label = NULL, value = 200, min = 1)
            ),
            rhandsontable::rHandsontableOutput(ns("hot_groups_users"), height = "600px"),
            div(br(), style = "font-size: 10px;"),
            div(
              style = "display: inline-block; vertical-align: top;",
              fileInput(
                ns("import_groups_users"),
                label = NULL, 
                buttonLabel = "Import groups and users"
              )
            ),
            div(
              style = "display: inline-block; vertical-align: top;",
              downloadButton(
                ns("export_groups_users"),
                "Export groups and users",
                icon = icon("save")
              )
            )
          )
        )
      ),
      box(
        title = "Users hours",
        width = 12,
        div(
          style = "display: inline-block; width: 50%; vertical-align: top;",
          uiOutput(ns("ui_select_users_hours_date"))
        ),
        div(
          style = "display: inline-block; width: 49%; vertical-align: top;",
          uiOutput(ns("ui_select_user_hours"))
        ),
        DT::DTOutput(ns("dt_users_hours"))
      ),
      box(
        title = "Contacts updates log",
        width = 12,
        DT::DTOutput(ns("dt_contacts_update_log"))
      )
    )
  )
  
}
    
# Module Server
    
#' @rdname mod_phoning
#' @export
#' @importFrom lubridate %--%
#' @keywords internal
    
mod_phoning_server <- function(input, output, session, rv){
  ns <- session$ns
  
  output$hot_users <- rhandsontable::renderRHandsontable({
    
    attributes_groups <- rv$df_config %>% 
      dplyr::filter(.data$key == "phoning_attributes_groups") %>% 
      tidyr::separate_rows(.data$value, sep = ";") %>% 
      dplyr::pull(.data$value)
    
    if (attributes_groups %in% names(rv$df_phoning_team_group)) {
      
      groups_remaining <- rv$df_participants_filter() %>% 
        dplyr::left_join(rv$df_phoning_team_group, by = attributes_groups) %>% 
        survey.phoning::df_groups(attributes_groups) %>% 
        tidyr::drop_na(.data$user)
      
      if (!is.null(input$maximal_date)) {
        groups_remaining <- dplyr::filter(groups_remaining, lubridate::as_date(.data$last_event_date) <= lubridate::as_date(input$maximal_date))
      }
      
      phoning_team_events <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "phoning_team_events")
      
      if (!is.null(input$maximal_date)) {
        phoning_team_events <- dplyr::filter(phoning_team_events, lubridate::as_date(.data$date) >= lubridate::today() - 7)
      }
      
      remaining <- rv$df_participants_filter() %>% 
        dplyr::filter(!.data$completed, !.data$optout) %>% 
        dplyr::left_join(rv$df_phoning_team_group, by = attributes_groups) %>% 
        tidyr::drop_na(.data$user) %>% 
        dplyr::semi_join(groups_remaining, by = attributes_groups) %>% 
        dplyr::anti_join(
          phoning_team_events,
          by = "token"
        )
      
    } else {
      
      remaining <- rv$df_phoning_team %>% 
        dplyr::filter(FALSE)
      
    }
    
    remaining <- dplyr::count(remaining, .data$user, name = "remaining_calls")
    
    rv$df_phoning_team %>% 
      dplyr::left_join(remaining, by = "user") %>% 
      rhandsontable::rhandsontable(rowHeaders = NULL) %>%
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") %>%
      rhandsontable::hot_rows(rowHeights = 35) %>%
      rhandsontable::hot_cols(valign = "htMiddle")
    
  })
  
  observeEvent(input$hot_users, {
    
    req(input$hot_users)
    
    changes <- input$hot_users$changes
    
    if (!is.null(changes[["ind"]]) | !is.null(changes[["changes"]])) {
      
      update <- input$hot_users %>% 
        rhandsontable::hot_to_r() %>% 
        dplyr::select(-.data$remaining_calls)
        
      impexp::sqlite_export(
        golem::get_golem_options("sqlite_base"),
        update,
        "phoning_team",
        overwrite = TRUE
      )
      
      rv$df_phoning_team <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "phoning_team")
      
    }

  })
  
  observeEvent(input$import_users, {
    
    req(input$import_users)
    
    rv$df_phoning_team <- utils::read.csv(input$import_users$datapath) %>% 
      dplyr::mutate_at("survey_id", as.character)
    
    impexp::sqlite_export(
      golem::get_golem_options("sqlite_base"),
      rv$df_phoning_team,
      "phoning_team",
      overwrite = TRUE
    )

  })
  
  output$export_users <- downloadHandler(
    filename = function() {
      "phoning_team.csv"
    },
    content = function(con) {
      utils::write.csv(rv$df_phoning_team, con, na = "", row.names = FALSE, quote = FALSE)
    }
  )
  
  output$picker_phoning_attributes_participants <- renderUI({
    
    selected <- rv$df_config %>% 
      dplyr::filter(.data$key == "phoning_attributes_participants") %>% 
      tidyr::separate_rows(.data$value, sep = ";") %>% 
      dplyr::pull(.data$value)
    
    shinyWidgets::pickerInput(
      ns("phoning_attributes_participants"),
      label = "Attributes as participants columns",
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
  
  observeEvent(input$phoning_attributes_participants, ignoreNULL = FALSE, ignoreInit = TRUE, {
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("UPDATE config SET value = \"{paste0(input$phoning_attributes_participants, collapse = ';')}\" WHERE key = \"phoning_attributes_participants\";")
    )
    
    rv$df_config <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "config"
    )
    
  })
  
  output$ui_text_help <- renderUI({
    
    rv$phoning_help_text <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "config"
    ) %>% 
      dplyr::filter(.data$key == "phoning_help_text") %>% 
      dplyr::pull(.data$value)
    
    textAreaInput(
      ns("phoning_help_text"),
      label = "Help text (html format accepted)",
      value = isolate(rv$phoning_help_text),
      placeholder = "My help text",
      rows = 6
    )
    
  })
  
  observeEvent(input$phoning_help_text, {
    
    req(input$phoning_help_text != rv$phoning_help_text)
    
    rv$phoning_help_text <- input$phoning_help_text
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("UPDATE config SET value = \"{input$phoning_help_text}\" WHERE key = \"phoning_help_text\";")
    )
    
  })
  
  output$picker_attributes_groups <- renderUI({
    
    selected <- rv$df_config %>% 
      dplyr::filter(.data$key == "phoning_attributes_groups") %>% 
      tidyr::separate_rows(.data$value, sep = ";") %>% 
      dplyr::pull(.data$value)
    
    shinyWidgets::pickerInput(
      ns("attributes_groups"),
      label = "Attributes as groups",
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
  
  observeEvent(input$attributes_groups, ignoreNULL = FALSE, ignoreInit = TRUE, {
    
    impexp::sqlite_execute_sql(
      golem::get_golem_options("sqlite_base"),
      glue::glue("UPDATE config SET value = \"{paste0(input$attributes_groups, collapse = ';')}\" WHERE key = \"phoning_attributes_groups\";")
    )
    
    rv$df_config <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "config"
    )
    
  })
  
  output$ui_picker_user <- renderUI({
    
    users <- rv$df_phoning_team %>% 
      dplyr::filter(.data$user != "admin") %>% 
      dplyr::pull(.data$user)
    
    req(length(users) >= 1)
    
    shinyWidgets::pickerInput(
      ns("picker_user"),
      label = "Select users",
      choices = users,
      multiple = TRUE,
      options = list(
        "showTick" = TRUE,
        "actions-box" = TRUE,
        "dropdown-align-right" = TRUE
      )
    )
    
  })
  
  output$ui_picker_maximal_date <- renderUI({
    
    shinyWidgets::airDatepickerInput(
      ns("maximal_date"),
      label = "Maximal last event date",
      value = lubridate::today() - months(1)
    )
    
  })
  
  observeEvent(input$affect_groups, {
    
    attributes_groups <- rv$df_config %>% 
      dplyr::filter(.data$key == "phoning_attributes_groups") %>%
      tidyr::separate_rows(.data$value, sep = ";") %>%
      dplyr::pull(.data$value) %>% 
      dplyr::na_if("") %>% 
      stats::na.omit()
    
    users <- rv$df_phoning_team %>% 
      dplyr::filter(.data$user != "admin")
    
    if (!is.null(input$picker_user)) {
      users <- dplyr::filter(users, .data$user %in% input$picker_user)
    }
    
    df_groups <- rv$df_participants_filter() %>% 
      dplyr::left_join(rv$df_phoning_team_group, by = attributes_groups) %>% 
      survey.phoning::df_groups(c("survey_id", attributes_groups)) %>% 
      dplyr::semi_join(users, by = "survey_id") %>% 
      dplyr::filter(!.data$order %in% -1) %>% 
      dplyr::arrange(.data$response_rate)
    
    if (!is.null(input$maximal_date)) {
      df_groups <- dplyr::filter(df_groups, is.na(.data$last_event_date) | lubridate::as_date(.data$last_event_date) <= lubridate::as_date(input$maximal_date))
    }
    
    summary_user_groups <- function(df_groups) {
      
      df_groups %>% 
        tidyr::drop_na(.data$user) %>% 
        dplyr::filter(.data$user != "") %>% 
        dplyr::group_by(.data$user) %>% 
        dplyr::summarise_at("to_contact", sum) %>% 
        dplyr::ungroup()
      
    }
    
    while(
      (nrow(summary_user_groups(df_groups)) == 0 | any(summary_user_groups(df_groups)$to_contact < input$to_contact)) & any(is.na(df_groups$user))
    ) {

      num_line <- which(is.na(df_groups$user))[1]
      
      user <- users %>% 
        dplyr::filter(.data$survey_id %in% c(df_groups$survey_id[num_line], NA_character_)) %>% 
        dplyr::left_join(
          summary_user_groups(df_groups),
          by = "user"
        ) %>% 
        tidyr::replace_na(list(to_contact = 0L)) %>% 
        dplyr::arrange(.data$to_contact) %>% 
        dplyr::pull(.data$user) %>% 
        utils::head(1)
      
      if (length(user) == 1) {
        df_groups$user[num_line] <- user
      } else {
        df_groups <- dplyr::filter(df_groups, dplyr::row_number() != num_line)
      }
      
    }
    
    rv$df_phoning_team_group <- rv$df_phoning_team_group %>% 
      patchr::df_update(
        df_groups %>% 
          dplyr::group_by(.data$user) %>% 
          dplyr::mutate(order = dplyr::row_number()) %>% 
          dplyr::ungroup() %>% 
          dplyr::select_at(names(rv$df_phoning_team_group)),
        by = attributes_groups
      )
    
    impexp::sqlite_export(
      golem::get_golem_options("sqlite_base"),
      rv$df_phoning_team_group,
      "phoning_team_group",
      overwrite = TRUE
    )
    
  })
  
  output$hot_groups_users <- rhandsontable::renderRHandsontable({
    
    req(input$attributes_groups)
    
    attributes_groups <- rv$df_config %>% 
      dplyr::filter(.data$key == "phoning_attributes_groups") %>% 
      tidyr::separate_rows(.data$value, sep = ";") %>% 
      dplyr::pull(.data$value)
    
    if (!any(attributes_groups %in% names(rv$df_phoning_team_group))) {
      
      rv$df_phoning_team_group <- rv$df_participants_filter() %>% 
        dplyr::select_at(attributes_groups) %>% 
        unique() %>% 
        dplyr::mutate(
          user = NA_character_,
          order = NA_integer_
        )
      
    }
    
    groups_user <- rv$df_participants_filter() %>% 
      dplyr::left_join(rv$df_phoning_team_group, by = attributes_groups) %>% 
      survey.phoning::df_groups(attributes_groups)
    
    users <- rv$df_phoning_team %>% 
      dplyr::filter(.data$user != "admin") %>% 
      dplyr::pull(.data$user)
    
    req(length(users) >= 1)
    
    if (!is.null(input$picker_user)) {
      groups_user <- dplyr::filter(groups_user, .data$user %in% input$picker_user)
    }
    
    if (!is.null(input$maximal_date)) {
      groups_user <- dplyr::filter(groups_user, is.na(.data$last_event_date) | lubridate::as_date(.data$last_event_date) <= lubridate::as_date(input$maximal_date))
    }
    
    groups_user %>% 
      dplyr::select_at(c(attributes_groups, "response_rate", "n_events", "last_event_date", "user", "order", "to_contact")) %>% 
      dplyr::arrange(.data$response_rate) %>% 
      rhandsontable::rhandsontable(rowHeaders = NULL) %>%
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") %>%
      rhandsontable::hot_rows(rowHeights = 35) %>%
      rhandsontable::hot_cols(valign = "htMiddle", colWidths = c(100, 15, 15, 15, 15, 15, 15), columnSorting = TRUE) %>% 
      rhandsontable::hot_col(col = "user", type = "dropdown", source = users) %>% 
      rhandsontable::hot_col(col = c(attributes_groups, "response_rate", "n_events", "last_event_date", "to_contact"), readOnly = TRUE)
    
  })
  
  observeEvent(input$hot_groups_users, {
    
    req(input$hot_groups_users)
    
    changes <- input$hot_groups_users$changes
    
    if (!is.null(changes[["ind"]]) | !is.null(changes[["changes"]])) {
      
      update <- input$hot_groups_users %>% 
        rhandsontable::hot_to_r()
      
      attributes_groups <- rv$df_config %>% 
        dplyr::filter(.data$key == "phoning_attributes_groups") %>%
        tidyr::separate_rows(.data$value, sep = ";") %>%
        dplyr::pull(.data$value) %>% 
        dplyr::na_if("") %>% 
        stats::na.omit()
      
      rv$df_phoning_team_group <- rv$df_phoning_team_group %>% 
        patchr::df_update(
          dplyr::select_at(update, names(rv$df_phoning_team_group)), 
          by = attributes_groups
        )
      
      impexp::sqlite_export(
        golem::get_golem_options("sqlite_base"),
        rv$df_phoning_team_group,
        "phoning_team_group",
        overwrite = TRUE
      )
      
    }

  })
  
  observeEvent(input$import_groups_users, {
    
    req(input$import_groups_users)
    
    attributes_groups <- rv$df_config %>% 
      dplyr::filter(.data$key == "phoning_attributes_groups") %>%
      tidyr::separate_rows(.data$value, sep = ";") %>%
      dplyr::pull(.data$value) %>% 
      dplyr::na_if("") %>% 
      stats::na.omit()
    
    rv$df_phoning_team_group <- utils::read.csv(input$import_groups_users$datapath, fileEncoding = "UTF-8") %>% 
      dplyr::mutate_at("user", dplyr::na_if, "") %>% 
      dplyr::rename_all(stringr::str_replace_all, "\\.", " ") %>% 
      dplyr::select_at(c(attributes_groups, "user", "order"))
    
    impexp::sqlite_export(
      golem::get_golem_options("sqlite_base"),
      rv$df_phoning_team_group,
      "phoning_team_group",
      overwrite = TRUE
    )

  })
  
  output$export_groups_users <- downloadHandler(
    filename = function() {
      "phoning_team_group.csv"
    },
    content = function(con) {
      
      attributes_groups <- rv$df_config %>% 
        dplyr::filter(.data$key == "phoning_attributes_groups") %>%
        tidyr::separate_rows(.data$value, sep = ";") %>%
        dplyr::pull(.data$value) %>% 
        dplyr::na_if("") %>% 
        stats::na.omit()
      
      rv$df_phoning_team_group %>% 
        dplyr::select(attributes_groups, .data$user, .data$order, .data$to_contact) %>% 
        utils::write.csv(con, na = "", row.names = FALSE)
    }
  )
  
  output$ui_select_users_hours_date <- renderUI({
    
    last_date <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "phoning_team_events"
    ) %>% 
      dplyr::arrange(.data$date) %>% 
      utils::tail(1) %>% 
      dplyr::pull(.data$date)
    
    shinyWidgets::airDatepickerInput(
      ns("users_hours_date"),
      label = "Date",
      value = last_date
    )
    
  })
  
  output$ui_select_user_hours <- renderUI({
    
    users <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "phoning_team_events"
    ) %>% 
      dplyr::filter(.data$user != "admin") %>% 
      dplyr::pull(.data$user) %>% 
      unique() %>% sort()
    
    selectInput(
      ns("select_user_hours"),
      label = "User",
      choices = users
    )
    
  })
  
  output$dt_users_hours <- DT::renderDT({
    
    req(input$select_user_hours)
    
    phoning_team_events <- impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "phoning_team_events"
    )
    
    users_hours <- phoning_team_events %>% 
      dplyr::bind_rows(
        impexp::r_import(golem::get_golem_options("cron_responses")) %>% 
          dplyr::filter(.data$completed) %>% 
          dplyr::inner_join(
            phoning_team_events,
            by = "token"
          ) %>% 
          dplyr::select(.data$token, datetime = .data$submitdate, .data$user) %>% 
          dplyr::mutate(
            type = "validation_questionnaire",
            date = substr(.data$datetime, 1, 10)
          )
      ) %>% 
      dplyr::arrange(.data$date, .data$user, .data$datetime) %>% 
      unique() %>% 
      dplyr::filter(
        .data$user == input$select_user_hours,
        .data$date == substr(.data$datetime, 1, 10),
        lubridate::hour(lubridate::ymd_hms(.data$datetime)) >= 17,
        lubridate::hour(lubridate::ymd_hms(.data$datetime)) <= 20,
        .data$date != "2020-01-22" # DUT TC
      ) %>% 
      dplyr::mutate(
        diff = round((dplyr::lag(.$datetime) %--% .$datetime) / lubridate::dminutes(), .1),
        diff = dplyr::if_else(.data$type == "validation_questionnaire", NA_real_, .data$diff)
      ) %>% 
      dplyr::group_by(.data$user, .data$date) %>%
      dplyr::mutate(diff = ifelse(dplyr::row_number() == 1, NA_real_, .data$diff)) %>%
      dplyr::ungroup() %>% 
      dplyr::select(.data$date, .data$user, .data$datetime, .data$type, .data$diff)
    
    if (!is.null(input$users_hours_date)) {
      users_hours <- dplyr::filter(users_hours, .data$date == input$users_hours_date)
    }
    
    users_hours %>% 
      DT::datatable(
        rownames = FALSE
      )
    
  })
  
  output$dt_contacts_update_log <- DT::renderDT({
    
    impexp::sqlite_import(
      golem::get_golem_options("sqlite_base"),
      "phoning_crowdsourcing_log"
    ) %>% 
      dplyr::filter(is.na(.data$status)) %>% 
      DT::datatable(
        rownames = FALSE
      )
    
  })
  
}
