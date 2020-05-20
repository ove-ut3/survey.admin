#' @import shiny
app_server <- function(input, output, session) {
  
  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(golem::get_golem_options("credentials"))
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  rv <- reactiveValues()
  
  callModule(mod_init_sqlite_server, "init_sqlite_ui", rv)
  
  config <- impexp::sqlite_import(
    golem::get_golem_options("sqlite_base"),
    "config"
  ) %>%
    dplyr::filter(stringr::str_detect(.data$key, "^lime_")) %>%
    split(x = .$value, f = .$key)

  options(lime_api = config$lime_api)
  options(lime_username = config$lime_username)
  options(lime_password = config$lime_password)
  
  callModule(mod_config_limesurvey_server, "config_limesurvey_ui_1", rv)
  callModule(mod_config_limesurvey_server, "config_limesurvey_ui_2", rv)
  callModule(mod_config_api_server, "config_api_ui", rv)
  
  callModule(mod_surveys_server, "surveys_ui", rv)
  
  callModule(mod_filters_server, "filters_ui", rv)
  callModule(mod_import_contacts_server, "import_contacts_ui", rv)
  
  df_participants_attributes <- impexp::sqlite_import(
    golem::get_golem_options("sqlite_base"),
    "participants_attributes"
  ) %>%
    dplyr::filter(.data$description != janitor::make_clean_names(.data$description))
  labels <- df_participants_attributes$description
  names(labels) <- df_participants_attributes$description %>%
    janitor::make_clean_names()

  callModule(
    shiny.modules::selected_filters_server, "selected_filters_ui",
    group_inputs = rv$filter_inputs,
    labels = c(
      survey_title = "Survey title",
      labels
    )
  )
  
  callModule(mod_participants_server, "participants_ui", rv)

  callModule(mod_crowdsourcing_server, "crowdsourcing_ui", rv)

  callModule(mod_email_validation_server, "email_validation_ui", rv)

  callModule(mod_mailing_server, "mailing_ui", rv)

  callModule(mod_linkedin_server, "linkedin_ui", rv)

  callModule(mod_phoning_server, "phoning_ui", rv)
  
  callModule(mod_incomplete_responses_server, "incomplete_responses_ui", rv)

}
