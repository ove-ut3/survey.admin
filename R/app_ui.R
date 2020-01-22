#' @import shiny shinydashboard shinydashboardPlus
app_ui <- function() {
  
  ui <- tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    dashboardPagePlus(
      title = "Limesurvey manager",
      dashboardHeaderPlus(
        left_menu = tagList(
          div(
            "Limesurvey manager",
            style = "font-size: 22px; color: white;"
          )
        ),
        enable_rightsidebar = TRUE,
        rightSidebarIcon = "gears"
      ),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Surveys", tabName = "tab_surveys", icon = icon("clipboard-list")),
          menuItem("Participants", tabName = "tab_participants", icon = icon("user")),
          menuItem("Crowdsourcing", tabName = "tab_crowdsourcing", icon = icon("plus")),
          menuItem("Email validation", tabName = "tab_email_validation", icon = icon("at")),
          menuItem("Mailing", tabName = "tab_mailing", icon = icon("paper-plane")),
          menuItem("Linkedin", tabName = "tab_linkedin", icon = icon("linkedin", class = "fa fa-linkedin")),
          menuItem("SMS sending", tabName = "tab_sms_sending", icon = icon("mobile")),
          menuItem("Phoning team", tabName = "tab_phoning", icon = icon("headset")),
          menuItem("Incomplete responses", tabName = "tab_incomplete_responses"),
          menuItem("Responses stats", tabName = "tab_reponses_stats", icon = icon("chart-bar")),
          hr(),
          menuItem("Selected filters", icon = icon("filter")),
          shiny.modules::selected_filters_ui("selected_filters_ui")
        )
      ),
      rightsidebar = rightSidebar(
        background = "dark", 
        width = 250,
        rightSidebarTabContent(
          id = 1,
          icon = "filter",
          active = TRUE,
          mod_filters_ui("filters_ui")
        ),
        rightSidebarTabContent(
          id = 2,
          icon = "lemon",
          mod_config_limesurvey_ui("config_limesurvey_ui_2")
        ),
        rightSidebarTabContent(
          id = 3,
          icon = "file-upload",
          mod_import_contacts_ui("import_contacts_ui")
        ),
        rightSidebarTabContent(
          id = 4,
          icon = "link",
          mod_config_api_ui("config_api_ui")
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(
            tabName = "tab_surveys",
            mod_surveys_ui("surveys_ui")
          ),
          tabItem(
            tabName = "tab_participants",
            mod_participants_ui("participants_ui")
          ),
          tabItem(
            tabName = "tab_email_validation",
            mod_email_validation_ui("email_validation_ui")
          ),
          tabItem(
            tabName = "tab_crowdsourcing",
            mod_crowdsourcing_ui("crowdsourcing_ui")
          ),
          tabItem(
            tabName = "tab_mailing",
            mod_mailing_ui("mailing_ui")
          ),
          tabItem(
            tabName = "tab_linkedin",
            mod_linkedin_ui("linkedin_ui")
          ),
          tabItem(
            tabName = "tab_sms_sending",
            mod_sms_sending_ui("sms_sending_ui")
          ),
          tabItem(
            tabName = "tab_phoning",
            mod_phoning_ui("phoning_ui")
          ),
          tabItem(
            tabName = "tab_incomplete_responses",
            mod_incomplete_responses_ui("incomplete_responses_ui")
          )
        )
      )
    )
  )
  
  ui <- shinymanager::secure_app(ui)
  
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'survey.admin')
  )
 
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    # Add here all the external resources
    shinyalert::useShinyalert(),
    rclipboard::rclipboardSetup(),
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
