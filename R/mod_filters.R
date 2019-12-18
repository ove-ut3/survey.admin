# Module UI
  
#' @title   mod_filters_ui and mod_filters_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_filters
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_filters_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("filters")),
    div("", style = "height: 10px;"),
    hr(),
    uiOutput(ns("picker_attributes")),
    hr(),
    uiOutput(ns("picker_contacts"))
  )
}
    
# Module Server
    
#' @rdname mod_filters
#' @export
#' @keywords internal
    
mod_filters_server <- function(input, output, session, rv){
  ns <- session$ns
  
  output$filters <- renderUI({

    list_init <- list(
      survey_title = list(inputId = "survey_title", title = "Survey title :"),
      optout = list(inputId = "optout", title = "OptOut :"),
      completed = list(inputId = "completed", title = "Completed :")
    )
    
    if (!is.null(input[["attributes-selectized"]])) {
      
      params_attributes <- dplyr::tibble(
        inputId = input[["attributes-selectized"]] %>% 
          patchr::str_normalise_colnames(),
        title = paste0(input[["attributes-selectized"]], " :")
      ) %>%
        split(f = 1:nrow(.)) %>%
        purrr::map(as.list)
      
      names(params_attributes) <- input[["attributes-selectized"]] %>% 
        patchr::str_normalise_colnames()
      
      vars_attributes <- input[["attributes-selectized"]] %>% 
        patchr::str_normalise_colnames()
      
    } else {
      params_attributes <- NULL
      vars_attributes <- NULL
    }
    
    if (!is.null(input[["contacts-selectized"]])) {
      
      params_contacts <- dplyr::tibble(
        inputId = input[["contacts-selectized"]] %>% 
          patchr::str_normalise_colnames(),
        title = paste0(input[["contacts-selectized"]], " :")
      ) %>%
        split(f = 1:nrow(.)) %>%
        purrr::map(as.list)
      
      names(params_contacts) <- input[["contacts-selectized"]] %>% 
        patchr::str_normalise_colnames()
      
      vars_contacts <- input[["contacts-selectized"]] %>% 
        patchr::str_normalise_colnames()
      
    } else {
      params_contacts <- NULL
      vars_contacts <- NULL
    }
    
    params <- c(list_init, params_attributes, params_contacts)
    vars <- c("survey_title", "optout", "completed", vars_attributes, vars_contacts)
    
    one_cellphone <- rv$dt_participants_contacts %>% 
      dplyr::filter(stringr::str_detect(value, "^0[67] ")) %>% 
      dplyr::select(token) %>% 
      unique() %>% 
      dplyr::mutate(one_cellphone = TRUE)
    
    one_valid_email <- rv$dt_participants_contacts %>% 
      dplyr::filter(key == "email") %>% 
      dplyr::semi_join(
        rv$dt_email_validation %>% 
          dplyr::filter(status %in% c("valid", "unknown")),
        by = c("value" = "email")
      ) %>% 
      dplyr::select(token) %>% 
      unique() %>% 
      dplyr::mutate(one_valid_email = TRUE)
    
    picker_contacts <- rv$dt_participants %>% 
      dplyr::select(token) %>% 
      dplyr::left_join(one_cellphone, by = "token") %>% 
      dplyr::left_join(one_valid_email, by = "token") %>% 
      tidyr::replace_na(list(one_cellphone = FALSE, one_valid_email = FALSE))
    
    # shinyWidgets::selectizeGroupServer does not accept column names with space
    rv$dt_participants_filter <- callModule(
      module = shinyWidgets::selectizeGroupServer,
      id = "filter_attributes",
      data = rv$dt_participants %>% 
        dplyr::left_join(rv$dt_surveys, by = "survey_id") %>% 
        dplyr::left_join(
          golem::get_golem_options("cron_responses") %>% 
            impexp::r_import(),
          by = c("survey_id", "token")
        ) %>% 
        dplyr::left_join(picker_contacts, by = "token") %>% 
        patchr::normalise_colnames() %>% 
        dplyr::mutate_if(is.logical, as.character),
      vars = vars
    )

    shinyWidgets::selectizeGroupUI(
      ns("filter_attributes"),
      params = params,
      inline = FALSE
    )

  })

  output$picker_attributes <- renderUI({

    # id Important to be finished by '-selectized' !
    shinyWidgets::pickerInput(
      ns("attributes-selectized"),
      label = "Set attributes as filters",
      choices = rv$dt_participants_attributes$description,
      multiple = TRUE,
      options = list("showTick" = TRUE,
                     "actions-box" = TRUE,
                     "dropdown-align-right" = TRUE),
      choicesOpt = list(
        subtext = paste("- ", rv$dt_participants_attributes$attribute)
      )
    )
  })
  
  output$picker_contacts <- renderUI({
    
    # id Important to be finished by '-selectized' !
    shinyWidgets::pickerInput(
      ns("contacts-selectized"),
      label = "Set contacts as filters",
      choices = c("one_cellphone", "one_valid_email"),
      multiple = TRUE,
      options = list("showTick" = TRUE,
                     "actions-box" = TRUE,
                     "dropdown-align-right" = TRUE)
    )
  })
  
  rv$filter_inputs <- input

  }
