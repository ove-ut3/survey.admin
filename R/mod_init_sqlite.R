# Module UI
  
#' @title   mod_init_sqlite_ui and mod_init_sqlite_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_init_sqlite
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_init_sqlite_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_init_sqlite
#' @export
#' @keywords internal
    
mod_init_sqlite_server <- function(input, output, session, rv){
  ns <- session$ns
  
  sqlite_base <- golem::get_golem_options("sqlite_base")
  
  impexp::sqlite_create(sqlite_base)
  
  if (! "config" %in% impexp::sqlite_list_tables(sqlite_base)) {
    
    impexp::sqlite_export(
      sqlite_base, 
      dplyr::tibble(
        key = c(
          "lime_api", 
          "lime_username", 
          "lime_password",
          "api_key_bulkemailchecker",
          "api_key_listflow",
          "api_key_quickemailverification",
          "api_key_emailmarker",
          "api_key_spothit"
        )
      ) %>% 
        dplyr::mutate(value = ""),
      "config"
    )
  }
  
  if (! "surveys" %in% impexp::sqlite_list_tables(sqlite_base)) {
    
    impexp::sqlite_export(
      sqlite_base, 
      dplyr::tibble(
        survey_id = character(0),
        survey_title = character(0),
        expires = character(0)
      ),
      "surveys"
    )
  }
  
  if (! "participants" %in% impexp::sqlite_list_tables(sqlite_base)) {
    
    impexp::sqlite_export(
      sqlite_base, 
      dplyr::tibble(
        survey_id = character(0),
        token = character(0),
        firstname = character(0),
        lastname = character(0)
      ),
      "participants"
    )
  }
  
  if (! "participants_attributes" %in% impexp::sqlite_list_tables(sqlite_base)) {
    
    impexp::sqlite_export(
      sqlite_base, 
      dplyr::tibble(
        attribute = character(0),
        description = character(0),
        survey_id = character(0)
      ),
      "participants_attributes"
    )
  }
  
  if (! "participants_contacts" %in% impexp::sqlite_list_tables(sqlite_base)) {
    
    impexp::sqlite_export(
      sqlite_base, 
      dplyr::tibble(
        token = character(0),
        key = character(0),
        value = character(0),
        source = character(0),
        date = character(0)
      ),
      "participants_contacts"
    )
  }
  
  if (! "participants_events" %in% impexp::sqlite_list_tables(sqlite_base)) {
    
    impexp::sqlite_export(
      sqlite_base, 
      dplyr::tibble(
        token = character(0),
        type = character(0),
        comment = character(0),
        date = character(0)
      ),
      "participants_events"
    )
  }
  
  if (! "email_domains" %in% impexp::sqlite_list_tables(sqlite_base)) {
    
    impexp::sqlite_export(
      sqlite_base,
      dplyr::tibble(
        domain = character(0),
        n = numeric(0),
        status = character(0)
      ),
      "email_domains"
    )
  }
  
  if (! "email_validation" %in% impexp::sqlite_list_tables(sqlite_base)) {
    
    impexp::sqlite_export(
      sqlite_base, 
      dplyr::tibble(
        email = character(0),
        service = integer(0),
        status = character(0),
        date = character(0)
      ),
      "email_validation"
    )
  }
  
  if (! "crowdsourcing_contributors" %in% impexp::sqlite_list_tables(sqlite_base)) {
    
    impexp::sqlite_export(
      sqlite_base, 
      dplyr::tibble(
        email = character(0),
        password = character(0)
      ),
      "crowdsourcing_contributors"
    )
  }
  
  if (! "crowdsourcing_columns" %in% impexp::sqlite_list_tables(sqlite_base)) {
    
    impexp::sqlite_export(
      sqlite_base, 
      dplyr::tibble(
        column = character(0),
        description = character(0),
        description_new = character(0),
        restriction = integer(0),
        display = integer(0),
        edit = integer(0),
        filter = integer(0)
      ),
      "crowdsourcing_columns"
    )
  }
  
  if (! "crowdsourcing_mail_template" %in% impexp::sqlite_list_tables(sqlite_base)) {
    
    impexp::sqlite_export(
      sqlite_base, 
      dplyr::tibble(
        key = c("sender_email", "sender_alias", "subject", "body"),
        value = character(4)
      ),
      "crowdsourcing_mail_template"
    )
  }
  
  if (! "mail_template" %in% impexp::sqlite_list_tables(sqlite_base)) {
    
    impexp::sqlite_export(
      sqlite_base, 
      dplyr::tibble(
        key = c("sender_email", "sender_alias", "subject", "body"),
        value = character(4)
      ),
      "mail_template"
    )
  }
  
  if (! "linkedin" %in% impexp::sqlite_list_tables(sqlite_base)) {
    
    impexp::sqlite_export(
      sqlite_base, 
      dplyr::tibble(
        key = c("search_text_input"),
        value = character(1)
      ),
      "linkedin"
    )
  }
  
  if (! "sms_template" %in% impexp::sqlite_list_tables(sqlite_base)) {
    
    impexp::sqlite_export(
      sqlite_base, 
      dplyr::tibble(
        key = c("sender", "body"),
        value = character(2)
      ),
      "sms_template"
    )
  }
  
  rv$dt_config <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "config")
  rv$dt_surveys <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "surveys")
  rv$dt_participants <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "participants", check.names = FALSE)
  rv$dt_participants_attributes <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "participants_attributes") %>% 
    dplyr::mutate(num_attribute = as.integer(stringr::str_match(attribute, "ATTRIBUTE_(\\d+)")[, 2])) %>% 
    dplyr::arrange(num_attribute)
  
  rv$dt_participants_contacts <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "participants_contacts")
  rv$dt_participants_events <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "participants_events")
  rv$dt_email_domains <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "email_domains")
  rv$dt_email_validation <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "email_validation") %>% 
    tidyr::replace_na(list(status = "missing"))
  rv$dt_crowdsourcing_columns <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "crowdsourcing_columns") %>% 
    dplyr::mutate_if(is.integer, as.logical)
  rv$dt_crowdsourcing_contributors <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "crowdsourcing_contributors")
  rv$dt_crowdsourcing_mail_template <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "crowdsourcing_mail_template")
  rv$dt_mail_template <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "mail_template")
  rv$dt_sms_template <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "sms_template")

}
