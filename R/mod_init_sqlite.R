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
          "api_key_spothit",
          "participants_dt_attributes",
          "phoning_attributes_groups",
          "phoning_attributes_participants",
          "phoning_help_text"
        )
      ) %>% 
        dplyr::mutate(value = character(nrow(.))),
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
  
  if (! "crowdsourcing_contributors" %in% impexp::sqlite_list_tables(sqlite_base)) {
    
    impexp::sqlite_export(
      sqlite_base, 
      dplyr::tibble(
        user = character(0),
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
        display = logical(0),
        order = integer(0),
        edit = logical(0),
        filter = logical(0),
        restriction = logical(0)
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
        key = c("dt_attributes", "search_text_input", "invitation_text_fr", "invitation_text_en", "survey_text_fr", "survey_text_en")
      ) %>% 
        dplyr::mutate(value = character(nrow(.))),
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
  
  if (! "phoning_team" %in% impexp::sqlite_list_tables(sqlite_base)) {
    
    impexp::sqlite_export(
      sqlite_base, 
      dplyr::tibble(
        name = "Administrator",
        user = "admin",
        password = NA_character_,
        survey_id = NA_character_
      ),
      "phoning_team"
    )
  }
  
  if (! "phoning_team_group" %in% impexp::sqlite_list_tables(sqlite_base)) {
    
    impexp::sqlite_export(
      sqlite_base, 
      dplyr::tibble(
        user = character(0),
        order = integer(0)
      ),
      "phoning_team_group"
    )
  }
  
  rv$df_config <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "config")
  rv$df_surveys <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "surveys")
  rv$df_participants <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "participants", check.names = FALSE) %>% 
    dplyr::rename_all(stringr::str_replace_all, "\\.", " ")
  rv$df_participants_attributes <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "participants_attributes") %>% 
    dplyr::mutate(num_attribute = as.integer(stringr::str_match(.data$attribute, "ATTRIBUTE_(\\d+)")[, 2])) %>% 
    dplyr::arrange(.data$num_attribute)
  
  rv$df_participants_contacts <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "participants_contacts")
  rv$df_participants_events <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "participants_events")
  rv$df_email_domains <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "email_domains")
  rv$df_crowdsourcing_columns <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "crowdsourcing_columns") %>% 
    dplyr::mutate_at(c("display", "edit", "filter", "restriction"), as.logical)
  rv$df_crowdsourcing_contributors <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "crowdsourcing_contributors")
  rv$df_crowdsourcing_mail_template <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "crowdsourcing_mail_template")
  rv$df_mail_template <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "mail_template")
  
  rv$df_linkedin <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "linkedin")
  
  rv$df_sms_template <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "sms_template")
  
  rv$df_phoning_team <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "phoning_team") %>% 
    dplyr::mutate_at("user", dplyr::na_if, "") %>% 
    dplyr::mutate_at("survey_id", as.character)
  rv$df_phoning_team_group <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "phoning_team_group") %>% 
    dplyr::mutate_at("user", dplyr::na_if, "") %>% 
    dplyr::rename_all(stringr::str_replace_all, "\\.", " ")

}
