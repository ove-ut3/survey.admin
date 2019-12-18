# Module UI
  
#' @title   mod_import_contacts_ui and mod_import_contacts_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_import_contacts
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_import_contacts_ui <- function(id){
  ns <- NS(id)
  tagList(
    fileInput(ns("import_contacts"), "Import conctacts file"),
    downloadButton(ns("export_contacts"), "Export contacts", icon = icon("file-export"))
  )
}
    
# Module Server
    
#' @rdname mod_import_contacts
#' @export
#' @keywords internal
    
mod_import_contacts_server <- function(input, output, session, rv){
  ns <- session$ns
  
  observeEvent(input$import_contacts, {
    
    if (!is.null(input$import_contacts)) {

      import_contacts <- read.csv(input$import_contacts$datapath, na.strings = "") %>% 
        dplyr::semi_join(rv$dt_participants, by = "token")
      
      rv$dt_participants_contacts <- import_contacts %>% 
        dplyr::select(names(impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "participants_contacts")))

      if (nrow(impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "participants_contacts")) >= 1) {
        impexp::sqlite_execute_sql(golem::get_golem_options("sqlite_base"), "DELETE FROM participants_contacts;")
      }
      
      impexp::sqlite_append_rows(
        path = golem::get_golem_options("sqlite_base"), 
        data = rv$dt_participants_contacts, 
        table_name = "participants_contacts"
      )
      
      if (all(c("service", "status", "status_date") %in% names(import_contacts))) {
        
        rv$dt_email_validation <- import_contacts %>% 
          dplyr::filter(key == "email") %>% 
          dplyr::select(-date) %>% 
          dplyr::rename(email = value, date = status_date) %>% 
          dplyr::select(names(impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "email_validation"))) %>% 
          tidyr::replace_na(list(status = "missing")) %>% 
          unique()
        
      } else {
        
        rv$dt_email_validation <- import_contacts %>% 
          dplyr::filter(key == "email") %>% 
          dplyr::select(email = value) %>% 
          dplyr::mutate(
            service = NA_character_,
            status = NA_character_,
            date = NA_character_
          )
        
      }
      
      impexp::sqlite_export(
        golem::get_golem_options("sqlite_base"),
        rv$dt_email_validation,
        "email_validation",
        overwrite = TRUE
      )
      
    }

  })
  
  output$export_contacts <- downloadHandler(
    filename = function() {
      "participants_contacts.csv"
    },
    content = function(con) {
      rv$dt_participants_contacts %>% 
        dplyr::left_join(
          rv$dt_email_validation %>% 
            dplyr::rename(status_date = date),
          by = c("value" = "email")
        ) %>% 
        write.csv(con, row.names = FALSE, na = "", quote = FALSE)
    }
  )

}
