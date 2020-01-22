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
    fileInput(
      ns("import_contacts"),
      "Import conctacts file"
    ),
    downloadButton(
      ns("export_contacts"),
      "Export contacts",
      icon = icon("file-export")
    )
  )
}
    
# Module Server
    
#' @rdname mod_import_contacts
#' @export
#' @keywords internal
    
mod_import_contacts_server <- function(input, output, session, rv){
  ns <- session$ns
  
  observeEvent(input$import_contacts, {
    
    req(input$import_contacts)
    
    import_contacts <- read.csv(input$import_contacts$datapath, na.strings = "") %>% 
      dplyr::semi_join(rv$df_participants, by = "token")
    
    rv$df_participants_contacts <- import_contacts %>% 
      dplyr::select(names(impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "participants_contacts")))

    impexp::sqlite_export(
      path = golem::get_golem_options("sqlite_base"), 
      data = rv$df_participants_contacts, 
      table_name = "participants_contacts",
      overwrite = TRUE
    )

  })
  
  output$export_contacts <- downloadHandler(
    filename = function() {
      "participants_contacts.csv"
    },
    content = function(con) {
       write.csv(rv$df_participants_contacts, con, row.names = FALSE, na = "", quote = FALSE)
    }
  )

}
