# Module UI
  
#' @title   mod_linkedin_ui and mod_linkedin_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_linkedin
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_linkedin_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Participants", width = 10,
        DT::DTOutput(ns("dt_participants"))
      ),
      box(
        title = "Search participant", width = 2,
        uiOutput(ns("search")),
        uiOutput(ns("search_suffix"))
      ),
      box(
        title = "Invitation text", width = 2,
        uiOutput(ns("invitation_fr")),
        uiOutput(ns("invitation_en")),
        fluidRow(div(style = "height: 15px;", "")),
        uiOutput(ns("save_invitation"))
      ),
      box(
        title = "Survey text", width = 2,
        uiOutput(ns("survey_link_fr")),
        uiOutput(ns("survey_link_en")),
        uiOutput(ns("survey_link_fr_2")),
        fluidRow(div(style = "height: 15px;", "")),
        uiOutput(ns("save_survey_link"))
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_linkedin
#' @export
#' @keywords internal
    
mod_linkedin_server <- function(input, output, session, rv){
  ns <- session$ns
  
  output$dt_participants <- DT::renderDataTable({
    
    rv$dt_participants_filter() %>%
      dplyr::select(token, firstname, lastname, libelle_diplome) %>%
      DT::datatable(
        selection = list(mode = 'single', selected = 1),
        rownames = FALSE,
        options = list(
          pageLength = -1,
          dom = 'rft',
          scrollY = '75vh'
        )
      )
    
  })
  
  proxy <- DT::dataTableProxy("dt_participants")
  
  observeEvent(input$dt_participants_search, ignoreInit = TRUE, {
    
    if (!is.null(input$dt_participants_rows_current)) {
      
      DT::selectRows(proxy, input$dt_participants_rows_current[1])
      
    }
    
  })
  
  output$search <- renderUI({
    
    req(input$dt_participants_rows_selected)
    
    linkedin <- rv$dt_participants_filter() %>%
      dplyr::filter(dplyr::row_number() == input$dt_participants_rows_selected)
    
    actionButton(ns("button_search"), "Search", icon = icon("search"), onclick = paste0("window.open('", paste0("https://www.linkedin.com/search/results/index/?keywords=", linkedin$firstname, "%20", linkedin$lastname, "%20", input$search_suffix_text), "', '_blank')"))
    
  })
  
  # observeEvent(input$search_suffix_text, {
  # 
  #   browser()
  # 
  #   impexp::sqlite_execute_sql(
  #     golem::get_golem_options("sqlite_base"),
  #     paste0("UPDATE linkedin SET search_text_input = \"", input$search_suffix_text, "\";")
  #   )
  # 
  # })
  
  output$search_suffix <- renderUI({

    req(input$dt_participants_rows_selected)

    value <- impexp::sqlite_import(golem::get_golem_options("sqlite_base"), "linkedin") %>%
      dplyr::pull(value)

    textInput(
      ns("search_suffix_text"),
      label = NULL,
      value = value,
      placeholder = "Search suffix (for example, a city to filter results)"
    )

  })
  
  output$invitation_fr <- renderUI({
    
    req(input$dt_participants_rows_selected)
    
    linkedin <- rv$dt_participants_filter() %>%
      dplyr::filter(dplyr::row_number() == input$dt_participants_rows_selected)
    
    lib_type_diplome <- stringr::str_match(linkedin$libelle_diplome, "^(\\w+) ")[, 2]
    
    texte <- glue::glue(
      "Bonjour {linkedin$firstname} {linkedin$lastname},",
      "Vous êtes {ifelse(linkedin$sexe == 'Femme', 'diplômée', 'diplômé')} de L’UPS en {lib_type_diplome} en 2017.",
      .sep = "\n"
    )
    
    if (is.na(linkedin$type_diplome_precedent)) {
      texte <- glue::glue(
        texte,
        "Qu’êtes-vous {ifelse(linkedin$sexe == 'Femme', 'devenue', 'devenu')} ?",
        "Votre parcours nous intéresse !",
        .sep = "\n"
      )
      
    } else {
      texte <- glue::glue(
        texte,
        "L’année dernière, nous vous avions {ifelse(linkedin$sexe == 'Femme', 'interrogée', 'interrogé')} suite à votre {linkedin$type_diplome_precedent}, votre situation a-t-elle changé ?",
        .sep = "\n"
      )
    }

    rclipboard::rclipButton(ns("button_linkedin_1"), "Invitation (fr)", iconv(texte, from = "UTF-8"), icon("clipboard"))
    
  })
  
  output$invitation_en <- renderUI({

    req(input$dt_participants_rows_selected)

    linkedin <- rv$dt_participants_filter() %>%
      dplyr::filter(dplyr::row_number() == input$dt_participants_rows_selected)
    
    lib_type_diplome <- stringr::str_match(linkedin$libelle_diplome, "^(\\w+) ")[, 2]

    texte <- glue::glue(
      "Hello {linkedin$firstname} {linkedin$lastname},",
      "You graduated at University Paul Sabatier in 2017.",
      .sep = "\n"
    )

    if (!is.na(linkedin$type_diplome_precedent)) {
      texte <- glue::glue(
        texte,
        "Last year, you answered to our survey about your {linkedin$type_diplome_precedent}, did your situation evolve ?",
        .sep = "\n"
      )

    } else {
      texte <- glue::glue(
        texte,
        "What did you become ?",
        "We are interested in your profile !",
        .sep = "\n"
      )
    }

    rclipboard::rclipButton(ns("button_linkedin_1_en"), "Invitation (en)", texte, icon("clipboard"))

  })
  
  output$save_invitation <- renderUI({
    
    req(input$dt_participants_rows_selected)
    
    actionButton(ns("button_save_invitation"), "Save invitation", icon = icon("save"))
    
  })
  
  observeEvent(input$button_save_invitation, {
    
    req(input$dt_participants_rows_selected)
    
    linkedin <- rv$dt_participants_filter() %>%
      dplyr::filter(dplyr::row_number() == input$dt_participants_rows_selected)
    
    browser()
    
    event <- dplyr::tibble(
      token = linkedin$token,
      type = "linkedin",
      comment = "invitation",
      date = as.character(lubridate::today())
    ) %>% 
      dplyr::anti_join(
        impexp::sqlite_import(
          golem::get_golem_options("sqlite_base"),
          "participants_events"
        ),
        by = c("token", "type", "comment")
      )
    
    if (nrow(event) == 1) {
      
      impexp::sqlite_append_rows(
        golem::get_golem_options("sqlite_base"),
        event,
        "participants_events"
      )
      
      rv$dt_participants_events <- dplyr::bind_rows(
        rv$dt_participants_events,
        event
      )
      
    }
    
  })
  
  # Placé avant survey_fr car ça bugue sinon...
  output$survey_link_en <- renderUI({

    req(input$dt_participants_rows_selected)
    
    linkedin <- rv$dt_participants_filter() %>%
      dplyr::filter(dplyr::row_number() == input$dt_participants_rows_selected)

    lib_type_diplome <- stringr::str_match(linkedin$libelle_diplome, "^(\\w+) ")[, 2]
    
    texte <- glue::glue(
      "Hello {linkedin$firstname} {linkedin$lastname},",
      
      "The University Toulouse Paul Sabatier carries a survey about the profile of the {lib_type_diplome} graduated students in 2017, in order to know better what they did since they got thier diploma. The outcomes of this survey are very useful in order to inform the students interested by the {lib_type_diplome} degree  you passed two years ago.",
      
      "We take the liberty of contacting you to participate in this study that will only take you a few minuts:\n{linkedin$surveyurl}{ifelse(lib_type_diplome %in% c('LP', 'Master'), 'lang=en&', '')}",
      
      # "If you encounter any difficulty, feel free to contact us at +335.61.55.82.27 from Monday to Friday, from 2 pm to 8 pm from 14 to 25 january and from 28 january from 9 am to 6 pm . We will be happy to help you.",
      
      "Best regards and all the best in your projects.",
      
      "The University Paul Sabatier Team", .sep = "\n\n"
    )

    rclipboard::rclipButton(ns("button_linkedin_2_en"), "Survey link (en)", texte, icon("clipboard"))

  })

  output$survey_link_fr <- renderUI({

    req(input$dt_participants_rows_selected)

    linkedin <- rv$dt_participants_filter() %>%
      dplyr::filter(dplyr::row_number() == input$dt_participants_rows_selected)

    lib_type_diplome <- stringr::str_match(linkedin$libelle_diplome, "^(\\w+) ")[, 2]
    
    texte <- glue::glue(
      "Bonjour {linkedin$firstname} {linkedin$lastname},",
      
      "L’OVE de l’Université Paul Sabatier interroge, dans le cadre d’une étude nationale, les anciens diplômés de {lib_type_diplome} en 2017 afin de connaître leur parcours depuis l’obtention de ce diplôme. Le résultat de cette enquête est précieux afin d’informer les étudiants intéressés par la formation que vous avez suivie il y a 2 ans.",
      
      "Nous nous permettons de solliciter votre participation à cette étude qui ne vous prendra que quelques minutes :\n{linkedin$surveyurl}",
      
      # "Si vous rencontrez des difficultés, n'hésitez pas à nous contacter au 05.61.55.82.27 du lundi au vendredi de 14h à 20h du 14 au 25/01/2019 puis à partir du 28/01/2019 de 9 h à 18 h. Nous serons ravis de pouvoir vous porter assistance.",
      
      "Bien à vous et bonne continuation dans tous vos projets.",
      
      "L'équipe de l'Observatoire de la Vie Etudiante de l'Université Toulouse III - Paul Sabatier", .sep = "\n\n"
    )

    rclipboard::rclipButton(ns("button_linkedin_2"), "Survey link (fr)", iconv(texte, from = "UTF-8"), icon("clipboard"))

  })

  output$survey_link_fr_2 <- renderUI({

    req(input$dt_participants_rows_selected)

    linkedin <- rv$dt_participants_filter() %>%
      dplyr::filter(dplyr::row_number() == input$dt_participants_rows_selected)

    lib_type_diplome <- stringr::str_match(linkedin$libelle_diplome, "^(\\w+) ")[, 2]
    
    texte <- glue::glue(
      "Bonjour {linkedin$firstname} {linkedin$lastname},",
      
      "Merci beaucoup de votre retour.",
      
      "Pour que notre étude soit optimale, nous vous invitons à répondre à notre questionnaire :\n{linkedin$surveyurl}",
      
      # "Si vous rencontrez des difficultés, n'hésitez pas à nous contacter au 05.61.55.82.27 du lundi au vendredi de 14h à 20h du 14 au 25/01/2019 puis à partir du 28/01/2019 de 9 h à 18 h. Nous serons ravis de pouvoir vous porter assistance.",
      
      "Bien à vous et bonne continuation dans tous vos projets.",
      
      "L'équipe de l'Observatoire de la Vie Etudiante de l'Université Toulouse III - Paul Sabatier", .sep = "\n\n"
    )

    rclipboard::rclipButton(ns("button_linkedin_3"), HTML("Survey link (fr)<br/>(parcours exposé)"), iconv(texte, from = "UTF-8"), icon("clipboard"))

  })
  
  output$save_survey_link <- renderUI({
    
    req(input$dt_participants_rows_selected)
    
    actionButton(ns("button_save_survey_link"), "Save survey link", icon = icon("save"))
    
  })
  
  observeEvent(input$button_save_survey_link, {
    
    req(input$dt_participants_rows_selected)
    
    linkedin <- rv$dt_participants_filter() %>%
      dplyr::filter(dplyr::row_number() == input$dt_participants_rows_selected)
    
    browser()
    
    event <- dplyr::tibble(
      token = linkedin$token,
      type = "linkedin",
      comment = "survey_link",
      date = as.character(lubridate::today())
    ) %>% 
      dplyr::anti_join(
        impexp::sqlite_import(
          golem::get_golem_options("sqlite_base"),
          "participants_events"
        ),
        by = c("token", "type", "comment")
      )
    
    if (nrow(event) == 1) {
      
      impexp::sqlite_append_rows(
        golem::get_golem_options("sqlite_base"),
        event,
        "participants_events"
      )
      
      rv$dt_participants_events <- dplyr::bind_rows(
        rv$dt_participants_events,
        event
      )
      
    }

  })
  
  
}
