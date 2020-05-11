#' cron_responses_rda
#'
#' @param sqlite_base \dots
#' @param output_file \dots
#'
#' @export
#' @importFrom dplyr %>%
cron_responses_rda <- function(sqlite_base, output_file = "/home/shiny/cron_responses.rda") {
  
  if (Sys.info()[1] == "Linux") {
    
    participants <- sqlite_base %>% 
      impexp::sqlite_import("participants") %>% 
      janitor::clean_names() %>% 
      dplyr::arrange(.data$token) %>% 
      dplyr::select(survey_id, .data$token)
    
    config_limesurvey <- impexp::sqlite_import(
      sqlite_base,
      "config"
    ) %>% 
      dplyr::filter(stringr::str_detect(key, "^lime_")) %>% 
      split(x = .$value, f = .$key)
    
    options(lime_api = config_limesurvey$lime_api)
    options(lime_username = config_limesurvey$lime_username)
    options(lime_password = config_limesurvey$lime_password)
    
    key <- limer::get_session_key()
    
    survey_id <- sqlite_base %>% 
      impexp::sqlite_import("surveys") %>% 
      dplyr::pull(survey_id)
    
    completed <- survey_id %>% 
      limer::get_responses(session = FALSE) %>%
      dplyr::select(survey_id, .data$token, .data$submitdate) %>%
      dplyr::mutate(completed = TRUE)
    
    optout <- survey_id %>%
      limer::get_participants(conditions = list("emailstatus" = "OptOut"), session = FALSE) %>%
      dplyr::select(survey_id, .data$token) %>%
      dplyr::mutate(optout = TRUE)
    
    questions <- purrr::map_df(
      survey_id,
      ~ limer::call_limer("list_questions", list("iSurveyID" = .)) %>% 
        dplyr::filter(parent_qid == "0", type != "*") %>% 
        dplyr::select(survey_id = sid, group_id = gid, title, question_order)
    ) %>% 
      dplyr::left_join(
        purrr::map_df(
          survey_id,
          ~ limer::call_limer("list_groups", list("iSurveyID" = .)) %>% 
            dplyr::select(survey_id = sid, group_id = gid, .data$group_order)
        ),
        by = c("survey_id", "group_id")
      ) %>% 
      dplyr::mutate_at(c("group_order", "question_order"), as.integer) %>% 
      dplyr::arrange(survey_id, .data$group_order, .data$question_order) %>% 
      unique()
    
    question_groups_number <- questions %>% 
      dplyr::group_by(survey_id) %>% 
      dplyr::summarise(groups_number = max(.data$group_order)) %>% 
      dplyr::ungroup()
    
    lastpage_rate <- survey_id %>%
      limer::get_responses(sCompletionStatus = "incomplete", session = FALSE) %>%
      dplyr::mutate_if(is.character, dplyr::na_if, "") %>% 
      dplyr::select(-c(1:2, 4:5, 7, 9)) %>% 
      dplyr::mutate(
        situationProN1 = dplyr::if_else((is.na(.data$emploiN2DateDebut) | is.na(.data$emploiN2TPremierEmp)) & .data$situationProN1 == "A1", NA_character_, .data$situationProN1),
        situationProN = dplyr::if_else((is.na(.data$emploiN2DateDebut) | is.na(.data$emploiN2TPremierEmp)) & .data$situationProN == "A1", NA_character_, .data$situationProN)
      ) %>% 
      tidyr::gather("key", "value", -survey_id, -.data$token, -.data$lastpage, -.data$datestamp, na.rm = TRUE) %>% 
      dplyr::mutate(key = stringr::str_match(.data$key, "^([^\\.]+)")[, 2]) %>% 
      dplyr::mutate_at("datestamp", lubridate::ymd_hms) %>% 
      dplyr::inner_join(questions, by = c("survey_id", "key" = "title")) %>% 
      dplyr::arrange(.data$token, .data$group_order, .data$question_order) %>% 
      dplyr::group_by(.data$token, survey_id) %>% 
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::left_join(question_groups_number, by = "survey_id") %>% 
      dplyr::mutate(max_page = purrr::map2_int(.data$lastpage, .data$group_order, ~ max(.x, .y))) %>% 
      dplyr::mutate(lastpage_rate = .data$max_page / .data$groups_number) %>%
      dplyr::select(survey_id, .data$token, .data$datestamp, .data$lastpage, .data$group_order, .data$lastpage_rate)
    
    release <- limer::release_session_key()
    
    cron_responses <- participants %>%
      dplyr::full_join(completed, by = c("survey_id", "token")) %>%
      dplyr::full_join(optout, by = c("survey_id", "token")) %>%
      dplyr::full_join(lastpage_rate, by = c("survey_id", "token")) %>% 
      tidyr::replace_na(list(completed = FALSE, optout = FALSE))
    
    save(cron_responses, file = output_file, compress = "bzip2")
    
  }
  
}

#' escape_space_glue
#' 
#' @param string \dots
#' @param participants_attributes \dots
#' 
#' @export
#' @keywords internal
escape_space_glue <- function(string, participants_attributes) {
  
  attributes_replace <- participants_attributes %>% 
    dplyr::select(.data$description) %>% 
    unique() %>% 
    dplyr::filter(stringr::str_detect(.data$description, " ")) %>% 
    dplyr::mutate(replace = paste0("`", .data$description, "`")) %>% 
    split(x = .$replace, f = .$description) %>% 
    rev()
  
  if (length(attributes_replace) == 0) return(string)
  
  for (num_replace in seq_along(attributes_replace)) {
    
    string <- stringr::str_replace_all(
      string, 
      glue::glue("(?<!`){names(attributes_replace)[num_replace]}"),
      attributes_replace[[num_replace]]
    )
    
  }
  
  return(string)
  
}

#' mailing
#' 
#' @param rv \dots
#' @param participants \dots
#' @param participants_attributes \dots
#' @param from \dots
#' @param subject \dots
#' @param body \dots
#' @param sleep \dots
#' @param delete_survey \dots
#' @param progress \dots
#' @param crowdsourcing \dots
#' 
#' @export
#' @keywords internal
mailing <- function(rv, participants, participants_attributes = NULL, from, subject, body, sleep, delete_survey = FALSE, progress = FALSE, crowdsourcing = FALSE) {
  
  style <- "'font-family: calibri; font-size: 11pt;'"
  body <- glue::glue("<p style={style}>{body}</p>") %>% 
    stringr::str_replace_all("\n", "<br>")
  
  rename <- paste(from$alias, subject, body) %>% 
    stringr::str_match_all("\\{([^\\}]+?)\\}") %>% 
    .[[1]] %>% 
    .[, 2] %>% 
    unique() %>% 
    dplyr::tibble(
      attribute_body = .
    ) %>% 
    dplyr::semi_join(
      dplyr::tibble(attribute_body = names(participants)),
      by = "attribute_body"
    ) %>% 
    dplyr::mutate(
      column = janitor::make_clean_names(.data$attribute_body),
      attribute = glue::glue("ATTRIBUTE_{dplyr::row_number()}") %>% 
        as.character(),
      attribute = dplyr::if_else(.data$attribute_body %in% c("firstname", "lastname"), toupper(.data$attribute_body), .data$attribute)
    ) %>% 
    dplyr::add_row(column = c("token", "email")) %>% 
    dplyr::rename(rename = .data$attribute)
  
  to <- patchr::rename(participants, rename) %>% 
    janitor::clean_names()
  
  glue_data <- rename %>% 
    dplyr::filter(column != rename) %>% 
    dplyr::select(-column) %>% 
    dplyr::mutate_at("rename", ~ paste0("{", .,"}")) %>% 
    tidyr::spread(.data$attribute_body, rename)

  subject <- subject %>% 
    survey.admin::escape_space_glue(participants_attributes) %>% 
    glue::glue_data(.x = glue_data)
  
  body <- body %>% 
    survey.admin::escape_space_glue(participants_attributes) %>% 
    glue::glue_data(.x = glue_data)
  
  attribute_sender <- from$alias %>% 
    stringr::str_match_all("\\{([^\\}]+?)\\}") %>% 
    .[[1]] %>% 
    .[, 2] %>% 
    unique()
  
  if (any(attribute_sender %in% names(participants))) {
    
    attribute_sender <- rename %>% 
      dplyr::filter(column == !!attribute_sender) %>% 
      dplyr::pull(rename) %>% 
      tolower()
    
    to <- to %>% 
      split(f = .[[attribute_sender]])
    
  } else {
    
    to <- list(to)
    names(to) <- from$alias
    
  }

  key <- limer::get_session_key()
  
  survey_id_tid <- purrr::map2_df(
    to,
    names(to),
     ~ limer::mailing_create_survey(
         from = list(
           "email" = from$email,
           "alias" = .y
          ),
         to = .x,
         subject = subject,
         body = body
       )
  )
  
  survey_id <- survey_id_tid$survey_id
  tid <- survey_id_tid$tid

  to <- dplyr::tibble(
    name = names(to),
    to
  ) %>% 
    tidyr::unnest(to)
  
  if (progress == FALSE) {
    
    limer::mail_registered_participant(survey_id, tid = tid)
    
  } else if (progress == TRUE) {

    withProgress(message = "Sending email :", value = 0, detail = "0%", {
      
      for (i in 1:length(tid)) {
        
        if (i != 1) Sys.sleep(sleep)
        
        try <- tryCatch(
          limer::mail_registered_participant(survey_id[i], tid = tid[i]),
          error = function(e) e
        )
        
        if ("error" %in% class(try)) {
          
          key <- limer::get_session_key()
          mailing <- limer::mail_registered_participant(survey_id[i], tid = tid[i])
          
        } else if (!stringr::str_detect(try$status, "\\d+ left to send$")) {
          
          key <- limer::get_session_key()
          mailing <- limer::mail_registered_participant(survey_id[i], tid = tid[i])
          
        }
        
        if (crowdsourcing == FALSE) {
          
          event <- dplyr::tibble(
            token = to$token[i],
            type = "general mailing",
            comment = to$email[i],
            date = as.character(lubridate::today())
          )
          
          impexp::sqlite_append_rows(
            golem::get_golem_options("sqlite_base"),
            event,
            "participants_events"
          )
          
          rv$df_participants_events <- dplyr::bind_rows(
            rv$df_participants_events,
            event
          )
          
        }
        
        incProgress(
          1 / length(tid),
          detail = paste0(
            round(i / length(tid) * 100, 1), "% - ",
            to$email[i]
          )
        )
        
      }
      
    })
    
  }
  
  if (delete_survey == TRUE) {
    
    suppression <- limer::call_limer("delete_survey", params = list("iSurveyID" = survey_id))
    
  }
  
  release <- limer::release_session_key()
  
}

#' mail_delivery_failure
#' 
#' @param sqlite_base \dots
#' @param imap_server \dots
#' @param username \dots
#' @param password \dots
#' @param mailbox \dots
#' @param delete \dots
#' 
#' @export
mail_delivery_failure <- function(sqlite_base, imap_server, username, password, mailbox, delete = FALSE) {
  
  imapconf <- mRpostman::configure_imap(
    url = imap_server,
    username = username,
    password = password
  )
  
  Sys.setlocale("LC_TIME", "C")
  
  delivery_failure <- imapconf %>%
    mRpostman::select_mailbox(mbox = mailbox) %>%
    mRpostman::custom_search(
      #mRpostman::AND(
      mRpostman::OR(
        mRpostman::string("From", "Mail Delivery Subsystem"),
        mRpostman::string("From", "Mail Delivery System"),
        mRpostman::string("From", "postmaster@")
      )#,
      #mRpostman::sent_since(format.Date(lubridate::today(), "%d-%b-%Y"))
      #)
    )
  
  if (length(delivery_failure$msg_id) != 0) {
    
    emails <- mRpostman::fetch_msg_text(delivery_failure$imapconf, delivery_failure$msg_id) %>% 
      purrr::map(stringr::str_extract_all, "[a-z0-9\\._%-]+@[a-z0-9\\.-]+\\.[a-z]{2,4}") %>% 
      purrr::map(1) %>% 
      purrr::map_chr(utils::head, 1)
    
    date <- mRpostman::fetch_msg_header(delivery_failure$imapconf, delivery_failure$msg_id, fields = "Date") %>% 
      purrr::map_chr(stringr::str_extract, "\\d{1,2} \\w{3} \\d{4}") %>% 
      lubridate::dmy()
    
    purrr::walk(
      emails,
      ~ impexp::sqlite_execute_sql(
        sqlite_base,
        paste0("UPDATE participants_contacts SET service = \"mail delivery failure\" WHERE value = \"", ., "\";")
      )
    )
    
    purrr::walk(
      emails,
      ~ impexp::sqlite_execute_sql(
        sqlite_base,
        paste0("UPDATE participants_contacts SET service = \"invalid\" WHERE value = \"", ., "\";")
      )
    )
    
    purrr::walk2(
      date,
      emails,
      ~ impexp::sqlite_execute_sql(
        sqlite_base,
        paste0("UPDATE participants_contacts SET service = \"", .x, "\" WHERE value = \"", .y, "\";")
      )
    )
    
    if (delete == TRUE) {
      mRpostman::delete_msg(delivery_failure$imapconf, delivery_failure$msg_id)
    }
    
  }
  
}

#' set_finished_almost_complete
#' 
#' @param sqlite_base \dots
#' @param cron_responses \dots
#' @param almost_complete_group \dots
#' @param token \dots
#' @param force_today \dots
#' 
#' @export
set_finished_almost_complete <- function(sqlite_base, cron_responses, almost_complete_group = c("123459" = 16, "123458" = 16, "123456" = 16), token = NULL, force_today = FALSE) {
  
  config_limesurvey <- impexp::sqlite_import(
    sqlite_base,
    "config"
  ) %>% 
    dplyr::filter(stringr::str_detect(key, "^lime_")) %>% 
    split(x = .$value, f = .$key)
  
  options(lime_api = config_limesurvey$lime_api)
  options(lime_username = config_limesurvey$lime_username)
  options(lime_password = config_limesurvey$lime_password)
  
  key <- limer::get_session_key()
  
  survey_id <- sqlite_base %>% 
    impexp::sqlite_import("surveys") %>% 
    dplyr::pull(.data$survey_id)
  
  questions <- purrr::map_df(
    survey_id,
    ~ limer::call_limer("list_questions", list("iSurveyID" = .)) %>% 
      dplyr::filter(.data$parent_qid == "0", .data$type != "*") %>%
      dplyr::select(survey_id = .data$sid, group_id = .data$gid, question_id = .data$qid, .data$title, .data$question_order)
  ) %>% 
    dplyr::left_join(
      purrr::map_df(
        survey_id,
        ~ limer::call_limer("list_groups", list("iSurveyID" = .)) %>% 
          dplyr::select(survey_id = .data$sid, group_id = .data$gid, .data$group_order)
      ),
      by = c("survey_id", "group_id")
    ) %>% 
    dplyr::mutate_at(c("group_order", "question_order"), as.integer) %>% 
    dplyr::arrange(.data$survey_id, .data$group_order, .data$question_order) %>% 
    unique()
  
  question_groups_number <- questions %>% 
    dplyr::group_by(.data$survey_id) %>% 
    dplyr::summarise(groups_number = max(.data$group_order)) %>% 
    dplyr::ungroup()
  
  #### Get incomplete responses ####
  
  incomplete_responses <- survey_id %>%
    limer::get_responses(sCompletionStatus = "incomplete", session = FALSE) %>%
    dplyr::rename_all(stringr::str_remove, "\\.$") %>% 
    dplyr::mutate_all(as.character) %>% 
    dplyr::mutate_all(dplyr::na_if, "") %>% 
    dplyr::select(-c(1:2, 4:5, 7, 9))
  
  if (!is.null(token)) {
    incomplete_responses <- dplyr::filter(incomplete_responses, .data$token %in% !!token)
  }
  
  almost_complete <- incomplete_responses %>% 
    dplyr::mutate(
      situationProN1 = dplyr::if_else((is.na(.data$emploiN2DateDebut) | is.na(.data$emploiN2TPremierEmp)) & .data$situationProN1 == "A1", NA_character_, .data$situationProN1),
      situationProN = dplyr::if_else((is.na(.data$emploiN2DateDebut) | is.na(.data$emploiN2TPremierEmp)) & .data$situationProN == "A1", NA_character_, .data$situationProN)
    ) %>% 
    tidyr::gather("key", "value", -.data$survey_id, -.data$token, -.data$lastpage, -.data$datestamp, na.rm = TRUE) %>% 
    dplyr::mutate(key = stringr::str_match(.data$key, "^([^\\.]+)")[, 2]) %>% 
    dplyr::mutate_at("datestamp", lubridate::ymd_hms) %>% 
    dplyr::inner_join(questions, by = c("survey_id", "key" = "title")) %>% 
    dplyr::arrange(.data$token, .data$group_order, .data$question_order) %>% 
    dplyr::group_by(.data$token, .data$survey_id) %>% 
    dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(question_groups_number, by = "survey_id") %>% 
    dplyr::mutate_at("lastpage", as.integer) %>% 
    dplyr::mutate(max_page = purrr::map2_int(.data$lastpage, .data$group_order, ~ max(.x, .y))) %>% 
    dplyr::mutate(lastpage_rate = .data$max_page / .data$groups_number) %>%
    dplyr::select(.data$survey_id, .data$token, .data$datestamp, .data$lastpage, .data$group_order,.data$ max_page, .data$lastpage_rate) %>% 
    dplyr::left_join(
      dplyr::tibble(
        survey_id = names(almost_complete_group),
        almost_complete_group
      ),
      by = "survey_id"
    ) %>% 
    dplyr::filter(.data$max_page >= almost_complete_group)
  
  if (force_today == FALSE) {
    almost_complete <- dplyr::filter(almost_complete, lubridate::date(.data$datestamp) != lubridate::today())
  }
  
  almost_complete_responses <- incomplete_responses %>% 
    dplyr::semi_join(almost_complete, by = c("survey_id", "token"))
  
  #### Emploi n1 pr\u00e9rempli ####
  
  maj_emploi_n1 <- almost_complete_responses %>% 
    dplyr::filter(
      is.na(!.data$emploiN2DateDebut),
      stringr::str_detect(.data$situationProN1Aide, "si votre emploi actuel a commenc\u00e9"),
      .data$situationProN1 == "A1",
      is.na(.data$emploiN1Intitule) & is.na(.data$emploiN1Niveau)
    ) %>% 
    dplyr::mutate(
      emploiN1Intitule = dplyr::if_else(!is.na(.data$emploiN2Intitule), .data$emploiN2Intitule, .data$emploiN1Intitule),
      emploiN1Type = dplyr::if_else(!is.na(.data$emploiN2Type), .data$emploiN2Type, .data$emploiN1Type),
      emploiN1Niveau = dplyr::if_else(!is.na(.data$emploiN2Niveau), .data$emploiN2Niveau, .data$emploiN1Niveau),
      emploiN1TPart = dplyr::if_else(!is.na(.data$emploiN2TPart), .data$emploiN2TPart, .data$emploiN1TPart),
      emploiN1Quotite = dplyr::if_else(!is.na(.data$emploiN2Quotite), .data$emploiN2Quotite, .data$emploiN1Quotite),
      emploiN1ChoixPartiel = dplyr::if_else(!is.na(.data$emploiN2ChoixPartiel), .data$emploiN2ChoixPartiel, .data$emploiN1ChoixPartiel),
      emploiN1Salaire = dplyr::if_else(!is.na(.data$emploiN2Salaire), .data$emploiN2Salaire, .data$emploiN1Salaire),
      emploiN1TPrimes = dplyr::if_else(!is.na(.data$emploiN2TPrimes), .data$emploiN2TPrimes, .data$emploiN1TPrimes),
      emploiN1Primes = dplyr::if_else(!is.na(.data$emploiN2Primes), .data$emploiN2Primes, .data$emploiN1Primes),
      emploiN1SalaireBrut = dplyr::if_else(!is.na(.data$emploiN2SalaireBrut), .data$emploiN2SalaireBrut, .data$emploiN1SalaireBrut)
    ) %>% 
    dplyr::select(.data$survey_id, .data$token, dplyr::starts_with("emploiN1"), -dplyr::ends_with("Aide")) %>% 
    tidyr::gather("key", "value", dplyr::starts_with("emploiN1"), na.rm = TRUE) %>% 
    dplyr::left_join(questions, by = c("survey_id", "key" = "title")) %>% 
    dplyr::mutate(key = glue::glue("{survey_id}X{group_id}X{question_id}"))
  
  if (nrow(maj_emploi_n1) >= 1) {
    
    update <- maj_emploi_n1 %>% 
      split(1:nrow(.)) %>% 
      lapply(function(update_token) {
        
        limer::update_responses(
          survey_id = update_token$survey_id,
          token = update_token$token,
          question = update_token$key,
          value = update_token$value
        )
        
      })
    
  }
  
  #### Annuaire et emailResultats ####
  
  annuaire_email <- almost_complete_responses %>% 
    dplyr::select(.data$survey_id, .data$token, .data$contact.annuaire, .data$contact.temoignage, .data$contactAnnuaire, .data$destinataireResultat) %>% 
    tidyr::gather("key", "value", -.data$survey_id, -.data$token) %>% 
    dplyr::filter(is.na(.data$value)) %>% 
    dplyr::mutate(value = "N") %>% 
    dplyr::mutate(
      title = stringr::str_match(.data$key, "^([^\\.]+)\\.?")[, 2],
      title_sq = stringr::str_match(.data$key, "^([^\\.]+)\\.(.+)")[, 3]
    ) %>% 
    dplyr::inner_join(questions, by = c("survey_id", "title")) %>% 
    tidyr::unite(.data$question_id, .data$question_id, .data$title_sq, sep = "", na.rm = TRUE) %>%
    tidyr::unite(.data$key, .data$survey_id, .data$group_id, .data$question_id, remove = FALSE, sep = "X")
  
  if (nrow(annuaire_email) >= 1) {
    
    update <- annuaire_email %>% 
      split(1:nrow(.)) %>% 
      lapply(function(update_token) {
        
        limer::update_responses(
          survey_id = update_token$survey_id,
          token = update_token$token,
          question = update_token$key,
          value = update_token$value
        )
        
      })
    
  }
  
  #### Mise \u00e0 jour en questionnaire complet ####
  
  complete <- almost_complete %>% 
    dplyr::mutate(submitdate = .data$datestamp) %>% 
    dplyr::select(.data$survey_id, .data$token, .data$submitdate) %>% 
    dplyr::mutate_at("submitdate", as.character) %>% 
    tidyr::gather("key", "value", .data$submitdate)
  
  if (nrow(complete) >= 1) {
    
    update <- complete %>% 
      split(1:nrow(.)) %>% 
      lapply(function(update_token) {
        
        limer::update_responses(
          survey_id = update_token$survey_id,
          token = update_token$token,
          question = update_token$key,
          value = update_token$value
        )
        
      })
    
  }
  
  participants_complete <- almost_complete %>% 
    dplyr::mutate(completed = .data$datestamp) %>% 
    dplyr::select(.data$survey_id, .data$token, .data$completed) %>% 
    dplyr::mutate_at("completed", format, "%d-%m-%Y %H:%M") %>% 
    dplyr::arrange(.data$survey_id, .data$token) %>% 
    dplyr::left_join(
      sqlite_base %>% 
        impexp::sqlite_import("participants") %>% 
        janitor::clean_names() %>% 
        dplyr::arrange(.data$token) %>% 
        dplyr::select(.data$survey_id, .data$token, .data$tid),
      by = c("survey_id", "token")
    ) %>% 
    tidyr::gather("key", "value", .data$completed)
  
  if (nrow(participants_complete) >= 1) {
    
    update <- participants_complete %>% 
      split(1:nrow(.)) %>% 
      lapply(function(update_token) {
        
        limer::set_participant_properties(
          survey_id = update_token$survey_id,
          tid = update_token$tid,
          property = update_token$key,
          value = update_token$value
        )
        
      })
    
  }
  
  #### Optout compl\u00e9t\u00e9s ####
  
  cancel_optout <- almost_complete %>% 
    dplyr::semi_join(
      impexp::r_import(cron_responses) %>% 
        dplyr::filter(.data$optout),
      by = "token"
    )
  
  if (nrow(participants_complete) >= 1) {
    
    update <- participants_complete %>% 
      split(1:nrow(.)) %>% 
      lapply(function(update_token) {
        
        set_participant_properties <- limer::call_limer(
          method = "set_participant_properties", 
          params = list(
            "iSurveyID" = update_token$survey_id,
            "aTokenQueryProperties" = update_token$tid,
            "aTokenData" = list("emailstatus" = "OK")
          )
        )
        
      })
    
  }
  
  release <- limer::release_session_key()
  
}

#' pours_etud_perte_reprise
#' 
#' @param sqlite_base \dots
#' 
#' @export
pours_etud_perte_reprise <- function(sqlite_base) {
  
  config_limesurvey <- impexp::sqlite_import(
    sqlite_base,
    "config"
  ) %>% 
    dplyr::filter(stringr::str_detect(.data$key, "^lime_")) %>% 
    split(x = .$value, f = .$key)
  
  options(lime_api = config_limesurvey$lime_api)
  options(lime_username = config_limesurvey$lime_username)
  options(lime_password = config_limesurvey$lime_password)
  
  key <- limer::get_session_key()
  
  survey_id <- sqlite_base %>% 
    impexp::sqlite_import("surveys") %>% 
    dplyr::pull(.data$survey_id)
  
  incomplete_responses <- survey_id %>%
    limer::get_responses(sCompletionStatus = "incomplete", session = FALSE) %>%
    dplyr::rename_all(stringr::str_remove, "\\.$") %>% 
    dplyr::mutate_all(as.character) %>% 
    dplyr::mutate_if(is.character, dplyr::na_if, "") %>% 
    dplyr::select(-c(1:2, 4:5, 7, 9)) %>% 
    dplyr::filter(lubridate::date(.data$datestamp) != lubridate::today())
  
  complete_responses <- survey_id %>%
    limer::get_responses(sCompletionStatus = "complete", session = FALSE) %>%
    dplyr::rename_all(stringr::str_remove, "\\.$") %>% 
    dplyr::mutate_all(as.character) %>% 
    dplyr::mutate_if(is.character, dplyr::na_if, "") %>% 
    dplyr::select(-c(1:2, 4:5, 7, 9))
  
  questions <- purrr::map_df(
    survey_id,
    ~ limer::call_limer("list_questions", list("iSurveyID" = .)) %>% 
      dplyr::filter(.data$parent_qid == "0", .data$type != "*") %>%
      dplyr::select(survey_id = .data$sid, group_id = .data$gid, question_id = .data$qid, .data$title, .data$question_order)
  ) %>% 
    dplyr::left_join(
      purrr::map_df(
        survey_id,
        ~ limer::call_limer("list_groups", list("iSurveyID" = .)) %>% 
          dplyr::select(survey_id = .data$sid, group_id = .data$gid, .data$group_order)
      ),
      by = c("survey_id", "group_id")
    ) %>% 
    dplyr::mutate_at(c("group_order", "question_order"), as.integer) %>% 
    dplyr::arrange(.data$survey_id, .data$group_order, .data$question_order) %>% 
    unique()
  
  maj_pours_etud <- dplyr::bind_rows(incomplete_responses, complete_responses) %>% 
    dplyr::select(.data$survey_id, .data$token, dplyr::matches("^poursEtud\\.n\\d?N\\d"), dplyr::matches("^poursEtud.+Form$"), dplyr::matches("^poursEtud.+Intitule?$"), .data$situationProN2, .data$fcStatut) %>% 
    tidyr::gather("key", "value", dplyr::matches("^poursEtud\\.n\\d?N\\d")) %>% 
    dplyr::filter(
      is.na(.data$fcStatut),
      is.na(.data$value)
    ) %>% 
    dplyr::mutate(
      title = stringr::str_match(.data$key, "^(.+?)\\.")[, 2],
      title_sq = stringr::str_match(.data$key, "^(.+?)\\.(.+)")[, 3]
    ) %>% 
    dplyr::left_join(questions, by = c("survey_id", "title")) %>% 
    dplyr::mutate(key = glue::glue("{survey_id}X{group_id}X{question_id}{title_sq}")) %>% 
    dplyr::mutate(
      value = dplyr::case_when(
        stringr::str_detect(.data$token, "^[LM]") & !is.na(.data$situationProN2) ~ "A3",
        stringr::str_detect(.data$token, "^[LM]") & .data$title_sq == "nN1" & is.na(.data$poursEtudNN1Form) & !is.na(.data$poursEtudNN1Intitule) ~ "A1",
        stringr::str_detect(.data$token, "^[LM]") & .data$title_sq == "nN1" & !is.na(.data$poursEtudNN1Form) ~ "A2",
        stringr::str_detect(.data$token, "^[LM]") & .data$title_sq == "nN1" & is.na(.data$poursEtudNN1Form) & is.na(.data$poursEtudNN1Intitule) ~ "A3",
        stringr::str_detect(.data$token, "^[LM]") & .data$title_sq == "n1N2" & is.na(.data$poursEtudN1N2Form) & !is.na(.data$poursEtudN1N2Intitul) ~ "A1",
        stringr::str_detect(.data$token, "^[LM]") & .data$title_sq == "n1N2" & !is.na(.data$poursEtudN1N2Form) ~ "A2",
        stringr::str_detect(.data$token, "^[LM]") & .data$title_sq == "n1N2" & is.na(.data$poursEtudN1N2Form) & is.na(.data$poursEtudN1N2Intitul) ~ "A3",
        stringr::str_detect(.data$token, "^[LM]") & .data$title_sq == "n2N3" & is.na(.data$poursEtudN2N3Form) & !is.na(.data$poursEtudN2N3Intitul) ~ "A1",
        stringr::str_detect(.data$token, "^[LM]") & .data$title_sq == "n2N3" & !is.na(.data$poursEtudN2N3Form) ~ "A2",
        stringr::str_detect(.data$token, "^[LM]") & .data$title_sq == "n2N3" & is.na(.data$poursEtudN2N3Form) & is.na(.data$poursEtudN2N3Intitul) ~ "A3",
        stringr::str_detect(.data$token, "^I") & .data$title_sq == "nN1" & !is.na(.data$poursEtudNN1Form) ~ "A1",
        stringr::str_detect(.data$token, "^I") & .data$title_sq == "nN1" & is.na(.data$poursEtudNN1Form) ~ "A2",
        stringr::str_detect(.data$token, "^I") & .data$title_sq == "n1N2" & !is.na(.data$poursEtudN1N2Form) ~ "A1",
        stringr::str_detect(.data$token, "^I") & .data$title_sq == "n1N2" & is.na(.data$poursEtudN1N2Form) ~ "A2",
        stringr::str_detect(.data$token, "^I") & .data$title_sq == "n2N3" & !is.na(.data$poursEtudN2N3Form) ~ "A1",
        stringr::str_detect(.data$token, "^I") & .data$title_sq == "n2N3" & is.na(.data$poursEtudN2N3Form) ~ "A2"
      )
    ) %>% 
    tidyr::drop_na(.data$value)
  
  if (nrow(maj_pours_etud) >= 1) {
    
    update <- maj_pours_etud %>% 
      split(1:nrow(.)) %>% 
      lapply(function(update_token) {
        
        limer::update_responses(
          survey_id = update_token$survey_id,
          token = update_token$token,
          question = update_token$key,
          value = update_token$value
        )
        
      })
    
  }
  
  maj_etud_act <- dplyr::bind_rows(incomplete_responses, complete_responses) %>% 
    tidyr::drop_na(.data$poursEtudN2N3Form) %>% 
    dplyr::select(.data$survey_id, .data$token, .data$poursEtudN2N3ActPrin) %>% 
    tidyr::gather("title", "value", .data$poursEtudN2N3ActPrin) %>% 
    dplyr::filter(is.na(.data$value)) %>% 
    dplyr::left_join(questions, by = c("survey_id", "title")) %>% 
    dplyr::mutate(key = glue::glue("{survey_id}X{group_id}X{question_id}")) %>% 
    dplyr::mutate(
      value = "Y"
    ) %>% 
    tidyr::drop_na(.data$value)
  
  if (nrow(maj_etud_act) >= 1) {
    
    update <- maj_etud_act %>% 
      split(1:nrow(.)) %>% 
      lapply(function(update_token) {
        
        limer::update_responses(
          survey_id = update_token$survey_id,
          token = update_token$token,
          question = update_token$key,
          value = update_token$value
        )
        
      })
    
  }
  
  release <- limer::release_session_key()
  
}

#' df_participants_contacts_crowdsourcing
#' 
#' @param data \dots
#' 
#' @export
#' @keywords internal
df_participants_contacts_crowdsourcing <- function(data) {
  
  df_participants_contacts_crowdsourcing <- data %>% 
    dplyr::filter(!.data$key %in% c("tel_fixe", "tel_etudiant")) %>% 
    dplyr::mutate_at("status", dplyr::recode, "unknown" = "valid", .missing = "valid") %>% 
    dplyr::mutate(key = dplyr::if_else(.data$status == "invalid", paste0(.data$key, "_", .data$status), .data$key)) %>% 
    dplyr::arrange(.data$token, .data$key, dplyr::desc(date)) %>% 
    dplyr::group_by(.data$token, .data$key) %>% 
    dplyr::summarise_at("value", paste, collapse = " ; ") %>% 
    dplyr::ungroup() %>% 
    tidyr::spread(.data$key, .data$value)
  
  if (ncol(df_participants_contacts_crowdsourcing) == 1) {
    
    df_participants_contacts_crowdsourcing <- df_participants_contacts_crowdsourcing %>% 
      dplyr::mutate(empty = NA_character_)
    
  }
  
  return(df_participants_contacts_crowdsourcing)
}
