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
      patchr::normalise_colnames() %>% 
      dplyr::arrange(token) %>% 
      dplyr::select(survey_id, token)

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
      dplyr::select(survey_id, token, submitdate) %>%
      dplyr::mutate(completed = TRUE)
    
    optout <- survey_id %>%
      limer::get_participants(conditions = list("emailstatus" = "OptOut"), session = FALSE) %>%
      dplyr::select(survey_id, token) %>%
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
            dplyr::select(survey_id = sid, group_id = gid, group_order)
        ),
        by = c("survey_id", "group_id")
      ) %>% 
      dplyr::mutate_at(c("group_order", "question_order"), as.integer) %>% 
      dplyr::arrange(survey_id, group_order, question_order) %>% 
      unique()
    
    question_groups_number <- questions %>% 
      dplyr::group_by(survey_id) %>% 
      dplyr::summarise(groups_number = max(group_order)) %>% 
      dplyr::ungroup()
    
    lastpage_rate <- survey_id %>%
      limer::get_responses(sCompletionStatus = "incomplete", session = FALSE) %>%
      dplyr::mutate_if(is.character, dplyr::na_if, "") %>% 
      dplyr::select(-c(1:2, 4:5, 7, 9)) %>% 
      dplyr::mutate(
        situationProN1 = dplyr::if_else((is.na(emploiN2DateDebut) | is.na(emploiN2TPremierEmp)) & situationProN1 == "A1", NA_character_, situationProN1),
        situationProN = dplyr::if_else((is.na(emploiN2DateDebut) | is.na(emploiN2TPremierEmp)) & situationProN == "A1", NA_character_, situationProN)
      ) %>% 
      tidyr::gather("key", "value", -survey_id, -token, -lastpage, -datestamp, na.rm = TRUE) %>% 
      dplyr::mutate(key = stringr::str_match(key, "^([^\\.]+)")[, 2]) %>% 
      dplyr::mutate_at("datestamp", lubridate::ymd_hms) %>% 
      dplyr::inner_join(questions, by = c("survey_id", "key" = "title")) %>% 
      dplyr::arrange(token, group_order, question_order) %>% 
      dplyr::group_by(token, survey_id) %>% 
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::left_join(question_groups_number, by = "survey_id") %>% 
      dplyr::mutate(max_page = purrr::map2_int(lastpage, group_order, ~ max(.x, .y))) %>% 
      dplyr::mutate(lastpage_rate = max_page / groups_number) %>%
      dplyr::select(survey_id, token, datestamp, lastpage_rate)

    release <- limer::release_session_key()
    
    cron_responses <- participants %>%
      dplyr::full_join(completed, by = c("survey_id", "token")) %>%
      dplyr::full_join(optout, by = c("survey_id", "token")) %>%
      dplyr::full_join(lastpage_rate, by = c("survey_id", "token")) %>% 
      tidyr::replace_na(list(completed = FALSE, optout = FALSE))
    
    save(cron_responses, file = output_file, compress = "bzip2")
    
  }
  
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
      mRpostman::AND(
        mRpostman::OR(
          mRpostman::string("From", "Mail Delivery Subsystem"),
          mRpostman::string("From", "Mail Delivery System"),
          mRpostman::string("From", "postmaster@")
        ),
        mRpostman::sent_since(format.Date(lubridate::today(), "%d-%b-%Y"))
      )
    )
  
  emails <- mRpostman::fetch_msg_text(delivery_failure$imapconf, delivery_failure$msg_id) %>% 
    purrr::map(stringr::str_extract_all, "[a-z0-9\\._%-]+@[a-z0-9\\.-]+\\.[a-z]{2,4}") %>% 
    purrr::map(1) %>% 
    purrr::map_chr(head, 1)
  
  date <- mRpostman::fetch_msg_header(delivery_failure$imapconf, delivery_failure$msg_id, fields = "Date") %>% 
    purrr::map_chr(stringr::str_extract, "\\d{1,2} \\w{3} \\d{4}") %>% 
    lubridate::dmy()
  
  impexp::sqlite_execute_sql(
    sqlite_base,
    glue::glue("UPDATE email_validation SET service = \"mail delivery failure\" WHERE email = \"{emails}\";")
  )
  impexp::sqlite_execute_sql(
#' set_finished_almost_complete
#' 
#' @param sqlite_base \dots
#' @param almost_complete_group \dots
#' 
#' @export
set_finished_almost_complete <- function(sqlite_base, almost_complete_group = c("123459" = 16, "123458" = 16, "123456" = 16)) {

  config_limesurvey <- impexp::sqlite_import(
    sqlite_base,
    glue::glue("UPDATE email_validation SET status = \"invalid\" WHERE email = \"{emails}\";")
  )
  impexp::sqlite_execute_sql(
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
  
  questions <- purrr::map_df(
    survey_id,
    ~ limer::call_limer("list_questions", list("iSurveyID" = .)) %>% 
      dplyr::filter(parent_qid == "0", type != "*") %>%
      dplyr::select(survey_id = sid, group_id = gid, question_id = qid, title, question_order)
  ) %>% 
    dplyr::left_join(
      purrr::map_df(
        survey_id,
        ~ limer::call_limer("list_groups", list("iSurveyID" = .)) %>% 
          dplyr::select(survey_id = sid, group_id = gid, group_order)
      ),
      by = c("survey_id", "group_id")
    ) %>% 
    dplyr::mutate_at(c("group_order", "question_order"), as.integer) %>% 
    dplyr::arrange(survey_id, group_order, question_order) %>% 
    unique()
  
  question_groups_number <- questions %>% 
    dplyr::group_by(survey_id) %>% 
    dplyr::summarise(groups_number = max(group_order)) %>% 
    dplyr::ungroup()
  
  #### Get incomplete responses ####
  
  incomplete_responses <- survey_id %>%
    limer::get_responses(sCompletionStatus = "incomplete", session = FALSE) %>%
    dplyr::rename_all(stringr::str_remove, "\\.$") %>% 
    dplyr::mutate_all(as.character) %>% 
    dplyr::mutate_all(dplyr::na_if, "") %>% 
    dplyr::select(-c(1:2, 4:5, 7, 9))
  
  almost_complete <- incomplete_responses %>% 
    dplyr::mutate(
      situationProN1 = dplyr::if_else((is.na(emploiN2DateDebut) | is.na(emploiN2TPremierEmp)) & situationProN1 == "A1", NA_character_, situationProN1),
      situationProN = dplyr::if_else((is.na(emploiN2DateDebut) | is.na(emploiN2TPremierEmp)) & situationProN == "A1", NA_character_, situationProN)
    ) %>% 
    tidyr::gather("key", "value", -survey_id, -token, -lastpage, -datestamp, na.rm = TRUE) %>% 
    dplyr::mutate(key = stringr::str_match(key, "^([^\\.]+)")[, 2]) %>% 
    dplyr::mutate_at("datestamp", lubridate::ymd_hms) %>% 
    dplyr::inner_join(questions, by = c("survey_id", "key" = "title")) %>% 
    dplyr::arrange(token, group_order, question_order) %>% 
    dplyr::group_by(token, survey_id) %>% 
    dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(question_groups_number, by = "survey_id") %>% 
    dplyr::mutate_at("lastpage", as.integer) %>% 
    dplyr::mutate(max_page = purrr::map2_int(lastpage, group_order, ~ max(.x, .y))) %>% 
    dplyr::mutate(lastpage_rate = max_page / groups_number) %>%
    dplyr::filter(lubridate::date(datestamp) != lubridate::today()) %>% 
    dplyr::select(survey_id, token, datestamp, lastpage, group_order, max_page, lastpage_rate) %>% 
    dplyr::left_join(
      dplyr::tibble(
        survey_id = names(almost_complete_group),
        almost_complete_group
      ),
      by = "survey_id"
    ) %>% 
    dplyr::filter(max_page >= almost_complete_group)
  
  almost_complete_responses <- incomplete_responses %>% 
    dplyr::semi_join(almost_complete, by = c("survey_id", "token"))
  
  #### Emploi n1 prérempli ####
  
  maj_emploi_n1 <- almost_complete_responses %>% 
    dplyr::filter(
      !is.na(emploiN2DateDebut),
      stringr::str_detect(situationProN1Aide, "si votre emploi actuel a commencé"),
      situationProN1 == "A1",
      is.na(emploiN1Intitule) & is.na(emploiN1Niveau)
    ) %>% 
    dplyr::mutate(
      emploiN1Intitule = dplyr::if_else(!is.na(emploiN2Intitule), emploiN2Intitule, emploiN1Intitule),
      emploiN1Type = dplyr::if_else(!is.na(emploiN2Type), emploiN2Type, emploiN1Type),
      emploiN1Niveau = dplyr::if_else(!is.na(emploiN2Niveau), emploiN2Niveau, emploiN1Niveau),
      emploiN1TPart = dplyr::if_else(!is.na(emploiN2TPart), emploiN2TPart, emploiN1TPart),
      emploiN1Quotite = dplyr::if_else(!is.na(emploiN2Quotite), emploiN2Quotite, emploiN1Quotite),
      emploiN1ChoixPartiel = dplyr::if_else(!is.na(emploiN2ChoixPartiel), emploiN2ChoixPartiel, emploiN1ChoixPartiel),
      emploiN1Salaire = dplyr::if_else(!is.na(emploiN2Salaire), emploiN2Salaire, emploiN1Salaire),
      emploiN1TPrimes = dplyr::if_else(!is.na(emploiN2TPrimes), emploiN2TPrimes, emploiN1TPrimes),
      emploiN1Primes = dplyr::if_else(!is.na(emploiN2Primes), emploiN2Primes, emploiN1Primes),
      emploiN1SalaireBrut = dplyr::if_else(!is.na(emploiN2SalaireBrut), emploiN2SalaireBrut, emploiN1SalaireBrut)
    ) %>% 
    dplyr::select(survey_id, token, dplyr::starts_with("emploiN1"), -dplyr::ends_with("Aide")) %>% 
    tidyr::gather("key", "value", dplyr::starts_with("emploiN1"), na.rm = TRUE) %>% 
    dplyr::left_join(questions, by = c("survey_id", "key" = "title")) %>% 
    dplyr::mutate(key = glue::glue("{survey_id}X{group_id}X{question_id}"))
    
  if (nrow(maj_emploi_n1) >= 1) {
    
    update <- maj_emploi_n1 %>% 
      split(1:nrow(.)) %>% 
      pbapply::pblapply(function(update_token) {
        
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
    dplyr::select(survey_id, token, contact.annuaire, contact.temoignage, contactAnnuaire, destinataireResultat) %>% 
    tidyr::gather("key", "value", -survey_id, -token) %>% 
    dplyr::filter(is.na(value)) %>% 
    dplyr::mutate(value = "N") %>% 
    dplyr::mutate(
      title = stringr::str_match(key, "^([^\\.]+)\\.?")[, 2],
      title_sq = stringr::str_match(key, "^([^\\.]+)\\.(.+)")[, 3]
    ) %>% 
    dplyr::inner_join(questions, by = c("survey_id", "title")) %>% 
    dplyr::mutate(question_id = caractr::str_paste(question_id, title_sq, sep = "")) %>% 
    dplyr::mutate(key = glue::glue("{survey_id}X{group_id}X{question_id}"))
  
  if (nrow(annuaire_email) >= 1) {
    
    update <- annuaire_email %>% 
      split(1:nrow(.)) %>% 
      pbapply::pblapply(function(update_token) {
        
        limer::update_responses(
          survey_id = update_token$survey_id,
          token = update_token$token,
          question = update_token$key,
          value = update_token$value
        )
        
      })
    
  }
  
  #### Mise à jour en questionnaire complet ####
  
  complete <- almost_complete %>% 
    dplyr::mutate(submitdate = datestamp) %>% 
    dplyr::select(survey_id, token, submitdate) %>% 
    dplyr::mutate_at("submitdate", as.character) %>% 
    tidyr::gather("key", "value", submitdate)

  if (nrow(complete) >= 1) {
    
    update <- complete %>% 
      split(1:nrow(.)) %>% 
      pbapply::pblapply(function(update_token) {
        
        limer::update_responses(
          survey_id = update_token$survey_id,
          token = update_token$token,
          question = update_token$key,
          value = update_token$value
        )
        
      })
    
  }
  
  participants_complete <- almost_complete %>% 
    dplyr::mutate(completed = datestamp) %>% 
    dplyr::select(survey_id, token, completed) %>% 
    dplyr::mutate_at("completed", format, "%d-%m-%Y %H:%M") %>% 
    dplyr::arrange(survey_id, token) %>% 
    dplyr::left_join(
      sqlite_base %>% 
        impexp::sqlite_import("participants") %>% 
        patchr::normalise_colnames() %>% 
        dplyr::arrange(token) %>% 
        dplyr::select(survey_id, token, tid),
      by = c("survey_id", "token")
    ) %>% 
    tidyr::gather("key", "value", completed)
  
  if (nrow(participants_complete) >= 1) {
    
    update <- participants_complete %>% 
      split(1:nrow(.)) %>% 
      pbapply::pblapply(function(update_token) {
        
        limer::set_participant_properties(
          survey_id = update_token$survey_id,
          tid = update_token$tid,
          property = update_token$key,
          value = update_token$value
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
    glue::glue("UPDATE email_validation SET date = \"{date}\" WHERE email = \"{emails}\";")
  )
    "config"
  ) %>% 
    dplyr::filter(stringr::str_detect(key, "^lime_")) %>% 
    split(x = .$value, f = .$key)
  
  options(lime_api = config_limesurvey$lime_api)
  options(lime_username = config_limesurvey$lime_username)
  options(lime_password = config_limesurvey$lime_password)
  
  key <- limer::get_session_key()
  
  if (delete == TRUE) {
    mRpostman::delete_msg(delivery_failure$imapconf, delivery_failure$msg_id)
  survey_id <- sqlite_base %>% 
    impexp::sqlite_import("surveys") %>% 
    dplyr::pull(survey_id)
  
  incomplete_responses <- survey_id %>%
    limer::get_responses(sCompletionStatus = "incomplete", session = FALSE) %>%
    dplyr::rename_all(stringr::str_remove, "\\.$") %>% 
    dplyr::mutate_all(as.character) %>% 
    dplyr::mutate_if(is.character, dplyr::na_if, "") %>% 
    dplyr::select(-c(1:2, 4:5, 7, 9)) %>% 
    dplyr::filter(lubridate::date(datestamp) != lubridate::today())
  
  complete_responses <- survey_id %>%
    limer::get_responses(sCompletionStatus = "complete", session = FALSE) %>%
    dplyr::rename_all(stringr::str_remove, "\\.$") %>% 
    dplyr::mutate_all(as.character) %>% 
    dplyr::mutate_if(is.character, dplyr::na_if, "") %>% 
    dplyr::select(-c(1:2, 4:5, 7, 9))
  
  questions <- purrr::map_df(
    survey_id,
    ~ limer::call_limer("list_questions", list("iSurveyID" = .)) %>% 
      dplyr::filter(parent_qid == "0", type != "*") %>%
      dplyr::select(survey_id = sid, group_id = gid, question_id = qid, title, question_order)
  ) %>% 
    dplyr::left_join(
      purrr::map_df(
        survey_id,
        ~ limer::call_limer("list_groups", list("iSurveyID" = .)) %>% 
          dplyr::select(survey_id = sid, group_id = gid, group_order)
      ),
      by = c("survey_id", "group_id")
    ) %>% 
    dplyr::mutate_at(c("group_order", "question_order"), as.integer) %>% 
    dplyr::arrange(survey_id, group_order, question_order) %>% 
    unique()
  
  maj_pours_etud <- dplyr::bind_rows(incomplete_responses, complete_responses) %>% 
    dplyr::select(survey_id, token, dplyr::matches("^poursEtud\\.n\\d?N\\d"), dplyr::matches("^poursEtud.+Intitule?$"), situationProN2) %>% 
    tidyr::gather("key", "value", dplyr::matches("^poursEtud\\.n\\d?N\\d")) %>% 
    dplyr::filter(is.na(value)) %>% 
    dplyr::mutate(
      title = stringr::str_match(key, "^(.+?)\\.")[, 2],
      title_sq = stringr::str_match(key, "^(.+?)\\.(.+)")[, 3]
    ) %>% 
    dplyr::left_join(questions, by = c("survey_id", "title")) %>% 
    dplyr::mutate(key = glue::glue("{survey_id}X{group_id}X{question_id}{title_sq}")) %>% 
    dplyr::mutate(
      value = dplyr::case_when(
        stringr::str_detect(token, "^[LM]") & !is.na(situationProN2) ~ "A3",
        stringr::str_detect(token, "^I") & title_sq == "nN1" & !is.na(poursEtudNN1Intitule) ~ "A1",
        stringr::str_detect(token, "^I") & title_sq == "nN1" & is.na(poursEtudNN1Intitule) ~ "A2",
        stringr::str_detect(token, "^I") & title_sq == "n1N2" & !is.na(poursEtudN1N2Intitul) ~ "A1",
        stringr::str_detect(token, "^I") & title_sq == "n1N2" & is.na(poursEtudN1N2Intitul) ~ "A2",
        stringr::str_detect(token, "^I") & title_sq == "n2N3" & !is.na(poursEtudN2N3Intitul) ~ "A1",
        stringr::str_detect(token, "^I") & title_sq == "n2N3" & is.na(poursEtudN2N3Intitul) ~ "A2"
      )
    ) %>% 
    tidyr::drop_na(value)
  
  if (nrow(maj_pours_etud) >= 1) {
    
    update <- maj_pours_etud %>% 
      split(1:nrow(.)) %>% 
      pbapply::pblapply(function(update_token) {
        
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