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
    
    question_groups_number <- survey_id %>%
      purrr::map_int(
        ~ limer::call_limer("list_groups", list("iSurveyID" = .)) %>%
          dplyr::pull(gid) %>%
          unique() %>%
          length()
      ) %>%
      dplyr::tibble(groups_number = .) %>%
      dplyr::mutate(survey_id = !!survey_id)
    
    lastpage_rate <- survey_id %>%
      limer::get_responses(sCompletionStatus = "incomplete", session = FALSE) %>%
      dplyr::select(survey_id, token, lastpage) %>%
      dplyr::left_join(question_groups_number, by = "survey_id") %>%
      dplyr::mutate_at("lastpage", dplyr::na_if, 0) %>% 
      dplyr::mutate_at("lastpage", dplyr::na_if, -1) %>% 
      dplyr::mutate(lastpage_rate = lastpage / groups_number) %>%
      dplyr::select(survey_id, token, lastpage_rate)
    
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
    sqlite_base,
    glue::glue("UPDATE email_validation SET status = \"invalid\" WHERE email = \"{emails}\";")
  )
  impexp::sqlite_execute_sql(
    sqlite_base,
    glue::glue("UPDATE email_validation SET date = \"{date}\" WHERE email = \"{emails}\";")
  )
  
  if (delete == TRUE) {
    mRpostman::delete_msg(delivery_failure$imapconf, delivery_failure$msg_id)
  }
  
}