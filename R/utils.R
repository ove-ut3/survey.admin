str_validate_email <- function(email) {
  
  stringr::str_detect(email, stringr::regex("^[a-z0-9\\._%-]+@[a-z0-9\\.-]+\\.[a-z]{2,4}$", ignore_case = TRUE))
  
}

test_url <- function(url, timeout = 10) {
  
  if (!stringr::str_detect(url, "^http")) url <- glue::glue("http://{url}") 
  
  try <- tryCatch(
    httr::HEAD(url, httr::timeout(timeout)),
    error = function(e) e
  )
  
  if ("error" %in% class(try)) {
    status_code <- 400L
  } else {
    status_code <- httr::status_code(try)
  }
  
  status_code
}

cron_responses <- function(operation) {
  
  if (Sys.info()[["sysname"]] == "Linux" & "cronR" %in% utils::installed.packages()[, 1]) {
    
    ls_id <- cronR::cron_ls() %>% 
      stringr::str_match_all("## id:\\s+(\\w+)\n") %>% 
      .[[1]] %>% 
      .[, 2]
    
    if (operation == "add") {
      
      if ("responses" %in% ls_id) {
        cronR::cron_rm("responses")
      }

      script <- glue::glue(
        'survey.admin::cron_responses_rda(
           \"{golem::get_golem_options("sqlite_base")}\",
           \"{golem::get_golem_options("cron_responses")}\"
        )'
      )
      writeLines(script, "/home/shiny/cron_responses.R")
      f <- "/home/shiny/cron_responses.R"
      cmd <- cronR::cron_rscript(f)
      cronR::cron_add(cmd, frequency = "minutely", id = "responses")
      
      source(f)
      
    } else if (operation == "remove") {
      
      cronR::cron_rm("responses")
      
    }
    
  }
  
}
