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
