#' RM2C2dev
#' @name score_symbol_search
#' @export
#' @import tidyverse
score_symbol_search <- function(data) {
  #' SCRIPT OVERVIEW
  #' Symbol Search is mostly scored as it comes from the application.
  #' This script simply calculates accuracy
  #' type convert response_time column if not numeric
  #' parameter validation: verify data passed in is data.frame or tibble
  if(is_data_frame_tibble(data)) {
    scored <- data %>% 
      mutate(accuracy = ifelse(user_response == correct_response, 1, 0))
  } else {
    #' if data is valid, stop code execution and present an error message
    stop("`data` is not a data.frame or tibble. Please try again.")
  }
  return(scored)
}