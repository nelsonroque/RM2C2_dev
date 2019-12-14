#' RM2C2dev
#' @name score_symbol_search
#' @export
#' @import tidyverse
score_symbol_search <- function(data) {
  # check if data.frame or tibble
  if(is_data_frame_tibble(data)) {
    scored <- data %>% 
      mutate(accuracy = ifelse(user_response == correct_response, 1, 0))
  } else {
    # raise error if not a data.frame or tibble
    stop("`data` is not a data.frame or tibble. Please try again.")
  }
  
  # add scored attribute
  scored <- add_data_tag(scored, tag_name="is_m2c2_scored", tag_value=T)
  
  return(scored)
}