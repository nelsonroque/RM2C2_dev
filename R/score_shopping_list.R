#' RM2C2dev
#' @name score_shopping_list
#' @export
#' @import tidyverse
score_shopping_list <- function(data) {
  
  # check if data.frame or tibble
  if(is_data_frame_tibble(data)) {
    
    # score the data
    scored <- data %>%
      mutate(correct = ifelse(target_price == choice, 1, 0),
             choice_RT = choiceRT)
    
  } else {
    
    # raise error if not a data.frame or tibble
    stop("`data` is not a data.frame or tibble. Please try again.")
  }
  
  # add processing hash and timestamp
  scored <- scored %>%
    append_process_cols()
  
  # add scored attribute
  scored <- add_data_tag(scored, tag_name="is_m2c2_scored", tag_value=T)
  
  # =======================================

  return(scored)
}