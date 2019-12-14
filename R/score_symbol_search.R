#' RM2C2dev
#' @name score_symbol_search
#' @export
#' @import tidyverse
score_symbol_search <- function(data) {
  
  # check if data.frame or tibble
  if(is_data_frame_tibble(data)) {
    
    # score the data
    # in this case, adding a 1/0 accuracy column
    scored <- data %>% 
      mutate(accuracy = ifelse(user_response == correct_response, 1, 0))
    
  } else {
    
    # raise error if not a data.frame or tibble
    stop("`data` is not a data.frame or tibble. Please try again.")
  }
  
  # add processing hash and timestamp
  scored <- scored %>%
    append_process_cols()
  
  # add scored attribute
  scored <- add_data_tag(scored, tag_name="is_m2c2_scored", tag_value=T)

  return(scored)
}