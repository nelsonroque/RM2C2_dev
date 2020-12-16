#' RM2C2dev
#' @name score_orca
#' @export
#' @import tidyverse
score_orca <- function(data) {
  
  # check if data.frame or tibble
  if(is_data_frame_tibble(data)) {
    
    # score the data
    scored <- data %>%
      mutate(response_label = ifelse(response == 0, "no-match", "match"),
             correct_response_label = ifelse(correct_response == 0, "no-match", "match"),
             phase_learning = grepl("_learning_", filename),
             phase_recognition = grepl("_recognition_", filename)) %>%
      mutate(response_accuracy = ifelse(response_label == correct_response_label, T, F),
             task_phase = ifelse(phase_learning, "learning", "recognition"))
    
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