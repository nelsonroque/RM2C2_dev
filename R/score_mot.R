#' RM2C2dev
#' @name score_mot
#' @export
#' @import tidyverse
score_mot <- function(data) {
  
  # check if data.frame or tibble
  if(is_data_frame_tibble(data)) {
    
    # score the data
    # in this case, adding a 1/0 accuracy column
    scored <- data %>%
      mutate(cue_before_time_deviance = t_cueBeforeTime - a_cueBeforeTime,
             cue_blink_time_deviance = t_cueBlinkTime - a_cueBlinkTime,
             cue_show_time_deviance = t_cueShowTime - a_cueShowTime,
             cue_after_time_deviance = t_cueAfterTime - a_cueAfterTime,
             movement_time_deviance = t_movementTime - a_movementTime,
             selection_response_time = totalResponseTime) %>%
      rowwise() %>%
      mutate(n.total.selections = length(sapply(strsplit(unlist(strsplit(selected_stim_list, " ")), "_"), `[`, 4)),
             n.correct.selections = sum(as.numeric(sapply(strsplit(unlist(strsplit(selected_stim_list, " ")), "_"), `[`, 4)))) %>%
      mutate(prop.correct.selections = n.correct.selections / n.total.selections) %>%
      mutate(perfect_trial = ifelse(target_num == n.correct.selections, 1, 0))
    
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