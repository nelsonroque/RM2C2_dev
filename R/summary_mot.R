#' RM2C2dev
#' @name summary_mot
#' @export
#' @import tidyverse
summary_mot <- function(data, group_var, var_prefix = "multi_obj_tracking", experimental = F, lure_label = "LURE", normal_label = "NORMAL") {
  
  # check if data.frame or tibble
  if(is_data_frame_tibble(data)) {
    
    # check if data is scored
    if(is_data_tag_valid(data, tag_name = "is_m2c2_scored", tag_value = T)) {
      
      # produce primary outcome summary
      summary_data <- data %>%
        group_by(.dots = c(group_var)) %>%
        summarise(mean.prop.correct.selections = mean(prop.correct.selections, na.rm=T),
                  
                  sum.correct.selections = sum(n.correct.selections, na.rm=T),
                  sum.total.selections = sum(n.total.selections, na.rm=T),
                  
                  n.perfect.trials = sum(perfect_trial, na.rm=T),
                  
                  mean.selection_response_time = mean(selection_response_time, na.rm=T),
                  median.selection_response_time = median(selection_response_time, na.rm=T),
                  sd.selection_response_time = sd(selection_response_time, na.rm=T),
                  
                  n.trials = n(),
                  
                  mean.cue_before_time_deviance = mean(cue_before_time_deviance, na.rm=T),
                  mean.cue_blink_time_deviance = mean(cue_blink_time_deviance, na.rm=T),
                  mean.cue_show_time_deviance = mean(cue_show_time_deviance, na.rm=T),
                  mean.cue_after_time_deviance = mean(cue_after_time_deviance, na.rm=T),
                  
                  mean.movement_time_deviance = mean(movement_time_deviance, na.rm=T)) %>%
        mutate(prop.perfect.trials = n.perfect.trials / n.trials)
      
      if(experimental) {
        exp_summary_data <- data %>%
          group_by_(.dots = group_var) %>%
          summarise()
        
        summary_data <- summary_data %>% full_join(exp_summary_data)
        
        # add summary attribute
        summary_data <- add_data_tag(summary_data, tag_name="is_m2c2_experimental_summary", tag_value=T)
      }
      
    } else {
      
      # raise error if `data` not scored
      stop("Please score `data` first. Please try again.")
    }
    
  } else {
    
    # raise error if not a data.frame or tibble
    stop("`data` is not a data.frame or tibble. Please try again.")
  }
  
  # make sure that all non-ID columns have a prefix that is unique to the task
  summary_data <- append_colname_prefix(summary_data, group_var, var_prefix)
  
  # add processing hash and timestamp
  summary_data <- summary_data %>%
    append_process_cols()
  
  # add summary attribute
  summary_data <- add_data_tag(summary_data, tag_name="is_m2c2_summary", tag_value=T)
  
  return(summary_data)
}