#' RM2C2dev
#' @name summary_orca
#' @export
#' @import tidyverse
summary_orca <- function(data, group_var, var_prefix = "orca", experimental = F) {
  
  # check if data.frame or tibble
  if(is_data_frame_tibble(data)) {
    
    # check if data is scored
    if(is_data_tag_valid(data, tag_name = "is_m2c2_scored", tag_value = T)) {
      
      # produce primary outcome summary
      summary_data <- data %>%
        group_by_(.dots = group_var) %>%
        summarise(mean.RT_correct = mean(response_time[response_accuracy == T], na.rm=T),
                  median.RT_correct = median(response_time[response_accuracy == T], na.rm=T),
                  sd.RT_correct = sd(response_time[response_accuracy == T], na.rm=T),
                  mean.RT_incorrect = mean(response_time[response_accuracy == F], na.rm=T),
                  median.RT_incorrect = median(response_time[response_accuracy == F], na.rm=T),
                  sd.RT_incorrect = sd(response_time[response_accuracy == F], na.rm=T),
            n_correct = sum(response_accuracy == T),
            n_incorrect = sum(response_accuracy == F)) %>%
  		mutate(n_trials_sumcorrect_incorrect = n_correct + n_incorrect,
  		       n_trials = n()) %>%
  		mutate(prop_correct = n_correct/n,
  			   prop_incorrect = n_incorrect/n)
            
      if(experimental) {
        exp_summary_data <- data %>%
          group_by_(.dots = group_var) %>%
          summarise()
        
        # experimental output not available for this task
        
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