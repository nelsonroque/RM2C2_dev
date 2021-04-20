#' RM2C2dev
#' @name summary_gonogo_fade
#' @export
#' @import tidyverse
summary_gonogo_fade <- function(data, group_var, var_prefix = "gonogofade", experimental = F) {
  
  # check if data.frame or tibble
  if(is_data_frame_tibble(data)) {
    
    # check if data is scored
    if(is_data_tag_valid(data, tag_name = "is_m2c2_scored", tag_value = T)) {
      
      # produce primary outcome summary
      summary_data <- data %>%
        group_by_(.dots = group_var) %>%
        summarise(median_response_time_all_trials = median(responseTime[responseTime != -1], na.rm=T),
                  median_response_time_correct_trials = median(responseTime[responseTime != -1 & accuracy == 1], na.rm=T),
                  median_response_time_error_trials = median(responseTime[responseTime != -1 & accuracy == 0], na.rm=T),
                  n_accurate_trials = sum(accuracy),
                  n_error_trials = n() - sum(accuracy),
                  n_trials = n())
      
      if(experimental) {
        exp_summary_data <- data %>%
          group_by_(.dots = group_var) %>%
          summarise(sd_response_time_all_trials = sd(responseTime[responseTime != -1], na.rm=T),
                    sd_response_time_accurate_trials = sd(responseTime[responseTime != -1 & accuracy == 1], na.rm=T),
                    sd_response_time_error_trials = sd(responseTime[responseTime != -1 & accuracy == 0], na.rm=T))
        
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