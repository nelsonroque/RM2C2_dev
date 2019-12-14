#' RM2C2dev
#' @name summary_symbol_search
#' @export
#' @import tidyverse
summary_symbol_search <- function(data, group_var, var_prefix = "symbol_search", experimental = F, lure_label = "LURE", normal_label = "NORMAL") {
  
  # check if data.frame or tibble
  if(is_data_frame_tibble(data)) {
    
    # check if data is scored
    if(is_data_tag_valid(data, tag_name = "is_m2c2_scored", tag_value = T)) {
      
      # produce primary outcome summary
      summary_data <- data %>%
        group_by_(.dots = group_var) %>%
        summarise(median_response_time_correct_trials = median(response_time[accuracy == 1], na.rm=T),
                  median_response_time_error_trials = median(response_time[accuracy == 0], na.rm=T),
                  n_accurate_trials = sum(accuracy),
                  n_error_trials = n() - sum(accuracy),
                  n_trials = n())
      
      if(experimental) {
        exp_summary_data <- data %>%
          group_by_(.dots = group_var) %>%
          summarise(median_response_time_lure_trials = median(response_time[trial_type == lure_label], na.rm=T),
                    median_response_time_normal_trials = median(response_time[trial_type == normal_label], na.rm=T),
                    sd_response_time_all_trials = sd(response_time, na.rm=T),
                    sd_response_time_accurate_trials = sd(response_time[accuracy == 1], na.rm=T),
                    sd_response_time_error_trials = sd(response_time[accuracy == 0], na.rm=T),
                    sd_response_time_lure_trials = sd(response_time[trial_type == lure_label], na.rm=T),
                    sd_response_time_normal_trials = sd(response_time[trial_type == normal_label], na.rm=T),
                    n_lure_trials = sum(trial_type == lure_label),
                    n_normal_trials = sum(trial_type == normal_label))
          
        summary_data <- summary_data %>% full_join(exp_summary_data)
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
  
  # add summary attribute
  summary_data <- add_data_tag(summary_data, tag_name="is_m2c2_summary", tag_value=T)
  
  return(summary_data)
}