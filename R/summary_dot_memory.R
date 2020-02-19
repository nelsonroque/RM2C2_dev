#' RM2C2dev
#' @name summary_dot_memory
#' @export
#' @import tidyverse
summary_dot_memory <- function(data, group_var, var_prefix = "dot_memory", experimental = F) {
  
  # check if data.frame or tibble
  if(is_data_frame_tibble(data)) {
    
    # check if data is scored
    if(is_data_tag_valid(data, tag_name = "is_m2c2_scored", tag_value = T)) {
      
      # produce primary outcome summary
      summary_data <- data %>%
        group_by(.dots = group_var) %>%
        summarise(median.RT.all_trials = median(response_time, na.rm=T),
                  sd.RT.all_trials = sd(response_time, na.rm=T),
                  
                  median.RT.perfect_trials = median(response_time[is_perfect_trial == 1], na.rm=T),
                  sd.RT.perfect_trials = sd(response_time[is_perfect_trial == 1], na.rm=T),
                  
                  median.RT.nonperfect_trials = median(response_time[is_perfect_trial == 0], na.rm=T),
                  sd.RT.nonperfect_trials = sd(response_time[is_perfect_trial == 0], na.rm=T),
                  
                  median.error.distance.overall = median(sum_error_distance, na.rm=T),
                  sd.error.distance.overall = sd(sum_error_distance, na.rm=T),
                  sum.error.distance.overall = sum(sum_error_distance, na.rm=T),
                  sum.ambiguous.responses = sum(n_ambiguous_responses, na.rm=T),
                  mean.prop.ambiguous.responses = mean(prop_ambiguous_responses, na.rm=T),
                  
                  mean_hausdorff_distance = mean(hausdorff_distance, na.rm=T),
                  
                  count.perfect.dots = sum(sum_perfect_dots),
                  count.perfect.trials = sum(is_perfect_trial, na.rm=T),
                  n.filtered.trials = sum(is.na(response_time)),
                  n.trials = n())
      
      if(experimental) {
        exp_summary_data <- data %>%
          group_by(.dots = group_var) %>%
          summarise(min_hausdorff_distance = min(hausdorff_distance, na.rm=T),
                    max_hausdorff_distance = max(hausdorff_distance, na.rm=T),
                    sd_hausdorff_distance = sd(hausdorff_distance, na.rm=T))
        
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