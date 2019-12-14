#' RM2C2dev
#' @name summary_symbol_search
#' @export
#' @import tidyverse
summary_symbol_search <- function(data, group_var, outcomes = "all", lure_label = "LURE", normal_label = "NORMAL") {
  task_name <- "symbol_search"
  summary_data <- data %>%
    group_by_(.dots = group_var) %>%
    mutate(accuracy = ifelse(user_response == correct_response,1,0)) %>%
    summarise(median_RT.accurate_trials = median(response_time[accuracy == 1], na.rm=T),
              median_RT.error_trials = median(response_time[accuracy == 0], na.rm=T),
              n_accurate.trials = sum(accuracy),
              n_error.trials = n() - sum(accuracy),
              n_trials = n())
      
      # median_RT.all_trials = median(response_time, na.rm=T),
      #         median_RT.accurate_trials = median(response_time[accuracy == 1], na.rm=T),
      #         median_RT.error_trials = median(response_time[accuracy == 0], na.rm=T),
      #         median_RT.lure_trials = median(response_time[trial_type == lure_label], na.rm=T),
      #         median_RT.normal_trials = median(response_time[trial_type == normal_label], na.rm=T),
      #         sd.RT.all_trials = sd(response_time, na.rm=T),
      #         sd.RT.accurate_trials = sd(response_time[accuracy == 1], na.rm=T),
      #         sd.RT.error_trials = sd(response_time[accuracy == 0], na.rm=T),
      #         sd.RT.lure_trials = sd(response_time[trial_type == lure_label], na.rm=T),
      #         sd.RT.normal_trials = sd(response_time[trial_type == normal_label], na.rm=T),
      #         proportion_accurate.trials = sum(accuracy)/n(),
      #         proportion_error.trials = (n() - sum(accuracy))/n(),
      #         n_filtered.trials = sum(is.na(response_time)),
      #         n_accurate.trials = sum(accuracy),
      #         n_error.trials = n() - sum(accuracy),
      #         n_lure.trials = sum(trial_type == lure_label),
      #         n_normal.trials = sum(trial_type == normal_label),
      #         # correct / incorrect tally for each of the breakdowns
      #         n_trials = n())
  
  # add task name to column names
  len_group_var = length(group_var)
  names(summary_data)[(len_group_var+1):ncol(summary_data)] <- paste0(task_name,".",names(summary_data)[(len_group_var+1):ncol(summary_data)])
  return(summary_data)
}