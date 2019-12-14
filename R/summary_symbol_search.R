#' RM2C2dev
#' @name summary_symbol_search
#' @export
#' @import tidyverse
summary_symbol_search <- function(data, group_var, outcomes = "all", lure_label = "LURE", normal_label = "NORMAL") {
  task_name <- "symbol_search"
  summary_data <- data %>%
    group_by_(.dots = group_var) %>%
    mutate(accuracy = ifelse(user_response == correct_response,1,0)) %>%
    summarise(median.RT.all_trials = median(response_time, na.rm=T),
              median.RT.accurate_trials = median(response_time[accuracy == 1], na.rm=T),
              median.RT.error_trials = median(response_time[accuracy == 0], na.rm=T),
              median.RT.lure_trials = median(response_time[trial_type == lure_label], na.rm=T),
              median.RT.normal_trials = median(response_time[trial_type == normal_label], na.rm=T),
              sd.RT.all_trials = sd(response_time, na.rm=T),
              sd.RT.accurate_trials = sd(response_time[accuracy == 1], na.rm=T),
              sd.RT.error_trials = sd(response_time[accuracy == 0], na.rm=T),
              sd.RT.lure_trials = sd(response_time[trial_type == lure_label], na.rm=T),
              sd.RT.normal_trials = sd(response_time[trial_type == normal_label], na.rm=T),
              proportion.accurate.trials = sum(accuracy)/n(),
              proportion.error.trials = (n() - sum(accuracy))/n(),
              n.filtered.trials = sum(is.na(response_time)),
              n.accurate.trials = sum(accuracy),
              n.error.trials = n() - sum(accuracy),
              n.lure.trials = sum(trial_type == lure_label),
              n.normal.trials = sum(trial_type == normal_label),
              # correct / incorrect tally for each of the breakdowns
              n.trials = n())
  
  # add task name to column names
  len_group_var = length(group_var)
  names(summary_data)[(len_group_var+1):ncol(summary_data)] <- paste0(task_name,".",names(summary_data)[(len_group_var+1):ncol(summary_data)])
  return(summary_data)
}