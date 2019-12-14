#' RM2C2dev
#' @name summary_symbol_search
#' @export
#' @import tidyverse
summary_symbol_search <- function(data, group_var, var_prefix = "symbol_search", basic_outcomes = T, lure_label = "LURE", normal_label = "NORMAL") {
  if(basic_outcomes){
    summary_data <- data %>%
      group_by_(.dots = group_var) %>%
      mutate(accuracy = ifelse(user_response == correct_response,1,0)) %>%
      summarise(median_RT_correct_trials = median(response_time[accuracy == 1], na.rm=T),
                median_RT_error_trials = median(response_time[accuracy == 0], na.rm=T),
                n_accurate.trials = sum(accuracy),
                n_error.trials = n() - sum(accuracy),
                n_trials = n())
  }
  
  # make sure that all non-ID columns have a prefix that is unique to the task
  summary_data <- append_colname_prefix(summary_data, group_var, var_prefix)
  return(summary_data)
}