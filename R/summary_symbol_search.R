#' RM2C2dev
#' @name summary_symbol_search
#' @export
#' @import tidyverse
summary_symbol_search <- function(data, group_var, var_prefix = "symbol_search", basic_outcomes = T, lure_label = "LURE", normal_label = "NORMAL") {
  # check if data.frame or tibble
  if(is_data_frame_tibble(data)) {
    # check if accuracy column exists
    if(is_col_present(data=data,colname = "accuracy")) {
      if(basic_outcomes){
        summary_data <- data %>%
          group_by_(.dots = group_var) %>%
          summarise(median_RT_correct_trials = median(response_time[accuracy == 1], na.rm=T),
                    median_RT_error_trials = median(response_time[accuracy == 0], na.rm=T),
                    n_accurate.trials = sum(accuracy),
                    n_error.trials = n() - sum(accuracy),
                    n_trials = n())
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
  return(summary_data)
}