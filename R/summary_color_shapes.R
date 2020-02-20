#' RM2C2dev
#' @name summary_color_shapes
#' @export
#' @import tidyverse
summary_color_shapes <- function(data, group_var, var_prefix = "color_shapes", experimental = F) {
  
  # check if data.frame or tibble
  if(is_data_frame_tibble(data)) {
    
    # check if data is scored
    if(is_data_tag_valid(data, tag_name = "is_m2c2_scored", tag_value = T)) {
      
      # produce primary outcome summary
      summary_data <- data %>%
        group_by(.dots = group_var) %>%
        summarise(median.RT.all_trials = median(response_time, na.rm=T),
                  median.RT.HIT_trials = median(response_time[HIT == 1], na.rm=T),
                  median.RT.FA_trials = median(response_time[FA == 1], na.rm=T),
                  median.RT.MISS_trials = median(response_time[MISS == 1], na.rm=T),
                  median.RT.CR_trials = median(response_time[CR == 1], na.rm=T),
                  sd.RT.all_trials = sd(response_time, na.rm=T),
                  sd.RT.HIT_trials = sd(response_time[HIT == 1], na.rm=T),
                  sd.RT.FA_trials = sd(response_time[FA == 1], na.rm=T),
                  sd.RT.MISS_trials = sd(response_time[MISS == 1], na.rm=T),
                  sd.RT.CR_trials = sd(response_time[CR == 1], na.rm=T),
                  n.HIT = sum(HIT),
                  n.FA = sum(FA),
                  n.MISS = sum(MISS),
                  n.CR = sum(CR),
                  n.change.trials = sum(trial_type == 1),
                  n.no_change.trials = sum(trial_type == 0),
                  n.filtered.trials = sum(is.na(response_time)),
                  n.trials = n()) %>%
        mutate(HIT.rate = n.HIT/n.change.trials,
               FA.rate = n.FA/n.no_change.trials) %>%
        mutate(MISS.rate = 1 - HIT.rate,
               CR.rate = 1 - FA.rate) %>%
        SDT_adj(.)
      
      if(experimental) {
        exp_summary_data <- data %>%
          group_by(.dots = group_var) %>%
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