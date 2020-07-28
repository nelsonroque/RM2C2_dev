#' RM2C2dev
#' @name summary_shopping_list
#' @export
#' @import tidyverse
summary_shopping_list <- function(data, group_var, var_prefix = "shopping_list", experimental = F, lure_label = "LURE", normal_label = "NORMAL") {
  
  # check if data.frame or tibble
  if(is_data_frame_tibble(data)) {
    
    # check if data is scored
    if(is_data_tag_valid(data, tag_name = "is_m2c2_scored", tag_value = T)) {
      
      # produce primary outcome summary
      summary_data <- data %>%
        group_by_(.dots = group_var) %>%
        summarise(mean.RT.judgement = mean(judgement_RT, na.rm = T),
                  median.RT.judgement = median(judgement_RT, na.rm = T),
                  sd.RT.judgement = sd(judgement_RT, na.rm = T),
                  mean.RT.choice = mean(choice_RT, na.rm = T),
                  median.RT.choice = median(choice_RT, na.rm = T),
                  sd.RT.choice = sd(choice_RT, na.rm = T),
                  n.correct = sum(correct),
                  n.incorrect = sum(correct == 0 & phase == 2),
                  n = max(trial_num[phase == 2])) %>%
        mutate(prop.correct = n.correct/n,
        prop.incorrect = n.incorrect/n)
            
      if(experimental) {
        exp_summary_data <- data %>%
          group_by_(.dots = group_var) %>%
          summarise(mean.RT.correct = mean(choice_RT[correct == 1], na.rm=T),
                  median.RT.correct = median(choice_RT[correct == 1], na.rm=T),
                  sd.RT.correct = sd(choice_RT[correct == 1], na.rm=T),
                  mean.RT.incorrect = mean(choice_RT[correct == 0], na.rm=T),
                  median.RT.incorrect = median(choice_RT[correct == 0], na.rm=T),
                  sd.RT.incorrect = sd(choice_RT[correct == 0], na.rm=T))
        
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