#' RM2C2dev
#' @name summary_color_dots
#' @export
#' @import tidyverse
summary_color_dots <- function(data, group_var, var_prefix = "color_dots", experimental = F) {
  
  # check if data.frame or tibble
  if(is_data_frame_tibble(data)) {
    
    # check if data is scored
    if(is_data_tag_valid(data, tag_name = "is_m2c2_scored", tag_value = T)) {
      
      # produce primary outcome summary
      summary_data <- data %>%
        group_by_(.dots = group_var) %>%
        summarise(median.RT.color = median(ColorRT,na.rm=T),
                  sd.RT.color = sd(ColorRT,na.rm=T),
                  
                  median.RT.location = median(LocRT,na.rm=T),
                  sd.RT.location = sd(LocRT,na.rm=T),
                  
                  stage2.median.precision.swap = median(stage2.distance.from.color.probe[stage2.classification == "SWAP"], na.rm=T),
                  stage2.sd.precision.swap = sd(stage2.distance.from.color.probe[stage2.classification == "SWAP"], na.rm=T),
                  
                  stage2.median.precision.correct = median(stage2.distance.from.color.probe[stage2.classification == "CORRECT"], na.rm=T),
                  stage2.sd.precision.correct = sd(stage2.distance.from.color.probe[stage2.classification == "CORRECT"], na.rm=T),
                  
                  stage2.median.precision.random = median(stage2.distance.from.color.probe[stage2.classification == "RANDOM"], na.rm=T),
                  stage2.sd.precision.random = sd(stage2.distance.from.color.probe[stage2.classification == "RANDOM"], na.rm=T),
                  
                  stage2.median.precision.correct.or.swap = median(stage2.distance.from.color.probe[stage2.classification == "CORRECT" | stage2.classification == "SWAP"], na.rm=T),
                  stage2.sd.precision.correct.or.swap = sd(stage2.distance.from.color.probe[stage2.classification == "CORRECT" | stage2.classification == "SWAP"], na.rm=T),
                  
                  stage1.swap.count = sum(stage1.classification == "SWAP"),
                  stage2.swap.count = sum(stage2.classification == "SWAP"),
                  
                  stage1.random.count = sum(stage1.classification == "RANDOM"),
                  stage2.random.count = sum(stage2.classification == "RANDOM"),
                  
                  stage1.correct.count = sum(stage1.classification == "CORRECT"),
                  stage2.correct.count = sum(stage2.classification == "CORRECT"),
                  
                  n = n()) %>%
        mutate(stage1.swap.prop = stage1.swap.count / n,
               stage2.swap.prop = stage2.swap.count / n,
               
               stage1.random.prop = stage1.random.count / n,
               stage2.random.prop = stage2.random.count / n,
               
               stage1.correct.prop = stage1.correct.count / n,
               stage2.correct.prop = stage2.correct.count / n)
      
      if(experimental) {
        exp_summary_data <- data %>%
          group_by_(.dots = group_var) %>%
          summarise(median_RT_stage1_swap = median(ColorRT[stage1.classification == "SWAP"], na.rm=T))
        
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