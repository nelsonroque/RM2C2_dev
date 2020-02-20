  #' RM2C2dev
  #' @name score_color_shapes
  #' @export
  #' @import tidyverse
  score_color_shapes <- function(data) {
    
    # check if data.frame or tibble
    if(is_data_frame_tibble(data)) {
      
      # score the data
      # in this case, adding a 1/0 accuracy column
      scored <- data %>%
        mutate(HIT = ifelse(trial_type == 1 & button_pressed == 1, 1, 0),
               FA = ifelse(trial_type == 0 & button_pressed == 1, 1, 0),
               MISS = ifelse(trial_type == 1 & button_pressed == 0, 1, 0),
               CR = ifelse(trial_type == 0 & button_pressed == 0, 1, 0))
    
    } else {
      
      # raise error if not a data.frame or tibble
      stop("`data` is not a data.frame or tibble. Please try again.")
    }
    
    # add processing hash and timestamp
    scored <- scored %>%
      append_process_cols()
    
    # add scored attribute
    scored <- add_data_tag(scored, tag_name="is_m2c2_scored", tag_value=T)
    
    return(scored)
  }