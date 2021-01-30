#' RM2C2dev: Scoring, Summarizing

#' @name score_dot_memory_interference
#' @export
score_dot_memory_interference <- function(df) {
  
  restructured <- df %>%
    mutate(interference_locations = strsplit(as.character(interference_locations), " ")) %>% 
    unnest(interference_locations) %>%
    separate(interference_locations, c("timestamp", "interf_page", "tap_column", "tap_row", "tap_correct", "pixel_x", "pixel_y")) %>%
    mutate(interf_page = as.numeric(interf_page),
           tap_row = as.numeric(tap_row),
           tap_column = as.numeric(tap_column),
           tap_correct = as.numeric(tap_correct),
           pixel_x = as.numeric(pixel_x),
           pixel_y = as.numeric(pixel_y)) %>% 
    mutate(is_data_restructured = TRUE) %>%
    mutate(timestamp_diff = timestamp - lag(timestamp))
  
  # add processing hash and timestamp
  scored <- restructured %>%
    append_process_cols()
  
  # add scored attribute
  scored <- add_data_tag(scored, tag_name="is_m2c2_scored", tag_value=T)
  
  return(scored)
}