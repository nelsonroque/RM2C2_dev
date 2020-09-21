#' RM2C2dev
#' @name inspect_pipeline_data
#' @export
#' @import tidyverse
inspect_pipeline_data <- function(pack_list) {
  all_slim_check <- bind_rows(pack_list)
  
  unique_gap_instances <- all_slim_check %>%
    filter(flag_session_gap) %>%
    distinct()
  
  hour2_sessions <- all_slim_check %>%
    filter(session_hours >= 2)
  
  hour2_sessions_ids_timestamps <- hour2_sessions %>%
    select(participant_id, device_id, session_id, start_timestamp, end_timestamp, session_hours, session_minutes) %>%
    distinct() %>%
    arrange(-session_hours)
  
  return(list(data_uniquegaps = unique_gap_instances,
              data_session2hrs = hour2_sessions,
              data_session2hrs_detail = hour2_sessions_ids_timestamps))
}