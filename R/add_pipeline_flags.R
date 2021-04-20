#' RM2C2dev
#' @name add_pipeline_flags
#' @export
#' @import tidyverse
add_pipeline_flags <- function(pack_list, app_version = 1.3) {
  pack_df <- bind_rows(pack_list)
  
  if(app_version < 1.4) {
    all_slim_check <- pack_df %>%
      arrange(participant_id, session_id, start_timestamp) %>%
      group_by(participant_id, device_id) %>%
      arrange(session_id) %>%
      mutate(dt_doy = lubridate::yday(start_timestamp),
             next_session_id = lead(session_id),
             session_difference = next_session_id - session_id) %>%
      mutate(flag_session_gap = ifelse(session_difference > 1 & !is.na(session_difference), TRUE, FALSE)) %>%
      mutate(flag_timeout = if_else(exit_status == "TIMED_OUT", TRUE, FALSE)) %>%
      mutate(flag_status_minimize_forceclose = if_else(exit_status == "MINIMIZE/FORCE_CLOSE", TRUE, FALSE)) %>%
      mutate(flag_status_normal = if_else(exit_status == "NORMAL", TRUE, FALSE)) %>%
      mutate(flag_status_gopack = if_else(exit_status == "GO_PACK", TRUE, FALSE)) %>%
      mutate(flag_exitscreen_closing = grepl("closing", exit_screen)) %>%
      mutate(flag_exitscreen_thankyou = grepl("thank_you|thankyou", exit_screen)) %>%
      rowwise() %>%
      mutate(flag_session_complete = if_else((flag_status_normal | flag_status_minimize_forceclose) & (flag_exitscreen_closing | flag_exitscreen_thankyou), TRUE, FALSE)) %>%
      mutate(flag_valid_survey_complete = if_else(flag_session_complete & survey_type != "checkin", TRUE, FALSE)) %>%
      group_by(participant_id, device_id, session_id) %>%
      mutate(flag_session_count = row_number()) %>%
      mutate(session_hours = difftime(anytime::anytime(end_timestamp), anytime::anytime(start_timestamp), units="hours"),
             session_minutes = difftime(anytime::anytime(end_timestamp), anytime::anytime(start_timestamp), units="min"))
  } else {
    all_slim_check <- pack_df %>%
      arrange(participant_id, session_id, start_timestamp) %>%
      group_by(participant_id, install_number) %>%
      arrange(session_id) %>%
      mutate(dt_doy = lubridate::yday(start_timestamp),
             next_session_id = lead(session_id),
             session_difference = next_session_id - session_id) %>%
      mutate(flag_session_gap = ifelse(session_difference > 1 & !is.na(session_difference), TRUE, FALSE)) %>%
      mutate(flag_timeout = if_else(exit_status == "TIMED_OUT", TRUE, FALSE)) %>%
      mutate(flag_status_minimize_forceclose = if_else(exit_status == "MINIMIZE/FORCE_CLOSE", TRUE, FALSE)) %>%
      mutate(flag_status_normal = if_else(exit_status == "NORMAL", TRUE, FALSE)) %>%
      mutate(flag_status_gopack = if_else(exit_status == "GO_PACK", TRUE, FALSE)) %>%
      mutate(flag_exitscreen_closing = grepl("closing", exit_screen)) %>%
      mutate(flag_exitscreen_thankyou = grepl("thank_you|thankyou", exit_screen)) %>%
      rowwise() %>%
      mutate(flag_session_complete = if_else((flag_status_normal | flag_status_minimize_forceclose) & (flag_exitscreen_closing | flag_exitscreen_thankyou), TRUE, FALSE)) %>%
      mutate(flag_valid_survey_complete = if_else(flag_session_complete & survey_type != "checkin", TRUE, FALSE)) %>%
      group_by(participant_id, device_id, session_id) %>%
      mutate(flag_session_count = row_number()) %>%
      mutate(session_hours = difftime(anytime::anytime(end_timestamp), anytime::anytime(start_timestamp), units="hours"),
             session_minutes = difftime(anytime::anytime(end_timestamp), anytime::anytime(start_timestamp), units="min"))
  }
  return(all_slim_check)
}