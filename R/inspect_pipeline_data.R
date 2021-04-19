#' RM2C2dev
#' @name inspect_pipeline_data
#' @export
#' @import tidyverse
inspect_pipeline_data <- function(pack_list, app_version = 1.3) {
  all_slim_check <- bind_rows(pack_list)
  
  unique_gap_instances <- all_slim_check %>%
    filter(flag_session_gap) %>%
    distinct()
  
  hour2_sessions <- all_slim_check %>%
    filter(session_hours >= 2)
  
  if (app_version <= 1.3) {
    hour2_sessions_ids_timestamps <- hour2_sessions %>%
      select(participant_id, device_id, session_id, start_timestamp, end_timestamp, session_hours, session_minutes) %>%
      distinct() %>%
      arrange(-session_hours)
    
    paradata_datatset_nsessions <- all_slim_check %>%
      mutate(qbert = difftime(anytime::anytime(end_timestamp), anytime::anytime(start_timestamp), units="secs")) %>%
      group_by(participant_id, device_id) %>%
      summarise(n_sessions = n(),
                mean_qbert = mean(as.numeric(qbert), na.rm=T),
                median_qbert = median(as.numeric(qbert), na.rm=T),
                sd_qbert = sd(as.numeric(qbert), na.rm=T))
    
    paradata_datatset <- all_slim_check %>%
      group_by(participant_id, device_id, exit_status) %>%
      summarise(n = n()) %>%
      pivot_wider(id_cols = c("participant_id", "device_id"), names_from = exit_status, values_from = c("n")) %>%
      mutate(part_id_len = str_length(participant_id)) %>%
      filter(part_id_len < 7) %>%
      ruf::make_tidy_colnames() %>%
      mutate(normal_minfc_ratio = normal/minimize_force_close,
             normal_timeout_ratio = normal/timed_out) %>%
      mutate(flag_potential_forceclose_usability_issue = ifelse(normal_minfc_ratio < 7, TRUE, FALSE),
             flag_potential_timeout_usability_issue = ifelse(normal_timeout_ratio < 7, TRUE, FALSE)) %>%
      inner_join(paradata_datatset_nsessions) %>%
      mutate(normal_minfc_ratio_adj = normal_minfc_ratio / n_sessions,
             normal_timeout_ratio_adj = normal_timeout_ratio / n_sessions)
  } else {
    hour2_sessions_ids_timestamps <- hour2_sessions %>%
      select(participant_id, installation_number, session_id, start_timestamp, end_timestamp, session_hours, session_minutes) %>%
      distinct() %>%
      arrange(-session_hours)
    
    paradata_datatset_nsessions <- all_slim_check %>%
      mutate(qbert = difftime(anytime::anytime(end_timestamp), anytime::anytime(start_timestamp), units="secs")) %>%
      group_by(participant_id, installation_number) %>%
      summarise(n_sessions = n(),
                mean_qbert = mean(as.numeric(qbert), na.rm=T),
                median_qbert = median(as.numeric(qbert), na.rm=T),
                sd_qbert = sd(as.numeric(qbert), na.rm=T))
    
    paradata_datatset <- all_slim_check %>%
      group_by(participant_id, installation_number, exit_status) %>%
      summarise(n = n()) %>%
      pivot_wider(id_cols = c("participant_id", "device_id"), names_from = exit_status, values_from = c("n")) %>%
      mutate(part_id_len = str_length(participant_id)) %>%
      filter(part_id_len < 7) %>%
      ruf::make_tidy_colnames() %>%
      mutate(normal_minfc_ratio = normal/minimize_force_close,
             normal_timeout_ratio = normal/timed_out) %>%
      mutate(flag_potential_forceclose_usability_issue = ifelse(normal_minfc_ratio < 7, TRUE, FALSE),
             flag_potential_timeout_usability_issue = ifelse(normal_timeout_ratio < 7, TRUE, FALSE)) %>%
      inner_join(paradata_datatset_nsessions) %>%
      mutate(normal_minfc_ratio_adj = normal_minfc_ratio / n_sessions,
             normal_timeout_ratio_adj = normal_timeout_ratio / n_sessions)
  }

  return(list(data_uniquegaps = unique_gap_instances,
              data_session2hrs = hour2_sessions,
              data_session2hrs_detail = hour2_sessions_ids_timestamps,
              data_user_paradata = paradata_datatset))
}