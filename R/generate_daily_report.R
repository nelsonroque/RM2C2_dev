#' RM2C2dev
#' @name generate_daily_report
#' @export
#' @import tidyverse
#' @import anytime
generate_daily_report <- function(pack_list, ignore_packs = NULL, by_pack=F, plot=F, debug=F, app_version=1.3) {
  `%nin%` <- Negate("%in%")
  
  pack_df <- bind_rows(pack_list) %>%
    mutate(dt_doy = anytime::anydate(start_timestamp)) %>%
    select(participant_id, dt_doy, session_id, everything()) %>%
    arrange(participant_id, dt_doy)
  
  if(app_version <= 1.3) {
    person_min_dates <- pack_df %>%
      group_by(participant_id, device_id) %>%
      summarise(min_date = min(dt_doy))
  } else {
    person_min_dates <- pack_df %>%
      group_by(participant_id, installation_number) %>%
      summarise(min_date = min(dt_doy))
  }

  
  if(debug) {
    View(pack_df)
    View(person_min_dates)
  }
  
  # filter packs if requested
  if(!is.null(ignore_packs)) {
    if(by_pack) {
      if(app_version <= 1.3) {
        all_slim_check <- pack_df %>%
          filter(survey_type %nin% ignore_packs) %>%
          group_by(participant_id, device_id, survey_type, dt_doy) %>%
          summarise(n_records = n())
      }
      else {
        all_slim_check <- pack_df %>%
          filter(survey_type %nin% ignore_packs) %>%
          group_by(participant_id, installation_number, survey_type, dt_doy) %>%
          summarise(n_records = n())
      }

    } else {
      if(app_version <= 1.3) {
        all_slim_check <- pack_df %>%
          filter(survey_type %nin% ignore_packs) %>%
          group_by(participant_id, device_id, dt_doy) %>%
          summarise(n_records = n())
      } else {
        all_slim_check <- pack_df %>%
          filter(survey_type %nin% ignore_packs) %>%
          group_by(participant_id, installation_number, dt_doy) %>%
          summarise(n_records = n())
      }

    }

  } else {
    if(by_pack) {
      if(app_version <= 1.3) {
        all_slim_check <- pack_df %>%
          group_by(participant_id, survey_type, device_id, dt_doy) %>%
          summarise(n_records = n())
      } else {
        all_slim_check <- pack_df %>%
          group_by(participant_id, survey_type, installation_number, dt_doy) %>%
          summarise(n_records = n())
      }

    } else {
      if(app_version <= 1.3) {
        all_slim_check <- pack_df %>%
          group_by(participant_id, device_id, dt_doy) %>%
          summarise(n_records = n())
      } else {
        all_slim_check <- pack_df %>%
          group_by(participant_id, installation_number, dt_doy) %>%
          summarise(n_records = n())
      }

    }
  }
  
  all_slim_final <- all_slim_check %>%
    inner_join(person_min_dates) %>%
    mutate(days_since_first_data = dt_doy - min_date)
  
  p <- ggplot(all_slim_final, aes(days_since_first_data, n_records)) +
    geom_point() +
    geom_line() +
    facet_grid(.~participant_id) +
    theme_minimal() +
    labs(x="Days Since First Data", y="Number of Records")
  
  if(by_pack) {
    p2 <- ggplot(all_slim_final, aes(days_since_first_data, n_records)) +
      geom_point() +
      geom_line() +
      facet_grid(survey_type~participant_id) +
      theme_minimal() +
      labs(x="Days Since First Data", y="Number of Records")
  }
  
  if(plot) {
    print(p)
    if(by_pack){
      print(p2)
    }
  }

  return(all_slim_final)
}