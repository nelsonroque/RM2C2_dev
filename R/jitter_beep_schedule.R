#' RM2C2dev
#' @name jitter_beep_schedule
#' @export
jitter_beep_schedule <- function(schedule=NA, schedule_name="default", jitter_time_by_hours=1, jitter_days_by_ndays = 0) {
  
  # determine directionality for schedule jitter
  if(jitter_time_by_hours > 0) {
    label_dir = "plus"
  } else {
    if(jitter_time_by_hours < 0) {
      label_dir = "minus"
    }
  }
  
  # jitter schedule times (by the `jitter_time_by_hours` parameter)
  jitter_schedule = schedule %>%
    mutate(startTime = round(as.numeric(startTime), 2),
           endTime = round(as.numeric(endTime), 2)) %>%
    mutate(startTime = startTime + jitter_time_by_hours,
           endTime = endTime + jitter_time_by_hours) %>%
    rowwise() %>%
    mutate(day = find_weekday(wday=day, days=jitter_days_by_ndays))
  
  # calculate minimum beep time
  min_beep_time = min(jitter_schedule$startTime)
  
  # create concatenated schedule name
  if(is.na(jitter_days_by_ndays)){
    schedule_name_f = paste0(schedule_name, " - ",
                             label_dir, " ",
                             as.character(abs(jitter_time_by_hours)), " - ",
                             "(earliest beep time: ", min_beep_time, ")")
  } else {
    schedule_name_f = paste0(schedule_name, " - ",
                             label_dir, " ",
                             as.character(abs(jitter_time_by_hours)), " - ",
                             "(earliest beep time: ", min_beep_time, ")", " - ",
                             "(jitter by days: ", jitter_days_by_ndays, ")")
  }
  
  # add jitter name
  jitter_schedule_f = jitter_schedule %>%
    mutate(schedule_name = schedule_name_f)
  
  return(jitter_schedule_f)
}