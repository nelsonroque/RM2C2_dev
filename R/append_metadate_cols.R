#' RM2C2dev
#' @name append_metadate_cols
#' @param data class: data.frame
#' @param time_var class: string
#' @param time_format  class: string
#' @param time_origin class: string
#' @param timezone class: string
#' @param append_rounded class: boolean
#' @import tidyverse
#' @import anytime
#' @examples
#' append_metadate_cols(data, time_var = 'timestamp')
#' @export
append_metadate_cols <- function(data, time_var = "timestamp", time_format = "%m/%d/%Y", time_origin = "1970-01-01", timezone = Sys.timezone(), append_rounded = T) {
  #' parameter validation
  
  #' verify data passed in is data.frame or tibble
  if(is_data_frame_tibble(data)) {
    
    #' append date features
    data_dates <- data %>%
      #' pass in time_stamp as string and convert to date time object (as.POSIXct)
      mutate(dt_datetime = as.POSIXct(UQ(sym(time_var)), 
                                           origin = time_origin, 
                                           format=time_format,
                                           tz=timezone)) %>%
      #' append date and time features
      mutate(dt_month = lubridate::month(dt_datetime),
             dt_day = lubridate::day(dt_datetime),
             dt_year = lubridate::year(dt_datetime),
             dt_week = lubridate::week(dt_datetime),
             dt_hms = format(anytime::anytime(dt_datetime), "%H:%M:%S"),
             dt_hour = lubridate::hour(dt_datetime),
             dt_minute = lubridate::minute(dt_datetime),
             dt_weekday_val = lubridate::wday(dt_datetime),
             dt_weekday_label = lubridate::wday(dt_datetime,label=T)) %>%
      #' add weekend label
      mutate(dt_is_weekend = ifelse(dt_weekday_label == "Sat" | dt_weekday_label == "Sun",T,F))
    
    #' append rounded date timestamps if requested
    if(append_rounded) {
    data_dates <- data_dates %>%
      #' append rounded date timestamps if requested
      mutate(dt_datetimestamp_round_5min = lubridate::round_date(dt_datetime, "5 minutes"),
             dt_datetimestamp_round_10min = lubridate::round_date(dt_datetime, "10 minutes"),
             dt_datetimestamp_round_15min = lubridate::round_date(dt_datetime, "15 minutes"),
             dt_datetimestamp_round_30min = lubridate::round_date(dt_datetime, "30 minutes")) %>%
      #' append rounded timestamps if requested
      mutate(dt_timestamponly_round_5min = format(anytime::anytime(dt_datetimestamp_round_5min), "%H:%M:%S"),
             dt_timestamponly_round_10min = format(anytime::anytime(dt_datetimestamp_round_10min), "%H:%M:%S"),
             dt_timestamponly_round_15min = format(anytime::anytime(dt_datetimestamp_round_15min), "%H:%M:%S"),
             dt_timestamponly_round_30min = format(anytime::anytime(dt_datetimestamp_round_30min), "%H:%M:%S"))
    }
      
  } else {
    #' if data is valid, stop code execution and present an error message
    stop("`data` is not a data.frame or tibble. Please try again.")
  }
  return(data_dates)
}
