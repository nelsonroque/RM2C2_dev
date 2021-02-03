#' RM2C2dev
#' @name find_weekday
#' @export
find_weekday <- function(wday="Sun", days = 1, weekstart = "Sun") {
  if(days > 7) {
    stop("Error. Support not available for jittering more than 7 days")
  }
  
  # determine week start by user input
  if(weekstart == "Sun") {
    days_table = c("Sun", "Mon", "Tue", "Wed", "Thurs", "Fri", "Sat")
  } else {
    days_table = c("Mon", "Tue", "Wed", "Thurs", "Fri", "Sat", "Sun")
  }
  
  # check current index
  index_of_current_day = which(days_table == wday)
  
  # increment to sought day
  index_of_next_day = index_of_current_day + days
  
  # check range of next day
  if(index_of_next_day > 7){
    ver_index_of_next_day = index_of_next_day - 7
    ver_index_of_next_day_label = days_table[ver_index_of_next_day]
  } else {
    ver_index_of_next_day = index_of_next_day
    ver_index_of_next_day_label = days_table[ver_index_of_next_day]
  }
  
  return(ver_index_of_next_day_label)
}
