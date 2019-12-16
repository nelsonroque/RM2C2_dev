#' RM2C2dev
#' @name get_tidy_datetime
#' @param datetime class: POSIXct
#' @param timezone class: string
#' @import tidyverse
#' @examples
#' get_tidy_datetime(datetime=NA, timezone="UTC")
#' @export
get_tidy_datetime <- function(delim = "_") {
  cur_dts <- Sys.time()
  dt <- format(cur_dts, paste("%Y", "%m", "%d", "T", "%H", "%M", "%S", sep=delim))
  return(dt)
}