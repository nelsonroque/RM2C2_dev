#' RM2C2dev
#' @name is_data_scored
#' @export
#' @import tidyverse
is_data_scored <- function(data) {
  # check scored attribute
  if(attr(data, 'is_m2c2_scored') == T) {
    v <- T
  } else {
    v <- F
  }
  return(v)
}