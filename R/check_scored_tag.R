#' RM2C2dev
#' @name check_scored_tag
#' @export
#' @import tidyverse
check_scored_tag <- function(data) {
  # check scored attribute
  if(attr(data, 'is_m2c2_scored') == T) {
    v <- T
  } else {
    v <- F
  }
  return(v)
}