#' RM2C2dev
#' @name add_scored_tag
#' @export
#' @import tidyverse
add_scored_tag <- function(data) {
  # add scored attribute
  attr(data, "is_m2c2_scored") <- T
  
  return(data)
}