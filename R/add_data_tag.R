#' RM2C2dev
#' @name add_data_tag
#' @export
#' @import tidyverse
add_data_tag <- function(data, tag_name = "", tag_value = T) {
  # add summary attribute
  attr(data, tag_name) <- tag_value
  
  return(data)
}