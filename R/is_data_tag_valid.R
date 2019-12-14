#' RM2C2dev
#' @name is_data_tag_valid
#' @export
#' @import tidyverse
is_data_tag_valid <- function(data, tag_name = "", tag_value = T) {
  
  # does the attribute exist?
  if(is.null(attr(data, tag_name))){
    v <- F
    
  } else {
    
    # check attribute against provided value
    if(attr(data, tag_name) == tag_value) {
      v <- T
    } else {
      v <- F
    }
  }
  
  return(v)
}