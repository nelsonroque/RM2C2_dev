#' RM2C2dev
#' @name is_col_present
#' @export
#' @import tidyverse
is_col_present <- function(data, colname) {
  if(is_data_frame_tibble(data)) {
    if(colname %in% colnames(data)) {
      v <- T
    } else{
      v <- F
    }
  } else {
    stop("`data` is not a data.frame or tibble. Please try again.")
  }
  return(v)
}