#' RM2C2dev
#' @name is_data_frame_tibble
#' @export
#' @param data class: data.frame
#' @import tidyverse
#' @examples
#' is_data_frame_tibble(data)
is_data_frame_tibble <- function(data) {
  #' check if data is tibble or data.frame
  if(is_tibble(data) | is.data.frame(data)) {
    v <- T
  } else {
    v <- F
  }
  return(v)
}