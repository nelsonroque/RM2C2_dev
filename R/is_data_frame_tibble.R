#' RM2C2dev: Scoring, Summarizing

#' @name is_data_frame_tibble
#' @param data class: data.frame or tibble
#' @import tidyverse
#' @examples
#' is_data_frame_tibble(data)
#' @export
is_data_frame_tibble <- function(data) {
  if(is_tibble(data) | is.data.frame(data)) {
    v <- T
  } else {
    v <- F
  }
  return(v)
}