#' RM2C2dev
#' @name get_unique_values
#' @export
#' @param col class: vector
#' @examples
#' get_unique_values(data$dates)
get_unique_values <- function(data) {
  return(c(na.omit(unique(data))))
}