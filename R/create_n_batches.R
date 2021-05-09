#' RM2C2dev
#' @name create_n_batches
#' @export
create_n_batches <- function(.data, batches = 10) {
  batch_list = split(.data, ceiling(seq_along(.data)/batches))
  return(batch_list)
}