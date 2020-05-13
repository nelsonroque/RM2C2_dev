#' RM2C2dev
#' @name catn
#' @export
catn <- function(x, n=1) {
  cat(x)
  for(i in 1:n) {
    cat("\n")
  }
}