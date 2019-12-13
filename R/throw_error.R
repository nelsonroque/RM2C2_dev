#' RM2C2_dev: Data Analysis Suite

#' @name throw_error
#' @param msg class: string
#' @examples
#' throw_error(text="") 
#' @export
throw_error <- function(msg="") {
  
  #' **************************************************
  #' parameter validation
  #' **************************************************
  if(is.na(msg) | msg == ""){
    stop("Please provide an informative error message `msg`")
  } else {
    stop(msg)
  }

}