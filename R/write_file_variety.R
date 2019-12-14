#' RM2C2dev
#' @name write_file_variety
#' @export
#' @import tidyverse
write_file_variety <- function(data, filename) {
  if(filename != "" | !is.na(filename) | !is.null(filename)) {
    write_csv(data, filename)
    data_to_json(data, filename = filename)
  } else {
    stop("`filename` invalid. Please try again.")
  }
} 
