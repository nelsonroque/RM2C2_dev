#' RM2C2dev
#' @name get_file_ext
#' @export
#' @param filepath class: string
#' @import tidyverse
#' @examples
#' get_file_ext("C:/filepath.csv")
get_file_ext <- function(filepath) {
  file_info = strsplit(filepath,"\\.")
  file_ext = file_info[[1]][2]
  return(file_ext)
}