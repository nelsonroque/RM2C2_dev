#' RM2C2dev
#' @name data_to_json
#' @export
#' @import jsonlite
data_to_json <- function(data, filename="") {
  x <- toJSON(unname(split(data, 1:nrow(data))))

  if(filename != "") {
    print(paste0("Writing json file: ", filename))
    write(x, filename)
  }
  
  return(x)
}