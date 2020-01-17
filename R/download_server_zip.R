#' RM2C2dev
#' @name download_server_zip
#' @export
#' @param url class: string
#' @param params class: list
#' @param save_filename class: string
#' @param overwrite_zip class: boolean
#' @param unzip class: boolean
#' @param remove_zip class: boolean
#' @import tidyverse
#' @import httr
#' @examples
#' download_server_zip(url, params, save_filename, overwrite_zip=T, unzip=T, remove_zip=F)

download_server_zip <- function(url, params, save_filename, overwrite_zip=F, unzip=T, remove_zip=F) {
  
  # print message to echo input
  print(paste0("Downloading data from remote server: ", url))
  
  #' make POST request and save zip file to disk
  res <- try(httr::POST(url, body = params, encode = "form", httr::write_disk(save_filename, overwrite = overwrite_zip)))
  
  #' unzip downloaded zip file if requested
  if(unzip) {
    # unzip the file
    try(unzip(save_filename))
  }
  
  #' remove zip if requested
  if(remove_zip) {
    file.remove(save_filename)
  }
  
  return(list(zip_location = paste0(save_filename)))
}