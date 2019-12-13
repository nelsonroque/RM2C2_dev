#' RM2C2_dev: Scoring, Summarizing

#' @name append_process_cols
#' @param data class: data.frame
#' @param algo class: string
#' @import tidyverse
#' @import digest
#' @examples
#' append_process_cols <- function(data, algo="md5")
#' @export
append_process_cols <- function(data, algo="md5") {
  
  #' **************************************************
  #' parameter validation
  #' **************************************************
  if(is_tibble(data) | is.data.frame(data)) {
    
    #' generate MD5 checksum of `data`
    data_md5 <- digest::digest(data, algo=algo)
    
    #' append MD5 checksum and file processing timestamp to `data`
    data_stamped <- data %>% 
      mutate(m2c2_processing_hash = data_md5) %>%
      mutate(m2c2_processing_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    
  } else {
    
    #' if data is valid, stop code execution and present an error message
    stop("`data` is not a data.frame or tibble. Please try again.")
  }
  
  return(data_stamped)
}