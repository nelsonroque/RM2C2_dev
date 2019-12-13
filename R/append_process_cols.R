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
  data_md5 <- digest::digest(data, algo=algo)
  return(data <- data %>% mutate(m2c2_processing_hash = data_md5,
                                 m2c2_processing_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
}