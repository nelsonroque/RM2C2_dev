#' RM2C2dev
#' @name get_data_glance
#' @export
get_data_glance <- function(x, id_var = "participant_id") {
  y <- tibble(x)
  y_digest <- digest::digest(y)
  uids <-tryCatch(y %>% select(id_var) %>% distinct() %>% paste(., collapse=","),  error = function(e) NA)
  luids <- tryCatch(y %>% select(id_var) %>% distinct() %>% pull(id_var) %>% length(.),  error = function(e) NA)
  xp <- tibble(file_md5=y_digest, fn=paste0(unique(y$filename)), nrow=nrow(y), ncol=ncol(y), unique_ids = uids, n_unique_ids = luids)
  return(xp)
}