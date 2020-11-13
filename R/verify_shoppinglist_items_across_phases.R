#' RM2C2dev
#' @name verify_shoppinglist_items_across_phases
#' @export
#' @param filepath class: string
#' @param debug class: boolean
#' @import tidyverse
#' @import readr
#' @examples
#' verify_shoppinglist_items_across_phases(filepath, debug=T)
verify_shoppinglist_items_across_phases <- function(filepath, debug=F, drop_dups = T) {
  sl_raw <- read_delim(filepath, "|", escape_double = FALSE, trim_ws = TRUE)
  
  sl_slim <- sl_raw %>%
    select(cogtask_run_uuid, participant_id, session_uuid, filename, item, target_price, phase)
  
  check_df <- tibble()
  for(i in unique(sl_slim$cogtask_run_uuid)) {
    
    if(drop_dups) {
      cur_df <- sl_slim %>% filter(cogtask_run_uuid == i) %>% distinct()
    } else {
      cur_df <- sl_slim %>% filter(cogtask_run_uuid == i)
    }
    
    phase1 = cur_df %>% filter(phase == ".")
    phase2 = cur_df %>% filter(phase == "2")
    
    items_match <- sort(phase1$item) %in% sort(phase2$item)
    n_items_match <- sum(items_match)
    phase1_items_str <- paste0(sort(phase1$item), collapse=",")
    phase2_items_str <- paste0(sort(phase2$item), collapse=",")
    
    # for debugging ------
    if(debug) {
      print(i)
      print(paste0(sort(phase1$item), collapse=","))
      print(n_items_match)
      print(items_match)
      print("----------")
    }
    export_df <- tibble(participant_id = cur_df$participant_id[1],
                        filename = cur_df$filename[1],
                        cogtask_run_uuid = i,
                        n_items_match = n_items_match,
                        phase1_items_str = phase1_items_str,
                        phase2_items_str = phase2_items_str)
    check_df <- bind_rows(check_df, export_df)
  }
  return(check_df)
}