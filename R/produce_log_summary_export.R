#' RM2C2dev
#' @name produce_log_summary_export
#' @export
#' @import tidyverse
#' @import anytime
produce_log_summary_export <- function(filepath, part_ids=NA, search_events=NA, debug=F) {
  
  if(is.na(part_ids)) {
    warning("set `part_ids` parameter (type = vector)")
  }
  
  if(is.na(search_events)) {
    warning("set `search_events` parameter (type = vector)")
  }
  
  # load in data ----
  log_table <- read_delim("log_collection_log_2020_11_13_10_11.txt", delim="|")
  
  # tidy column names for later use ----
  log_table_cc <- log_table %>% 
    ruf::make_tidy_colnames() %>%  # custom function from my other package
    select(-x18) %>%
    mutate(date_dts = anytime::anytime(local_time),
           date_d = anytime::anydate(local_time))
  
  # review unique elements - all possible headers
  poss_hd1 <- unique(log_table_cc$header1)
  poss_hd2 <- unique(log_table_cc$header2)
  n_uniq_parts <- ruf::lenu(log_table_cc$participant_id) # get number of participants in log file
  
  if(debug) {
    print(poss_hd1)
    print(poss_hd2)
  }
  
  notification_count_by_id <- log_table_cc %>%
    filter(participant_id %in% part_ids) %>%
    filter(header2 %in% search_events) %>%
    group_by(participant_id, date_d, header2) %>%
    summarise(n_events = n())
  
  return(list(notification_count_by_id=notification_count_by_id,
              log_table_filtered = log_table_cc %>% 
                filter(participant_id %in% part_ids) %>%
                filter(header2 %in% search_events)))
}
