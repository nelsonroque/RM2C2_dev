#' RM2C2dev
#' @name run_dataquality_checks
#' @export
#' @import tidyverse
run_dataquality_checks <- function(pack_list) {
  all_slim_check <- bind_rows(pack_list)
  
  # what percentage of the data has sessions longer than 2 hours? ----
  t1a <- as.data.frame(prop.table(table(all_slim_check$session_hours > 2)))
  t1b <- as.data.frame(table(all_slim_check$session_hours > 2))
  t1_session_gr_2hrs <- inner_join(t1a, t1b, by="Var1") %>% rename(status="Var1", proportion="Freq.x", count="Freq.y") %>% 
    mutate(datacheck = "t1_session_gr_2hrs")
  
  # proportion of drop to non-drop across surveys -----
  t2a <- as.data.frame(prop.table(table(all_slim_check$flag_session_gap)))
  t2b <- as.data.frame(table(all_slim_check$flag_session_gap))
  t2_session_gaps <- inner_join(t2a, t2b, by="Var1") %>% rename(status="Var1", proportion="Freq.x", count="Freq.y") %>% 
    mutate(datacheck = "t2_session_gaps")
  
  # what proportion of records are a complete survey? -----
  t3a <- as.data.frame(prop.table(table(all_slim_check$flag_session_complete)))
  t3b <- as.data.frame(table(all_slim_check$flag_session_complete))
  t3_complete_session <- inner_join(t3a, t3b, by="Var1") %>% rename(status="Var1", proportion="Freq.x", count="Freq.y") %>% 
    mutate(datacheck = "t3_complete_session")
  
  # what proportion of records are a complete survey + 2+ cogs? -----
  # TBD
  
  # what proportion of records get to the thank you screen ----
  t5a <- as.data.frame(prop.table(table(all_slim_check$flag_exitscreen_thankyou)))
  t5b <- as.data.frame(table(all_slim_check$flag_exitscreen_thankyou))
  t5_to_thankyou_screen <- inner_join(t5a, t5b, by="Var1") %>% rename(status="Var1", proportion="Freq.x", count="Freq.y") %>% 
    mutate(datacheck = "t5_to_thankyou_screen")
  
  # what proportion of records get to the closing screen ----
  t6a <- as.data.frame(prop.table(table(all_slim_check$flag_exitscreen_closing))) # checkin pack
  t6b <- as.data.frame(table(all_slim_check$flag_exitscreen_closing))
  t6_to_closing_screen <- inner_join(t6a, t6b, by="Var1") %>% rename(status="Var1", proportion="Freq.x", count="Freq.y") %>% 
    mutate(datacheck = "t6_to_closing_screen")
  
  # what proportion of records have minimize/force close event? ----
  t7a <- as.data.frame(prop.table(table(all_slim_check$flag_status_minimize_forceclose)))
  t7b <- as.data.frame(table(all_slim_check$flag_status_minimize_forceclose))
  t7_exit_min_forceclose <- inner_join(t7a, t7b, by="Var1") %>% rename(status="Var1", proportion="Freq.x", count="Freq.y") %>% 
    mutate(datacheck = "t7_exit_min_forceclose")
  
  # what proportion of records have normal exit event? ----
  t8a <- as.data.frame(prop.table(table(all_slim_check$flag_status_normal)))
  t8b <- as.data.frame(table(all_slim_check$flag_status_normal))
  t8_exit_normal <- inner_join(t8a, t8b, by="Var1") %>% rename(status="Var1", proportion="Freq.x", count="Freq.y") %>% 
    mutate(datacheck = "t8_exit_normal")
  
  t9a <- as.data.frame(prop.table(table(all_slim_check$flag_timeout)))
  t9b <- as.data.frame(table(all_slim_check$flag_timeout))
  t9_exit_timeout <- inner_join(t9a, t9b, by="Var1") %>% rename(status="Var1", proportion="Freq.x", count="Freq.y") %>% 
    mutate(datacheck = "t9_exit_timeout")
  
  all_checks <- bind_rows(t1_session_gr_2hrs, 
                          t2_session_gaps,
                          t3_complete_session,
                          #t4_complete_survey_2cogs,
                          t5_to_thankyou_screen,
                          t6_to_closing_screen,
                          t7_exit_min_forceclose,
                          t8_exit_normal,
                          t9_exit_timeout) %>%
    pivot_wider(id_cols = c("datacheck"), names_from = status, values_from = c("proportion", "count"))
}