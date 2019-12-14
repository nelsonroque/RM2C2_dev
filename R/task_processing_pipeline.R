#' RM2C2dev
#' @name task_processing_pipeline
#' @export
#' @import tidyverse
task_processing_pipeline <- function(data, source = "data.frame", score = T, summary= T, experimental = T, group_var = c("participant_id"), uuid_col_name = NA, synapse_cogtask_data_table_id = NA, synapse_survey_data_table_id = NA, synapse_email = NA, synapse_pw = NA) {
  if(source == "data.frame"){
    if(is_data_frame_tibble(data)) {
      ready_to_score <- T
    } else {
      reason_failed <- "`data` is not a data.frame or tibble."
      ready_to_score <- F
    }
  } else {
    if(source == "synapse") {
      # get raw synapse table of survey data containing task uuids
      uuid_table <- download_synapse_table_all(synapse_email=synapse_email, synapse_pw=synapse_pw, synapse_id=synapse_survey_data_table_id)
      
      # get unique UUIDs for each cog task
      task_uuids <- get_unique_values(uuid_table %>% select(uuid_col_name))
      
      # get raw synapse table of cog task data
      data <- download_synapse_cogtask_table_all(synapse_email=synapse_email, synapse_pw=synapse_pw, synapse_id=synapse_cogtask_data_table_id, uuids = task_uuids)
      
      if(nrow(data) > 0) {
        ready_to_score <- T
      } else {
        reason_failed <- "No records in `data`."
        ready_to_score <- F
      }
    } else {
      stop(paste0("Processing pipeline for ", source, "is not available yet. Please contact nur375@psu.edu to express interest."))
    }
  }
  
  if(ready_to_score) {
    if(is_col_present(data, "game_name")){
      if(data$game_name[1] == "symbol_search" | data$game_name[1] == "Symbol Search" | data$game_name[1] == "Symbol-Search"){
        
        # produce scored trial-level data.frame if requested
        if(score){
          data_scored <- RM2C2dev::score_symbol_search(symbol_search_test_data)
        } else {
          data_summary_exp <- data.frame()
        }
        
        # produce summary data.frame if requested
        if(summary){
          data_summary <- RM2C2dev::summary_symbol_search(ss_scored, group_var=group_var)
        } else {
          data_summary_exp <- data.frame()
        }
        
        # produce experimental summary data.frame if requested
        if(experimental) {
          data_summary_exp <- RM2C2dev::summary_symbol_search(ss_scored, group_var=group_var, experimental = T)
        } else {
          data_summary_exp <- data.frame()
        }
      }
    }
  } else{
    stop(paste0("Processing failed: ", reason_failed))
  }
  
  return(list(scored=data_scored, summary=data_summary, experimental=data_summary_exp))
}