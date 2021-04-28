#' RM2C2dev
#' @name get_src_data_from_zip
#' @export
#' @import tidyverse
get_src_data_from_zip <- function(study_id, zip_filename = NA, packs = NA) {
  
  # setup base flags for server login
  server_unzip_outname <- paste0("m2c2_zip_out_", study_id)
  
  # # unzip folder with known directory name ----
  unzip_dir <- unzip(zip_filename, exdir = server_unzip_outname)
  
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  # list files
  files_in_zip <- list.files(server_unzip_outname, recursive=T, full.names=T, pattern="*.txt")
  
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  # get unique_packs
  unique_packs = unique(packs)
  
  # init blank list for df storage
  pack_list = list()
  
  # for each pack, get columns of interest
  for(i in unique_packs) {
    print(i)
    survey_filename <- files_in_zip[grepl(i, files_in_zip)]
    survey_raw <- RM2C2dev::read_m2c2_local(survey_filename, na=".")
    survey_slim <- survey_raw %>%
      select(participant_id, session_id, device_id, start_timestamp, end_timestamp, 
             exit_status, exit_status_detail, exit_screen, beep_file, contains("_run_uuid")) %>%
      mutate(survey_type = i)
    
    pack_list[[i]] <- survey_slim
  }
  
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  # remove folder that unzips at the end of the process -----
  # so files don't get mixed on the next run
  unlink(server_unzip_outname, recursive = T, force = T)

  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  return(pack_list)
}