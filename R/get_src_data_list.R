#' RM2C2dev
#' @name get_src_data_list
#' @export
#' @import tidyverse
get_src_data_list <- function(study_id = NA, 
                               server_url = NA, 
                               study_code = NA, 
                               user_id = NA, 
                               user_pass = NA, 
                               packs = NA, 
                               participant_ids = NA,
                               app_version=1.3) {
  
  # setup base flags for server login
  server_parsedata <- "true"
  server_zip_outname <- paste0("M2C2_latest_parse_", study_id, ".zip")
  server_unzip_outname <- "m2c2_zip_out"
  
  # STEP 3) DOWNLOADING DATA FROM DREAM / SRC -----
  
  # set tibbles for each server type
  server_creds <- tibble::tibble(url = paste0(server_url, "pulldata.php"),
                                 studyname = study_id,
                                 study_code = study_code)
  
  # create list with parameters expected by data download endpoint ----
  batch_mode =F
  if(!is.na(participant_ids)) {
    batch_mode = T
    login <- list(
      study_id = server_creds$studyname,
      parsed = server_parsedata,
      participant_id_select = participant_ids,
      security_code = server_creds$study_code,
      auth_token = RM2C2dev::get_m2c2_auth_token(server_url = server_url, 
                                                 user_id = user_id, 
                                                 password = user_pass, 
                                                 study_id = study_id)
    )
  } else {
    login <- list(
      study_id = server_creds$studyname,
      parsed = server_parsedata,
      security_code = server_creds$study_code,
      auth_token = RM2C2dev::get_m2c2_auth_token(server_url = server_url, 
                                                 user_id = user_id, 
                                                 password = user_pass, 
                                                 study_id = study_id)
    )
  }
  
  # download zip file with parsed data ----
  zip_result <- RM2C2dev::download_zip_server(url = server_creds$url,
                                              params = login,
                                              save_filename = server_zip_outname,
                                              overwrite_zip=T,
                                              unzip=F,
                                              remove_zip=F)
  
  # unzip folder with known directory name ----
  unzip_dir <- unzip(zip_result$zip_location, exdir = server_unzip_outname)
  
  #score and summarise data
  files_in_zip <- list.files(server_unzip_outname, recursive=T, full.names=T, pattern="*.txt")
  
  # get unique_packs
  unique_packs = unique(packs)
  
  # init blank list for df storage
  pack_list = list()
  
  # for each pack, get columns of interest
  for(i in unique_packs) {
    print(paste0("Reading pack_id: ", i))
    
    # search for pack
    survey_filename <- files_in_zip[grepl(i, files_in_zip)]
    
    if(purrr::is_empty(survey_filename)) {
      survey_raw <- tibble(errors = c("file does not exist"), filename = c(survey_filename))
    } else {
      if(file.exists(survey_filename)) {
        survey_raw <- RM2C2dev::read_m2c2_local(survey_filename, na=".")
      } else {
        survey_raw <- tibble(errors = c("file read error"), filename = c(survey_filename))
      }
    }
    
    if(batch_mode == T) {
      # save data into list
      pack_list[[i]] <- survey_raw
    } else {
      # save data into list
      pack_list[[i]] <- survey_raw
    }
  
  }

  # remove folder that unzips at the end of the process -----
  # so files don't get mixed on the next run
  unlink(server_unzip_outname, recursive = T, force = T)
  unlink(server_zip_outname)
  
  return(pack_list)
}