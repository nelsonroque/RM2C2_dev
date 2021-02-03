#' RM2C2dev
#' @name beep_schedule_from_csv
#' @export
beep_schedule_from_csv <- function(filename=NA, schedule=NA, schedule_public_label="default") {
  
  # provide error if no schedule
  if(is.na(schedule) && is.na(filename)) {
    stop("Error: Please pass a parameter of either: `filename` string or `schedule` tibble/data.frame.")
  }
  
  # read template schedule if filename passed
  if(!is.na(filename)) {
    template_schedule <- read_csv(filename, col_types = cols(.default = "c"))
  }
  
  if(!is.na(schedule)) {
    template_schedule = schedule
  }
  
  # order based on key columns -----
  template_schedule_ord = template_schedule %>%
    arrange(pack_id, day, beep_order)
  
  # get unique packs -----
  unique_packs = unique(template_schedule_ord$pack_id)
  
  # open file
  outfilename = paste0("beep_", ruf::make_tidy_colnames(schedule_public_label), ".xml")
  sink(outfilename)
  
  # set base xml -----
  RM2C2dev::catn(paste0("<beepSchedule>"))
  RM2C2dev::catn(paste0("<beepScheduleInfo name='", schedule_public_label, "'>"))
  
  # iterate over packs to create nested schedule ------
  for(pack in unique_packs) {
    pack_data = template_schedule_ord %>% filter(pack_id == pack)
    
    # start pack schedule
    pack_open = paste0("<PackSchedule pack_id='", pack, "' activeType=","'dayweek'",">")
    RM2C2dev::catn(pack_open)
    
    # iterate over pack data to create pack schedules
    for(row in 1:nrow(pack_data)) {
      cur_row = pack_data[row,]
      
      # specify active tag for the PackSchedule
      active_tag = paste0("<active day='", cur_row$day,
                          "' startTime='",cur_row$startTime,
                          "' endTime='",cur_row$endTime,
                          "' beepInterval='",cur_row$beepInterval,
                          "' firstBeepOffset='",cur_row$beepInterval,
                          "' allowCount='",cur_row$allowCount,
                          "'>")
      RM2C2dev::catn(active_tag)
    }
    
    # close pack
    pack_close = paste0("</PackSchedule>")
    RM2C2dev::catn(pack_close)
    
  }
  # close the beep schedule
  RM2C2dev::catn(paste0("</beepSchedule>"))
  sink()
}