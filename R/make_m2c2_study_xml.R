#' RM2C2dev
#' @name make_m2c2_study_xml
#' @export
#' @import tidyverse
make_m2c2_study_xml <- function(csv_file = "", menu_str = "", study_name="", study_servername = "", study_wave="", study_server = "https://m2c2.survey.psu.edu/eas", app_version = "1.3", debug=0) {
  STUDY_NAME = study_name
  STUDY_WAVE = study_wave
  STUDY_SERVER = study_server
  STUDY_SERVER_NAME = study_servername
  
  # load file
  pack_raw <- read_csv(csv_file)
  
  # add metadata to protocol file
  pack_protocol <- pack_raw %>%
    group_by(survey_type) %>%
    mutate(question_n = row_number()) %>%
    mutate(screen_id = paste0(survey_type, "_q", question_n, "_", variable_name)) %>%
    mutate(maxquestion_n = max(question_n)) %>%
    mutate(lastquestion_in_survey = ifelse(question_n == maxquestion_n, TRUE, FALSE)) %>%
    mutate(nextscreen_id = lead(screen_id)) %>%
    mutate(nextscreen_id = ifelse(is.na(nextscreen_id), "exit", nextscreen_id))
  
  # get unique question types
  unique(pack_protocol$element_type)
  
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  # init new XML
  outfilename = paste0("outfile", ruf::make_tidy_datetime(), ".xml")
  sink(outfilename)
  catn('<?xml version="1.0" encoding="UTF-8"?>')
  
  # start Dolphin root
  catn(sprintf("<Dolphin studyID='%s' targetVersion='1.3' wave='%s'>", STUDY_SERVER_NAME, STUDY_WAVE))
  
  # setup Launch tag
  catn(sprintf("<Launch>
<savedata combineGameData='FALSE' gameDataType='1'/>
<gameSettings allowRelaunch='FALSE'/>

<main_menu id='start_menu'/>

<forceBeepVolume volume='25'/>
<forceBeepVolumeOnStartup volume='50'/>
<beepintro title='Time to do a survey!' text='Press the Start button to begin the Beeped Survey' button='Start' timeout='10' />
<timeout min='28'/>

<server type='custom' enable='true' name='psu_server' url='%s'/>
<server type='sagebridge' enable='FALSE'/>

<schedule alarm='beep' autoLaunchPack='true' />

<sensors>
<gps on='FALSE' time='FALSE' location='FALSE'/>
<light on='FALSE' time='FALSE'/>
<soundRec on='FALSE' time='FALSE'/>
</sensors>
</Launch>", STUDY_SERVER))
  
  # setting tag
  catn("
  <Settings>
  <textColor r='0' g='0' b='0' />
  <text size='18' />
  <titleText size='20' />
  <segmentTitleText size='18' />
  <slider hidetext='true' />
  </Settings>")
  
  # modify menu tag
  catn(sprintf(menu_str, STUDY_NAME))
  
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  row_count = 0
  for(pack in 1:length(unique(pack_protocol$survey_type))) {
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    cur_pack_type = unique(pack_protocol$survey_type)[pack]
    cur_pack = pack_protocol %>% filter(survey_type == cur_pack_type)
    
    if(debug){catn(paste0(cur_pack_type, ": ", nrow(cur_pack)))}
    
    # start the pack
    catn(paste0('<Pack id="',cur_pack_type,'" startScreen="','REPLACE_VALUE','" markScreenTime="true">'))
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    for(question in 1:length(unique(cur_pack$variable_name))) {
      row_count = row_count + 1
      cur_question_id = unique(cur_pack$variable_name)[question]
      cur_record_row = cur_pack %>% filter(variable_name == cur_question_id)
      question_id = paste0("q", question, "_", cur_question_id)
      screen_id = paste0(cur_pack_type, "_", question_id)
      
      # verify if text/element options has apostrophe
      
      if(!is.na(cur_record_row$element_text) & grepl("'", cur_record_row$element_text)) {
        cur_record_row$element_text <- gsub("'", "", cur_record_row$element_text)
      }
      
      if(!is.na(cur_record_row$`element_options (separate options with underscore)`) & grepl("'", cur_record_row$`element_options (separate options with underscore)`)) {
        cur_record_row$`element_options (separate options with underscore)` <- gsub("'", "", cur_record_row$`element_options (separate options with underscore)`)
      }
      
      # open screen -----
      catn(paste0("<Screen id='",screen_id,"'>"))
      
      # present title header (if requested by user) -----
      if(!is.na(cur_record_row$element_heading)) {
        catn(sprintf("<title text='%s'/>", cur_record_row$element_heading))
      }
      
      # draw item based on type -----
      if(cur_record_row$element_type == "slider") {
        if(is.na(cur_record_row$`slider minimum value`)) {
          cur_record_row$`slider minimum value` = "0"
        }
        
        if(is.na(cur_record_row$`slider max value`)) {
          cur_record_row$`slider max value` = "100"
        }
        
        
        catn(sprintf("<element type='slider' id='%s' text='%s' leftText='%s' rightText='%s' min='%s' max='%s' interval='%s'/>", 
                     question_id, cur_record_row$element_text, cur_record_row$`slider left anchor`, cur_record_row$`slider right anchor`,
                     cur_record_row$`slider minimum value`, cur_record_row$`slider max value`, "1"))
      }
      
      if(cur_record_row$element_type == "text") {
        catn(sprintf("<element type='text' id='%s' text='%s' leftText='%s' rightText='%s' />", question_id, cur_record_row$element_text, cur_record_row$`slider left anchor`, cur_record_row$`slider right anchor`))
      }
      
      if(cur_record_row$element_type == "radiobutton") {
        cur_opts = cur_record_row$`element_options (separate options with underscore)`
        cur_opt_list = unlist(strsplit(cur_opts, "_"))
        catn(sprintf("<element type='radiobutton' id='%s' text='%s'>", question_id, cur_record_row$element_text))
        for(opt in 1:length(unique(cur_opt_list))) {
          cur_opt_clean = trimws(cur_opt_list[opt], which = c("both"))
          catn(sprintf("<option text='%s' text='%s'/>", cur_opt_clean, cur_opt_clean))
        }
        catn("</element>")
      }
      
      if(cur_record_row$element_type == "checkbox") {
        cur_opts = cur_record_row$`element_options (separate options with underscore)`
        cur_opt_list = unlist(strsplit(cur_opts, "_"))
        catn(sprintf("<element type='checkbox' id='%s' text='%s' subText='%s'>", question_id, cur_record_row$element_text, "Select all that apply."))
        for(opt in 1:length(unique(cur_opt_list))) {
          cur_opt_clean = trimws(cur_opt_list[opt], which = c("both"))
          catn(sprintf("<option text='%s' text='%s'/>", cur_opt_clean, cur_opt_clean))
        }
        catn("</element>")
      }
      
      if(cur_record_row$element_type == "multitextedit") {
        catn(sprintf("<element type='multitextedit' id='%s' text='%s' subtext='%s' entrycount='%s' allowAddEntry='%s' allowRemoveEntry='%s' minEntry='%s' maxEntry='%s'>", 
                     question_id, cur_record_row$element_text, "Hit the + to add more items.", "1", "true", "true", "1", "6"))
        catn("</element>")
      }
      
      # for debugging
      if(debug) {
        catn(paste0(cur_question_id, ": ", nrow(cur_pack)))
        catn(cur_record_row)
      }
      
      # CHECK IF BRANCHING IS NEEDED
      if(!is.na(cur_record_row$branch_conds) & !is.na(cur_record_row$branch_targetid)) {
        cur_branch_conds = cur_record_row$branch_conds
        cur_branch_ids = cur_record_row$branch_targetid
        cur_conds_list = unlist(strsplit(cur_branch_conds, ";"))
        cur_ids_list = unlist(strsplit(cur_branch_ids, ";"))
        
        # create branch tag
        catn(sprintf("<nextScreen>"))
        
        for(branch in 1:length(cur_conds_list)) {
          # <branch cond="debug_cogselect=dotmemory" target_id="debug_dotmemory" />
          cur_cond = cur_conds_list[branch]
          cur_id = cur_ids_list[branch]
          cur_branch_comparison = ifelse(cur_cond == "[ELSE]", "!=", "==")
          catn(sprintf('<branch cond="%s%s%s" target_id="%s"/>', 
                       question_id, cur_branch_comparison, 
                       cur_cond,
                       paste0(cur_record_row$survey_type,"_q","[#]", "_", cur_id)))
        }
        
        # close nextscreen tag
        catn(sprintf("</nextScreen>"))
      } else {
        # set nextscreen if not a branching tag
        if(cur_record_row$nextscreen_id == "exit") {
          catn(sprintf("<nextScreen action='%s'/>", cur_record_row$nextscreen_id))
        } else {
          catn(sprintf("<nextScreen id='%s'/>", cur_record_row$nextscreen_id))
        }
      }
  
      # close screen -----
      catn("</Screen>")
      
    }
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # close the pack
    catn("</Pack>")
  }
  
  # close the xml to root
  catn("</Dolphin>")
  
  # close file
  sink()
  
  sink("readme_before_deploying_xml.txt")
  # show file if matches expected row count
  if(row_count == nrow(pack_protocol)) {
    #file.show(outfilename)
    catn("XML VALIDATION: SUCCESS")
    catn("-----------------------")
    catn("NEXT STEPS:")
    catn("- search xml for [#], and replace with item #")
    catn("- search xml for [ELSE], and replace with branch_cond")
  } else {
    catn("XML VALIDATION: FAILED")
    catn(row_count)
  }
  sink()

}