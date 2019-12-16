# install library from Github
devtools::install_github("nelsonroque/RM2C2_dev")

# load library
library(RM2C2dev)

# TESTING PARAMETERS =====

start_date = "2019-01-01 00:00:00"
end_date = "2019-01-08 00:00:00"
synapse_email = "nur375@psu.edu"
synapse_pw = "W1$3S0uL"
synapse_m2c2_project_id = "syn16804001"
synapse_survey_data_table_id = "syn18634455"
synapse_cogtask_data_table_id = "syn17083086"

# FEATURES =====

# adding date features
a <- data.frame(dates=seq(as.POSIXct(start_date), as.POSIXct(end_date), by=60))
b <- a %>% RM2C2dev::append_metadate_cols(a, time_var='dates')
c <- a %>% RM2C2dev::append_metadate_cols(a, time_var='dates', append_rounded = F)

# add processing uuids
d <- a %>% RM2C2dev::append_process_cols()
e <- b %>% RM2C2dev::append_process_cols()

# DOWNLOADING DATA FROM DREAM / SRC =====

# check zip download
server_url <- "https://m2c2-dev.survey.psu.edu/ctk/dolph_launch/pulldata.php"
server_studyname <- "script_flight3"
server_parsedata <- "true"
server_zip_outname <- "latest_parse.zip"

# create list with parameters expected by data download endpoint
#  download data from SRC servers
login <- list(
  study_id = server_studyname,
  parsed = server_parsedata
)

# download zip file
RM2C2dev::download_server_zip(url = server_url, 
                              params = login, 
                              save_filename = server_zip_outname, 
                              overwrite_zip=T, 
                              unzip=F, 
                              remove_zip=T)

# DOWNLOADING DATA FROM Sage's Synapse Platform =====

# first install synapser package if not present
RM2C2dev::install_synapser()

# get a listing of all M2C2 Synapse Tables available to download
aa <- RM2C2dev::get_synapse_tables(synapse_email=synapse_email, synapse_pw=synapse_pw, synapse_project_id = synapse_m2c2_project_id)
#View(aa$tables)
#aa$synapse_objects

# get raw synapse table of survey data containing task uuids
bb <- RM2C2dev::download_synapse_table_all(synapse_email=synapse_email, synapse_pw=synapse_pw, synapse_id=synapse_survey_data_table_id)

# get unique UUIDs for each cog task
task_uuids <- RM2C2dev::get_unique_values(bb$symbol_cogtask_uuid)

# get raw synapse table of cog task data
cc <- RM2C2dev::download_synapse_cogtask_table_all(synapse_email=synapse_email, synapse_pw=synapse_pw, synapse_id=synapse_cogtask_data_table_id, uuids = task_uuids)

# UTILITIES =====

# check tidy datetime
RM2C2dev::get_tidy_datetime(delim="_")

# data checking
RM2C2dev::is_data_frame_tibble(a)
RM2C2dev::is_data_frame_tibble("a")

# check whether column is present
RM2C2dev::is_col_present(bb, "phoneInfo")

# READ LOCAL FILE =====

# test_file <- RM2C2dev::read_m2c2_local(file.choose())

# WORKE TEST DATA =====

# generate test data
ss_test_data <- RM2C2dev::make_test_data("symbol_search", n=10, sessions=10)

# score and summarise data
ss_scored <- RM2C2dev::score_symbol_search(ss_test_data)
ss_summary <- RM2C2dev::summary_symbol_search(ss_scored, group_var=c("participant_id"))
ss_summary_exp <- RM2C2dev::summary_symbol_search(ss_scored, group_var=c("participant_id"), experimental = T)

# verify attributes added to data.frames after scoring
RM2C2dev::is_data_tag_valid(ss_scored, tag_name = "is_m2c2_scored", tag_value=T)
RM2C2dev::is_data_tag_valid(ss_scored, tag_name = "is_m2c2_summary", tag_value=T)
RM2C2dev::is_data_tag_valid(ss_summary, tag_name = "is_m2c2_summary", tag_value=T)
RM2C2dev::is_data_tag_valid(ss_summary_exp, tag_name = "is_m2c2_summary", tag_value=T)
RM2C2dev::is_data_tag_valid(ss_summary_exp, tag_name = "is_m2c2_experimental_summary", tag_value=T)

# save data as json
RM2C2dev::data_to_json(ss_summary, filename = "C:/Users/nar09/Desktop/test.json")

# post json as R object or as file to API endpoint

# PIPELINE TIME! ====

# data frame that was loaded into Environment
ss_pipeline <- RM2C2dev::task_processing_pipeline(ss_test_data, source = "data.frame", score = T, summary= T, experimental = T, group_var = c("participant_id"))

# directly from synapse
ss_synapse_pipeline <- RM2C2dev::task_processing_pipeline(source = "synapse", 
                                                score = T, 
                                                summary = T, 
                                                experimental = T, 
                                                group_var = c("participant_id"), 
                                                synapse_email = synapse_email, 
                                                synapse_pw = synapse_pw, 
                                                uuid_col_name = "symbol_cogtask_uuid",
                                                synapse_cogtask_id = synapse_cogtask_data_table_id, 
                                                synapse_survey_id = synapse_survey_data_table_id)

# from SRC servers

# VERIFY HASHES =====

# verify that data scored 'manually' step-wise, yields the same data as if scored in the automated pipeline
ss_pipeline$scored$m2c2_processing_hash[1] == ss_scored$m2c2_processing_hash[1]
ss_pipeline$summary$m2c2_processing_hash[1] == ss_summary$m2c2_processing_hash[1]
ss_pipeline$experimental$m2c2_processing_hash[1] == ss_summary_exp$m2c2_processing_hash[1]

