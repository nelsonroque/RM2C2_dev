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

# GENERATE TEST DATA =====

symbol_search_test_data <- expand.grid(participant_id = seq(100,150,1), session_id = seq(1,84,1)) %>%
  as.data.frame(.) %>%
  mutate(user_response = sample(x=c(0,1), size=nrow(.), replace=T),
         correct_response = sample(x=c(0,1), size=nrow(.), replace=T)) %>%
  mutate(response_time = rnorm(n=nrow(.), 500, 200)) %>%
  mutate(game_name = "symbol_search")

#ss_scored <- RM2C2dev::score_symbol_search(cc)
ss_summary <- RM2C2dev::summary_symbol_search(symbol_search_test_data, group_var=c("participant_id", "session_id"))