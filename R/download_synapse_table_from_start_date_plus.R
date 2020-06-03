#' RM2C2dev
#' @name download_synapse_table_from_start_date_plus
#' @export
#' @param synapse_email class: string
#' @param synapse_pw class: string
#' @param synapse_id class: string
#' @param date_col class: string
#' @param start_date class: string
#' @param search_op class: string
#' @import tidyverse
#' @examples
#' download_synapse_table_from_start_date_plus(synapse_email = synapse_email, synapse_pw =  synapse_pw, synapse_id = synapse_survey_data_table_id, start_date = "'2020-05-01'", search_op = ">=")
download_synapse_table_from_start_date_plus <- function(synapse_email = NA, synapse_pw = NA, synapse_id = NA, date_col = "uploadDate", start_date = NA, search_op = ">=") {
  
  print("For the latest version of `synapser`, run: install.packages('synapser', repos=c('https://sage-bionetworks.github.io/ran', 'http://cran.fhcrc.org'))")
  
  if(date_col %in% c("uploadDate", "createdOn")) {
    #' login to synapse
    synapser::synLogin(email = synapse_email, password = synapse_pw, rememberMe = T)
    
    #' build the query
    query <- sprintf("select * from %s WHERE %s %s %s", synapse_id, date_col, search_op, start_date)
    results <- synapser::synTableQuery(query)
    
    #' get raw, unmanipulated dataframe matching query results
    table <- results$asDataFrame()
  } else {
    table <- data.frame(error = "date_col must be equal to one of: uploadDate, createdOn")
  }
  
  return(table)
}

