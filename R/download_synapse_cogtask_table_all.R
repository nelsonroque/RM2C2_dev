#' RM2C2dev
#' @name download_synapse_cogtask_table_all
#' @export
#' @param synapse_email class: string
#' @param synapse_pw class: string
#' @param synapse_id class: string
#' @param uuids class: vector
#' @import tidyverse
#' @examples
#' download_synapse_cogtask_table_all(synapse_email = NA, synapse_pw = NA, synapse_id = NA, record_uuids = NA)
download_synapse_cogtask_table_all <- function(synapse_email = NA, synapse_pw = NA, synapse_id = NA, uuids=NA) {
  if(!require(synapser)) {
    print("ERROR: Missing `synapser` package. Please run: install.packages('synapser', repos=c('https://sage-bionetworks.github.io/ran', 'http://cran.fhcrc.org'))")
  } else {
    print("For the latest version of `synapser`, run: install.packages('synapser', repos=c('https://sage-bionetworks.github.io/ran', 'http://cran.fhcrc.org'))")
    
    #' login to synapse
    synLogin(email = synapse_email, password = synapse_pw, rememberMe = T)
    
    #' build the query
    base_query <- "select * from %s WHERE cogtask_run_uuid IN (%s)"
    clean_uuids <- uuids[!is.na(uuids)]
    print(clean_uuids)
    uuid_query <- paste0(sQuote(clean_uuids, "single"), collapse=",")
    query <- sprintf(base_query, synapse_id, uuid_query)
    print(query)
    results <- synTableQuery(query)
    
    #' get raw, unmanipulated dataframe matching query results
    table <- results$asDataFrame()
  }
  return(table)
}