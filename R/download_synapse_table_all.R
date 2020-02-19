#' RM2C2dev
#' @name download_synapse_table_all
#' @export
#' @param synapse_email class: string
#' @param synapse_pw class: string
#' @param synapse_id class: string
#' @import tidyverse
#' @examples
#' download_synapse_table_all(synapse_email = NA, synapse_pw = NA, synapse_id = NA, record_uuids = NA)
download_synapse_table_all <- function(synapse_email = NA, synapse_pw = NA, synapse_id = NA) {
  if(!require(synapser)) {
    print("ERROR: Missing `synapser` package. Please run: install.packages('synapser', repos=c('https://sage-bionetworks.github.io/ran', 'http://cran.fhcrc.org'))")
  } else {
    print("For the latest version of `synapser`, run: install.packages('synapser', repos=c('https://sage-bionetworks.github.io/ran', 'http://cran.fhcrc.org'))")
    
    #' login to synapse
    synapser::synLogin(email = synapse_email, password = synapse_pw, rememberMe = T)
    
    #' build the query
    query <- sprintf("select * from %s", synapse_id)
    results <- synapser::synTableQuery(query)
    
    #' get raw, unmanipulated dataframe matching query results
    table <- results$asDataFrame()
  }
  return(table)
}