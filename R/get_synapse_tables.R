#' RM2C2dev
#' @name get_synapse_tables
#' @export
#' @param synapse_email class: string
#' @param synapse_pw class: string
#' @param synapse_project_id class: string
#' @import tidyverse
#' @examples
#' get_synapse_tables(synapse_email = NA, synapse_pw = NA, synapse_project_id = NA)
get_synapse_tables <- function(synapse_email = NA, synapse_pw = NA, synapse_project_id = NA) {
  if(!require(synapser)) {
    print("ERROR: Missing `synapser` package. Please run: install.packages('synapser', repos=c('https://sage-bionetworks.github.io/ran', 'http://cran.fhcrc.org'))")
  } else {
    print("For the latest version of `synapser`, run: install.packages('synapser', repos=c('https://sage-bionetworks.github.io/ran', 'http://cran.fhcrc.org'))")
    
    #' login to synapse
    synapser::synLogin(email = synapse_email, password = synapse_pw, rememberMe = T)
    
    #' load all objects
    iterator <- synapser::synGetChildren(synapse_project_id, 
                               includeTypes=list("folder", "file", "table", "link", "entityview", "dockerrepo"), 
                               sortBy="CREATED_ON")
    
    #' type convert to list
    obj_list <- as.list(iterator)
    
    #' create blank data.frame for merging
    data_tabs <- data.frame()
    for(i in 1:length(obj_list)) {
      cur_obj <- as.data.frame(obj_list[[i]])
      data_tabs <- rbind(data_tabs, cur_obj)
    }
  }
  return(list(tables=data_tabs, synapse_objects = obj_list))
}