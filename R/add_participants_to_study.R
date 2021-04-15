#' RM2C2dev
#' @name add_participants_to_study
#' @export
#' @import tidyverse
add_participants_to_study <- function(server_url=NA, user_id=NA,  password=NA, study_id=NA, security_code=NA, part_range=NULL) {

  if(!is.na(server_url)) {
    # login to the server (once per run) -----
    get_auth_token <- httr::POST(paste0(server_url,"/gui/action/user_login.php"), 
                                 body = list(user_id = user_id, 
                                             password = password, 
                                             study_id = study_id), 
                                 encode = "form")
    
    # get auth token ----
    auth_results <- jsonlite::parse_json(httr::content(get_auth_token, "text"))
    auth_token <- auth_results$auth_token
    
    if(!is.null(part_range)) {
      for(part in part_range) {
        print(paste0("Attempting to add participant: ", part))
        
        if(is_character(part)) {
          part = part
        } else {
          part = as.character(part)
          print("Type converted to character")
        }
        
        #' make POST request to add participant -----
        add_participant <- try(httr::POST(paste0(server_url,"/gui/action/add_participant.php"),
                                          body = list(
                                            participant_id = part,
                                            groups = "",
                                            study_id = study_id,
                                            security_code = security_code),
                                          add_headers(auth_token = auth_token),
                                          encode = "form"))
        
        print("------------------------------")
      }
    } else {
      stop("Invalid `part_range` provided.")
    }
  } else {
    stop("Valid `server_url` required.")
  }
}
