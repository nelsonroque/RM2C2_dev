#' RM2C2dev
#' @name get_m2c2_auth_token
#' @export
#' @param server_url class: string
#' @param user_id class: string
#' @param password class: string
#' @param study_id class: string
#' @import httr
#' @import jsonlite
#' @examples
#' get_m2c2_auth_token(server_url = 'https://m2c2.survey.psu.edu/test/', user_id = 'r_user', password = 'test', study_id = 'test')
get_m2c2_auth_token <- function(server_url = '', user_id = '', password = '', study_id = '') {
  params <- list(user_id = user_id, password = password, study_id = study_id)
  
  res <- httr::POST(paste0(server_url,"gui/action/user_login.php"), 
                    body = params, encode = "form")
  
  server_login_resp <- jsonlite::parse_json(httr::content(res, "text"))
  return(server_login_resp$auth_token)
}