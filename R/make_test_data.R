#' RM2C2dev
#' @name make_test_data
#' @export
make_test_data <- function(game_name, n=10, sessions=10) {
  
  # check if `game_name` provided is for:
  
  # `Symbol Search` task
  if(game_name %in% c("symbol_search", "Symbol_Search", "Symbol-Search")){
    test_data <- expand.grid(participant_id = seq(1,n,1), session_id = seq(1,sessions,1)) %>%
      as.data.frame(.) %>%
      mutate(user_response = sample(x=c(0,1), size=nrow(.), replace=T),
             correct_response = sample(x=c(0,1), size=nrow(.), replace=T),
             trial_type = sample(x=c("LURE", "NORMAL"), size=nrow(.), replace=T)) %>%
      mutate(response_time = rnorm(n=nrow(.), 500, 200)) %>%
      mutate(game_name = game_name)
  } else {
    stop(paste0("Test data for ", game_name, "is not available yet. Please contact nur375@psu.edu to express interest."))
  }
  return(test_data)
}