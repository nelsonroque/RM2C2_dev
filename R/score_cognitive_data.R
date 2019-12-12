#' RM2C2: Scoring, Summarizing

#' @name score_cognitive_data
#' @param data class: data.frame
#' @export
score_cognitive_data <- function(data) {
  package_version <- packageVersion("RM2C2_dev")
  
  if(data$game_name == "Symbol Search") {
    scored <- score_symbol_search(data)
  } else {
    stop("ERROR: game_name value not valid.")
  }
  
  return(scored)
}