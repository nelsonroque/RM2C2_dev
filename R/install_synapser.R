#' RM2C2dev
#' @name install_synapser
#' @export
#' @examples
#' install_synapser()
install_synapser <- function() {
  if(!require(synapser)){
    print("Installing `synapser` and dependency `PythonEmbedInR`. This may take a while.")
    install.packages("PythonEmbedInR", repos=c("http://cran.fhcrc.org", "http://ran.synapse.org"))
    install.packages("synapser", repos=c("http://ran.synapse.org", "http://cran.fhcrc.org"))
  } else {
    stop("`synapser` already installed.")
  }
}