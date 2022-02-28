#' Wrapper for C++ function of patient transfer risk
#'
#'
#' @export
patient_risk <- function(){
  retval <- Rcpp_patient_risk()
  return(retval)
}
