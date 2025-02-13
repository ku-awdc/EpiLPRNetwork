#include <Rcpp.h>

#include "patient_risk.hpp"
#include "antal_indlagt.hpp"

RCPP_MODULE(ELPRN_helpers_module){

	using namespace Rcpp;
	function("Rcpp_patient_risk", &patient_risk);
	function("Rcpp_antal_indlagt", &antal_indlagt);

}
