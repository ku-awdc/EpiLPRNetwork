#include <Rcpp.h>

#include "patient_risk.hpp"

RCPP_MODULE(ELPRN_helpers_module){

	using namespace Rcpp;
	function("Rcpp_patient_risk", &patient_risk);

}
