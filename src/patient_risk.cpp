#include <Rcpp.h>

#include "patient_risk.hpp"

Rcpp::DataFrame patient_risk(Rcpp::DataFrame input, const bool within_hosp)
{
  Rcpp::DataFrame rv;

  if(within_hosp)
  {
    rv = patient_risk_template<true>(input);
  }
  else
  {
    rv = patient_risk_template<false>(input);
  }

  return rv;
}
