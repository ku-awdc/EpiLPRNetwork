#include <Rcpp.h>

#include "antal_indlagt.hpp"

void antal_indlagt(const Rcpp::DataFrame& input, const Rcpp::DataFrame& time_units, Rcpp::DataFrame& retval, const bool debug)
{
  if(debug)
  {
    antal_indlagt_template<true>(input, time_units, retval);
  }
  else
  {
    antal_indlagt_template<false>(input, time_units, retval);
  }
}
