#include <Rcpp.h>

#include "patient_risk.hpp"

DataFrame patient_risk()
{
  
  	NumericVector t1 = {1.0, 2.0};
	DataFrame temp = DataFrame::create(Named("colname") = t1);
	return temp;
}
