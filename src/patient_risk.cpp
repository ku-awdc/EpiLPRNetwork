#include <Rcpp.h>

#include "patient_risk.hpp"

Rcpp::DataFrame patient_risk(const Rcpp::DataFrame& input)
{
  using namespace Rcpp;

  IntegerVector in_cprnr = input["cprnr"];
  StringVector in_hospital_id = input["HospitalID"];
  StringVector in_type = input["Type"];
  DatetimeVector in_admission = input["Admission"];
  DatetimeVector in_discharge = input["Discharge"];

  // Assumption: we are ordered by CPR and then admission, and then discharge
  // TODO: checks for ordering

  if(input.nrow() == 0L) stop("Passed in data frame of length 0!");
  if(in_cprnr[0L] != 0L) stop("Indexing of cprnr should start at 0!");
  // TODOÆ return data frame of length 0

  // Run through once to get vector lengths:
  IntegerVector cprs = unique(in_cprnr);
  IntegerVector cpr_counts(cprs.length(), 0L);
  int lastcpr = 0L;
  int totalrows = 0L;
  for(int row=0L; row<input.nrow(); ++row)
  {
    if(in_cprnr[row] != lastcpr)
    {
      totalrows += (pow(cpr_counts[lastcpr], 2L) - cpr_counts[lastcpr]) / 2L;
      lastcpr++;
    }
    if(in_cprnr[row] != lastcpr)
    {
      stop("CPR numbers are not sorted consecutively");
    }
    cpr_counts[lastcpr]++;
  }
  totalrows += (pow(cpr_counts[lastcpr], 2L) - cpr_counts[lastcpr]) / 2L;

  StringVector oldhosp(totalrows);
  DatetimeVector oldadmission(totalrows);
  DatetimeVector olddischarge(totalrows);
  StringVector oldtype(totalrows);
  IntegerVector cprnr(totalrows);
  StringVector hosp(totalrows);
  StringVector type(totalrows);
  DatetimeVector admission(totalrows);
  DatetimeVector discharge(totalrows);
  IntegerVector event(totalrows);

  int outrow = 0L;
  int rowstart = 0L;
  int eve = 0L;

  for(int cpr = 0L; cpr < cpr_counts.length(); ++cpr)
  {
    // Rcout << cpr<< ", ";
    // If the CPR appears only once then ignore it:
    if(cpr_counts[cpr]>0L)
    {
      // TODO: comment out checks
      if(rowstart > in_cprnr.length()) stop("rowstart > in_cprnr.length()");

      for(int first = 0L; first < cpr_counts[cpr]; ++first)
      {
        for(int sec = first+1L; sec < cpr_counts[cpr]; ++sec)
        {
          // TODO: comment out checks
          if(outrow >= totalrows) stop("outrow >= totalrows");

          oldhosp[outrow] = in_hospital_id[rowstart+first];
          oldadmission[outrow] = in_admission[rowstart+first];
          olddischarge[outrow] = in_discharge[rowstart+first];
          oldtype[outrow] = in_type[rowstart+first];
          cprnr[outrow] = cpr;
          hosp[outrow] = in_hospital_id[rowstart+sec];
          type[outrow] = in_type[rowstart+sec];
          admission[outrow] = in_admission[rowstart+sec];
          discharge[outrow] = in_discharge[rowstart+sec];
          event[outrow] = eve;

          outrow++;
        }
        eve++;
      }
      rowstart += cpr_counts[cpr];
    }
  }

  DataFrame temp = DataFrame::create(Named("OldHospital") = oldhosp,
                                     Named("OldAdmission") = oldadmission,
                                     Named("OldDischarge") = olddischarge,
                                     Named("OldType") = oldtype,
                                     Named("cprnr") = cprnr,
                                     Named("HospitalID") = hosp,
                                     Named("Type") = type,
                                     Named("Admission") = admission,
                                     Named("Discharge") = discharge,
                                     Named("Event") = event
                                     );

	return temp;
}
