#include <Rcpp.h>

template <bool T_within_hosp>
Rcpp::DataFrame patient_risk_template(Rcpp::DataFrame input)
{
  using namespace Rcpp;

  IntegerVector in_cprnr = input["cprnr"];
  StringVector in_hospital_id = input["HospitalID"];
  StringVector in_type = input["Type"];
  DatetimeVector in_admission = input["Admission"];
  DatetimeVector in_discharge = input["Discharge"];

  // Some checks:
  {
    if(input.nrow() == 0L) stop("Passed in data frame of length 0!");
    const size_t mincpr = min(in_cprnr);
    if(mincpr!=0L) stop("Indexing of cprnr should start at 0!");
    if(in_cprnr[0L] != 0L) stop("cprnr should be sorted starting at 0!");
  }

  // Run through once to get vector lengths and check ordering:
  const size_t ncprs = max(in_cprnr)+1L;
  IntegerVector cpr_counts(ncprs, 0L);
  int lastcpr = 0L;
  int totalrows = 0L;
  time_t last_admit = 0.0L;
  time_t last_discharge = in_admission[0L]+1.0;
  int corrdis = 0L;

  for(int row=0L; row<input.nrow(); ++row)
  {
    if(in_cprnr[row] != lastcpr)
    {
      if constexpr(T_within_hosp) totalrows += (pow(cpr_counts[lastcpr], 2L) - cpr_counts[lastcpr]) / 2L;
      last_admit = 0.0L;
      last_discharge = in_admission[row]+1.0;
      lastcpr++;
    }
    if(in_cprnr[row] != lastcpr)
    {
      stop("CPR numbers are not sorted consecutively");
    }
    if(in_admission[row] > in_discharge[row])
    {
      stop("Admission date/time detected after corresponding discharge time");
    }
    if(in_admission[row] < last_admit)
    {
      stop("Admission date/times are not sorted within cprnr");
    }
    last_admit = in_admission[row];
    if(last_admit > last_discharge)
    {
      corrdis++;
      if(row==0L) stop("Logic error");
      in_discharge[row-1L] = last_admit;
    }
    last_discharge = in_discharge[row];

    cpr_counts[lastcpr]++;
  }
  if constexpr(T_within_hosp) totalrows += (pow(cpr_counts[lastcpr], 2L) - cpr_counts[lastcpr]) / 2L;

  if(corrdis > 0L)
  {
    warning("One or more patient readmitted before being discharged:  discharge date(s) corrected to next admission date");
  }

  if constexpr(!T_within_hosp)
  {
    int rowstart = 0L;
    totalrows = 0L;
    for(size_t cpr = 0L; cpr < ncprs; ++cpr)
    {
      for(int first = 0L; first < cpr_counts[cpr]; ++first)
      {
        for(int sec = first+1L; sec < cpr_counts[cpr]; ++sec)
        {
          if(in_hospital_id[rowstart+first] != in_hospital_id[rowstart+sec]) totalrows++;
        }
      }
      rowstart += cpr_counts[cpr];
    }
  }

  StringVector oldhosp(totalrows);
  DatetimeVector oldadmission(totalrows);
  DatetimeVector olddischarge(totalrows);
  StringVector oldtype(totalrows);
  IntegerVector cprnr(totalrows);
  StringVector hosp(totalrows);
  StringVector type(totalrows);
  DatetimeVector admission(totalrows);
  DatetimeVector discharge(totalrows);

  int outrow = 0L;
  int rowstart = 0L;

  for(int cpr = 0L; cpr < cpr_counts.length(); ++cpr)
  {
    // Rcout << "cprnr " << cpr << " starts at row " << rowstart << "\n";
    if(cpr_counts[cpr]>0L)
    {
      // TODO: comment out checks
      if(rowstart > in_cprnr.length()) stop("rowstart > in_cprnr.length()");

      for(int first = 0L; first < cpr_counts[cpr]; ++first)
      {
        for(int sec = first+1L; sec < cpr_counts[cpr]; ++sec)
        {
          if constexpr(!T_within_hosp)
          {
            if(in_hospital_id[rowstart+first] == in_hospital_id[rowstart+sec]) continue;
          }

          // TODO: comment out checks
          if(outrow >= totalrows){
            Rcout << "Note:  outrow >= totalrows (" << totalrows << ")\n";
            stop("outrow >= totalrows");
          }

          oldhosp[outrow] = in_hospital_id[rowstart+first];
          oldadmission[outrow] = in_admission[rowstart+first];
          olddischarge[outrow] = in_discharge[rowstart+first];
          oldtype[outrow] = in_type[rowstart+first];
          cprnr[outrow] = cpr;
          hosp[outrow] = in_hospital_id[rowstart+sec];
          type[outrow] = in_type[rowstart+sec];
          admission[outrow] = in_admission[rowstart+sec];
          discharge[outrow] = in_discharge[rowstart+sec];

          outrow++;
        }
      }
      rowstart += cpr_counts[cpr];
    }
  }
  if(outrow != totalrows) Rcout << "Note:  Final outrow " << outrow << " != totalrows " << totalrows << "\n";

  DataFrame temp = DataFrame::create(Named("OldHospital") = oldhosp,
                                     Named("OldAdmission") = oldadmission,
                                     Named("OldDischarge") = olddischarge,
                                     Named("OldType") = oldtype,
                                     Named("cprnr") = cprnr,
                                     Named("HospitalID") = hosp,
                                     Named("Type") = type,
                                     Named("Admission") = admission,
                                     Named("Discharge") = discharge
  );

  return temp;
}

Rcpp::DataFrame patient_risk(Rcpp::DataFrame input, const bool within_hosp = false);
