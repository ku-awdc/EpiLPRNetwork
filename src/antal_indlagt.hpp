#include <Rcpp.h>

template <bool t_debug>
double to_hours(const double secs){
  const double rv = secs/(60.0*60.0);
  return rv;
}

template <bool t_debug>
Rcpp::DataFrame antal_indlagt_template(Rcpp::DataFrame input, Rcpp::DataFrame time_units, Rcpp::DataFrame& retval)
{
  using namespace Rcpp;

  const IntegerVector& in_time_unit = input["TimeUnit"];
  const IntegerVector& in_hospital_id = input["HospitalID"];
  const IntegerVector& in_type = input["Type"];
  const DatetimeVector& in_admission = input["Admission"];
  const DatetimeVector& in_discharge = input["Discharge"];

  const IntegerVector& tu_time_unit = time_units["TimeUnit"];
  const DatetimeVector& tu_time_min = time_units["TimeMin"];
  const DatetimeVector& tu_time_max = time_units["TimeMax"];
  const NumericVector& tu_time_dur = time_units["TimeDuration"];

  const IntegerVector& rv_hospital_id = retval["HospitalID"];
  const IntegerVector& rv_type = retval["Type"];
  const IntegerVector& rv_time_unit = retval["TimeUnit"];
  NumericVector rv_patient_hours = retval["PatientHours"];
  IntegerVector rv_num_stays = retval["NumStays"];
  NumericVector rv_num_discharges = retval["NumDischarges"];

  // Some checks:
  if constexpr (t_debug)
  {
    if(input.nrow() == 0) stop("Passed in input data frame of length 0!");
    if(time_units.nrow() == 0) stop("Passed in time_units data frame of length 0!");
  }
  // TODO: check time_units and retval are sorted (input need not be)

  const int ntu = max(rv_time_unit)+1;
  const int mhosp = (max(rv_type)+1) * ntu;
  const int mtype = ntu;

  auto get_index = [=](const int hosp, const int type, const int tu) -> int
  {
    const int rv = hosp*mhosp + type*mtype + tu;
    if constexpr (t_debug)
    {
      if(rv < 0) Rcpp::stop("Index < 0");
      if(rv >= retval.nrow()) Rcpp::stop("Index > retval.nrow()");
    }
    return rv;
  };

  // Note: input$TimeUnit is the aggregated time unit for admission, so we can just start there for each row in input
  for (int row=0; row<input.nrow(); ++row)
  {
    const int start_tu = in_time_unit[row];
    const int hosp_id = in_hospital_id[row];
    const int type = in_type[row];
    const Datetime adm = in_admission[row];
    const Datetime dis = in_discharge[row];
    
    for (int tu=start_tu; tu<ntu; ++tu)
    {
      const Datetime time_min = tu_time_min[tu];
      const Datetime time_max = tu_time_max[tu];
      
      if constexpr (t_debug)
      {
        if(tu==start_tu && time_max < adm) Rcpp::stop("Logic error in time unit end");
        if(tu==start_tu && time_min > dis) Rcpp::stop("Logic error in time unit start");
      }
      
      // If we have finished with the row move on:
      if (time_min > dis) break;
      
      // Otherwise note times etc:
      const int ind = get_index(hosp_id, type, tu);
      rv_num_stays[ind]++;
      
      if (time_max < dis) {
        if (time_min > adm) {
          // Admission covers the complete time interval:
          rv_patient_hours[ind] += tu_time_dur[tu]; //to_hours<t_debug>(time_max - time_min);          
        } else {
          // Admission during the interval:
          rv_patient_hours[ind] += to_hours<t_debug>(time_max - adm);
        }
      } else {
        // Discharge within the interval:
        rv_num_discharges[ind]++;
        
        if (time_min > adm) {
          // Only discharge within the interval:
          rv_patient_hours[ind] += to_hours<t_debug>(dis - time_min);
        } else {
          // Admission and discharge within the time interval:
          rv_patient_hours[ind] += to_hours<t_debug>(dis - adm);
        }
      }
    }
  }

  // Note: I don't think this (shallow) copy is actually necessary...
  retval["PatientHours"] = rv_patient_hours;
  retval["NumStays"] = rv_num_stays;
  retval["NumDischarges"] = rv_num_discharges;
  
  return retval;
}

void antal_indlagt(const Rcpp::DataFrame& input, const Rcpp::DataFrame& time_units, Rcpp::DataFrame& retval, const bool debug);
