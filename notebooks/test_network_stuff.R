## Testing of function for networks:

library("EpiLPRNetwork")

calculate_risk <- function(new_admission, new_discharge, new_type, old_admission, old_discharge, old_type){
  case_when(
    as.numeric(new_admission-old_discharge, units="days") > 30 ~ 0.0,
    TRUE ~ 1 / (1+as.numeric(new_admission-old_discharge, units="days"))
  )
}

results <- get_edges(all_by = c("00","01","03"), risk_function = calculate_risk, testing=FALSE)
