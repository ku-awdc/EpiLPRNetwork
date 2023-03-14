#' Wrapper for C++ function of patient transfer risk
#'
#'
#' @param trisk
#' @param within_hosp
#'
#' @export
patient_risk <- function(trisk, within_hosp = FALSE){
  ## Convert cprnr into consecutive integers:
  cprnr_levels <- unique(trisk$cprnr)
  trisk <- trisk |>
    mutate(cprnr = as.integer(factor(cprnr, levels=cprnr_levels)) - 1L)

  retval <- Rcpp_patient_risk(trisk, within_hosp) |> as_tibble()

  ## Convert cprnr back to character:
  retval$cprnr <- factor(retval$cprnr)
  levels(retval$cprnr) <- cprnr_levels
  retval$cprnr <- as.character(retval$cprnr)

  return(retval)
}


# Not exported - just used for checking:
patient_risk_R <- function(trisk){
  trisk |>
    mutate(cprnr = as.character(cprnr), HospitalID = as.character(HospitalID), Type = as.character(Type)) |>
    mutate(Row = 1:n()) |>
    group_by(cprnr) |>
    group_split() |>
    lapply(function(x){
      x |>
        select(OldRow = Row, OldHospital=HospitalID, OldAdmission=Admission, OldDischarge=Discharge, OldType=Type) |>
        full_join(x, by=character(0)) |>
        filter(Admission >= OldAdmission, Row!=OldRow) |>
        select(-OldRow, -Row)
    }) |>
    bind_rows() |>
    select(OldHospital, OldAdmission, OldDischarge, OldType, cprnr, HospitalID, Type, Admission, Discharge) ->
  rv

  return(rv)
}
