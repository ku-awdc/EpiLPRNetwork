#' Wrapper for C++ function of number admitted per time unit
#'
#'
#' @param trisk
#' @param within_hosp
#'
#' @export
antal_indlagt <- function(admissions, time_units, debug=FALSE){

  stopifnot(c("HospitalID","Type","TimeUnit") %in% names(admissions))
  stopifnot(c("TimeUnit","TimeMin","TimeMax") %in% names(time_units))

  ## Convert HospitalID, Type, and TimeUnit into consecutive integers:
  hospid_levels <- unique(admissions$HospitalID)
  type_levels <- unique(admissions$Type)
  tu_levels <- unique(time_units$TimeUnit)

  admissions <- admissions |>
    mutate(HospitalID = as.integer(factor(HospitalID, levels=hospid_levels)) - 1L) |>
    mutate(Type = as.integer(factor(Type, levels=type_levels)) - 1L) |>
    mutate(TimeUnit = as.integer(factor(TimeUnit, levels=tu_levels)) - 1L)

  time_units <- time_units |>
    mutate(TimeUnit = as.integer(factor(TimeUnit, levels=tu_levels)) - 1L) |>
    mutate(TimeDuration = as.numeric(TimeMax-TimeMin, units="hours"))

  retval <- expand_grid(HospitalID=0L:(length(hospid_levels)-1L),
      Type = 0L:(length(type_levels)-1L), TimeUnit = 0L:(length(tu_levels)-1L)) |>
      mutate(PatientHours = 0.0, NumStays = 0L, NumDischarges = 0L) |>
      as.data.frame()

  Rcpp_antal_indlagt(admissions, time_units, retval, debug)

  ## Convert HospitalID, Type, and TimeUnit back to character:
  for(cc in c("HospitalID","Type","TimeUnit")){
    retval[[cc]] <- factor(retval[[cc]])
    levels(retval[[cc]]) <- list(HospitalID=hospid_levels, Type=type_levels, TimeUnit=tu_levels)[[cc]]
    retval[[cc]] <- as.character(retval[[cc]])
  }

  return(retval |> as_tibble())
}

