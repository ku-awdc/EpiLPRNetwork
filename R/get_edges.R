#' Title
#'
#' @export
get_edges <- function(all_by = str_replace(format(0:99), " ", "0"), risk_function = NULL, testing=FALSE){

  data(sor_hospitals)

  ## Check there aren't new SOR codes:
  cat("Checking SOR codes in epiLPR...\n")
  contacts <- get_sql_table("contacts")
  contacts %>%
    filter(contact_in<"2022-01-02 00:00") %>%
    count(unit_SOR) %>%
    collect() %>%
    identity() %>%
    arrange(desc(n)) ->
    sor_frq
  stopifnot(all(sor_frq$unit_SOR %in% sor_hospitals$unit_SOR))


  if(is.null(risk_function)){
    # Function to relate hospitalisation risk based on delta time:
    # Note: should be vectorised!!!
    calculate_risk <- function(new_admission, new_discharge, new_type, old_admission, old_discharge, old_type){
      case_when(
        as.numeric(new_admission-old_discharge, units="days") > 30 ~ 0.0,
        TRUE ~ 1
      )
    }

  }else{
    calculate_risk <- risk_function
  }

#  contact_head <- contacts %>% head() %>% collect()
#  attr(contact_head$contact_in, "tz")
#  attr(contact_head$contact_out, "tz")


  if(FALSE){
  # TODO: check CPR numbers
  all_cpr <- contacts %>%
    distinct(cprnr) %>%
    collect() %>%
    pull(cprnr)
  Encoding(all_cpr) <- "UTF-8"
  system.time(str_length(stringi::stri_escape_unicode(all_cpr[1:1e5])))
  #unique(str_length(all_cpr$cprnr))

  ## Find CPR that are not just numbers:
  ntcpr <- str_subset(all_cpr, ".*[^[:digit:]].*")
  ntcpr <- str_subset(all_cpr, "[[:digit:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]].*", negate=TRUE)
  ntcpr <- str_subset(all_cpr, "[[:digit:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]].*", negate=FALSE)
  str_subset(c(1111,"5657","1dv","dv2"), ".*[^[:digit:]].*")
  }


  cl <- parallel::makeCluster(getOption("cl.cores", 10))
  parallel::clusterEvalQ(cl, {library(tidyverse)})

  # Note: output of this is cumulative!
  cat("Looping over", length(all_by), "birth years..\n")
  for(by in all_by){

    cat("Processing year: ", by, "\n", sep="")

    ## Extract contacts for particular birth year:
    contacts_using <- contacts %>%
      filter(cprnr %LIKE% str_c("____", by, "%")) %>%
      select(cprnr, unit_SOR, contact_in, contact_out) %>%
      collect()

    ## Merge with hospital and extract admission and dicharge from entire course:
    admissions_using <- contacts_using %>%
      left_join(sor_hospitals, by="unit_SOR") %>%
      group_by(cprnr, HospitalID, Type) %>%
      arrange(cprnr, HospitalID, Type, contact_in, contact_out) %>%
      mutate(EventNumber = c(0, cumsum(as.numeric(lead(contact_in)) > (4*60*60 + cummax(as.numeric(contact_out))))[-n()])) %>%
      group_by(cprnr, HospitalID, Type, EventNumber) %>%
      summarise(Admission = min(contact_in), Discharge = max(contact_out), .groups='drop') %>%
      select(-EventNumber)

    ## Exclude anybody that has only been to a single hospital:
    admissions_using <- admissions_using %>%
      distinct(cprnr, HospitalID, Type) %>%
      group_by(cprnr) %>%
      filter(n() > 1) %>%
      ungroup() %>%
      inner_join(admissions_using, by=c("cprnr","HospitalID", "Type"))

    ## Calculate connection strength between different hospitals for the same patient:
    trisk <- admissions_using %>%
      arrange(cprnr, Admission) %>%
      split(.$cprnr)
    if(testing){
      trisk <- trisk[1:100]
    }
    trisk <- trisk %>%
      ## TODO: replace with parallel::mclapply on a non-laptop
      ## TODO: replace with a C++ function
      # pbapply::pblapply(function(x){
      parallel::parLapply(cl, X=., fun=function(x){
          x %>%
          select(OldHospital=HospitalID, OldAdmission=Admission, OldDischarge=Discharge, OldType=Type) %>%
          full_join(x %>% mutate(Event=1:n()), by=character(0)) %>%
          filter(Admission >= OldDischarge, HospitalID!=OldHospital) %>%
          # filter(Admission > OldAdmission, HospitalID!=OldHospital) %>%
          mutate(Risk = calculate_risk(Admission, Discharge, Type, OldAdmission, OldDischarge, OldType)) %>%
          mutate(Admission = as.Date(Admission, tz="Europe/Copenhagen")) %>%
          group_by(Admission, Event, HospitalID, OldHospital) %>%
          summarise(TotalRisk = sum(Risk), .groups='drop') %>%
          filter(TotalRisk > 0.0) %>%
          select(Admission, HospitalTo=HospitalID, HospitalFrom=OldHospital, TotalRisk)
      })

    ## Calculate patient days in hospital:
    thosp <- admissions_using %>%
      mutate(AdmissionHours = as.numeric(Discharge - Admission, units="hours")) %>%
      mutate(AdmissionNights = as.numeric(as.Date(Discharge)-as.Date(Admission), units="days")) %>%
      mutate(OvernightAdmissions = as.numeric(as.Date(Discharge)-as.Date(Admission), units="days") > 0) %>%
      mutate(MonthYear = strftime(Admission, "%Y-%m")) %>%
      group_by(HospitalID, MonthYear) %>%
      summarise(TotalAdmissions = n(), AdmissionHours = sum(AdmissionHours),
                AdmissionNights = sum(AdmissionNights), OvernightAdmissions = sum(OvernightAdmissions),
                TotalPatients = length(unique(cprnr)),  .groups="drop")


    if(by==all_by[1]){
      allrisk <- trisk %>% bind_rows()
      allhosp <- thosp
    }else{
      # allrisk <- bind_rows(allrisk, trisk)
      allrisk <- c(list(allrisk), trisk) %>% bind_rows()
      allhosp <- bind_rows(allhosp, thosp)
    }

    allrisk <- allrisk %>%
      group_by(Admission, HospitalTo, HospitalFrom) %>%
      summarise(TotalRisk = sum(TotalRisk), .groups='drop')

    allhosp <- allhosp %>%
      group_by(HospitalID, MonthYear) %>%
      # summarise(across(where(is.numeric), sum), .groups="drop")
      summarise_if(is.numeric, sum)

  }

  close(cl)
  cat("Done\n")

  return(list(risk=allrisk, hospital=allhosp))

}
