#' Title
#'
#' @import stringr
#'
#' @export
get_edges <- function(all_by_mth = NA, year_group=TRUE, risk_function = NULL, output_level = "month",
                      max_date = "2019-01-01 00:00", retry=Inf, timeout=c(1,5,15,60), testing=FALSE){

  # all_by_mth = NA; risk_function = NULL; output_level = "month"; max_date = "2022-01-01 00:00"; year_group=TRUE

  data(sor_hospitals)

  stopifnot(output_level %in% c("month","day"))

  if(identical(all_by_mth, NA)){
    if(year_group){
      # Note:^dates are not ISO compliant!
      all_by_mth <- str_replace(format(00:99), " ", "0")
      stopifnot(all(str_length(all_by_mth)==2L))
    }else{
      # Note:^dates are not ISO compliant!
      all_by_mth <- apply(expand_grid(str_replace(format(00:99), " ", "0"), str_replace(format(01:12), " ", "0"))[,2:1],1,paste,collapse="")
      stopifnot(all(str_length(all_by_mth)==4L))
      mnth <- as.numeric(str_sub(all_by_mth, 1L, 2L))
      stopifnot(all(mnth %in% 1:12))
      yr <- as.numeric(str_sub(all_by_mth, 3L, 4L))
      stopifnot(all(yr %in% 0:99))
    }
  }else{
    if(all(str_length(all_by_mth) == 2)){
      year_group <- TRUE
    }else if(!all(str_length(all_by_mth) == 4)){
      year_group <- FALSE
    }else{
      stop("One or more malformed all_by_mth")
    }
  }


  ## Check there aren't new SOR codes:
  cat("Checking SOR codes in epiLPR...\n")
  contacts <- get_sql_table("contacts")
  contacts %>%
    # filter(contact_in<"2022-01-01 00:00") %>%
    filter(contact_in < max_date) %>%
    count(unit_SOR) %>%
    collect() %>%
    identity() %>%
    arrange(desc(n)) ->
    sor_frq

  # Tolerate a small number of new error SOR:
  problems <- sor_frq %>% filter(!unit_SOR %in% sor_hospitals$unit_SOR)
  if(nrow(problems)>0L){
    if(testing){
      cat("NOTE:  There are new SOR (again)\n")
    }else
    {
      stop("There are new SOR: re-run data-raw/sor_hospitals and reinstall/load the package")
    }
  }
  if(!testing) stopifnot(all(sor_frq$unit_SOR %in% sor_hospitals$unit_SOR))


  if(is.null(risk_function)){
    # Function to relate hospitalisation risk based on delta time:
    # Note: should be vectorised!!!
    calculate_risk <- function(new_admission, new_discharge, new_type, old_admission, old_discharge, old_type, birth_year){
      case_when (as.numeric(new_admission-old_discharge, units="days")>30 ~ 0.0, TRUE ~ 1
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


  # by <- all_by_mth[1]

  # Note: output of this is cumulative!
  cat("Looping over", length(all_by_mth), "birth year/months..\n")
  fail_count_overall <- 0L

  for(by in all_by_mth){

    cat("Processing year: ", by, sep="")

    fail_count_loop <- 0L
    success <- FALSE
    st <- Sys.time()

    ## Extract contacts for particular birth year:
    while(!success){
      ss <- try({

        if(year_group){
          contacts_using <- contacts %>%
            filter(contact_in < max_date) %>%
            filter(cprnr %LIKE% str_c("____", by, "%")) %>%
            select(cprnr, unit_SOR, contact_in, contact_out)
        }else{
          contacts_using <- contacts %>%
            filter(contact_in < max_date) %>%
            filter(cprnr %LIKE% str_c("__", by, "%")) %>%
            select(cprnr, unit_SOR, contact_in, contact_out)
        }

        if(!testing){
          ## Make sure the database is not under active update:
          nrows1 <- contacts_using %>%
            summarise(rows = n()) %>%
            collect() %>%
            pull(rows)
          ## TODO:  rules for min rows
          if(nrows1 <= 100L){
            stop(paste0("Too few (", nrows1, ") rows in database"))
          }

          Sys.sleep(timeout[1L]*60L)
          
          # Removed second row count as it is unnecessary
          # (nrow of the collected data is checked below)
          #nrows2 <- contacts_using %>%
          #  summarise(rows = n()) %>%
          #  collect() %>%
          #  pull(rows)
          #if(nrows1 != nrows2){
          #  stop("Unstable number of rows in the database")
          #}
        }

        contacts_using <- contacts_using %>% collect()
        if(!testing && nrows1 != nrow(contacts_using)){
          stop("Unstable number of rows in the database after extraction")
        }

      })

      if(inherits(ss, "try-error")){
        fail_count_loop <- fail_count_loop + 1L
        fail_count_overall <- fail_count_overall + 1L

        if(fail_count_overall > retry){
          stop("Maximum failure time exceeded")
        }

        pause_time <- timeout[min(fail_count_loop, length(timeout))]
        cat(" [failed: timeout ", pause_time, " mins]", sep="")
        Sys.sleep( pause_time*60L )

      }else{
        cat(" (", nrow(contacts_using), " rows in contacts) ...", sep="")
        success <- TRUE
      }
    }

    if(nrow(contacts_using)==0L){
      cat("\tNo contacts found!\n", sep="")
      next
    }

    if(testing){
      contacts_using <- contacts_using %>% filter(unit_SOR %in% sor_hospitals$unit_SOR)
    }

    ## Merge with hospital and extract admission and dicharge from entire course:
    admissions_using <- contacts_using %>%
      left_join(sor_hospitals, by="unit_SOR") %>%
      group_by(cprnr, HospitalID, Type) %>%
      arrange(cprnr, HospitalID, Type, contact_in, contact_out) %>%
      mutate(EventNumber = c(0, cumsum(as.numeric(lead(contact_in)) > (4*60*60 + cummax(as.numeric(contact_out))))[-n()])) %>%
      group_by(cprnr, HospitalID, Type, EventNumber) %>%
      summarise(Admission = min(contact_in), Discharge = max(contact_out), .groups='drop') %>%
      select(-EventNumber)

    stopifnot(all(!is.na(admissions_using$HospitalID)))

    ## Exclude anybody that has only been to a single hospital:
    admissions_using <- admissions_using %>%
      distinct(cprnr, HospitalID, Type) %>%
      group_by(cprnr) %>%
      filter(n() > 1) %>%
      ungroup() %>%
      inner_join(admissions_using, by=c("cprnr","HospitalID", "Type"))

    ## Calculate connection strength between different hospitals for the same patient:
    trisk <- admissions_using %>%
      arrange(cprnr, Admission)

    ## These should be equivalent:
    if(FALSE){
      system.time({
        trisk2 <- EpiLPRNetwork:::Rcpp_patient_risk(trisk, within_hosp=TRUE) %>%
          filter(HospitalID!=OldHospital)
      })

      system.time({
        trisk3 <- EpiLPRNetwork:::Rcpp_patient_risk(trisk, within_hosp=FALSE)
      })

      stopifnot(nrow(trisk2)==nrow(trisk3))
    }

    trisk <- patient_risk(trisk, within_hosp=FALSE)
    stopifnot(all(!is.na(trisk$OldHospital)), all(!is.na(trisk$HospitalID)))

    # Equivalent but slower:
    if(FALSE){
      trisk2 <- trisk %>%
        ## TODO: replace with parallel::mclapply on a non-laptop
        ## TODO: replace with a C++ function
        # pbapply::pblapply(function(x){
        lapply(function(x){
          x %>%
            select(OldHospital=HospitalID, OldAdmission=Admission, OldDischarge=Discharge, OldType=Type) %>%
            full_join(x %>% mutate(Event=1:n()), by=character(0)) %>%
            filter(Admission >= OldAdmission, HospitalID!=OldHospital)
        }) %>% bind_rows()
    }

    if(output_level=="month"){
      aggfun <- function(x) strftime(x, "%Y-%m")
    }else if(output_level=="day"){
      aggfun <- function(x) as.Date(x, tz="Europe/Copenhagen")
    }else{
      stop("Unrecognised output_level")
    }

    ## Aggregate to time unit:
    trisk <- trisk %>%
      mutate(Risk = calculate_risk(Admission, Discharge, Type, OldAdmission, OldDischarge, OldType, as.numeric(str_sub(cprnr, start=5L, end=6L)))) %>%
      mutate(TimeUnit = aggfun(Admission)) %>%
      group_by(TimeUnit, HospitalID, Type, OldHospital, OldType) %>%
      summarise(TotalRisk = sum(Risk), .groups='drop') %>%
      filter(TotalRisk > 0.0) %>%
      select(TimeUnit, HospitalTo=HospitalID, TypeTo=Type, HospitalFrom=OldHospital, TypeFrom=OldType, TotalRisk)

    ## Calculate patient days in hospital:
    thosp <- admissions_using %>%
      mutate(AdmissionHours = as.numeric(Discharge - Admission, units="hours")) %>%
      mutate(AdmissionNights = as.numeric(as.Date(Discharge)-as.Date(Admission), units="days")) %>%
      mutate(OvernightAdmissions = as.numeric(as.Date(Discharge)-as.Date(Admission), units="days") > 0) %>%
      mutate(TimeUnit = aggfun(Admission)) %>%
      group_by(HospitalID, Type, TimeUnit) %>%
      summarise(TotalAdmissions = n(), AdmissionHours = sum(AdmissionHours),
                AdmissionNights = sum(AdmissionNights), OvernightAdmissions = sum(OvernightAdmissions),
                TotalPatients = length(unique(cprnr)),  .groups="drop")

    if(by==all_by_mth[1]){
      allrisk <- trisk
      allhosp <- thosp
    }else{
      allrisk <- bind_rows(allrisk, trisk)
      allhosp <- bind_rows(allhosp, thosp)
    }

    allrisk <- allrisk %>%
      group_by(TimeUnit, HospitalTo, TypeTo, HospitalFrom, TypeFrom) %>%
      summarise(TotalRisk = sum(TotalRisk), .groups='drop')

    allhosp <- allhosp %>%
      group_by(TimeUnit, HospitalID, Type) %>%
      # summarise(across(where(is.numeric), sum), .groups="drop")
      summarise_if(is.numeric, sum) %>%
      ungroup()

    cat(" finished in ", round(as.numeric(Sys.time()-st, units="mins"), 1), " minutes.\n", sep="")

  }

  cat("Done\n")

  return(list(risk=allrisk, hospital=allhosp))

}


