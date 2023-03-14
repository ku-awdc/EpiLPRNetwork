## Test patient_risk

library("tidyverse")
library("testthat")

# Set up test data frame:
set.seed(2023-03-14)
n <- 1e4
df <- tibble(cprnr = sample(10:100, n, TRUE), HospitalID = sample(LETTERS, n, TRUE), Admission = as.POSIXct("2023-03-14T12:00Z")+round(runif(n,0,365*24*60*60))) |>
  arrange(cprnr) |>
  mutate(Discharge = Admission + round(runif(n,1*60*60,30*24*60*60)), Type = sample(1:3, n, TRUE))

# Check we get an error for unsorted data frame:
test_that("errors correctly", {
  expect_error(patient_risk(df))
})

# Check we get a warning for overlapping admissions:
df <- df |> arrange(cprnr, Admission)
test_that("warns correctly", {
  expect_warning(patient_risk(df))
})

# Fix data and run:
df |>
  group_by(cprnr) |>
  mutate(Discharge = pmin(Discharge, lead(Admission, 1L), na.rm=TRUE)) |>
  ungroup() |>
  filter(!is.na(Admission)) ->
  df

test_that("function runs", {
  between_hosp <<- expect_no_error(patient_risk(df, within_hosp=FALSE))
  within_hosp <<- expect_no_error(patient_risk(df, within_hosp=TRUE))
  expect_true(nrow(between_hosp) < nrow(within_hosp))
})

# Compare to R code version:
within_R <- EpiLPRNetwork:::patient_risk_R(df) |> arrange(cprnr, OldHospital, OldAdmission, OldDischarge, HospitalID, Admission)
between_hosp <- between_hosp |> arrange(cprnr, OldHospital, OldAdmission, OldDischarge, HospitalID, Admission)
within_hosp <- within_hosp |> arrange(cprnr, OldHospital, OldAdmission, OldDischarge, HospitalID, Admission)

test_that("function equivalence - within_hosp", {
  expect_equal(nrow(within_R), nrow(within_hosp))
  expect_equal(within_R, within_hosp)
})

between_R <- within_R |> filter(OldHospital != HospitalID)
test_that("function equivalence - between_hosp", {
  expect_equal(nrow(between_R), nrow(between_hosp))
  expect_equal(between_R, between_hosp)
})
