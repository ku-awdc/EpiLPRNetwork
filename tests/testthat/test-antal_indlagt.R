## Test antal_indlagt (number admitted)

library("tidyverse")
library("testthat")

# Set up test data frames:
set.seed(2023-03-14)

aggfun <- function(x) strftime(x, "%Y-%m")
tibble(
  Date = seq(as.Date("2023-03-01"), as.Date("2024-03-01"), by=1L)
) |>
  mutate(TimeUnit = aggfun(Date)) |>
  distinct(TimeUnit) |>
  mutate(TimeMin = as.POSIXct(str_c(TimeUnit,"-01 00:00:00", tz="Europe/Copenhagen"))) |>
  mutate(TimeMax = ceiling_date(TimeMin, unit="month", change_on_boundary=TRUE)) ->
  time_units

n <- 1e5
df <- tibble(cprnr = sample(10:100, n, TRUE), HospitalID = sample(LETTERS, n, TRUE), Admission = as.POSIXct("2023-03-14T12:00Z")+round(runif(n,0,365*24*60*60))) |>
  arrange(cprnr) |>
  mutate(Discharge = Admission + round(runif(n,1*60*60,90*24*60*60)), Type = sample(1:3, n, TRUE)) |>
  mutate(TimeUnit = aggfun(Admission))

system.time({
cppval <- antal_indlagt(df, time_units)
})
cppval <- cppval |> arrange(HospitalID, Type, TimeUnit)

system.time({
expand_grid(
  df |> select(-TimeUnit),
  time_units
) |>
  filter(Admission <= TimeMax, Discharge >= TimeMin) |>
  mutate(PatientHours = as.numeric(pmin(TimeMax, Discharge) - pmax(TimeMin, Admission), units="hours")) |>
  mutate(NumDischarges = as.integer(Discharge < TimeMax)) |>
  group_by(HospitalID, Type, TimeUnit) |>
  summarise(PatientHours = sum(PatientHours), NumStays = n(), NumDischarges = sum(NumDischarges), .groups="drop") |>
  arrange(HospitalID, Type, TimeUnit) ->
  rval
})

test_that("data frames equivalent", {
  expect_equal(nrow(cppval), nrow(rval))
  expect_equal(cppval$TimeUnit, rval$TimeUnit)
})

test_that("hours the same", {
  expect_equal(cppval$PatientHours, rval$PatientHours)
})

test_that("integers the same", {
  expect_equal(cppval$NumStays, rval$NumStays)
  expect_equal(cppval$NumDischarges, rval$NumDischarges)
})

