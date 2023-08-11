library("EpiLPRNetwork")

## TODO: if region is missing for a SOR then take it from non-missing for the same hospital

## To remove type i.e. for comparing with old runs:
data(sor_hospitals)
sor_hospitals$Type <- "Whatever"
output_no_type <- get_edges(all_by_mth = "63", sor_hospitals = sor_hospitals)
using <- output_no_type$risk

output_with_type <- get_edges(all_by_mth = "43", sor_hospitals = NULL)
using <- output_with_type$risk |> filter(HospitalTo != HospitalFrom)


sor_hospitals |> count(Region, HospitalID) |> group_by(HospitalID) |> summarise(n=n()) |> filter(n==4)

hospital_regions <- sor_hospitals |> filter(!is.na(Region)) |> count(Region, HospitalID) |> group_by(HospitalID) |> arrange(desc(n)) |> slice(1) |> ungroup() |> select(-n)

using |>
  left_join(
    hospital_regions |> select(RegionFrom = Region, HospitalFrom = HospitalID),
    by="HospitalFrom"
  ) |>
  left_join(
    hospital_regions |> select(RegionTo = Region, HospitalTo = HospitalID),
    by="HospitalTo",
  ) |>
  filter(HospitalTo != HospitalFrom) |>
  filter(!is.na(RegionFrom), !is.na(RegionTo)) |>
  mutate(Region = case_when(
    RegionFrom == RegionTo ~ RegionFrom,
    TRUE ~ "Transfer"
  )) |>
  mutate(Year = str_sub(TimeUnit, 1, 4)) |>
  group_by(TypeFrom, TypeTo, Region, RegionFrom, RegionTo, Year) |>
  summarise(TotalRisk = sum(TotalRisk), N = n(), .groups="drop") |>
  mutate(Type = str_c(TypeFrom, " - ", TypeTo)) ->
  plotdata

plotdata |>
  group_by(Type, Region, Year) |>
  summarise(TotalRisk = sum(TotalRisk), N = n(), .groups="drop") |>
  ggplot(aes(x=Year, y=TotalRisk, col=Type, group=Type)) +
  geom_line() +
  facet_wrap(~Region)

plotdata |>
  filter(Region=="Transfer") |>
  ggplot(aes(x=Year, y=TotalRisk, col=Type, group=Type)) +
  geom_line() +
  facet_grid(RegionFrom~RegionTo)

## Separate output by birth year:
output <- vector('list', 100)
names(output) <- (format(0:99) |> str_replace(" ", "0"))
for(y in names(output)){
  cat(y, "...\n")
  get_edges(all_by_mth = y, sor_hospitals = NULL)$risk |>
    left_join(
      hospital_regions |> select(RegionFrom = Region, HospitalFrom = HospitalID),
      by="HospitalFrom"
    ) |>
    left_join(
      hospital_regions |> select(RegionTo = Region, HospitalTo = HospitalID),
      by="HospitalTo",
    ) |>
    filter(HospitalTo != HospitalFrom) |>
    filter(!is.na(RegionFrom), !is.na(RegionTo)) |>
    mutate(Region = case_when(
      RegionFrom == RegionTo ~ RegionFrom,
      TRUE ~ "Transfer"
    )) |>
    mutate(Year = str_sub(TimeUnit, 1, 4)) |>
    group_by(TypeFrom, TypeTo, Region, RegionFrom, RegionTo, Year, TimeUnit) |>
    summarise(TotalRisk = sum(TotalRisk), N = n(), .groups="drop") |>
    mutate(BirthYear = y, Type = str_c(TypeFrom, " - ", TypeTo)) ->
    output[[y]]
}

output[[1]] |>
  group_by(Type, Region, Year) |>
  summarise(TotalRisk = sum(TotalRisk), N = n(), .groups="drop") |>
  ggplot(aes(x=Year, y=TotalRisk, col=Type, group=Type)) +
  geom_line() +
  facet_wrap(~Region)

output[[1]] |>
  filter(Region=="Transfer") |>
  group_by(Type, RegionTo, RegionFrom, Year) |>
  summarise(TotalRisk = sum(TotalRisk), N = n(), .groups="drop") |>
  ggplot(aes(x=Year, y=TotalRisk, col=Type, group=Type)) +
  geom_line() +
  facet_grid(RegionFrom~RegionTo)
