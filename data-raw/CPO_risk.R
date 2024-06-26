## Find high-risk SOR for CPO


library("EpiLPRNetwork")

data("sor_hospitals")

contacts <- get_sql_table("contacts")
sor <- get_sql_table("sor")

### First extract all SOR used in contacts:
contacts %>%
  filter(contact_in<"2024-01-01 00:00") %>%
  count(unit_SOR) %>%
  collect() %>%
  identity() %>%
  arrange(desc(n)) ->
  sor_frq

matchfun <- function(text){
  text <- tolower(text)
  matrix <- vapply(keywords, function(x) str_detect(text, x), logical(length(text)))
  apply(matrix,1,any)
}

## Keywords:
speciale <- c("kirurgisk gastroenterologi", "h\u00e6matologi", "medicinsk gastroenterologi", "lungesygdomme")
keywords <- c("gastro","blod","haema","h\u00e6ma","lunge")
sor %>%
  select(unit_SOR = SorIdentifier, Enhedsnavn, Klinisk_speciale) %>%
  collect() %>%
  mutate(unit_SOR = as.character(unit_SOR)) %>%
  right_join(sor_frq, by="unit_SOR") %>%
  mutate(IsHighRisk = case_when(
    Klinisk_speciale %in% speciale ~ "Yes-sp",
    is.na(Enhedsnavn) ~ "Missing",
    matchfun(Enhedsnavn) ~ "Yes-en",
    .default = "No"
  )) |>
  full_join(
    sor_hospitals,
    by="unit_SOR"
  ) |>
  mutate(HighRisk = case_when(
    str_detect(IsHighRisk, "Yes") & !is.na(HospitalManual) ~ "Yes",
    .default="No"
  )) ->
  risk_sor
data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABgAAAASCAYAAABB7B6eAAABDklEQVR4Xp2UMaoCMRRFFcHCxsrO0tba/i/ADfwV/A2ICFYW7kAY/CSZOGYgnVjbW9taugFbG8f3OpO5A3lzIFh4Dg4ON53VelPp3NWOsW7R+SLVq1FVVbchfhtz/JV6EO99vyF+Ket+pB4ky/ywIX5qXUylHmRfFGMUm9w9+DupB/k/HCY4Lu9K+ZHUgyhbzlBM55pl54HUgxhTzlGscneil92TehBljn9xyMfYctfGq0HSMo748BO38QJ4PCS9UcSjk3oBPBqSXijisUm9AB6L5tGAiEcm9QJ4JDwWFMVDS/EC+FdJuqEovipSvAD+v0i6oCi+7FK8AH7T2roCRfF1neLVIGmLIvpctvFiPtxv6UGYMompAAAAAElFTkSuQmCC
stopifnot(all(sor_frq$unit_SOR %in% risk_sor$unit_SOR))

risk_sor |> count(HighRisk, HospitalID)
risk_sor |> count(HighRisk, HospitalManual) |> print(n=Inf)

risk_sor %>%
  group_by(HighRisk) %>%
  mutate(Index = str_replace_all(format(1:n()), " ", "0")) %>%
  ungroup() %>%
  mutate(HospitalID = case_when(
    HighRisk=="No" ~ HospitalID,
    HighRisk=="Yes" ~ str_c(HospitalID, "_HR", Index),
  )) ->
  sor_risk_hosp

sor_risk_hosp |> count(HighRisk, Type)

sor_risk_hosp |> filter(Type=="somatisk") |> count(HighRisk, HospitalID) |> count()
sor_risk_hosp |> count(HospitalID) |> arrange(HospitalID) |> View()


edge_res <- get_edges(sor_hospitals = sor_risk_hosp)
save(edge_res, file=str_c("edge_res_", as.character(Sys.Date()), ".rda"))

edge_res$risk |> count(HospitalFrom) |> arrange(HospitalFrom) |> View()
edge_res$hospital |> count(HospitalID) |> arrange(HospitalID) |> View()
edge_res$sor_hospitals |> filter(str_detect(HospitalID, "HR")) |> arrange(HospitalID)
