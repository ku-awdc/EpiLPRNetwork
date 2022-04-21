## code to prepare `DATASET` dataset goes here

library(RODBC)
library(odbc)
library("EpiLPRNetwork")
library(sf)
library(dbscan)
library(fpc)


contacts <- get_sql_table("contacts")
sor <- get_sql_table("sor")


SOREntity_daily_file <- "G:/PhD/Sygehus klassifikation/Nyeste SOR/SOREntity_daily.csv"
if(!file.exists(SOREntity_daily_file)) stop("SOREntity_daily file not found")
NoGIScsv_file <- "G:/PhD/Sygehus klassifikation/Nyeste SOR/UnitNoGIS_240222.xlsx"
if(!file.exists(NoGIScsv_file)) stop("NoGIScsv file not found")
manual_hospital_file <- "G:/PhD/Sygehus klassifikation/Nyeste SOR/missing_locations.xlsx"
if(!file.exists(manual_hospital_file)) stop("manual_hospital file not found")
missing_region_file <- "G:/PhD/Sygehus klassifikation/Nyeste SOR/missing_region.xlsx"
if(!file.exists(missing_region_file)) stop("missing_region file not found")

### First extract all SOR used in contacts:
contacts %>%
  filter(contact_in<"2022-01-02 00:00") %>%
  count(unit_SOR) %>%
  collect() %>%
  identity() %>%
  arrange(desc(n)) ->
  sor_frq

### Add any that we had previously but have now disappeared:
data(sor_hospitals)
sor_frq <- full_join(sor_frq, sor_hospitals %>% select(unit_SOR), by="unit_SOR")


### Then extract relevant information from the database:
sor_db <- sor %>%
  select(unit_SOR = SorIdentifier, OwnerType_db=IE_ejerforhold, UnitName_db = Enhedsnavn, SundhedsinstitutionNavn_db = Sundhedsinstitution, Latitude_db = Latitude, Longitude_db = Longitude, OphoertDato, FraDato, Region = Region) %>%
  collect() %>%
  mutate(Region = str_replace(Region, "Region ", "")) %>%
  mutate(unit_SOR = as.character(unit_SOR)) %>%
  right_join(sor_frq, by="unit_SOR")
with(sor_db %>% count(unit_SOR) %>% count(n, name="nn"), stopifnot(all(n==1)))
with(sor_db, range(Latitude_db, na.rm=TRUE))
with(sor_db, range(Longitude_db, na.rm=TRUE))

### Then extract the same information from this other CSV file:
SORcsv <- read_csv2(SOREntity_daily_file,locale=locale(encoding="UTF-8"), col_types = cols(.default=col_character(), PostalAddressCoordETRS89z32NMeasure = col_double(), PostalAddressCoordETRS89z32EMeasure = col_double())) %>%
  select(unit_SOR = SorId, UnitName = EntityName, OwnerType = InstitutionOwnerEntityTypeName, SundhedsinstitutionNavn = HealthInstitutionEntityName, Latitude = PostalAddressCoordETRS89z32NMeasure, Longitude = PostalAddressCoordETRS89z32EMeasure) %>%
  right_join(sor_frq, by="unit_SOR")
SORcsv %>% group_by(unit_SOR) %>% filter(n() > 1)
SORcsv <- SORcsv[!duplicated(SORcsv[c("unit_SOR","Latitude","Longitude")]),]
with(SORcsv %>% count(unit_SOR) %>% count(n, name="nn"), stopifnot(all(n==1)))
with(SORcsv, range(Latitude, na.rm=TRUE))
with(SORcsv, range(Longitude, na.rm=TRUE))

### Then extract further manual additions:
NoGIScsv <- readxl::read_excel(NoGIScsv_file, na=c("", "NA")) %>%
  ## Remove trailing commas from some Latitudes:
  mutate(Latitude = str_replace(Latitude,",$","")) %>%
  mutate(LatitudeNum = as.numeric(Latitude), LongitudeNum = as.numeric(Longitude)) %>%
  select(unit_SOR, LatitudeNum, LongitudeNum) %>%
  filter(!is.na(LatitudeNum), !is.na(LongitudeNum)) %>%
  st_as_sf(coords=c("LongitudeNum","LatitudeNum")) %>%
  st_set_crs("WGS84") %>%
  st_transform(25832) %>%  # https://epsg.io/25832
  mutate(Easting = st_coordinates(geometry)[,1], Northing = st_coordinates(geometry)[,2]) %>%
  as_tibble() %>%
  select(-geometry) %>%
  right_join(sor_frq, by="unit_SOR")
NoGIScsv <- NoGIScsv[!duplicated(NoGIScsv[c("unit_SOR","Easting","Northing")]),]
with(NoGIScsv %>% count(unit_SOR) %>% count(n, name="nn"), stopifnot(all(n==1)))
with(NoGIScsv, range(Easting, na.rm=TRUE))
with(NoGIScsv, range(Northing, na.rm=TRUE))

### Then join, and use the database first and CSV file if database is missing:
SORcombined <- full_join(sor_db, SORcsv, by=c("unit_SOR")) %>%
  full_join(NoGIScsv, by=c("unit_SOR")) %>%
  mutate(UnitName = case_when(
    !is.na(UnitName) ~ UnitName,
    TRUE ~ UnitName_db
  )) %>%
  mutate(OwnerType = case_when(
    !is.na(OwnerType) ~ OwnerType,
    TRUE ~ OwnerType_db
  )) %>%
  mutate(SundhedsinstitutionNavn = case_when(
    !is.na(SundhedsinstitutionNavn) ~ SundhedsinstitutionNavn,
    !is.na(SundhedsinstitutionNavn_db) ~ SundhedsinstitutionNavn_db,
    TRUE ~ UnitName
  )) %>%
  mutate(Easting = case_when(
    !is.na(Latitude) ~ Latitude,
    !is.na(Latitude_db) ~ Latitude_db,
    !is.na(Easting) ~ Easting,
    TRUE ~ NA_real_
  )) %>%
  mutate(Northing = case_when(
    !is.na(Longitude) ~ Longitude,
    !is.na(Longitude_db) ~ Longitude_db,
    !is.na(Northing) ~ Northing,
    TRUE ~ NA_real_
  )) %>%
  select(unit_SOR, Region, n, UnitName, OwnerType, SundhedsinstitutionNavn, Easting, Northing, OphoertDato, FraDato) %>%
  mutate(HasGIS = !(is.na(Easting) | is.na(Northing)))

sor_missing <- SORcombined %>%
  filter(is.na(Easting) | is.na(Northing)) %>%
  arrange(desc(n))
unit_sor_missing <- sor_missing$unit_SOR
contacts %>%
  filter(unit_SOR %in% unit_sor_missing) %>%
  collect() %>%
  identity() ->
  contacts_missing_sor


contacts_missing_sor %>%
  mutate(year = lubridate::year(contact_in)) %>%
  count(unit_SOR, responsible_SOR, year, source) %>%
  arrange(unit_SOR, responsible_SOR, source, year, desc(n)) ->
  missing_by_year_sor
contacts_missing_sor %>%
  mutate(year = lubridate::year(contact_in)) %>%
  count(unit_SOR) %>%
  arrange(desc(n)) ->
  missing_by_sor
contacts_missing_sor %>%
  mutate(year = lubridate::year(contact_in)) %>%
  count(year, source) %>%
  identity() ->
  missing_by_year

# writexl::write_xlsx(list(by_sor=sor_missing, by_year_sor=missing_by_year_sor, by_year=missing_by_year), path="missing_locations.xlsx", format_headers = FALSE)
# system('open missing_locations.xlsx')

contacts_missing_sor %>%
  mutate(year = lubridate::year(contact_in)) %>%
  count(year, source)

## Dataset 1:  hospital identifier

# List of private hospitals by name:
# aleris-hamlet, etc
# If private and not hospital then hospital = sor_unit
# If no spatial coords then use external lookup Excel file
# Otherwise use two clustering algorithms

# Used only when we don't have GIS coordinates:
manual_hospitals <- readxl::read_excel(manual_hospital_file, "by_sor") %>%
  select(unit_SOR, ManualHospital=SundhedsinstitutionNavn)

SORcombined %>%
  filter(OwnerType=="privat") %>%
  count(OwnerType, SundhedsinstitutionNavn) %>%
  arrange(desc(n)) ->
  private_hospitals
stopifnot(all(!is.na(private_hospitals$SundhedsinstitutionNavn)))

private_hospitals %>% print(n=Inf)

alerishamlet <- c("aleris","hamlet","euroeyes")
other_private <- c(
  capio="capio",
  filadelfia="filadelfia",
  valdemar="valdemar"
)

stopifnot(length(alerishamlet)==3)
private_hospitals %>%
  mutate(HospitalType = case_when(
    agrepl(alerishamlet[1], tolower(SundhedsinstitutionNavn)) ~ "Private_AlerisHamlet",
    agrepl(alerishamlet[2], tolower(SundhedsinstitutionNavn)) ~ "Private_AlerisHamlet",
    agrepl(alerishamlet[3], tolower(SundhedsinstitutionNavn)) ~ "Private_AlerisHamlet",
    agrepl(other_private[1], tolower(SundhedsinstitutionNavn)) ~ "Private_Capio",
    agrepl(other_private[2], tolower(SundhedsinstitutionNavn)) ~ "Private_Filadelfia",
    agrepl(other_private[3], tolower(SundhedsinstitutionNavn)) ~ "Private_Valdemar",
    TRUE ~ str_c("Private_", SundhedsinstitutionNavn)
  )) ->
  private_hospitals

SORbytype <- SORcombined %>%
  select(-n) %>%
  full_join(private_hospitals, by=c("OwnerType","SundhedsinstitutionNavn")) %>%
  mutate(HospitalType = case_when(
    is.na(Easting) | is.na(Northing) ~ "MissingGIS",
    is.na(OwnerType) ~ "Public",
    OwnerType=="stat" ~ "Public",
    OwnerType=="region" ~ "Public",
    TRUE ~ HospitalType
  )) %>%
  full_join(manual_hospitals, by="unit_SOR") %>%
  split(.$HospitalType)

set.seed(2022-02-08)
SORbytype %>%
  pbapply::pblapply(function(x){
    if(x$HospitalType[1]=="MissingGIS"){
      return(x %>%
               mutate(HospitalID = case_when(
                 !is.na(ManualHospital) ~ str_c(HospitalType, "_", ManualHospital),
                 TRUE ~ str_c(HospitalType, "_", gsub(" ", "0", format(1:n())))
               ))
      )
    }
    x %>%
      st_as_sf(coords=c("Northing","Easting")) %>%
      st_set_crs(25832) %>%
      st_transform("WGS84") %>%
      mutate(Latitude = st_coordinates(geometry)[,1], Longitude = st_coordinates(geometry)[,2]) %>%
      as_tibble() ->
      x
    cluster <- fpc::dbscan(x[,c("Longitude","Latitude")], eps = 0.01, MinPts = 1)$cluster
    x %>% mutate(
      HospitalID = str_c(HospitalType, "_cluster_", gsub(" ", "0", format(cluster)))
    )
  }) %>%
  bind_rows() ->
  finalSOR
stopifnot(all(!is.na(finalSOR$HospitalType)))
stopifnot(all(finalSOR$unit_SOR %in% sor_db$unit_SOR))

sor_hospitals <- finalSOR %>%
  as_tibble() %>%
  select(unit_SOR, Region, OwnerType, HospitalID, Latitude, Longitude)

# writexl::write_xlsx(list(missing_region = sor_hospitals %>% filter(is.na(Region) | is.na(OwnerType))), missing_region_file)
## TODO: fix missing region and ownertype
missingregion <- readxl::read_xlsx(missing_region_file)



sor_hospitals <- sor_hospitals %>%
  mutate(HospitalID = gsub(" ", "_", HospitalID))
Encoding(sor_hospitals$OwnerType) %>% table()
Encoding(sor_hospitals$Region) %>% table()
Encoding(sor_hospitals$HospitalID) %>% table()

final_hospital <- sor_hospitals %>%
  count(HospitalID) %>%
  arrange(desc(n))

## TODO: add unit type e.g. OutPatient, InPatient, Radiology, or by Speciality, or something:
sor_hospitals <- sor_hospitals %>%
  mutate(Type = "Unknown") %>%
  mutate(HospitalName = HospitalID) %>%
  mutate(HospitalID = as.integer(factor(HospitalName)), HospitalID = str_c("Hospital_", str_replace_all(format(HospitalID), " ", "0")))

if(TRUE){
  writexl::write_xlsx(list(sor_hospitals=sor_hospitals, final_hospital=final_hospital), str_c("G:/PhD/Sygehus klassifikation/sor_hospitals_", strftime(Sys.Date(), "%Y%m%d"), ".xlsx"))
}


usethis::use_data(sor_hospitals, overwrite = TRUE)
