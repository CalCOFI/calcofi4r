# code to prepare `bottle` and related datasets

# libraries ----
if (!require("librarian")){
  install.packages("librarian")
  library(librarian)
}
librarian::shelf(
  dplyr, DT, dygraphs, glue, gstat, here, lubridate, mapview, purrr, readr,
  raster, rmapshaper, sf, skimr, stars, stringr, tidyr)
select <- dplyr::select

# paths ----
dir_data <- switch(
  Sys.info()["nodename"],
  #`ben-mbpro` = "/Users/bbest/My Drive (ben@ecoquants.com)/projects/calcofi/data",
  `Bens-MacBook-Pro.local` = "/Users/bbest/My Drive/projects/calcofi/data",
  `Cristinas-MacBook-Pro.local` = "/Volumes/GoogleDrive/.shortcut-targets-by-id/13pWB5x59WSBR0mr9jJjkx7rri9hlUsMv/calcofi/data")
# TODO: get Erin's Google Drive path and "nodename")

# Gdrive source paths
bottle_csv      <- file.path(dir_data, "/oceanographic-data/bottle-database/CalCOFI_Database_194903-202001_csv_22Sep2021/194903-202001_Bottle.csv")
cast_csv        <- file.path(dir_data, "/oceanographic-data/bottle-database/CalCOFI_Database_194903-202001_csv_22Sep2021/194903-202001_Cast.csv")
bottle_cast_rds <- file.path(dir_data, "/oceanographic-data/bottle-database/bottle_cast.rds")
DIC_csv         <- file.path(dir_data, "/DIC/CalCOFI_DICs_200901-201507_28June2018.csv")

# calcofi4r destination paths
calcofi_geo           <- here("data/calcofi_oceano-bottle-stations_convex-hull.geojson")
calcofi_offshore_geo  <- here("data/calcofi_oceano-bottle-stations_convex-hull_offshore.geojson")
calcofi_nearshore_geo <- here("data/calcofi_oceano-bottle-stations_convex-hull_nearshore.geojson")

# check paths
stopifnot(dir.exists(dir_data))
stopifnot(any(file.exists(bottle_csv,cast_csv)))

# read csv sources ----
d_bottle <- read_csv(bottle_csv, skip=1, col_names = F, guess_max = 1000000)
#d_bottle_problems() <- problems()
names(d_bottle) <- str_split(
  readLines(bottle_csv, n=1), ",")[[1]] %>%
  str_replace("\xb5", "µ")

d_cast  <- read_csv(cast_csv) %>%
  separate(
    Sta_ID, c("Sta_ID_line", "Sta_ID_station"),
    sep=" ", remove=F) %>%
  mutate(
    Sta_ID_line    = as.double(Sta_ID_line),
    Sta_ID_station = as.double(Sta_ID_station))

d_DIC <- read_csv(DIC_csv, skip=1, col_names = F, guess_max = 1000000)
names(d_DIC) <- str_split(
  readLines(DIC_csv, n=1), ",")[[1]] %>%
  str_replace("\xb5", "µ")
d_DIC <- d_DIC %>%
  rename("Sta_ID"="Line Sta_ID") %>%
  separate(
    Sta_ID, c("Sta_ID_line", "Sta_ID_station"),
    sep=" ", remove=F) %>%
  mutate(
    Sta_ID_line    = as.double(Sta_ID_line),
    Sta_ID_station = as.double(Sta_ID_station))

# stations ----
stations <- d_cast %>%
  select(Lon_Dec, Lat_Dec, Sta_ID, Sta_ID_line, Sta_ID_station) %>%
  filter(
    !is.na(Lon_Dec),
    !is.na(Lat_Dec)) %>%
  group_by(
    Sta_ID_station) %>%
  summarize(
    lon            = mean(Lon_Dec),
    lat            = mean(Lat_Dec),
    lon_sd         = sd(Lon_Dec),
    lat_sd         = sd(Lat_Dec),
    Sta_ID_lines   = paste(Sta_ID_line, collapse=";")) %>%
  mutate(
    offshore = ifelse(Sta_ID_station > 60, T, F)) %>%
  st_as_sf(
    coords = c("lon", "lat"), crs=4326, remove = F)
usethis::use_data(stations, overwrite = TRUE)

# bottle ----
bottle <- d_cast %>%
  left_join(
    d_bottle %>% select(-Sta_ID),
    by = "Cst_Cnt") %>%
  mutate(Date = lubridate::as_date(Date, format = "%m/%d/%Y"))
#saveRDS(d, bottle_cast_rds)
usethis::use_data(bottle, overwrite = TRUE)

# dic ----
# for now... ideally would join with all the data but this causes some issues with shared variables that have different values
dic <- d_cast %>%
  left_join(
    d_DIC %>%
      select(-Line.Sta_ID) %>%
      rename(
        Depthm = Depth.m.,
        Btl_Cnt = Bottle_Index,
        Bottle_O2_ml_L = `Bottle.O2.ml_L.`,
        Bottle_O2_µmol_kg = `Bottle.O2..æmol.Kg.`),
    by = c("Cst_Cnt" = "ID")) %>%
  mutate(Date = lubridate::as_date(Date, format = "%m/%d/%Y"))





usethis::use_data(dic, overwrite = TRUE)



# for summary, want to group by Sta_Code because each data point has a diff Sta_ID
get_pts <- function(data) {
  data %>%
    filter(
      !is.na(Lat_Dec),
      !is.na(Lon_Dec)) %>%
    group_by(
      Sta_ID) %>%
    summarize(
      lon            = mean(Lon_Dec),
      lat            = mean(Lat_Dec),
      Sta_ID_line    = mean(Sta_ID_line),
      Sta_ID_station = mean(Sta_ID_station)) %>%
    st_as_sf(
      coords = c("lon", "lat"), crs=4326, remove = F) %>%
    mutate(
      offshore = ifelse(Sta_ID_station > 60, T, F))
}


get_pts(bottle_cast) %>% mapview(zcol="offshore")
get_pts(DIC_cast) %>% mapview(zcol="offshore")

# keys ----

# downloaded CSV from CalCOFI website and added extra columns for plotting
key_bottle <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/18c6eSGRf0bSdraDocjn3-j1rUIxN2WKqxsEnPQCR6rA/edit#gid=2046976359") %>%
  na_if("n.a.") %>%
  mutate(
    dataset    = "bottle_cast",
    source_url = source_url[1])


key_dic <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1SGfGMJUhiYZKIh746p5pGn7cD0cOcTA3g_imAnvyz88/edit#gid=0") %>%
  na_if("n.a.") %>%
  mutate(
    dataset    = "DIC_cast",
    source_url = source_url[1])

data_key <- bind_rows(key_bottle, key_DIC) %>%
  select(dataset, field_name, title, description_abbv, everything())


# convert to var_lookup
var_lookup_key_tbl <- data_key %>%
  filter(!is.na(description_abbv))
var_lookup_key <- var_lookup_key_tbl %>%
  split(seq(nrow(.))) %>%
  lapply(as.list)
names(var_lookup_key) <- var_lookup_key_tbl$field_name
