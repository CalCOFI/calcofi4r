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
  `ben-mbpro` = "/Users/bbest/My Drive (ben@ecoquants.com)/projects/calcofi/data",
  `Cristinas-MacBook-Pro.local` = "/Volumes/GoogleDrive/.shortcut-targets-by-id/13pWB5x59WSBR0mr9jJjkx7rri9hlUsMv/calcofi/data")
# TODO: get Erin's Google Drive path and "nodename")
bottle_csv <- file.path(dir_data, "/oceanographic-data/bottle-database/CalCOFI_Database_194903-202001_csv_22Sep2021/194903-202001_Bottle.csv")
cast_csv <- file.path(dir_data, "/oceanographic-data/bottle-database/CalCOFI_Database_194903-202001_csv_22Sep2021/194903-202001_Cast.csv")
bottle_cast_rds <- file.path(dir_data, "/oceanographic-data/bottle-database/bottle_cast.rds")

DIC_csv  <- file.path(dir_data, "/DIC/CalCOFI_DICs_200901-201507_28June2018.csv")
calcofi_geo           <- here("data/calcofi_oceano-bottle-stations_convex-hull.geojson")
calcofi_offshore_geo  <- here("data/calcofi_oceano-bottle-stations_convex-hull_offshore.geojson")
calcofi_nearshore_geo <- here("data/calcofi_oceano-bottle-stations_convex-hull_nearshore.geojson")

# check paths
stopifnot(dir.exists(dir_data))
stopifnot(any(file.exists(bottle_csv,cast_csv)))

# read data
d_bottle <- read_csv(bottle_csv, skip=1, col_names = F)
names(d_bottle) <- str_split(
  readLines(bottle_csv, n=1), ",")[[1]] %>%
  str_replace("\xb5", "µ")
d_cast   <- read_csv(cast_csv)
d_DIC    <- read.csv(DIC_csv, fileEncoding="latin1")
# %>%
#   separate(
#     `Line Sta_ID`, c("Sta_ID_Line", "Sta_ID_Station"),
#     sep=" ", remove=F) %>%
#   mutate(
#     Sta_ID_Line    = as.double(Sta_ID_Line),
#     Sta_ID_Station = as.double(Sta_ID_Station),
#     offshore       = ifelse(Sta_ID_Station > 60, T, F))


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
