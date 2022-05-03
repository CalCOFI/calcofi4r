# Get stations from database
#   already loaded by https://github.com/CalCOFI/scripts/blob/6a81e3b420b5512d7040bfa076acd0a319e35200/load_bottle-stations.Rmd

# packages ----
if (!require("librarian")){
  install.packages("librarian")
  library(librarian)
}
librarian::shelf(
  DBI, dbplyr, dplyr, here, RPostgres, sf)

# database connect ----
db_pass_txt <- "~/.calcofi_db_pass.txt"
stopifnot(file.exists(db_pass_txt))

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname   = "gis",
  host     = "db.calcofi.io",
  port     = 5432,
  user     = "admin",
  password = readLines(db_pass_txt))

# get stations ----
stations <- sf::read_sf(con, "stations") %>%
  select(
    sta_id, sta_id_line, sta_id_station,
    lon, lat,
    is_offshore,
    is_cce, is_ccelter, is_sccoos)
usethis::use_data(stations, overwrite = TRUE)
