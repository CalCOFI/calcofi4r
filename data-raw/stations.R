# TODO: next, determine which stations to use:
# - remove duplicate line_sta from stations_cce w/ diff't lon/lat?
# - sf::st_make_grid then get all non-duplicating from proj calcofi,
#   expanding into Baja / NorCal?

librarian::shelf(
  glue, here, dplyr, mapview, PROJ, purrr, readr, sf, stringr)

cc_stations_csv  <- here("data-raw/CalCOFI station LatLong.csv")
cce_stations_txt <- here("data-raw/CCE_Stations.txt")

stations_cc <- read_csv(cc_stations_csv) %>%
  rename_with(tolower) %>%
  select(order, line, sta, lon = `lon dec`, lat = `lat dec`) %>%
  mutate(
    line_sta = glue("{line} {sta}") %>% as.character(),
    offshore = ifelse(sta > 60, T, F)) %>%
  st_as_sf(
    coords = c("lon", "lat"), crs=4326, remove = F)

# mapviewOptions(fgb = FALSE)
# mapview(stations_cc, zcol = "offshore")
usethis::use_data(stations_cc, overwrite = TRUE)

proj <- "/Users/bbest/homebrew/bin/proj" # on Ben's MacBookPro

stations_cce <- read_tsv(cce_stations_txt, skip = 2) %>%
  select(lon = LonDec, lat = LatDec) %>%
  st_as_sf(
    coords = c("lon", "lat"), crs=4326, remove = F) %>%
  mutate(
    line_sta = map2_chr(lon, lat, function(x, y){
      system(glue("echo {x} {y} | {proj} +proj=calcofi +epsg=4326"), intern=T) }),
    line = map_dbl(line_sta, function(x)
      str_split(x, "\t", simplify = T)[1] %>% as.numeric()),
    sta = map_dbl(line_sta, function(x)
      str_split(x, "\t", simplify = T)[2] %>% as.numeric()),
    offshore = ifelse(sta > 60, T, F))

# mapviewOptions(fgb = FALSE)
# mapview(cce_stations)
usethis::use_data(stations_cce, overwrite = TRUE)
