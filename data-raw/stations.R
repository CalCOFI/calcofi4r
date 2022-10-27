# Get stations from database
#   already loaded by https://github.com/CalCOFI/scripts/blob/6a81e3b420b5512d7040bfa076acd0a319e35200/load_bottle-stations.Rmd
source(here::here("data-raw/db.R"))

# stations ----
stations <- sf::read_sf(con, "stations") %>%
  select(
    sta_id, sta_id_line, sta_id_station,
    lon, lat,
    is_offshore,
    is_cce, is_ccelter, is_sccoos)
usethis::use_data(stations, overwrite = TRUE)

# bottle_temp_lonlat ----
cc_bottle_temp <- dbGetQuery(
  con,
  "SELECT
     c.longitude AS lon, c.latitude AS lat,
     g.sta_dpos,
     AVG(b.t_degc) AS t_degc, COUNT(*) AS n
   FROM ctd_casts c
   INNER JOIN effort_grid g
     ON ST_Intersects(c.geom, g.geom)
   LEFT JOIN ctd_bottles b
     ON c.cast_count = b.cast_count
   GROUP BY c.longitude, c.latitude, g.sta_dpos;") %>%
  as_tibble()

cc_bottle <- dbGetQuery(
  con,
  "SELECT
     c.longitude AS lon, c.latitude AS lat,
     c.date, c.quarter,
     b.depthm AS depth_m,
     g.sta_dpos,
     AVG(b.t_degc)   AS t_degc,
     AVG(b.salinity) AS salinity,
     AVG(b.o2sat)    AS o2sat,
     COUNT(*) AS n
   FROM ctd_casts c
   INNER JOIN effort_grid g
     ON ST_Intersects(c.geom, g.geom)
   LEFT JOIN ctd_bottles b
     ON c.cast_count = b.cast_count
   GROUP BY
     c.longitude, c.latitude, g.sta_dpos,
     c.date, c.quarter,
     b.depthm") %>%
  as_tibble()
usethis::use_data(cc_bottle, overwrite = TRUE)

shelf(ggplot2, plotly)
g_depth <- ggplot(cc_bottle, aes(x = depth_m)) +
  geom_density(alpha = 0.5, fill = "black", color=NA) +
  scale_x_reverse() +
  coord_flip()
ggplotly(g_depth)

g_time <- ggplot(cc_bottle, aes(x = date)) +
  geom_density(alpha = 0.5, fill = "black", color=NA)
ggplotly(g_time)


cc_bottle %>%
  filter(n > 1)

cc_bottle_temp %>%
  st_as_sf(
    coords = c("lon", "lat"), crs = 3426) %>%
  mapview()

  tbl(con, "stations_order") %>%
  left_join(
    tbl(con, "ctd_casts"),
    by = c(
      "LINE" = "rptline",
      "STA"  = "rptsta")) %>%
  left_join(
    tbl(con, "ctd_bottles"),
    by="cast_count") %>%
  group_by(lon = longitude, lat = latitude) %>%
  summarize(
    v = mean(t_degc, na.rm=T),
    .groups = "drop") %>%
  collect()
usethis::use_data(bottle_temp_lonlat, overwrite = TRUE)

# area_calcofi_extended ----
h <- bottle_temp_lonlat %>%
  st_as_sf(
    coords = c("lon", "lat"), crs = 4326, remove = T) %>%
  st_union() %>%
  st_convex_hull()
ca <- rnaturalearthhires::states10 %>%
  st_as_sf() %>%
  filter(
    name == "California")
area_calcofi_extended <- st_difference(h, ca) # mapview::mapView(h)
usethis::use_data(area_calcofi_extended, overwrite = TRUE)


# bottle_temp_depth ----
bottle_temp_depth <- tbl(con, "stations_order") %>%
  left_join(
    tbl(con, "ctd_casts"),
    by = c(
      "LINE" = "rptline",
      "STA"  = "rptsta")) %>%
  left_join(
    tbl(con, "ctd_bottles"),
    by="cast_count") %>%
  group_by(cast_count, depth_m = depthm) %>%
  summarize(
    v = mean(t_degc, na.rm=T),
    .groups = "drop") %>%
  filter(!is.na(v)) %>%
  group_by(cast_count) %>%
  mutate(
    n_bottles = n()) %>%
  collect() %>%
  filter(
    n_bottles >= 70) %>%
  select(-n_bottles)
# z <- bottle_temp_depth %>%
#   group_by(cast_count) %>%
#   summarize(
#     n = n()) %>%
#   arrange(desc(n)) %>%
#   tibble::rowid_to_column("id")
#
# z %>%
#   group_by(n) %>%
#   summarize(
#     nrows = last(id),
#     .groups = "drop") %>%
#   arrange(desc(n)) %>% View()
usethis::use_data(bottle_temp_depth, overwrite = TRUE)

