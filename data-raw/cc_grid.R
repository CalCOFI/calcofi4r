source(here::here("data-raw/db.R"))
librarian::shelf(
  # janitor, leaflet,
  mapview)

rng_lin <- tbl(con, "stations_order") %>%
  filter(STA <= 60) %>%
  pull(LINE) %>%
  c(93.7) %>%
  range() # 60.0 93.4
rng_pos <- tbl(con, "stations_order") %>%
  filter(STA <= 60) %>%
  pull(STA) %>%
  c(25) %>%
  range() # 26.4 60.0

g5 <- expand_grid(
  lin = c(
    57.7,
    map(seq(60, 100, 10), function(x){
      cumsum(c(x, 3.3, 3.4)) })) %>% unlist(),
  pos = c(seq(15, 60, 5), 70)) %>%
  st_as_sf(
    coords = c("lin", "pos"), remove = F,
    crs = st_crs("+proj=calcofi"))
# mapView(g5)

v_pos5 <- st_voronoi(st_union(st_geometry(g5))) %>%
  st_collection_extract(type = "POLYGON") %>%
  st_sf() %>%
  st_make_valid() %>%
  st_join(g5) %>%
  filter(
    lin <= rng_lin[2],
    lin >= rng_lin[1],
    pos <= 60,
    pos >= rng_pos[1]) %>%
  mutate(
    dpos = 5)
# mapView(v_pos5) + mapView(g5)

g10 <- expand_grid(
  lin = c(
    map(seq(50, 100, 10), function(x){
      cumsum(c(x, 3.3, 3.4)) })) %>% unlist(),
  pos = seq(60, 130, 10)) %>%
  st_as_sf(
    coords = c("lin", "pos"), remove = F,
    crs = st_crs("+proj=calcofi"))
# mapView(g)

v_pos10 <- st_voronoi(st_union(st_geometry(g10))) %>%
  st_collection_extract(type = "POLYGON") %>%
  st_sf() %>%
  st_make_valid() %>%
  st_join(g10) %>%
  filter(
    lin >= 60 & lin <= 93.3,
    pos >= 70 & pos <= 120,
    ifelse(
      lin < 83.3,
      pos <= 100,
      T),
    ifelse(
      lin %in% c(83.3, 86.7),
      pos <= 110,
      T)) %>%
  mutate(
    dpos = 10)
# mapView(v_pos10) +
#   mapView(g10)

lnd <- rnaturalearth::ne_countries(
  country =c(
    "United States of America", "Mexico", "Canada"),
  scale = 10, returnclass = "sf") %>%
  st_union()

# h0 <- h
h <- st_read(
  con,
  query =
    "SELECT
       ST_ConvexHull(ST_COLLECT(geom))
     FROM ctd_casts") %>%
  st_difference(lnd) %>%
  st_make_valid()
# mapView(h)

hp <- st_bbox(h) %>%
  st_as_sfc() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  st_as_sf(
    coords = c("X", "Y"),
    crs = 4326)
# hp

hb <- hp %>%
  st_transform(st_crs("+proj=calcofi")) %>%
  st_bbox()

g20 <- expand_grid(
  lin = c(
    seq(60, hb["xmax"] + 10,  10),
    seq(60, hb["xmin"] - 10, -10)) %>%
    unique(),
  pos = c(
    seq(100, hb["ymax"]+ 20,  20),
    seq(100, hb["ymin"]- 20, -20)) %>%
    unique()) %>%
  st_as_sf(
    coords = c("lin", "pos"), remove = F,
    crs = st_crs("+proj=calcofi"))
# mapView(g20)

v_pos20 <- st_voronoi(st_union(st_geometry(g20))) %>%
  st_collection_extract(type = "POLYGON") %>%
  st_sf() %>%
  st_make_valid() %>%
  st_join(g20) %>%
  filter(
    lin < hb["xmax"],
    lin > hb["xmin"],
    pos < hb["ymax"],
    pos > hb["ymin"]) %>%
  mutate(
    dpos = 20)

v_pos20 <- v_pos20 %>%
  filter(
    st_intersects(
      v_pos20,
      h %>% st_transform(st_crs("+proj=calcofi")),
      sparse = F)[,1])

v_pos10 <- st_difference(
  v_pos10, st_union(v_pos5))

V <- v_pos20 %>%
  st_difference(
    st_union(
      st_union(v_pos10),
      st_union(v_pos5))) %>%
  bind_rows(v_pos10) %>%
  bind_rows(v_pos5)

V <- V %>%
  filter(
    st_intersects(
      V,
      h %>% st_transform(st_crs("+proj=calcofi")),
      sparse = F)[,1])
# mapView(V)

u <- V %>%
  filter(
    (dpos == 20 & lin == 60 & pos == 120) |
      (dpos == 20 & lin == 60 & pos == 100)) %>%
  st_union() %>%
  st_as_sf() %>%
  rename(geometry = x) %>%
  mutate(
    dpos = 20,
    lin  = 60,
    pos  = 110)
# mapView(V) + mapView(u)
V <- st_difference(V, st_geometry(u)) %>%
  bind_rows(u)

u <- V %>%
  filter(
    (dpos == 20 & lin == 70 & pos == 120) |
      (dpos == 20 & lin == 70 & pos == 100)) %>%
  st_union() %>%
  st_as_sf() %>%
  rename(geometry = x) %>%
  mutate(
    dpos = 20,
    lin  = 70,
    pos  = 110)
# mapView(V) + mapView(u)
V <- st_difference(V, st_geometry(u)) %>%
  bind_rows(u)

u <- V %>%
  filter(
    (dpos == 20 & lin == 80 & pos == 120) |
      (dpos == 20 & lin == 80 & pos == 100)) %>%
  st_union() %>%
  st_as_sf() %>%
  rename(geometry = x) %>%
  mutate(
    dpos = 20,
    lin  = 80,
    pos  = 110)
# mapView(V) +  mapView(u)
V <- st_difference(V, st_geometry(u)) %>%
  bind_rows(u)

u <- V %>%
  filter(
    (dpos == 20 & lin == 50 & pos == 80) |
      (dpos == 20 & lin == 60 & pos == 80)) %>%
  st_union() %>%
  st_as_sf() %>%
  rename(geometry = x) %>%
  mutate(
    dpos = 20,
    lin  = 55,
    pos  = 80)
# mapView(V) + mapView(u)
V <- st_difference(V, st_geometry(u)) %>%
  bind_rows(u)

u <- V %>%
  filter(
    (dpos == 20 & lin == 50 & pos == 60) |
      (dpos == 20 & lin == 60 & pos == 60)) %>%
  st_union() %>%
  st_as_sf() %>%
  rename(geometry = x) %>%
  mutate(
    dpos = 20,
    lin  = 55,
    pos  = 60)
# mapView(V) + mapView(u)
V <- st_difference(V, st_geometry(u)) %>%
  bind_rows(u)

u <- V %>%
  filter(
    (dpos == 20 & lin == 50 & pos == 40) |
      (dpos == 20 & lin == 60 & pos == 40)) %>%
  st_union() %>%
  st_as_sf() %>%
  rename(geometry = x) %>%
  mutate(
    dpos = 20,
    lin  = 55,
    pos  = 40)
# mapView(V) + mapView(u)
V <- st_difference(V, st_geometry(u)) %>%
  bind_rows(u)
# mapView(V)

V <-  V %>%
  mutate(
    sta_key = glue("{lin},{pos}")) %>%
  relocate(sta_key)

# mapView(V) +
#   mapView(iea_ca_y, col.regions = "red")

sta_keys <- read_csv(here("data-raw/cc_grid_sta-keys.csv")) %>%
  pull(sta_key)
# TODO: rm 60,45 from cc_grid_sta-keys.csv in SF Bay?
# mapView(V)
V <- V %>%
  filter(sta_key %in% sta_keys)
# mapView(V)

cc_grid <- st_difference(
  V, st_transform(lnd, st_crs("+proj=calcofi"))) %>%
  st_transform(4326) %>%
  rename(geom = geometry)
# mapView(cc_grid)

slivers <- st_difference(
  st_union(V) %>%
    st_transform(4326),
  cc_grid) %>%
  st_cast("POLYGON") %>%
  st_as_sf() %>%
  mutate(
    area = st_area(x)) %>%
  rowid_to_column("id")
# slivers$area
# mapview(slivers)

# mapview(V %>% st_transform(4326)) +
#   mapview(slivers, color="red", col.regions = "red", lwd=10)
# mapview(slivers)

# m <- mapview(st_union(V))
# mapedit()
# m <- mapview(st_union(V %>% st_transform(4326)))
# y <- mapedit::editMap(m)
# write_sf(y$all, "data-raw/cc_grid_box4slivers.geojson")
y <- read_sf("data-raw/cc_grid_box4slivers.geojson")

# get slivers
s <- st_difference(
  y,
  st_union(V %>% st_transform(4326))) %>%
  st_cast("POLYGON") %>%
  rowid_to_column("id")
# mapview(V %>% st_transform(4326)) +
#   mapview(s, color="red", col.regions = "red", lwd=10)

g_key = "93.3,110"; s_id = 5
st_geometry(cc_grid[cc_grid$sta_key == g_key,]) <- st_union(
  filter(cc_grid, sta_key == g_key) %>% pull(geom),
  filter(s, id==s_id) %>% pull(geometry))

g_key = "93.3,90"; s_id = 4
st_geometry(cc_grid[cc_grid$sta_key == g_key,]) <- st_union(
  filter(cc_grid, sta_key == g_key) %>% pull(geom),
  filter(s, id==s_id) %>% pull(geometry))

g_key = "93.3,70"; s_id = 3
st_geometry(cc_grid[cc_grid$sta_key == g_key,]) <- st_union(
  filter(cc_grid, sta_key == g_key) %>% pull(geom),
  filter(s, id==s_id) %>% pull(geometry))

g_key = "93.3,50"; s_id = 1
st_geometry(cc_grid[cc_grid$sta_key == g_key,]) <- st_union(
  filter(cc_grid, sta_key == g_key) %>% pull(geom),
  filter(s, id==s_id) %>% pull(geometry))

g_key = "93.3,30"; s_id = 2
st_geometry(cc_grid[cc_grid$sta_key == g_key,]) <- st_union(
  filter(cc_grid, sta_key == g_key) %>% pull(geom),
  filter(s, id==s_id) %>% pull(geometry))

cc_grid <- cc_grid %>%
  select(sta_key, sta_lin = lin, sta_pos = pos, sta_dpos = dpos) %>%
  as_tibble() %>% st_as_sf()

# add categories for https://github.com/CalCOFI/calcofi4r/issues/4
cc_grid <- cc_grid %>%
  mutate(
    sta_shore = case_when(
      sta_pos <= 60 ~ "nearshore",
      TRUE ~ "offshore"),
    sta_pattern = case_when(
      sta_lin >= 76.7 & sta_dpos %in% c(5,10) ~ "standard",
      sta_lin <  76.7 & sta_dpos %in% c(5,10) ~ "extended",
      TRUE ~ "historical"))
# mapview::mapView(cc_grid, zcol="sta_pattern")
# mapview::mapView(cc_grid, zcol="sta_shore")

cc_grid <- read_sf(con, "effort_grid") %>%
  mutate(
    sta_lin  = as.integer(sta_lin),
    sta_pos  = as.integer(sta_pos),
    sta_dpos = as.integer(sta_dpos))

# write to database
st_write(
  cc_grid, con, "effort_grid",
  layer_options = c(
    "OVERWRITE=yes", "LAUNDER=true"))
dbSendQuery(
  con,
  "CREATE INDEX IF NOT EXISTS effort_grid_idx ON effort_grid USING GIST (geom);")

cc_grid_ctrs <- cc_grid %>%
  mutate(
    geom = st_centroid(
      geom, of_largest_polygon = T))
# mapView(cc_grid) + mapView(cc_grid_ctrs)

st_write(
  cc_grid_ctrs, con, "effort_ctrs",
  layer_options = c(
    "OVERWRITE=yes", "LAUNDER=true"))
dbSendQuery(
  con,
  "CREATE INDEX IF NOT EXISTS effort_ctrs_idx ON effort_ctrs USING GIST (geom);")

cc_grid_zones <- read_sf(con, "effort_grid") %>%
  group_by(
    sta_pattern, sta_shore) %>%
  summarize(
    sta_dpos    = unique(sta_dpos),
    sta_lin_min = min(sta_lin),
    sta_lin_max = max(sta_lin),
    sta_pos_min = min(sta_pos),
    sta_pos_max = max(sta_pos),
    .groups = "drop") %>%
  mutate(
    zone_key = glue("{sta_pattern}-{sta_shore}")) %>%
  relocate(zone_key)
# cc_grid_zones
# mapview::mapView(cc_grid_zones, zcol="zone_key")

st_write(
  cc_grid_zones, con, "effort_zones",
  layer_options = c(
    "OVERWRITE=yes", "LAUNDER=true"))
dbSendQuery(
  con,
  "CREATE INDEX IF NOT EXISTS effort_zones_idx ON effort_zones USING GIST (geom);")

usethis::use_data(cc_grid, overwrite = TRUE)
usethis::use_data(cc_grid_ctrs, overwrite = TRUE)
usethis::use_data(cc_grid_zones, overwrite = TRUE)
