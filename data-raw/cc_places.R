source(here::here("data-raw/db.R"))
librarian::shelf(
  # janitor, leaflet,
  geojsonsf,
  mapview,
  mregions,
  noaa-onms/onmsR,
  rnaturalearth,
  units)
devtools::load_all()

# load places and intersect with CalCOFI extended area,
#  noting % overlap

# CalCOFI areas ----

cc_areas <- calcofi4r::cc_grid_areas %>%
  mutate(
    key = recode(
      area_dpos,
      "5,10"    = "cc_core",
      "5,10,20" = "cc_extended",
      "5"       = "cc_nearshore",
      "10"      = "cc_offshore",
      .default  = "")) %>%
  filter(key != "") %>%
  select(key)

# TODO: setup lookup table with columns: key | name | description

# prep areas: cc_ext, land ----

# get CalCOFI extended study area
cc_ext <- calcofi4r::cc_grid_areas %>%
  filter(area_dpos == "5,10,20") %>%
  select(geom)
# mapView(cc_ext)

# get land
land <- rnaturalearth::ne_countries(
  country =c(
    "United States of America", "Mexico", "Canada"),
  scale = 10, returnclass = "sf") %>%
  st_union() %>%
  st_transform(4326)
# mapView(land)

# National Marine Sanctuaries ----

sanct <- onmsR::sanctuaries %>%
  select(-spatial) %>%
  filter(st_intersects(., cc_ext, sparse = FALSE)[,1]) %>%
  st_difference(land) %>%
  st_make_valid()  %>%
  mutate(
    key = recode(
      nms,
      "CINMS" = "nms_ci",
      "CBNMS" = "nms_cb",
      "GFNMS" = "nms_gf",
      "MBNMS" = "nms_mb",
      "OCNMS" = "nms_oc",
      .default  = ""))
# mapView(sanct)

sanct_x <- st_intersection(sanct, cc_ext)
sanct_y <- st_difference(sanct, cc_ext) %>%
  st_cast("POLYGON") %>%
  mutate(
    area_km2 = st_area(geom) %>%
      set_units(km^2) %>%
      drop_units()) %>%
  arrange(desc(area_km2)) %>%
  filter(area_km2 > 1)
# range(sanct_y$area_km2)
# mapView(sanct_y %>% slice(1))
# no big area pieces from differencing,
#   so all intersecting sanctuaries in extended CalCOFI area
write_csv(
  sanct %>% st_drop_geometry(),
  here("data-raw/cc_places_nms.csv"))
sanct <- sanct %>%
  select(key)

# Integrated Ecosystem Assessment - California Current ----

# https://ecowatch.noaa.gov/regions/california-current
#   The California Current Ecosystem (CCE) is a dynamic environment in the eastern North Pacific Ocean. Spanning nearly 3,000 km from southern British Columbia, Canada to Baja California, Mexico, the California Current encompasses the United States Exclusive Economic Zone, the coastal land-sea interface, and adjacent terrestrial watersheds along the West Coast.
# United States EEZ
#   http://marineregions.org/mrgid/8456

lyr_eez <- "MarineRegions:eez"
us_id <- mr_names(lyr_eez) %>%
  filter(
    geoname == "United States Exclusive Economic Zone") %>%
  pull(id)

iea_ca <- mregions::mr_features_get(
  type      = lyr_eez,
  featureID = us_id) %>%
  geojson_sf() %>%
  select(geometry) %>%
  st_cast("POLYGON") %>%
  st_as_sf() %>%
  mutate(
    ctr_lon = map_dbl(geometry, function(g){
      g %>%
      st_centroid() %>%
      st_coordinates() %>%
      .[1,"X"]})) %>%
  filter(ctr_lon < -100) %>%
  mutate(
    key = "iea_ca") %>%
  st_difference(land) %>%
  select(key, geom = geometry) %>%
  st_make_valid()
# mapView(iea_ca)

iea_ca_x <- st_intersection(iea_ca, cc_ext)
iea_ca_y <- st_difference(iea_ca, cc_ext) %>%
  st_cast("POLYGON") %>%
  mutate(
    area_km2 = st_area(geom) %>%
      set_units(km^2) %>%
      drop_units()) %>%
  arrange(desc(area_km2)) %>%
  filter(area_km2 > 1)
# mapView(iea_ca_y)
# range(iea_ca_y$area_km2)

# cc_places final ----
cc_places <- rbind(
  cc_areas,
  sanct,
  iea_ca)
cc_places %>%
  st_drop_geometry() %>%
  write_csv(here("data-raw/cc_places_keys.csv"))

# join attributes set manually
attr_csv <- here("data-raw/cc_places_attributes.csv")
stopifnot(file.exists(attr_csv))
# cc_places_0 <- cc_places
cc_places <- cc_places %>%
  left_join(
    read_csv(attr_csv),
    by = "key") %>%
  arrange(key)

# write to database
st_write(
  cc_places, con, "places",
  layer_options = c(
    "OVERWRITE=yes", "LAUNDER=true"))
dbSendQuery(
  con,
  "CREATE INDEX IF NOT EXISTS places_geom_idx ON places USING GIST (geom);")

usethis::use_data(cc_places, overwrite = TRUE)
