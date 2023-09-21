# TODO: setup lookup table with columns: key | name | description

source(here::here("data-raw/db.R"))
librarian::shelf(
  # janitor, leaflet,
  geojsonsf,
  glue,
  mapview,
  mregions,
  noaa-onms/onmsR,
  rnaturalearth,
  units)
devtools::load_all()

# load places and intersect with CalCOFI extended area,
#  noting % overlap

# CalCOFI zones ----
cc_zones <- calcofi4r::cc_grid_zones |>
  mutate(
    category = "CalCOFI Zones",
    key      = glue("cc_{zone_key}"),
    name     = glue("{str_to_title(sta_pattern)} {str_to_title(sta_shore)}")) |>
  select(category, key, name)

# prep areas: cc_all, land ----

# get CalCOFI all zones
cc_all <- summarize(cc_zones)
# mapView(cc_all)

# get land
land <- rnaturalearth::ne_countries(
  country =c(
    "United States of America", "Mexico", "Canada"),
  scale = 10, returnclass = "sf") %>%
  st_union() %>%
  st_transform(4326)
# mapView(land)

# nms: National Marine Sanctuaries ----
sanct <- onmsR::sanctuaries |>
  rename(name = sanctuary) |>
  select(-spatial) %>%
  filter(st_intersects(., cc_all, sparse = FALSE)[,1]) |>
  st_difference(land) |>
  st_make_valid() |>
  mutate(
    key      = recode(
      nms,
      "CINMS" = "nms_ci",
      "CBNMS" = "nms_cb",
      "GFNMS" = "nms_gf",
      "MBNMS" = "nms_mb",
      "OCNMS" = "nms_oc",
      .default  = "")) |>
  select(key, name) |>
  arrange(name)
# mapView(sanct)

# * add Chumash Proposed Action ----
chumash_shp <- "/Users/bbest/My Drive/projects/calcofi/data/ONMS-Sanctuaries/Chumash_PropAction_Bndy_09082022/Chumash_ProposedAction_09082022.shp"
chumash <- read_sf(chumash_shp) |>
  mutate(
    name     = "Chumash Proposed Action",
    key      = "nms_cp") |>
  select(key, name) |>
  st_transform(4326)
st_geometry(chumash) <- "geom"

sanct <- rbind(
  sanct,
  chumash) |>
  mutate(
    category = "National Marine Sanctuaries") |>
  select(category, key, name) |>
  arrange(name)

# iea: Integrated Ecosystem Assessment - California Current ----

# https://ecowatch.noaa.gov/regions/california-current
#   The California Current Ecosystem (CCE) is a dynamic environment in the eastern North Pacific Ocean. Spanning nearly 3,000 km from southern British Columbia, Canada to Baja California, Mexico, the California Current encompasses the United States Exclusive Economic Zone, the coastal land-sea interface, and adjacent terrestrial watersheds along the West Coast.
# United States EEZ
#   http://marineregions.org/mrgid/8456

# * OLD: no longer works ----
# lyr_eez <- "MarineRegions:eez"
# us_id <- mr_names(lyr_eez) %>%
#   filter(
#     geoname == "United States Exclusive Economic Zone") %>%
#   pull(id)
# us_eez <- mregions::mr_features_get(
#   type      = lyr_eez,
#   featureID = us_id) %>%
#   geojson_sf() %>%
#   select(geometry)

# * NEW: working geoserver MarineRegions.org polygon ----
#   - search: [Marine Regions](https://marineregions.org/gazetteer.php?p=search)
#   - find:   [Marine Regions - United States Exclusive Economic Zone (EEZ)](https://marineregions.org/gazetteer.php?p=details&id=8456)
mr_id <- 8456
us_eez <- read_sf(glue(
  "https://geo.vliz.be/geoserver/MarineRegions/wfs?service=WFS&version=1.0.0&request=GetFeature&typeNames=eez&cql_filter=mrgid={mr_id}&outputFormat=application/json"))
# mapView(us_eez)

iea_ca <- us_eez |>
  st_cast("POLYGON") |>
  st_as_sf() |>
  mutate(
    ctr_lon = map_dbl(geometry, function(g){
      g %>%
      st_centroid() %>%
      st_coordinates() %>%
      .[1,"X"]})) %>%
  filter(ctr_lon < -100) %>%
  mutate(
    category = "Integrated Ecosystem Assessment",
    key      = "iea_ca",
    name     = "California Current") |>
  st_difference(land) |>
  select(category, key, name, geom = geometry) |>
  st_make_valid()
# mapView(iea_ca)


# iea_ca_x <- st_intersection(iea_ca, cc_all)
# iea_ca_y <- st_difference(iea_ca, cc_all) %>%
#   st_cast("POLYGON") %>%
#   mutate(
#     area_km2 = st_area(geom) %>%
#       set_units(km^2) %>%
#       drop_units()) %>%
#   arrange(desc(area_km2)) %>%
#   filter(area_km2 > 1)
# mapView(iea_ca_y)
# range(iea_ca_y$area_km2)

# boem_wpa: BOEM Wind Planning Areas ----
wpa_gpkg <- "/Users/bbest/My Drive/projects/calcofi/data/BOEM-WindPlanningAreas/BOEMWindLayers_4Download.gdb"

# sf::st_layers(wpa_gpkg)
# Driver: OpenFileGDB
# Available layers:
#   layer_name geometry_type features fields crs_name
# 1     BOEM_MHKLeasesandPlanningAreas Multi Polygon       17     21   WGS 84
# 2         Wind_Lease_Outlines_2_2023 Multi Polygon       34     16   WGS 84
# 3            BOEM_Wind_Leases_2_2023 Multi Polygon     3858     18   WGS 84
# 4 Wind_Planning_Area_Outlines_2_2023 Multi Polygon       42      8   WGS 84
# 5    BOEM_Wind_Planning_Areas_2_2023 Multi Polygon     9833     11   WGS 84

wpa <- st_read(wpa_gpkg, "Wind_Planning_Area_Outlines_2_2023") |>
  st_make_valid() %>%
  filter(st_intersects(., cc_all, sparse = FALSE)[,1])

# colnames(wpa)
# [1] "PROTRACTION_NUMBER"     "ADDITIONAL_INFORMATION" "CATEGORY1"
# [4] "CATEGORY2"              "URL1"                   "URL2"
# [7] "Shape_Length"           "Shape_Area"             "Shape"
# st_drop_geometry(wpa) |> View()

wpa <- wpa |>
  mutate(
    category = "BOEM Wind Planning Areas",
    key      = glue("boem-wpa_{PROTRACTION_NUMBER}")) |>
  select(
    category,
    key,
    name = ADDITIONAL_INFORMATION,
    geom = Shape)
# mapview(wpa)

# noaa_aoa: NOAA Aquaculture Opportunity Areas ----
aoa_shp <- "/Users/bbest/My Drive/projects/calcofi/data/aquaculture_NOAA-SoCal-AOAs/AOA_SOCAL_Final_Options.shp"
aoa <- read_sf(aoa_shp)

# colnames(aoa)
# [1] "Shape_Leng" "Shape_Area" "StudyArea"  "Option"     "Acres"      "geometry"
# st_drop_geometry(aoa) |> View()

aoa <- aoa |>
  mutate(
    category = "NOAA Aquaculture Opportunity Areas",
    key      = glue("noaa-aoa_{Option}"),
    name     = glue("{StudyArea}: {Option}")) |>
  select(category, key, name, geom = geometry) |>
  st_transform(4326)

# cc_places final ----
cc_places <- rbind(
  cc_zones,
  sanct,
  iea_ca,
  wpa,
  aoa) %>%
  arrange(category, name, key)

# write to database
con <- calcofi4r::cc_db_connect()
st_write(
  cc_places, con, "places",
  layer_options = c(
    "OVERWRITE=yes", "LAUNDER=true"))
create_index(con, "places", "geom", is_geom = T)
create_index(con, "places", "key", is_unique = T)

usethis::use_data(cc_places, overwrite = TRUE)

# output for R/data.R
for (k in unique(cc_places$category)){
  message(glue("#' 1. {k}"))
  d_k <- filter(cc_places, category == k)
  for (i in 1:nrow(d_k)){ # r == rows[1]
    message(glue("#'    - {d_k$name[i]} \\[{d_k$key[i]}\\]"))
  }
}
#' 1. BOEM Wind Planning Areas
#'    - California Call Area - Diablo Canyon \[boem-wpa_NI10-03\]
#'    - California Call Area - Morro Bay \[boem-wpa_NI10-01\]
#'    - Oregon Call Area - Brookings \[boem-wpa_NK10-04\]
#'    - Oregon Call Area - Coos Bay \[boem-wpa_NK10-01\]
#' 1. CalCOFI Zones
#'    - Extended Nearshore \[cc_nearshore-extended\]
#'    - Extended Offshore \[cc_offshore-extended\]
#'    - Historical Nearshore \[cc_nearshore-historical\]
#'    - Historical Offshore \[cc_offshore-historical\]
#'    - Standard Nearshore \[cc_nearshore-standard\]
#'    - Standard Offshore \[cc_offshore-standard\]
#' 1. Integrated Ecosystem Assessment
#'    - California Current \[iea_ca\]
#' 1. NOAA Aquaculture Opportunity Areas
#'    - Central North: CN1-A \[noaa-aoa_CN1-A\]
#'    - Central North: CN1-B \[noaa-aoa_CN1-B\]
#'    - North: N1-A \[noaa-aoa_N1-A\]
#'    - North: N1-B \[noaa-aoa_N1-B\]
#'    - North: N1-C \[noaa-aoa_N1-C\]
#'    - North: N2-A \[noaa-aoa_N2-A\]
#'    - North: N2-B \[noaa-aoa_N2-B\]
#'    - North: N2-C \[noaa-aoa_N2-C\]
#'    - North: N2-D \[noaa-aoa_N2-D\]
#'    - North: N2-E \[noaa-aoa_N2-E\]
#' 1. National Marine Sanctuaries
#'    - Channel Islands \[nms_ci\]
#'    - Chumash Proposed Action \[nms_cp\]
#'    - Cordell Bank \[nms_cb\]
#'    - Greater Farallones \[nms_gf\]
#'    - Monterey Bay \[nms_mb\]
#'    - Olympic Coast \[nms_oc\]
