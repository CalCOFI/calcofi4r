# data-raw/cc_grid.R
# -----------------------------------------------------------------------------
# The CalCOFI grid, rebuilt from the official station positions (CalCOFI/workflows#130).
#
# Every bundled grid dataset is made here, from three inputs and no hand edits:
#
#   1. data-raw/station_positions.csv — the "Station Position (Lat/Lon), Depth, and Type" table
#      of https://calcofi.org/sampling-info/station-positions/ (113 stations), as tidied by
#      CalCOFI/workflows `libs/download_station_positions.R`, which cross-checks the page's table
#      against the linked CalCOFIStationOrder.csv and CalCOFI_113StationMap.kml. To refresh it:
#        source("../workflows/libs/download_station_positions.R")
#        download_station_positions(dir_out = tempdir(), overwrite = TRUE)   # then copy the CSV here
#      Fetched 2026-10-02.
#   2. the previous grid — `data/cc_grid.rda` and `data/cc_grid_ctrs.rda` as of calcofi4r 1.24.2
#      (git commit GRID_V1_COMMIT; built by data-raw/cc_grid_v1.R from a PostGIS table and a
#      hand-drawn sliver file, so it is read from git, never rebuilt). It supplies the outer hull
#      and the historical cells beyond the official pattern, and ships as `cc_grid_v1`.
#   3. OpenStreetMap land polygons — https://osmdata.openstreetmap.de/download/land-polygons-split-4326.zip
#      (926 MB; the copy CalCOFI/workflows `ingest_spatial.qmd` keeps under
#      cc_stage_dir()/reference/). Read once, prepared by cc_grid_land_prep() and shipped as
#      `cc_grid_land`, so step 4 runs from the package alone. Set CC_GRID_REBUILD_LAND=true (and
#      CC_OSM_LAND_ZIP) to re-read the zip.
#
#   4. cc_grid_build() -> cc_grid, cc_grid_ctrs, cc_grid_zones, and the six "CalCOFI Zones" rows
#      of cc_places.
#
# Run from the package root: Rscript data-raw/cc_grid.R

librarian::shelf(dplyr, glue, readr, sf, tibble, usethis, quiet = TRUE)
devtools::load_all(quiet = TRUE)

GRID_V1_COMMIT <- "1701c48"   # calcofi4r 1.24.2, the last commit carrying the previous grid
OSM_LAND_ZIP   <- Sys.getenv("CC_OSM_LAND_ZIP", "~/_big/calcofi/reference/land-polygons-split-4326.zip")
REBUILD_LAND   <- tolower(Sys.getenv("CC_GRID_REBUILD_LAND", "false")) == "true"

# 1. official station positions ----
cc_station_positions <- read_csv("data-raw/station_positions.csv", show_col_types = FALSE) |>
  mutate(
    grid_key    = paste0("st", station, "-ln", line),
    order_occ   = as.integer(order_occ),
    depth_est_m = as.integer(depth_est_m)) |>
  select(
    station_key, grid_key, order_occ, line, station, longitude, latitude, depth_est_m, sta_type,
    in_75, navy_ops_area)
stopifnot(
  nrow(cc_station_positions) == 113,
  !anyDuplicated(cc_station_positions$grid_key),
  all(cc_station_positions$sta_type %in% c("ROS", "SCCOOS")),
  !anyNA(cc_station_positions[c("line", "station", "longitude", "latitude")]))

# 2. the previous grid, from git ----
git_rda <- function(path, commit = GRID_V1_COMMIT) {
  tmp <- tempfile(fileext = ".rda")
  stopifnot(system2("git", c("show", glue("{commit}:{path}")), stdout = tmp) == 0)
  e <- new.env(); load(tmp, envir = e); get(ls(e)[1], envir = e)
}
g1 <- git_rda("data/cc_grid.rda")
c1 <- git_rda("data/cc_grid_ctrs.rda")
stopifnot(nrow(g1) == 218, identical(g1$sta_key, c1$sta_key), identical(g1$sta_pattern, c1$sta_pattern))
ctr_xy <- st_coordinates(c1)
cc_grid_v1 <- g1 |>
  transmute(
    # the key the releases through v2026.10.01 carry (calcofi4db::build_grid_reference() <= 4.17)
    sta_lin  = as.double(sub(",.*$", "", sta_key)),
    sta_pos  = as.double(sub("^.*,", "", sta_key)),
    grid_key = paste0("st", sta_pos, "-ln", sta_lin, ifelse(sta_pattern == "historical", "_hist", "")),
    sta_key, sta_dpos, sta_shore, sta_pattern,
    zone_key = as.character(zone_key),
    lon_ctr  = ctr_xy[, 1],
    lat_ctr  = ctr_xy[, 2]) |>
  relocate(grid_key, sta_key, sta_lin, sta_pos) |>
  st_set_geometry("geom")
stopifnot(!anyDuplicated(cc_grid_v1$grid_key), st_crs(cc_grid_v1) == st_crs(4326))

# 3. the land mask ----
if (REBUILD_LAND || !file.exists("data/cc_grid_land.rda")) {
  zip <- path.expand(OSM_LAND_ZIP)
  stopifnot("no OSM land-polygons zip: set CC_OSM_LAND_ZIP" = file.exists(zip))
  bb  <- st_bbox(cc_grid_v1)
  box <- st_as_sfc(st_bbox(c(bb["xmin"] - .5, bb["ymin"] - .5, bb["xmax"] + .5, bb["ymax"] + .5), crs = 4326))
  osm <- st_read(
    glue("/vsizip/{zip}/land-polygons-split-4326/land_polygons.shp"), wkt_filter = st_as_text(box), quiet = TRUE)
  s2  <- sf_use_s2(FALSE)
  osm <- suppressWarnings(suppressMessages(st_crop(st_make_valid(osm), box)))
  sf_use_s2(s2)
  z   <- unzip(zip, list = TRUE)
  cc_grid_land <- st_sf(
    source  = "OpenStreetMap land polygons (osmdata.openstreetmap.de), (c) OpenStreetMap contributors, ODbL",
    version = as.character(as.Date(z$Date[grepl("land_polygons\\.shp$", z$Name)][1])),
    geom    = st_union(cc_grid_land_prep(osm, near = cc_grid_v1)))   # islets < 1 km2 dropped, coast -300 m, simplified 100 m
  use_data(cc_grid_land, overwrite = TRUE, compress = "xz")
} else {
  load("data/cc_grid_land.rda")
}

# 4. the grid ----
b <- cc_grid_build(cc_station_positions, cc_grid_v1, cc_grid_land, verbose = TRUE)
cc_grid       <- b$grid
cc_grid_ctrs  <- b$ctrs
cc_grid_zones <- b$zones
stopifnot(
  "every cell is one polygon"            = all(st_geometry_type(cc_grid) == "POLYGON"),
  "every cell is valid"                  = all(st_is_valid(cc_grid)),
  "keys are unique"                      = !anyDuplicated(cc_grid$grid_key) && !anyDuplicated(cc_grid$sta_key),
  "every official station has its cell"  = all(cc_station_positions$grid_key %in% cc_grid$grid_key),
  "every generator is in its own cell"   =
    identical(cc_grid_key(st_coordinates(cc_grid_ctrs)[, 1], st_coordinates(cc_grid_ctrs)[, 2], cc_grid),
              cc_grid_ctrs$grid_key),
  "six zones"                            = nrow(cc_grid_zones) == 6)

# cc_places carries the zones as its "CalCOFI Zones" category (data-raw/cc_places.R): re-cut them
load("data/cc_places.rda")
i <- match(paste0("cc_", cc_grid_zones$zone_key), cc_places$key)
stopifnot(!anyNA(i), all(cc_places$category[i] == "CalCOFI Zones"))
st_geometry(cc_places)[i] <- st_geometry(cc_grid_zones)

use_data(cc_station_positions, overwrite = TRUE)
use_data(cc_grid_v1,           overwrite = TRUE)
use_data(cc_grid,              overwrite = TRUE)
use_data(cc_grid_ctrs,         overwrite = TRUE)
use_data(cc_grid_zones,        overwrite = TRUE)
use_data(cc_places,            overwrite = TRUE)

# the record of what the build did, for the docs and the evidence notebook
# (CalCOFI/workflows explore_grid_voronoi.qmd)
write_csv(b$seeds, "data-raw/cc_grid_seeds.csv", na = "")
write_sf(b$pockets, "data-raw/cc_grid_pockets.geojson", delete_dsn = TRUE)
cat("cc_grid:", nrow(cc_grid), "cells;",
    sum(vapply(st_geometry(cc_grid), function(g) nrow(st_coordinates(g)), 1)), "vertices\n")
print(b$report)
