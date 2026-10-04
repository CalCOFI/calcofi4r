# bottle_temp_depth ----
#' Bottle data of temperature with depth (m)
#'
#' Extended CalCOFI station bottle cast data with temperature (Celsius) as example data frame
#' for visualization functions. Data were filtered to casts with a minimum of 50 depth readings.
#'
#' @format A data frame with 1,077 rows and 3 variables \describe{
#'   \item{cast_count}{unique identifier for bottle cast}
#'   \item{depth_m}{depth below the surface in meters}
#'   \item{v}{variable, in this case temperature (Celsius)}
#' }
#' @source \url{https://calcofi.org/sampling-info/station-positions/} \url{https://calcofi.org/data/oceanographic-data/bottle-database/}
#' @concept data
"bottle_temp_depth"

# bottle_temp_lonlat ----
#' Bottle data of temperature in space (latitude, longitude)
#'
#' Extended CalCOFI station bottle cast with temperature (Celsius) as example data frame
#' for visualization functions.
#'
#' @format A data frame with 9,865 rows and 3 variables \describe{
#'   \item{lon}{longitude}
#'   \item{lat}{latitude}
#'   \item{v}{variable, in this case temperature (Celsius)}
#' }
#' @source \url{https://calcofi.org/sampling-info/station-positions/} \url{https://calcofi.org/data/oceanographic-data/bottle-database/}
#' @concept data
"bottle_temp_lonlat"

# cc_bottle ----
#' Bottle data in space and time
#'
#' CTD bottle cast data as example data frame for visualization functions.
#'
#' @format A data frame (851,493: rows x columns) with variables:
#' \describe{
#'   \item{lon}{longitude}
#'   \item{lat}{latitude}
#'   \item{date}{date}
#'   \item{quarter}{quarter}
#'   \item{depth_m}{depth below the surface in meters}
#'   \item{sta_dpos}{difference in position, from 5 (nearshore), 10 (offshore) to 20 (outside 113 station extended area)}
#'   \item{t_degc}{temperature (Celsius)}
#'   \item{salinity}{salinity (TODO: units)}
#'   \item{o2sat}{oxygen saturation (TODO: units)}
#' }
#' @source \url{https://calcofi.org/data/oceanographic-data/bottle-database/}
#' @concept data
"cc_bottle"

# cc_grid ----
#' CalCOFI grid: one cell per official station
#'
#' The cells of the CalCOFI grid: the Voronoi tessellation of the 113 official station positions
#' ([cc_station_positions]), clipped to the outer hull of the previous grid ([cc_grid_v1]) and to
#' the coastline ([cc_grid_land]), plus the previous grid's cells beyond 20 nautical miles of the
#' official pattern, so the extended historical grid keeps its cells and keys. Every cell is one
#' polygon holding its own station, and the release's `grid` table is these cells
#' (`calcofi4db::build_grid_reference()`). Built by [cc_grid_build()] in `data-raw/cc_grid.R`;
#' the rules are documented there.
#'
#' Before this grid, cells were Voronoi polygons of an idealized lattice and an inshore cell held
#' several real stations ([cc_grid_v1]). **84 of the 113 station cells reuse a key whose polygon
#' changed**, so never map a key between the two grids by name: use the release's `grid_crosswalk`
#' table (`calcofi4db::build_grid_crosswalk()`), and [cc_grid_key()] to key a position.
#'
#' @format An `sf` of 225 polygons (EPSG:4326) with
#' \describe{
#'   \item{grid_key}{the release key, `st{station}-ln{line}`, with `_hist` for a cell of the
#'     historical pattern (e.g. `st26.7-ln93.3`, `st120-ln110_hist`)}
#'   \item{sta_key}{station key in the form "`line`,`station`" (e.g. `"93.3,26.7"`)}
#'   \item{sta_lin}{line (alongshore) in the CalCOFI coordinate system (double: `76.7`, `93.4`)}
#'   \item{sta_pos}{station (offshore position) in the CalCOFI coordinate system (double: `26.7`)}
#'   \item{sta_dpos}{nominal spacing class in station units: 5 (nearshore), 10 (offshore) or 20
#'     (historical pattern)}
#'   \item{sta_shore}{"nearshore" (station 60 or less) or "offshore"}
#'   \item{sta_pattern}{"standard" (lines 76.7 and south: the 75-station pattern), "extended"
#'     (lines north of 76.7) or "historical" (kept from the previous grid)}
#'   \item{zone_key}{`"{sta_shore}-{sta_pattern}"`, the key of [cc_grid_zones]}
#'   \item{sta_type}{the station's type on the official list: "ROS" (rosette) or "SCCOOS" (the
#'     nine ~20 m inshore stations); `NA` for a cell kept from the previous grid}
#'   \item{sta_source}{"official" (a station of the official list) or "previous" (a cell kept
#'     from [cc_grid_v1])}
#'   \item{geom}{the cell, one polygon (EPSG:4326); edges are straight in longitude/latitude}
#' }
#' @source [Station Positions - CalCOFI](https://calcofi.org/sampling-info/station-positions);
#'   coastline (c) OpenStreetMap contributors (ODbL)
#' @concept data
"cc_grid"

# cc_grid_ctrs ----
#' CalCOFI grid sites: the station of each cell
#'
#' The site of each [cc_grid] cell, i.e. the generator its cell was built from: the official
#' station position for a station cell (on its line by construction), and for a cell kept from the
#' previous grid its labelled (line, station) under `+proj=calcofi`, or the previous cell's centre
#' where that label is on land. The release's `grid.geom_ctr`.
#'
#' @format An `sf` of 225 points (EPSG:4326) with the columns of [cc_grid] and
#' \describe{
#'   \item{geom}{the site (EPSG:4326)}
#' }
#' @source [Station Positions - CalCOFI](https://calcofi.org/sampling-info/station-positions)
#' @concept data
"cc_grid_ctrs"

# cc_grid_zones ----
#' CalCOFI Grid Zones
#'
#' The six zones [cc_grid] dissolves into by position relative to shore (`sta_shore`: "nearshore"
#' or "offshore") and station pattern (`sta_pattern`: "standard", "extended" or "historical");
#' [cc_grid_zones_build()] of [cc_grid].
#'
#' @format An `sf` of 6 rows x 9 columns:
#' \describe{
#'   \item{zone_key}{unique zone key of the form `"{sta_shore}-{sta_pattern}"`}
#'   \item{sta_pattern}{the CalCOFI station pattern; one of: "standard", "extended" or "historical"}
#'   \item{sta_shore}{the position wrt shore; one of: "nearshore" or "offshore"}
#'   \item{sta_dpos}{the spacing class: 5 (nearshore), 10 (offshore) or 20 (historical)}
#'   \item{sta_lin_min}{the minimum `sta_lin` of the zone's cells}
#'   \item{sta_lin_max}{the maximum `sta_lin` of the zone's cells}
#'   \item{sta_pos_min}{the minimum `sta_pos` of the zone's cells}
#'   \item{sta_pos_max}{the maximum `sta_pos` of the zone's cells}
#'   \item{geom}{the dissolved zone (EPSG:4326)}
#' }
#' @source [Station Positions - CalCOFI](https://calcofi.org/sampling-info/station-positions)
#' @concept data
"cc_grid_zones"

# cc_grid_v1 ----
#' The previous CalCOFI grid (through calcofi4r 1.24 and release v2026.10.01)
#'
#' The grid as `cc_grid` shipped it through calcofi4r 1.24.2 and as every database release through
#' v2026.10.01 carries it in `grid`: Voronoi polygons of an idealized lattice (5, 10 and 20 station
#' units) in `+proj=calcofi` coordinates, clipped by Natural Earth land. Kept because the current
#' [cc_grid] is built inside its outer hull and keeps its cells beyond the official pattern, and
#' because the release's `grid_crosswalk` is the overlap of the two.
#'
#' @format An `sf` of 218 polygons and multipolygons (EPSG:4326) with
#' \describe{
#'   \item{grid_key}{the release key through v2026.10.01 (`st{station}-ln{line}`, `_hist` for the
#'     historical pattern)}
#'   \item{sta_key}{"`line`,`station`"; not unique (`"90,120"` is a standard and a historical cell)}
#'   \item{sta_lin, sta_pos}{line and station of the cell's label}
#'   \item{sta_dpos, sta_shore, sta_pattern, zone_key}{as in [cc_grid]}
#'   \item{lon_ctr, lat_ctr}{the cell's centre as released (`grid.geom_ctr`): the centroid of its
#'     largest polygon}
#'   \item{geom}{the cell (EPSG:4326)}
#' }
#' @source calcofi4r 1.24.2 `data/cc_grid.rda` and `data/cc_grid_ctrs.rda` (`data-raw/cc_grid_v1.R`)
#' @concept data
"cc_grid_v1"

# cc_grid_land ----
#' The land mask the CalCOFI grid is clipped with
#'
#' OpenStreetMap land polygons around the grid, prepared by [cc_grid_land_prep()]: islets smaller
#' than 1 km2 dropped, the coastline pulled back 300 m (so a ship at its berth or a nearshore
#' position rounded to the minute still falls in a cell) and simplified to 100 m. It is the `land`
#' argument of [cc_grid_build()], bundled so the grid can be rebuilt, and a variant built, from
#' the package alone. It is not a coastline to draw.
#'
#' @format An `sf` of one multipolygon (EPSG:4326) with
#' \describe{
#'   \item{source}{the source and its licence}
#'   \item{version}{the date of the OpenStreetMap extract}
#'   \item{geom}{the mask}
#' }
#' @source <https://osmdata.openstreetmap.de/data/land-polygons.html>, (c) OpenStreetMap
#'   contributors, [ODbL](https://opendatacommons.org/licenses/odbl/)
#' @concept data
"cc_grid_land"

# cc_station_positions ----
#' Official CalCOFI station positions
#'
#' The 113 stations of the "Station Position (Lat/Lon), Depth, and Type" table on
#' <https://calcofi.org/sampling-info/station-positions/> (fetched 2026-10-02): the generators of
#' the [cc_grid] cells.
#'
#' @format A tibble of 113 rows with
#' \describe{
#'   \item{station_key}{`"LLL.L SSS.S"`, the form of the database's `site_key` (e.g. `"093.3 026.7"`)}
#'   \item{grid_key}{the key of the station's cell in [cc_grid]}
#'   \item{order_occ}{order of occupation on a cruise}
#'   \item{line, station}{line and station in the CalCOFI coordinate system}
#'   \item{longitude, latitude}{the listed position, decimal degrees (WGS 84)}
#'   \item{depth_est_m}{estimated bottom depth, m}
#'   \item{sta_type}{"ROS" (rosette) or "SCCOOS" (the nine ~20 m inshore stations)}
#'   \item{in_75}{in the 75-station pattern (lines 76.7 to 93.4), the standard pattern since 1984}
#'   \item{navy_ops_area}{the operations area, for a station Navy operations may close}
#' }
#' @source <https://calcofi.org/sampling-info/station-positions/>
#' @concept data
"cc_station_positions"

# cc_places ----
#' CalCOFI Places
#'
#' A set of places for commonly extracting CalCOFI data.
#'
#' Here are the categories and names \[key\]:
#'
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
#'
#' @format A `sf` spatial feature set with
#' \describe{
#'   \item{key}{character key uniquely identifying the record}
#'   \item{category}{character key}
#'   \item{name}{name of the place, given the category}
#'   \item{geom}{polygon geometry in geographic coordinates (SRID 4326)}
#' }
#' @concept data
"cc_places"

# stations ----
#' Oceanographic stations
#'
#' The geographic locations of every bottle sampling station utilized on a CalCOFI
#' cruise. This data set is an extraction and modification of the CalCOFI cast table.
#'
#' @format A data frame with 2634 rows and 11 variables
#' \describe{
#'   \item{sta_id}{Station ID}
#'   \item{sta_id_line}{Line component of the Station ID}
#'   \item{sta_id_station}{Station component of the Station ID}
#'   \item{lon}{Station longitude in decimal degrees}
#'   \item{lat}{Station latitude in decimal degrees}
#'   \item{is_offshore}{`Sta_ID_station` > 60}
#'   \item{is_cce}{In the California Coastal Ecosystem (CCE) set of stations}
#'   \item{is_ccelter}{In the California Coastal Ecosystem (CCE) Long-Term Ecological Research (LTER) set of stations}
#'   \item{is_sccoos}{In the Southern California Coastal Ocean Observing (SCOOS) set of stations}
#'   \item{geometry}{Station latitude and longitude as a geographic projection (SRID 4326)}
#' }
#' @source \url{https://calcofi.org/data/oceanographic-data/bottle-database/}
#' @concept data
"stations"
