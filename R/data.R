# bottle_temp_depth ----
#' Bottle data of temperature with depth (m)
#'
#' Extended CalCOFI station bottle cast data with temperature (º Celsius) as example data frame
#' for visualization functions. Data were filtered to casts with a minimum of 50 depth readings.
#'
#' @format A data frame with 1,077 rows and 3 variables \describe{
#'   \item{cast_count}{unique identifier for bottle cast}
#'   \item{depth_m}{depth below the surface in meters}
#'   \item{v}{variable, in this case temperature (º Celsius)}
#' }
#' @source \url{https://calcofi.org/sampling-info/station-positions/} \url{https://calcofi.org/data/oceanographic-data/bottle-database/}
#' @concept data
"bottle_temp_depth"

# bottle_temp_lonlat ----
#' Bottle data of temperature in space (latitude, longitude)
#'
#' Extended CalCOFI station bottle cast with temperature (º Celsius) as example data frame
#' for visualization functions.
#'
#' @format A data frame with 9,865 rows and 3 variables \describe{
#'   \item{lon}{longitude}
#'   \item{lat}{latitude}
#'   \item{v}{variable, in this case temperature (º Celsius)}
#' }
#' @source \url{https://calcofi.org/sampling-info/station-positions/} \url{https://calcofi.org/data/oceanographic-data/bottle-database/}
#' @concept data
"bottle_temp_lonlat"

# cc_bottle ----
#' Bottle data in space and time
#'
#' CTD bottle cast data as example data frame for visualization functions.
#'
#' @format A data frame (851,493 × 10 : rows x columns) with variables:
#' \describe{
#'   \item{lon}{longitude}
#'   \item{lat}{latitude}
#'   \item{date}{date}
#'   \item{quarter}{quarter}
#'   \item{depth_m}{depth below the surface in meters}
#'   \item{sta_dpos}{difference in position, from 5 (nearshore), 10 (offshore) to 20 (outside 113 station extended area)}
#'   \item{t_degc}{temperature (º Celsius)}
#'   \item{salinity}{salinity (TODO: units)}
#'   \item{o2sat}{oxygen saturation (TODO: units)}
#' }
#' @source \url{https://calcofi.org/data/oceanographic-data/bottle-database/}
#' @concept data
"cc_bottle"

# cc_grid ----
#' CalCOFI Grid for Extracting Effort
#'
#' A grid for calculating effort by station using Voronoi diagram to fetch nearest station, additionally clipped by land (`rnaturalearthhires::states10`).
#'
#' @format A `sf` spatial feature set with
#' \describe{
#'   \item{sta_key}{station key in the form of "`lin`,`pos`"}
#'   \item{sta_lin}{alongshore line in CalCOFI coordinate system}
#'   \item{sta_pos}{offshore position in CalCOFI coordinate system}
#'   \item{sta_dpos}{difference in position, from 5 (nearshore), 10 (offshore) to 20 (outside 113 station extended area)}
#'   \item{sta_pattern}{the CalCOFI station pattern; one of: "standard", "extended" or "historical"}
#'   \item{sta_shore}{the position wrt shore; one of: "nearshore" or "offshore"}
#'   \item{geom}{station voronoi polygon with latitude and longitude in decimal degree geographic coordinates (SRID 4326)}
#' }
#' @source [Station Positions – CalCOFI](https://calcofi.org/sampling-info/station-positions)
#' @concept data
"cc_grid"

# cc_grid_ctrs ----
#' CalCOFI Grid Centroids for Extracting Effort
#'
#' A set of centroids for the grid to calculate effort by station using Voronoi diagram to fetch nearest station, additionally clipped by land (`rnaturalearthhires::states10`).
#'
#' @format A `sf` spatial feature set with
#' \describe{
#'   \item{sta_key}{station key in the form of "`lin`,`pos`"}
#'   \item{sta_lin}{alongshore line in CalCOFI coordinate system}
#'   \item{sta_pos}{offshore position in CalCOFI coordinate system}
#'   \item{sta_dpos}{difference in position, from 5 (nearshore), 10 (offshore) to 20 (outside 113 station extended area)}
#'   \item{geom}{station latitude and longitude in decimal degree geographic coordinates (SRID 4326)}
#' }
#' @source \url{https://calcofi.org/sampling-info/station-positions/}
#' @concept data
"cc_grid_ctrs"

# cc_grid_zones ----
#' CalCOFI Grid Zones
#'
#' A set of zones based on dissolving `cc_grid` for differentiating position wrt
#' the shore (`sta_shore`: "nearshore" or "offshore") and station patterns (
#' `sta_pattern`: "standard", "extended" or "historical").
#'
#' @format A `sf` spatial feature set with 6 rows × 9 columns:
#' \describe{
#'   \item{zone_key}{unique zone key of the form `"{sta_pattern}-{sta_shore}"` }
#'   \item{sta_pattern}{the CalCOFI station pattern; one of: "standard", "extended" or "historical"}
#'   \item{sta_shore}{the position wrt shore; one of: "nearshore" or "offshore"}
#'   \item{sta_dpos}{the difference in position: 5 (nearshore), 10 (offshore) or 20 (historical)}
#'   \item{sta_lin_min}{the minimum value of dissolved `sta_lin` from `cc_grid`}
#'   \item{sta_lin_max}{the maximum value of dissolved `sta_lin` from `cc_grid`}
#'   \item{sta_pos_min}{the minimum value of dissolved `sta_pos` from `cc_grid`}
#'   \item{sta_pos_max}{the maximum value of dissolved `sta_pos` from `cc_grid`}
#'   \item{geom}{geometry of dissolved zone from `cc_grid` with latitude and longitude in decimal degree geographic coordinates (EPSG:4326)}
#' }
#' @source [Station Positions – CalCOFI](https://calcofi.org/sampling-info/station-positions)
#' @concept data
"cc_grid_zones"

# cc_places ----
#' CalCOFI Places
#'
#' A set of places for commonly extracting CalCOFI data.
#'
#' There are three categories of places \[key\]:
#'
#' 1. CalCOFI
#'    - Standard Nearshore \[cc_extended-nearshore\]
#'    - Standard Offshore \[cc_extended-offshore\]
#'    - Extended Nearshore \[cc_historical-nearshore\]
#'    - Extended Offshore \[cc_historical-offshore\]
#'    - Historical Nearshore \[cc_standard-nearshore\]
#'    - Historical Offshore \[cc_standard-offshore\]
#' 1. Integrated Ecosystem Assessment
#'    - California Current \[iea_ca\]
#' 1. National Marine Sanctuary
#'    - Cordell Bank \[nms_cb\]
#'    - Channel Islands \[nms_ci\]
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
