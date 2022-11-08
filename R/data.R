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
#'   \item{geom}{station voronoi polygon with latitude and longitude in decimal degree geographic coordinates (SRID 4326)}
#' }
#' @source [Station Positions – CalCOFI](https://calcofi.org/sampling-info/station-positions)
#' @concept data
"cc_grid"

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

#' CalCOFI Grid Study Areas
#'
#' A set of study areas based on dissolving `cc_grid` for differentiating
#' various combinations of 5 (nearshore), 10 (offshore) to 20 (outside 113 station extended area).
#' For instance, to extract the study area for:
#' - only nearshore grid cells, use
#'   `dplyr::filter(cc_grid_areas, area_dpos="5")`;
#' - all grid cells, use
#'   `dplyr::filter(cc_grid_areas, area_dpos="5,10,20")`;
#' - all commonly sampled stations (113 station extended area), use
#'   `dplyr::filter(cc_grid_areas, area_dpos="5,10")`.
#'
#' @format A `sf` spatial feature set with
#' \describe{
#'   \item{area_dpos}{character identifier for combinations of difference in position, from 5 (nearshore), 10 (offshore) to 20 (outside 113 station extended area); one of: 5; 10; 20; 5,10; 10,20; 5,10,20}
#'   \item{geom}{dissolved study area from `cc_grid` with latitude and longitude in decimal degree geographic coordinates (SRID 4326)}
#' }
#' @source [Station Positions – CalCOFI](https://calcofi.org/sampling-info/station-positions)
#' @concept data
"cc_grid_areas"

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
