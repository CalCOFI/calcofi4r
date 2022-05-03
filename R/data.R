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
#'   \item{geometry}{Station latitude and longitude as a geographic projection (SIRD 4326)}
#' }
#' @source \url{https://calcofi.org/data/oceanographic-data/bottle-database/}
#' @concept data
"stations"
