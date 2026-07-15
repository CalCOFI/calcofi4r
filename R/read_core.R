# read helpers for the consolidated core tables (obs / sample / obs_freq /
# sample_measurement / obs_ctd_full). These replace the per-dataset readers
# (cc_read_bottle/ichthyo/tow/site/measurements) with one common surface; the
# per-dataset tables survive as compatibility VIEWs, so the old readers keep
# working too. See design_env-bio-consolidation.md.

#' Read consolidated observations (`obs`)
#'
#' The common occurrence-headline surface across every dataset: one scalar per
#' row, `realm` `'env'`/`'bio'`, provenance-stamped with `dataset_key`, keyed to
#' the sampling event via `sample_key`, and carrying `grid_key`/`cruise_key`/
#' `hex_id` for rollups. Environmental CTD is represented by the thinned
#' `ctd_thin`; use [cc_read_ctd_full()] for full-resolution scans.
#'
#' @param ... `dplyr::filter()` expressions applied to `obs`.
#' @param realm optional `'env'` or `'bio'` to restrict the realm.
#' @param datasets optional vector of `dataset_key`s (e.g. `"calcofi_bottle"`).
#' @param measurement_types optional vector of `measurement_type`s.
#' @param version database version (default `"latest"`).
#' @param collect if `TRUE` (default) return a tibble; else the lazy `dplyr::tbl`.
#' @return Tibble of observations (if `collect=TRUE`) or a lazy table.
#' @export
#' @concept read
#' @examples
#' \dontrun{
#' # larval abundance per station for one species
#' cc_read_obs(realm = "bio", measurement_types = "abundance",
#'             datasets = "swfsc_ichthyo")
#' }
#' @importFrom dplyr tbl filter collect
#' @importFrom rlang enquos
cc_read_obs <- function(
  ...,
  realm = NULL,
  datasets = NULL,
  measurement_types = NULL,
  version = "latest",
  collect = TRUE
) {
  con <- cc_get_db(version = version)
  tbl <- dplyr::tbl(con, "obs")
  if (!is.null(realm))             tbl <- dplyr::filter(tbl, realm %in% !!realm)
  if (!is.null(datasets))          tbl <- dplyr::filter(tbl, dataset_key %in% !!datasets)
  if (!is.null(measurement_types)) tbl <- dplyr::filter(tbl, measurement_type %in% !!measurement_types)
  dots <- rlang::enquos(...)
  if (length(dots) > 0) tbl <- dplyr::filter(tbl, !!!dots)
  if (collect) dplyr::collect(tbl) else tbl
}

#' Read the sampling-event dimension (`sample`)
#'
#' One row per physical sampling event across every dataset, at its native grain
#' (`sample_type` = site/tow/net/cast/bottle/underway/transect/region_pool), with
#' `parent_sample_key` + `root_sample_key` encoding the `site->tow->net` and
#' `cast->bottle` hierarchies. Count sampling events with
#' `dplyr::count(sample_type)` or `count(DISTINCT root_sample_key)`.
#'
#' @param ... `dplyr::filter()` expressions applied to `sample`.
#' @param sample_types optional vector of `sample_type`s.
#' @param datasets optional vector of `dataset_key`s.
#' @param version database version (default `"latest"`).
#' @param collect if `TRUE` (default) return a tibble; else the lazy `dplyr::tbl`.
#' @return Tibble of sampling events (if `collect=TRUE`) or a lazy table.
#' @export
#' @concept read
#' @importFrom dplyr tbl filter collect
#' @importFrom rlang enquos
cc_read_sample <- function(
  ...,
  sample_types = NULL,
  datasets = NULL,
  version = "latest",
  collect = TRUE
) {
  con <- cc_get_db(version = version)
  tbl <- dplyr::tbl(con, "sample")
  if (!is.null(sample_types)) tbl <- dplyr::filter(tbl, sample_type %in% !!sample_types)
  if (!is.null(datasets))     tbl <- dplyr::filter(tbl, dataset_key %in% !!datasets)
  dots <- rlang::enquos(...)
  if (length(dots) > 0) tbl <- dplyr::filter(tbl, !!!dots)
  if (collect) dplyr::collect(tbl) else tbl
}

#' Read sub-occurrence frequency distributions (`obs_freq`)
#'
#' The `(bin, count)` distributions within a taxon-occurrence — e.g. ichthyo
#' larval `body_length` and developmental `stage` distributions — linked to
#' [cc_read_obs()] by `sample_key` + `taxon_id` + `life_stage`.
#'
#' @param ... `dplyr::filter()` expressions applied to `obs_freq`.
#' @param version database version (default `"latest"`).
#' @param collect if `TRUE` (default) return a tibble; else the lazy `dplyr::tbl`.
#' @return Tibble of frequency bins (if `collect=TRUE`) or a lazy table.
#' @export
#' @concept read
#' @importFrom dplyr tbl filter collect
#' @importFrom rlang enquos
cc_read_obs_freq <- function(..., version = "latest", collect = TRUE) {
  con <- cc_get_db(version = version)
  tbl <- dplyr::tbl(con, "obs_freq")
  dots <- rlang::enquos(...)
  if (length(dots) > 0) tbl <- dplyr::filter(tbl, !!!dots)
  if (collect) dplyr::collect(tbl) else tbl
}

#' Read event-level effort measurements (`sample_measurement`)
#'
#' Effort measurements that apply to all taxa in a sampling event — e.g. net
#' `volume_sampled` / `std_haul_factor` / `prop_sorted` / plankton biomass, and
#' bottle cast conditions — keyed by `sample_key`.
#'
#' @param ... `dplyr::filter()` expressions applied to `sample_measurement`.
#' @param version database version (default `"latest"`).
#' @param collect if `TRUE` (default) return a tibble; else the lazy `dplyr::tbl`.
#' @return Tibble of effort measurements (if `collect=TRUE`) or a lazy table.
#' @export
#' @concept read
#' @importFrom dplyr tbl filter collect
#' @importFrom rlang enquos
cc_read_sample_measurement <- function(..., version = "latest", collect = TRUE) {
  con <- cc_get_db(version = version)
  tbl <- dplyr::tbl(con, "sample_measurement")
  dots <- rlang::enquos(...)
  if (length(dots) > 0) tbl <- dplyr::filter(tbl, !!!dots)
  if (collect) dplyr::collect(tbl) else tbl
}

#' Read full-resolution CTD scans (`obs_ctd_full`, supplemental)
#'
#' The supplemental full per-scan CTD table (~216M rows), same schema as `obs`.
#' The default `obs` already carries CTD via the thinned `ctd_thin`; for
#' full-resolution work filter CTD out of `obs` and union `obs_ctd_full` — never
#' sum both (`ctd_thin` is a subset of `ctd_measurement`). Attached only when the
#' release catalog lists it; expect a large scan.
#'
#' @param ... `dplyr::filter()` expressions applied to `obs_ctd_full`.
#' @param version database version (default `"latest"`).
#' @param collect if `TRUE` (default) return a tibble; else the lazy `dplyr::tbl`.
#' @return Tibble of full-resolution CTD observations or a lazy table.
#' @export
#' @concept read
#' @importFrom dplyr tbl filter collect
#' @importFrom rlang enquos
cc_read_ctd_full <- function(..., version = "latest", collect = TRUE) {
  con <- cc_get_db(version = version)
  tbl <- dplyr::tbl(con, "obs_ctd_full")
  dots <- rlang::enquos(...)
  if (length(dots) > 0) tbl <- dplyr::filter(tbl, !!!dots)
  if (collect) dplyr::collect(tbl) else tbl
}
