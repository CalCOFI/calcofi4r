# cc_datasets() — the dataset-catalog record's read side (plan 2026-09-05, WS-P2). Every release
# writes a `datasets.json` (calcofi4db::build_dataset_catalog(), calcofi4db >= 4.1.0): one record per
# integrated dataset, plus `holdings` (not yet in the database) and `reference` (cruises, stations,
# spatial layers). cc_datasets() is the one place that reads it into a data frame — a page's Access
# table, a query's Datasets category, a consumer's "dataset page ↗" link all resolve the same way:
# `https://calcofi.io/datasets/{dataset_key}/`, built from the key, never a hard-coded list.
#
# Mirrors calcofi4py.cc_datasets() (src/calcofi4py/catalog.py) byte-for-byte over the shared fixture
# `tests/testthat/fixtures/datasets_sample.json` (== calcofi4py's `tests/fixtures/datasets_sample.json`)
# — keep the two in step.

#' The `https://calcofi.io/datasets/{dataset_key}/` page URL for a dataset
#'
#' The one place a URL is built from a `dataset_key` — every consumer that names a dataset should
#' call this (or its equivalent in another language) rather than hard-code the pattern.
#'
#' @param dataset_key one or more dataset keys
#' @return character vector of page URLs, same length as `dataset_key`
#' @concept database
#' @export
#' @examples
#' cc_dataset_page_url("calcofi_bottle")
cc_dataset_page_url <- function(dataset_key) {
  sprintf("https://calcofi.io/datasets/%s/", dataset_key)
}

#' List CalCOFI datasets (the dataset-catalog record)
#'
#' Reads a release's `datasets.json` — the record every dataset page, ERDDAP `infoUrl`, and
#' `cc_cite()`'s page line are built from (`calcofi4db::build_dataset_catalog()`, calcofi4db >=
#' 4.1.0) — into a data frame, one row per record. `distributions`, `registrations`, `keywords`,
#' `tables`, `objects` and `coverage$years`/`coverage$variables` arrive as list-columns (one list
#' per row); `provider`, `category`, `attribution`, `links`, `coverage`, `status` arrive as nested
#' data frames (`jsonlite`'s `simplifyVector` behavior) — `tidyr::unnest_wider()` flattens one when
#' needed.
#'
#' A release frozen before the dataset catalog (calcofi4db < 4.1.0, before 2026-09) carries no
#' `datasets.json`; this errors naming the version rather than returning an empty table, so a
#' caller does not mistake "no catalog yet" for "no datasets".
#'
#' @param version release version (default `"latest"`)
#' @param what which table of the record to return: `"datasets"` (default, the 16+ integrated
#'   datasets), `"holdings"` (datasets known but not yet in the database) or `"reference"` (cruises,
#'   stations, spatial layers, bathymetry — not datasets, but linked the same way)
#' @param base_https https root of the bucket
#' @return a [tibble::tibble()], one row per record
#' @export
#' @concept database
#' @examples
#' \dontrun{
#' ds <- cc_datasets()
#' ds[, c("dataset_key", "dataset_name_short")]
#' ds$links$page  # the dataset page URL, straight from the record
#' cc_datasets(what = "holdings")
#' }
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
cc_datasets <- function(version = "latest", what = c("datasets", "holdings", "reference"),
                        base_https = "https://storage.googleapis.com/calcofi-db") {
  what <- match.arg(what)
  version <- .cc_resolve_version(version)
  url <- glue::glue("{base_https}/ducklake/releases/{version}/datasets.json")
  .cc_datasets_read(url, what = what, version_hint = version)
}

# internal: read one table (`datasets` | `holdings` | `reference`) of a datasets.json record
# from any jsonlite-readable location (an https URL or a local path — tests use a fixture
# file directly, so the record-reading logic and the URL-building logic are tested apart)
.cc_datasets_read <- function(url, what = c("datasets", "holdings", "reference"),
                              version_hint = url) {
  what <- match.arg(what)
  rec <- tryCatch(
    jsonlite::fromJSON(url, simplifyVector = TRUE),
    error = function(e) stop(glue::glue(
      "cc_datasets(): could not read datasets.json for {version_hint} ({url}). ",
      "Releases before calcofi4db 4.1.0 (2026-09) carry no dataset catalog."), call. = FALSE))
  if (is.null(rec[[what]]) || !length(rec[[what]]))
    return(tibble::tibble())
  tibble::as_tibble(rec[[what]])
}
