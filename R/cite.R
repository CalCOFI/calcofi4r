# cc_cite() — the attribution contract's read side (plan 2026-09-03, WS-A2). Every
# release cites itself and every dataset in it carries a checked citation_main, a
# registered license and, where the source gives one, a DOI and acknowledgement
# (calcofi4db::check_dataset_citation() / release_citation(), calcofi4db R/citation.R,
# calcofi4db >= 3.30.0). cc_cite() reads the `dataset` table of a connection and a
# release's catalog.json and formats them the way someone putting CalCOFI data in a
# paper needs them. calcofi4py.cite_text()/cite_bibtex()/cite_csl() mirror this file
# byte-for-byte (tests/testthat/fixtures/cite_*, cmp'd against
# calcofi4py/tests/fixtures/cite_* in CI-adjacent review) — keep the two in step.

# The release-citation wording, mirrored from calcofi4db::release_citation()
# (R/citation.R there). calcofi4r/calcofi4py do not depend on calcofi4db, so the
# formula is duplicated here and in calcofi4py/src/calcofi4py/cite.py on purpose —
# the same precedent as cc_density_sql(). Keep all three in step when it changes.
.CC_CITE_PUBLISHER <- paste(
  "Scripps Institution of Oceanography, NOAA Fisheries, and",
  "California Department of Fish and Wildlife")
.CC_CITE_DB_SCHEMA_URL <- "https://calcofi.io/db-schema/"

# NA/NULL/empty -> "", else the trimmed first element as a string
.s0 <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  x <- as.character(x)[1]
  if (is.na(x)) "" else x
}

# Computed exactly like calcofi4db::release_citation(version, date, doi); used only
# when the catalog predates the attribution contract (2026-09-03) and carries no
# `citation` of its own (see cc_catalog()'s `catalog_legacy.json`-style releases).
.cc_cite_release_computed <- function(version, release_date = NULL, doi = NULL) {
  rd   <- .s0(release_date)
  year <- if (nzchar(rd)) format(as.Date(rd), "%Y") else substr(version, 2, 5)
  doi  <- .s0(doi)
  locator <- if (nzchar(doi)) paste0("https://doi.org/", doi) else
    sprintf("%s?v=%s", .CC_CITE_DB_SCHEMA_URL, version)
  sprintf("CalCOFI (%s). CalCOFI Integrated Database, release %s [Data set]. %s. %s",
          year, version, .CC_CITE_PUBLISHER, locator)
}

# The release entry's fields (id, citation text, title, author, year, publisher,
# doi, url), in this fixed order, plus `source` ("release" when catalog$citation
# was used as is, "computed" when it had to be derived — mirrors cc_climatology()'s
# `source` attribute).
.cc_cite_release_fields <- function(catalog) {
  version  <- .s0(catalog$version)
  has_cit  <- nzchar(.s0(catalog$citation))
  doi      <- .s0(catalog$doi)
  rd       <- .s0(catalog$release_date)
  year     <- if (nzchar(rd)) format(as.Date(rd), "%Y") else substr(version, 2, 5)
  locator  <- if (nzchar(doi)) paste0("https://doi.org/", doi) else
    sprintf("%s?v=%s", .CC_CITE_DB_SCHEMA_URL, version)
  citation <- if (has_cit) catalog$citation else
    .cc_cite_release_computed(version, catalog$release_date, doi)
  list(
    id        = paste0("calcofi_release_", gsub("[^A-Za-z0-9]+", "_", version)),
    citation  = citation,
    title     = sprintf("CalCOFI Integrated Database, release %s", version),
    author    = "CalCOFI",
    year      = year,
    publisher = .CC_CITE_PUBLISHER,
    doi       = if (nzchar(doi)) doi else NA_character_,
    url       = locator,
    source    = if (has_cit) "release" else "computed")
}

# Resolve which dataset_key values to cite, in the order to cite them: NULL ->
# every dataset in `con`'s `dataset` table, alphabetical dataset_key order; a
# character vector or a data frame carrying dataset_key -> those keys,
# de-duplicated, in first-occurrence order (so cc_cite(my_query_result) works). An
# unmatched key is an error naming it — cc_cite() never silently drops one.
.cc_cite_rows <- function(con, x) {
  all <- DBI::dbGetQuery(con, "
    SELECT dataset_key, dataset_name, citation_main, license, license_url,
           doi, acknowledgement, pi_names
    FROM dataset")
  if (is.null(x)) {
    keys <- sort(all$dataset_key)
  } else if (is.data.frame(x)) {
    if (!"dataset_key" %in% names(x))
      stop("cc_cite(): `x` is a data frame with no dataset_key column", call. = FALSE)
    keys <- unique(as.character(x$dataset_key))
  } else {
    keys <- unique(as.character(x))
  }
  unknown <- setdiff(keys, all$dataset_key)
  if (length(unknown))
    stop("cc_cite(): unknown dataset_key(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  all[match(keys, all$dataset_key), , drop = FALSE]
}

# "License: <id>" (+ " (<license_url>)" when custom); NA when license is empty
.cc_cite_license_line <- function(row) {
  lic <- .s0(row$license)
  if (!nzchar(lic)) return(NA_character_)
  if (identical(lic, "custom") && nzchar(.s0(row$license_url)))
    sprintf("License: %s (%s)", lic, row$license_url) else
    sprintf("License: %s", lic)
}

# "DOI: https://doi.org/<doi>"; NA when doi is empty
.cc_cite_doi_line <- function(row) {
  doi <- .s0(row$doi)
  if (!nzchar(doi)) return(NA_character_)
  sprintf("DOI: https://doi.org/%s", doi)
}

# "Acknowledgement: <text>"; NA when acknowledgement is empty
.cc_cite_ack_line <- function(row) {
  ack <- .s0(row$acknowledgement)
  if (!nzchar(ack)) return(NA_character_)
  sprintf("Acknowledgement: %s", ack)
}

# license + acknowledgement collapsed to one line (bibtex `note` / CSL `note`)
.cc_cite_note <- function(row) {
  paste(stats::na.omit(c(.cc_cite_license_line(row), .cc_cite_ack_line(row))), collapse = "; ")
}

# the first 4-digit year (17xx-20xx window not needed here; CalCOFI is 1949+) in a
# citation string, or NA when there is none
.cc_cite_year <- function(citation_main) {
  m <- regmatches(citation_main, regexpr("(18|19|20)[0-9]{2}", citation_main))
  if (length(m)) m else NA_character_
}

# format = "text": citation_main, then one line each for license, DOI,
# acknowledgement — whichever are present
.cc_cite_text_one <- function(row) {
  cit <- .s0(row$citation_main)
  first <- if (nzchar(cit)) cit else
    sprintf("%s [dataset].", if (nzchar(.s0(row$dataset_name))) row$dataset_name else row$dataset_key)
  lines <- c(first, .cc_cite_license_line(row), .cc_cite_doi_line(row), .cc_cite_ack_line(row))
  paste(stats::na.omit(lines), collapse = "\n")
}

# format = "bibtex": one @misc{...} block, fields padded so every `=` in the
# entry lines up (repo style); empty/NA fields are dropped, never emitted blank
.cc_bibtex_entry <- function(key, fields) {
  fields <- fields[!vapply(fields, function(v) is.null(v) || is.na(v) || !nzchar(v), TRUE)]
  w <- max(nchar(names(fields)))
  body <- paste(sprintf("  %-*s = {%s}", w, names(fields), unlist(fields, use.names = FALSE)),
                collapse = ",\n")
  sprintf("@misc{%s,\n%s\n}", key, body)
}

.cc_cite_bibtex_release <- function(rel) {
  .cc_bibtex_entry(rel$id, list(
    title = rel$title, author = rel$author, year = rel$year,
    publisher = rel$publisher, doi = rel$doi, url = rel$url))
}

.cc_cite_bibtex_one <- function(row) {
  cit  <- .s0(row$citation_main)
  ttl  <- if (nzchar(.s0(row$dataset_name))) row$dataset_name else row$dataset_key
  doi  <- .s0(row$doi)
  note <- .cc_cite_note(row)
  .cc_bibtex_entry(row$dataset_key, list(
    title = ttl, howpublished = cit, year = .cc_cite_year(cit),
    doi = if (nzchar(doi)) doi else NA_character_,
    url = if (nzchar(doi)) paste0("https://doi.org/", doi) else NA_character_,
    note = if (nzchar(note)) note else NA_character_))
}

# format = "csl": one CSL-JSON item (type "dataset") per entry; empty/NA fields
# are omitted from the list rather than written as `null`
.cc_cite_csl_author <- function(pi_names, fallback = "CalCOFI") {
  nm <- .s0(pi_names)
  people <- if (nzchar(nm)) trimws(strsplit(nm, ";")[[1]]) else fallback
  lapply(people, function(p) list(literal = p))
}

.cc_cite_csl_release <- function(rel) {
  item <- list(
    id = rel$id, type = "dataset", title = rel$title,
    author = list(list(literal = rel$author)),
    issued = list(`date-parts` = list(list(as.integer(rel$year)))),
    publisher = rel$publisher)
  if (!is.na(rel$doi)) item$DOI <- rel$doi
  item$URL <- rel$url
  item
}

.cc_cite_csl_one <- function(row) {
  cit  <- .s0(row$citation_main)
  yr   <- .cc_cite_year(cit)
  ttl  <- if (nzchar(.s0(row$dataset_name))) row$dataset_name else row$dataset_key
  doi  <- .s0(row$doi)
  note <- .cc_cite_note(row)
  item <- list(id = row$dataset_key, type = "dataset", title = ttl,
              author = .cc_cite_csl_author(row$pi_names))
  if (!is.na(yr)) item$issued <- list(`date-parts` = list(list(as.integer(yr))))
  if (nzchar(doi)) { item$DOI <- doi; item$URL <- paste0("https://doi.org/", doi) }
  if (nzchar(note)) item$note <- note
  item
}

#' Cite CalCOFI data
#'
#' Every CalCOFI release cites itself (`catalog.json`'s `citation`, set by
#' `calcofi4db::add_release_citation()`) and every dataset in it carries a
#' checked `citation_main`, a registered `license` and, where the source gives
#' one, a `doi` and `acknowledgement` (`calcofi4db::check_dataset_citation()`,
#' calcofi4db >= 3.30.0, the attribution contract). `cc_cite()` is the one place
#' that formats them for a paper, a data-management plan or a `.bib` file — read
#' the `dataset` table off `con`, do not build a citation string by hand.
#'
#' Every call returns the **release citation first**, then one entry per
#' dataset. With `x = NULL` (default) that is every dataset in the release,
#' alphabetical by `dataset_key`; a character vector of `dataset_key` or a data
#' frame/tibble carrying a `dataset_key` column (so `cc_cite(cc_read_obs(...))`
#' works directly on a query result) cites just those, de-duplicated, in the
#' order given. A `dataset_key` that does not exist in the release is an error
#' naming it.
#'
#' Each dataset entry always carries its `citation_main`; `format = "text"`
#' appends a `License: <id>` line (plus the URL, for a `custom` license), a
#' `DOI:` line when the dataset has one, and an `Acknowledgement:` line when the
#' source requires one. `format = "bibtex"` and `format = "csl"` fold license
#' and acknowledgement into one `note`/`note` field instead, since neither
#' format has a natural place for more than one.
#'
#' `format = "bibtex"` builds every `@misc{...}` entry **offline**, from the
#' fields already on `dataset` and in the catalog — nothing here calls the
#' network by default. `resolve = TRUE` instead fetches
#' `https://doi.org/<doi>` with `Accept: application/x-bibtex` for any entry
#' that has a DOI (falling back to the offline entry for one that does not, or
#' if the fetch fails), which is closer to what many reference managers expect
#' but is slower and requires a live connection.
#'
#' A release frozen before the attribution contract (2026-09-03) carries no
#' `citation` in its catalog; `cc_cite()` computes the same wording
#' `calcofi4db::release_citation()` would have written (`source = "computed"`
#' on the result's `source` attribute, mirroring [cc_climatology()]'s
#' `source`), rather than erroring or citing nothing.
#'
#' The **software** itself is cited separately — `citation("calcofi4r")` (from
#' `DESCRIPTION`'s `Authors@R`) for R, `calcofi4py.__citation__` for Python;
#' `cc_cite()` is for the *data*.
#'
#' @param x `NULL` (every dataset), a character vector of `dataset_key`, or a
#'   data frame/tibble carrying a `dataset_key` column.
#' @param version release version (default `"latest"`). Only consulted for the
#'   release-level citation (`cc_catalog(version)`) — with `con` supplied,
#'   pass the version `con` was opened on if it is not `"latest"`, the same
#'   caveat as [cc_describe_table()].
#' @param format `"text"` (a character vector, release citation first),
#'   `"bibtex"` (one string, every `@misc{...}` entry concatenated) or `"csl"`
#'   (a list of CSL-JSON items, one per entry).
#' @param con optional open connection from [cc_get_db()]; when given it is
#'   used as is (no new connection).
#' @param resolve `format = "bibtex"` only: fetch the DOI's own BibTeX from
#'   `doi.org` for any entry with a DOI, instead of building it offline
#'   (default `FALSE`).
#' @return See `format`. The result carries a `source` attribute
#'   (`"release"` or `"computed"`) describing where the release-level citation
#'   came from.
#' @export
#' @concept database
#' @examples
#' \dontrun{
#' cc_cite()
#' cc_cite("calcofi_dic")
#' cc_cite(format = "bibtex") |> cat()
#' cc_cite(cc_read_obs(datasets = "calcofi_dic"))
#' }
#' @importFrom DBI dbGetQuery
cc_cite <- function(x = NULL, version = "latest", format = c("text", "bibtex", "csl"),
                    con = NULL, resolve = FALSE) {
  format <- match.arg(format)
  if (is.null(con)) {
    version <- .cc_resolve_version(version)
    con <- cc_get_db(version = version)
  }
  catalog <- cc_catalog(version)
  rel  <- .cc_cite_release_fields(catalog)
  rows <- .cc_cite_rows(con, x)

  if (format == "text") {
    out <- c(rel$citation, vapply(seq_len(nrow(rows)), function(i) .cc_cite_text_one(rows[i, ]), ""))
    attr(out, "source") <- rel$source
    return(out)
  }

  if (format == "bibtex") {
    ds_entries <- vapply(seq_len(nrow(rows)), function(i) {
      row <- rows[i, ]
      if (isTRUE(resolve) && nzchar(.s0(row$doi))) {
        got <- tryCatch(.cc_cite_doi_bibtex(row$doi), error = function(e) NA_character_)
        if (!is.na(got) && nzchar(got)) return(got)
      }
      .cc_cite_bibtex_one(row)
    }, "")
    out <- paste(c(.cc_cite_bibtex_release(rel), ds_entries), collapse = "\n\n")
    attr(out, "source") <- rel$source
    return(out)
  }

  # csl
  out <- c(list(.cc_cite_csl_release(rel)),
          lapply(seq_len(nrow(rows)), function(i) .cc_cite_csl_one(rows[i, ])))
  attr(out, "source") <- rel$source
  out
}

# format = "bibtex", resolve = TRUE only: doi.org content negotiation. Never
# called by default — cc_cite()'s default path is offline.
.cc_cite_doi_bibtex <- function(doi) {
  if (!requireNamespace("curl", quietly = TRUE)) return(NA_character_)
  h <- curl::new_handle(followlocation = TRUE, timeout = 15, connecttimeout = 10)
  curl::handle_setheaders(h, Accept = "application/x-bibtex")
  r <- curl::curl_fetch_memory(sprintf("https://doi.org/%s", doi), handle = h)
  if (r$status_code != 200) return(NA_character_)
  out <- rawToChar(r$content)
  Encoding(out) <- "UTF-8"
  trimws(out)
}
