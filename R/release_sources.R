#' Resolve where a release table's parquet bytes live
#'
#' Since the v2026.09 releases the database is **content-addressed**: each table
#' (or each partition of a partitioned table) is one immutable object under
#' `gs://calcofi-db/ducklake/tables/{table}/{content_hash}/…`, and a release's
#' `catalog.json` lists those objects per table in `objects[]`. A release that
#' changed three tables shares every other object with the release before it,
#' so a pinned consumer that has already fetched a table need not fetch it
#' again — and the archive can drop a retired version's own copies without
#' breaking anyone who reads through the catalog.
#'
#' This is the one place a consumer turns a catalog entry into URLs. Read the
#' catalog with [cc_catalog()] (or `jsonlite::fromJSON(simplifyVector = FALSE)`)
#' and never build a `releases/{version}/parquet/…` path by hand: that path is
#' only guaranteed to answer for the promoted and consolidated versions.
#'
#' Rules, in order:
#' 1. entry has `objects[]` → one https URL per object, in catalog order
#'    (partition files carry their `key=value` segment, so DuckDB's
#'    `hive_partitioning = true` recovers the partition column from the URL);
#' 2. otherwise (releases before v2026.09) → the legacy per-release path:
#'    `…/releases/{version}/parquet/{table}.parquet`, or an `s3://` glob for a
#'    partitioned table (DuckDB cannot glob over https).
#'
#' @param catalog a release catalog as returned by [cc_catalog()] (a list; a
#'   `jsonlite`-simplified data frame form is accepted too)
#' @param table table name
#' @param base_https https root of the bucket (default the public GCS host)
#' @return list with `urls` (character; https, or one `s3://` glob for a legacy
#'   partitioned table), `hive` (logical: pass `hive_partitioning = true`),
#'   `canonical` (logical: from `objects[]`), `hashes` (content hashes, or `NA`)
#'   and `local_paths` (relative cache paths, one per url, content-addressed so
#'   a table unchanged between releases is cached once)
#' @concept database
#' @export
#' @examples
#' \dontrun{
#' src <- cc_release_sources(cc_catalog("latest"), "obs")
#' con <- DBI::dbConnect(duckdb::duckdb())
#' DBI::dbExecute(con, paste0("CREATE VIEW obs AS SELECT * FROM ", cc_read_parquet_sql(src)))
#' }
cc_release_sources <- function(catalog, table,
                               base_https = "https://storage.googleapis.com/calcofi-db") {
  tbls <- .cc_catalog_tables(catalog)
  entry <- Filter(function(t) identical(as.character(t$name), table), tbls)
  if (!length(entry))
    stop(glue::glue("table '{table}' is not in the catalog for {catalog$version}"))
  entry <- entry[[1]]
  partitioned <- isTRUE(entry$partitioned)
  version <- as.character(catalog$version)

  if (length(entry$objects)) {
    paths  <- vapply(entry$objects, function(o) as.character(o$path), "")
    hashes <- vapply(entry$objects, function(o) as.character(o$content_hash %||% NA), "")
    return(list(
      urls        = paste0(base_https, "/", paths),
      hive        = partitioned,
      canonical   = TRUE,
      hashes      = hashes,
      # local mirror of the canonical layout: tables/{table}/[{key}={value}/]{hash}/file
      local_paths = sub("^ducklake/", "", paths)))
  }

  if (partitioned) {
    list(urls = glue::glue("s3://calcofi-db/ducklake/releases/{version}/parquet/{table}/**/*.parquet"),
         hive = TRUE, canonical = FALSE, hashes = NA_character_,
         local_paths = NA_character_)
  } else {
    list(urls = glue::glue("{base_https}/ducklake/releases/{version}/parquet/{table}.parquet"),
         hive = FALSE, canonical = FALSE, hashes = NA_character_,
         local_paths = glue::glue("releases/{version}/parquet/{table}.parquet"))
  }
}

#' The `read_parquet(...)` SQL for a resolved source
#'
#' @param src result of [cc_release_sources()]
#' @param paths override the paths to read (e.g. local downloads); default `src$urls`
#' @return a length-one character SQL fragment
#' @concept database
#' @export
cc_read_parquet_sql <- function(src, paths = src$urls) {
  paths <- as.character(paths)
  lst <- if (length(paths) == 1) glue::glue("'{paths}'") else
    glue::glue("[{paste0(\"'\", paths, \"'\", collapse = ', ')}]")
  if (isTRUE(src$hive))
    glue::glue("read_parquet({lst}, hive_partitioning = true)")
  else
    glue::glue("read_parquet({lst})")
}

#' Read a release catalog
#'
#' @param version release version (`"latest"` resolves through `latest.txt`)
#' @param base_https https root of the bucket
#' @return the catalog as a nested list (not simplified — `objects[]` stays a
#'   list of one record per object)
#' @concept database
#' @export
cc_catalog <- function(version = "latest",
                       base_https = "https://storage.googleapis.com/calcofi-db") {
  if (identical(version, "latest"))
    version <- trimws(readLines(glue::glue("{base_https}/ducklake/releases/latest.txt"), warn = FALSE)[1])
  jsonlite::fromJSON(glue::glue("{base_https}/ducklake/releases/{version}/catalog.json"),
                     simplifyVector = FALSE)
}

# a catalog's tables as a list of records, whether it was read with
# simplifyVector = TRUE (data frame + list-columns) or FALSE (nested lists)
.cc_catalog_tables <- function(catalog) {
  tbls <- catalog$tables
  if (is.data.frame(tbls)) {
    lapply(seq_len(nrow(tbls)), function(i) {
      t <- lapply(tbls, function(col) col[[i]])
      if (!is.null(t$objects)) {
        o <- t$objects
        t$objects <- if (is.data.frame(o))
          lapply(seq_len(nrow(o)), function(j) as.list(o[j, , drop = FALSE])) else NULL
      }
      t
    })
  } else {
    tbls
  }
}
