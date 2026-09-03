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
#'   `canonical` (logical: from `objects[]`), `hashes` (content hashes, or `NA`),
#'   `local_paths` (relative cache paths, one per url, content-addressed so a
#'   table unchanged between releases is cached once) and `single_file` (the
#'   whole-table file a partitioned table may also publish — `obs` does — for
#'   https-only readers that cannot take a list; `NA` otherwise. Read one or the
#'   other, never both)
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
  if (!length(entry)) {
    views <- cc_catalog_views(catalog)
    if (table %in% names(views))
      stop(glue::glue(
        "'{table}' is a view in the catalog for {catalog$version} (over ",
        "{paste(cc_view_tables(views[[table]]), collapse = ', ')}), not a table with parquet ",
        "objects: cc_get_db() creates it, and cc_view_sql(catalog, '{table}', rp) is its SQL over ",
        "however you read those tables"), call. = FALSE)
    stop(glue::glue("table '{table}' is not in the catalog for {catalog$version}"))
  }
  entry <- entry[[1]]
  partitioned <- isTRUE(entry$partitioned)
  version <- as.character(catalog$version)
  # a table the catalog deprecates still resolves — its objects ship through the
  # deprecation window — but says so, so a caller can warn or migrate
  dep <- list(
    deprecated  = isTRUE(entry$deprecated),
    replaced_by = as.character(unlist(entry$replaced_by)),
    removed_in  = if (is.null(entry$removed_in)) NA_character_ else as.character(entry$removed_in))
  dep$replaced_by <- dep$replaced_by[!is.na(dep$replaced_by)]

  if (length(entry$objects)) {
    objs <- entry$objects
    single_file <- NA_character_
    if (partitioned) {
      # a partitioned table may ALSO publish one whole-table file (obs does, for
      # browser DuckDB-WASM and other https-only readers that cannot take a
      # list): it is the object without a partition, and it must never be read
      # alongside the partitions — that would double every row
      is_part <- vapply(objs, function(o) !is.null(o$partition_by), TRUE)
      twin <- objs[!is_part]
      if (length(twin)) single_file <- paste0(base_https, "/", as.character(twin[[1]]$path))
      objs <- objs[is_part]
    }
    paths  <- vapply(objs, function(o) as.character(o$path), "")
    hashes <- vapply(objs, function(o) as.character(o$content_hash %||% NA), "")
    return(c(list(
      urls        = paste0(base_https, "/", paths),
      hive        = partitioned,
      canonical   = TRUE,
      hashes      = hashes,
      # local mirror of the canonical layout: tables/{table}/[{key}={value}/]{hash}/file
      local_paths = sub("^ducklake/", "", paths),
      single_file = single_file), dep))
  }

  if (partitioned) {
    c(list(urls = glue::glue("s3://calcofi-db/ducklake/releases/{version}/parquet/{table}/**/*.parquet"),
           hive = TRUE, canonical = FALSE, hashes = NA_character_,
           local_paths = NA_character_,
           # obs is the one legacy partitioned table with a single-file twin
           single_file = if (table == "obs")
             glue::glue("{base_https}/ducklake/releases/{version}/parquet/obs.parquet") else NA_character_),
      dep)
  } else {
    c(list(urls = glue::glue("{base_https}/ducklake/releases/{version}/parquet/{table}.parquet"),
           hive = FALSE, canonical = FALSE, hashes = NA_character_,
           local_paths = glue::glue("releases/{version}/parquet/{table}.parquet"),
           single_file = NA_character_),
      dep)
  }
}

#' Views a release catalog carries beside its tables
#'
#' Since the v2026.09 releases (calcofi4db 3.31.0, pre-release plan D-S1) `catalog.json` may carry a
#' top-level `views` map: view name → SQL over `{{table}}` tokens, one token per table the view
#' reads. `obs` is the first: the UNION ALL over `obs_bio` and `obs_env` that reconstructs its 18
#' columns under their original names, so `FROM obs` keeps working while the observation rows ship
#' once, as the pair. The table a view replaces is marked `deprecated` in `tables[]` (with
#' `replaced_by` and `removed_in`) for the release it still ships in.
#'
#' `cc_catalog_views()` lists the views (an empty list for a catalog without any);
#' `cc_view_tables()` the tables one reads; `cc_view_sql()` its SQL with every token replaced by
#' `rp(table)` — a quoted identifier by default (the tables exist in the connection, as
#' [cc_get_db()] arranges), or a `read_parquet(...)` from [cc_release_sources()] +
#' [cc_read_parquet_sql()] for a connection that has none. Wrap the result in parentheses to use
#' it in a `FROM`.
#'
#' @param catalog a release catalog as returned by [cc_catalog()] (either jsonlite form)
#' @param name the view's name
#' @param sql a view's SQL carrying `{{table}}` tokens
#' @param rp `function(table) -> character(1)`
#' @return `cc_catalog_views()`: a named list of SQL strings; `cc_view_tables()`: the distinct
#'   table names in order of first appearance; `cc_view_sql()`: a length-one SQL string.
#' @concept database
#' @export
#' @examples
#' \dontrun{
#' cat_ <- cc_catalog("latest")
#' names(cc_catalog_views(cat_))
#' rp  <- function(t) cc_read_parquet_sql(cc_release_sources(cat_, t))
#' sql <- paste0("SELECT count(*) FROM (", cc_view_sql(cat_, "obs", rp), ")")
#' }
cc_catalog_views <- function(catalog) {
  v <- catalog$views
  if (is.null(v) || !length(v)) return(list())
  if (is.data.frame(v)) v <- as.list(v[1, , drop = FALSE])
  out <- lapply(v, function(s) as.character(unlist(s))[1])
  out[!is.na(names(out)) & nzchar(names(out))]
}

#' @rdname cc_catalog_views
#' @export
cc_view_tables <- function(sql) {
  m <- regmatches(sql, gregexpr("\\{\\{([A-Za-z0-9_]+)\\}\\}", sql))[[1]]
  unique(gsub("^\\{\\{|\\}\\}$", "", m))
}

#' @rdname cc_catalog_views
#' @export
cc_view_sql <- function(catalog, name, rp = function(table) paste0('"', table, '"')) {
  stopifnot(is.function(rp))
  views <- cc_catalog_views(catalog)
  if (!name %in% names(views))
    stop(glue::glue("'{name}' is not a view in the catalog for {catalog$version}",
                    if (length(views)) " (views: {paste(names(views), collapse = ', ')})" else ""),
         call. = FALSE)
  sql <- views[[name]]
  for (t in cc_view_tables(sql))
    sql <- gsub(paste0("{{", t, "}}"), rp(t), sql, fixed = TRUE)
  sql
}

#' The `read_parquet(...)` SQL for a resolved source
#'
#' @param src result of [cc_release_sources()]
#' @param paths override the paths to read (e.g. local downloads); default `src$urls`
#' @param prefer_single_file when `TRUE` and `src` has a `single_file` twin, read
#'   that one object instead of the partition list (for https-only readers that
#'   cannot expand a glob). Ignored when `paths` is given.
#' @return a length-one character SQL fragment
#' @concept database
#' @export
cc_read_parquet_sql <- function(src, paths = NULL, prefer_single_file = FALSE) {
  # a partitioned table may publish a whole-table single-file twin (obs does);
  # an https-only reader that cannot expand a glob — a vignette in CI, browser
  # DuckDB-WASM — reads that one object instead of the partition list (and never
  # both, which would double every row). The twin is read as a plain file.
  if (is.null(paths) && isTRUE(prefer_single_file) &&
      !is.null(src$single_file) && !is.na(src$single_file)) {
    return(glue::glue("read_parquet('{src$single_file}')"))
  }
  if (is.null(paths)) paths <- src$urls
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
        # a data-frame row carries NA for fields other objects have (a single-file
        # twin has no partition_by): drop them so the record reads as the list form
        t$objects <- if (is.data.frame(o))
          lapply(seq_len(nrow(o)), function(j) Filter(function(v) !(length(v) == 1 && is.na(v)),
                                                      as.list(o[j, , drop = FALSE]))) else NULL
      }
      t
    })
  } else {
    tbls
  }
}
