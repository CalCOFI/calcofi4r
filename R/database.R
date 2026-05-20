# ─── frozen ducklake access (new) ─────────────────────────────────────────────

#' Connect to CalCOFI Database
#'
#' Connects to a frozen CalCOFI DuckLake release. Downloads and caches the
#' database locally for fast subsequent access.
#'
#' The frozen releases contain clean, stable data without provenance columns,
#' suitable for analysis and visualization. Use \code{cc_list_versions()} to
#' see available releases.
#'
#' @param version Version string (e.g., "v2026.02") or "latest" (default)
#' @param local_cache Use local cache if available (default: TRUE)
#' @param cache_dir Directory for local cache. Default uses
#'   \code{rappdirs::user_cache_dir("calcofi4r")} if rappdirs is installed,
#'   otherwise a temp directory.
#' @param refresh Force re-download even if cached (default: FALSE)
#' @param local_data Download parquet files locally and create tables instead
#'   of remote views (default: FALSE). Useful for apps that need fast local
#'   queries without network overhead.
#' @param tables Character vector of table names to include. NULL (default)
#'   includes all tables. Use to exclude large tables like CTD data.
#'
#' @return DuckDB connection object
#' @export
#' @concept database
#'
#' @details
#' When \code{local_data = FALSE} (default), the connection points to Parquet
#' files from the frozen release registered as views in DuckDB. This allows
#' querying without downloading the entire database.
#'
#' When \code{local_data = TRUE}, parquet files are downloaded to
#' \code{cache_dir/parquet/{version}/} and loaded as local tables for faster
#' queries. Files are only downloaded if missing or if \code{refresh = TRUE}.
#'
#' Data is stored at \code{gs://calcofi-db/ducklake/releases/{version}/}.
#'
#' @examples
#' \dontrun{
#' # connect to latest release (remote views)
#' con <- cc_get_db()
#' DBI::dbListTables(con)
#'
#' # connect with local data (downloads parquets)
#' con <- cc_get_db(local_data = TRUE, cache_dir = "data")
#'
#' # exclude CTD tables
#' con <- cc_get_db(
#'   local_data = TRUE,
#'   tables     = setdiff(cc_db_info()$tables$name,
#'     c("ctd_cast", "ctd_measurement", "ctd_summary")))
#' }
#' @importFrom glue glue
cc_get_db <- function(
    version     = "latest",
    local_cache = TRUE,
    cache_dir   = NULL,
    refresh     = FALSE,
    local_data  = FALSE,
    tables      = NULL) {

  if (!requireNamespace("duckdb", quietly = TRUE)) {
    stop("Package 'duckdb' is required. Install with: install.packages('duckdb')")
  }

  # resolve cache directory
  if (is.null(cache_dir)) {
    if (requireNamespace("rappdirs", quietly = TRUE)) {
      cache_dir <- rappdirs::user_cache_dir("calcofi4r")
    } else {
      cache_dir <- file.path(tempdir(), "calcofi4r_cache")
    }
  }

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  # resolve "latest" to actual version
  if (version == "latest") {
    # get latest version from GCS
    latest_file <- tryCatch({
      .cc_download_gcs_file(
        "gs://calcofi-db/ducklake/releases/latest.txt",
        file.path(cache_dir, "latest.txt"),
        overwrite = TRUE)
      readLines(file.path(cache_dir, "latest.txt"))[1]
    }, error = function(e) {
      # fallback: try to find from directory listing
      message("Could not determine latest version, using v2026.02 as default")
      "v2026.02"
    })
    version <- latest_file
  }

  # validate version format
  if (!grepl("^v\\d{4}\\.\\d{2}", version)) {
    stop("Version must be in format vYYYY.MM or vYYYY.MM.DD (e.g., 'v2026.03.14')")
  }

  # create DuckDB connection
  db_path <- if (local_cache) {
    file.path(cache_dir, glue::glue("calcofi_{version}.duckdb"))
  } else {
    ":memory:"
  }

  # native GEOMETRY requires latest storage format (DuckDB >= 1.5)
  db_config <- list(
    autoload_known_extensions     = "true",
    storage_compatibility_version = "latest")

  # try to connect, with fallback to read-only if locked
  con <- tryCatch({
    drv <- duckdb::duckdb(dbdir = db_path, config = db_config)
    DBI::dbConnect(drv)
  }, error = function(e) {
    if (grepl("lock", e$message, ignore.case = TRUE)) {
      message("Database locked, opening in read-only mode")
      drv <- duckdb::duckdb(
        dbdir = db_path, read_only = TRUE, config = db_config)
      DBI::dbConnect(drv)
    } else {
      stop(e)
    }
  })

  # get catalog for this version (needed for both cached and fresh paths)
  gcs_base     <- glue::glue("gs://calcofi-db/ducklake/releases/{version}")
  catalog_path <- file.path(cache_dir, glue::glue("catalog_{version}.json"))

  tryCatch({
    .cc_download_gcs_file(
      glue::glue("{gcs_base}/catalog.json"),
      catalog_path,
      overwrite = refresh)
  }, error = function(e) {
    stop(glue::glue("Could not find release {version}. Use cc_list_versions() to see available releases."))
  })

  catalog <- jsonlite::fromJSON(catalog_path)

  # configure httpfs if any tables are hive-partitioned (S3 glob required);
  # must run on EVERY connection since DuckDB settings are per-session
  has_partitioned <- "partitioned" %in% names(catalog$tables) &&
    any(catalog$tables$partitioned, na.rm = TRUE)

  if (has_partitioned) {
    .cc_setup_gcs_httpfs(con)
  }

  # check if already initialized
  existing_tables <- DBI::dbListTables(con)
  if (length(existing_tables) > 0 && !refresh) {
    message(glue::glue("Using cached database: {version}"))
    return(con)
  }

  # filter catalog to requested tables
  tbl_catalog <- catalog$tables
  if (!is.null(tables)) {
    tbl_catalog <- tbl_catalog[tbl_catalog$name %in% tables, ]
  }

  message(glue::glue(
    "Loading {nrow(tbl_catalog)} tables from {version}",
    if (local_data) " (local data)" else " (remote views)",
    "..."))

  gcs_https_base <- glue::glue(
    "https://storage.googleapis.com/calcofi-db/ducklake/releases/{version}/parquet")
  gcs_s3_base <- glue::glue(
    "s3://calcofi-db/ducklake/releases/{version}/parquet")

  # local parquet download directory
  if (local_data) {
    parquet_dir <- file.path(cache_dir, "parquet", version)
    dir.create(parquet_dir, recursive = TRUE, showWarnings = FALSE)
  }

  for (i in seq_len(nrow(tbl_catalog))) {
    tbl_name       <- tbl_catalog$name[i]
    is_partitioned <- has_partitioned &&
      isTRUE(tbl_catalog$partitioned[i])

    if (local_data && !is_partitioned) {
      # download parquet locally and create table
      local_pq <- file.path(parquet_dir, paste0(tbl_name, ".parquet"))
      if (!file.exists(local_pq) || refresh) {
        message(glue::glue("  downloading {tbl_name}.parquet..."))
        .cc_download_gcs_file(
          glue::glue("gs://calcofi-db/ducklake/releases/{version}/parquet/{tbl_name}.parquet"),
          local_pq,
          overwrite = refresh)
      }
      tryCatch({
        DBI::dbExecute(con, glue::glue(
          "CREATE OR REPLACE TABLE \"{tbl_name}\" AS ",
          "SELECT * FROM read_parquet('{local_pq}')"))
      }, error = function(e) {
        warning(glue::glue("Failed to load table {tbl_name}: {e$message}"))
      })

    } else if (local_data && is_partitioned) {
      # partitioned tables: use remote S3 glob (downloading partition
      # directories is complex; keep as remote view for now)
      parquet_url <- glue::glue("{gcs_s3_base}/{tbl_name}/**/*.parquet")
      read_expr   <- glue::glue(
        "read_parquet('{parquet_url}', hive_partitioning = true)")
      tryCatch({
        DBI::dbExecute(con, glue::glue(
          "CREATE OR REPLACE VIEW \"{tbl_name}\" AS SELECT * FROM {read_expr}"))
      }, error = function(e) {
        warning(glue::glue("Failed to load table {tbl_name}: {e$message}"))
      })

    } else if (is_partitioned) {
      # remote view for partitioned table
      parquet_url <- glue::glue("{gcs_s3_base}/{tbl_name}/**/*.parquet")
      read_expr   <- glue::glue(
        "read_parquet('{parquet_url}', hive_partitioning = true)")
      tryCatch({
        DBI::dbExecute(con, glue::glue(
          "CREATE OR REPLACE VIEW \"{tbl_name}\" AS SELECT * FROM {read_expr}"))
      }, error = function(e) {
        warning(glue::glue("Failed to load table {tbl_name}: {e$message}"))
      })

    } else {
      # remote view for single-file table
      parquet_url <- glue::glue("{gcs_https_base}/{tbl_name}.parquet")
      read_expr   <- glue::glue("read_parquet('{parquet_url}')")
      tryCatch({
        DBI::dbExecute(con, glue::glue(
          "CREATE OR REPLACE VIEW \"{tbl_name}\" AS SELECT * FROM {read_expr}"))
      }, error = function(e) {
        warning(glue::glue("Failed to load table {tbl_name}: {e$message}"))
      })
    }
  }

  message(glue::glue("Connected to CalCOFI database {version}"))
  return(con)
}

#' List available CalCOFI database versions
#'
#' Lists all available frozen CalCOFI database releases by reading
#' the versions manifest from the public GCS bucket.
#'
#' @return Tibble with columns: version, release_date, tables, total_rows, size_mb, is_latest
#'
#' @export
#' @concept database
#'
#' @examples
#' \dontrun{
#' cc_list_versions()
#' }
#' @importFrom tibble tibble
cc_list_versions <- function() {
  # download versions manifest from public bucket
  tryCatch({
    versions_url <- "https://storage.googleapis.com/calcofi-db/ducklake/releases/versions.json"
    versions_file <- tempfile(fileext = ".json")

    utils::download.file(
      url      = versions_url,
      destfile = versions_file,
      mode     = "wb",
      quiet    = TRUE)

    versions_data <- jsonlite::fromJSON(versions_file)

    # get latest
    latest <- tryCatch({
      latest_url <- "https://storage.googleapis.com/calcofi-db/ducklake/releases/latest.txt"
      latest_file <- tempfile()
      utils::download.file(latest_url, latest_file, quiet = TRUE)
      trimws(readLines(latest_file, warn = FALSE)[1])
    }, error = function(e) {
      # fallback: use first version as latest
      if (length(versions_data$versions) > 0)
        versions_data$versions[[1]]$version
      else
        NA_character_
    })

    # build tibble from versions list
    # jsonlite parses single-element arrays as data frames, multi-element as lists
    if (is.data.frame(versions_data$versions)) {
      # single version case - already a data frame
      result <- tibble::as_tibble(versions_data$versions) |>
        dplyr::mutate(is_latest = version == latest)
    } else if (length(versions_data$versions) > 0) {
      # multiple versions case - list of lists
      result <- tibble::tibble(
        version      = sapply(versions_data$versions, `[[`, "version"),
        release_date = sapply(versions_data$versions, `[[`, "release_date"),
        tables       = as.integer(sapply(versions_data$versions, `[[`, "tables")),
        total_rows   = as.integer(sapply(versions_data$versions, `[[`, "total_rows")),
        size_mb      = as.numeric(sapply(versions_data$versions, `[[`, "size_mb")),
        is_latest    = sapply(versions_data$versions, `[[`, "version") == latest)
    } else {
      result <- tibble::tibble(
        version      = character(),
        release_date = character(),
        tables       = integer(),
        total_rows   = integer(),
        size_mb      = numeric(),
        is_latest    = logical())
    }
    result |> dplyr::arrange(dplyr::desc(version))

  }, error = function(e) {
    # fallback: return known version if manifest unavailable
    message(glue::glue("Could not fetch versions manifest: {e$message}"))
    message("Using fallback version list.")
    tibble::tibble(
      version      = "v2026.02",
      release_date = "2026-02-05",
      tables       = 17L,
      total_rows   = 13410422L,
      size_mb      = 80.9,
      is_latest    = TRUE)
  })
}

#' Get CalCOFI database information
#'
#' Returns metadata about a specific database release including
#' table schemas and statistics.
#'
#' @param version Version string or "latest" (default)
#'
#' @return List with release metadata, table info, and schema
#'
#' @export
#' @concept database
#'
#' @examples
#' \dontrun{
#' cc_db_info()
#' cc_db_info("v2026.02")
#' }
#' @importFrom glue glue
cc_db_info <- function(version = "latest") {
  # resolve latest
  if (version == "latest") {
    versions <- cc_list_versions()
    if (nrow(versions) > 0) {
      version <- versions$version[versions$is_latest][1]
      if (is.na(version)) version <- versions$version[1]
    } else {
      version <- "v2026.02"
    }
  }

  # get catalog
  cache_dir    <- file.path(tempdir(), "calcofi4r_cache")
  dir.create(cache_dir, showWarnings = FALSE)
  catalog_path <- file.path(cache_dir, glue::glue("catalog_{version}.json"))

  tryCatch({
    .cc_download_gcs_file(
      glue::glue("gs://calcofi-db/ducklake/releases/{version}/catalog.json"),
      catalog_path,
      overwrite = TRUE)
  }, error = function(e) {
    stop(glue::glue("Could not find release {version}"))
  })

  catalog <- jsonlite::fromJSON(catalog_path)

  list(
    version      = catalog$version,
    release_date = catalog$release_date,
    total_rows   = catalog$total_rows,
    tables       = tibble::as_tibble(catalog$tables),
    gcs_path     = glue::glue("gs://calcofi-db/ducklake/releases/{version}"))
}

#' View CalCOFI database release notes
#'
#' Displays the release notes for a specific database version.
#'
#' @param version Version string or "latest" (default)
#'
#' @return Character string with release notes
#'
#' @export
#' @concept database
#'
#' @examples
#' \dontrun{
#' cc_release_notes("v2026.02")
#' }
#' @importFrom glue glue
cc_release_notes <- function(version = "latest") {
  # resolve latest
  if (version == "latest") {
    versions <- cc_list_versions()
    if (nrow(versions) > 0) {
      version <- versions$version[versions$is_latest][1]
      if (is.na(version)) version <- versions$version[1]
    } else {
      version <- "v2026.02"
    }
  }

  # get release notes
  cache_dir  <- file.path(tempdir(), "calcofi4r_cache")
  dir.create(cache_dir, showWarnings = FALSE)
  notes_path <- file.path(cache_dir, glue::glue("RELEASE_NOTES_{version}.md"))

  tryCatch({
    .cc_download_gcs_file(
      glue::glue("gs://calcofi-db/ducklake/releases/{version}/RELEASE_NOTES.md"),
      notes_path,
      overwrite = TRUE)
    paste(readLines(notes_path), collapse = "\n")
  }, error = function(e) {
    glue::glue("No release notes available for {version}")
  })
}

# configure DuckDB httpfs to access GCS as S3-compatible (public, anonymous)
.cc_setup_gcs_httpfs <- function(con) {
  DBI::dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
  DBI::dbExecute(con, "SET s3_region = 'auto';")
  DBI::dbExecute(con, "SET s3_endpoint = 'storage.googleapis.com';")
  DBI::dbExecute(con, "SET s3_url_style = 'path';")
  DBI::dbExecute(con, "SET s3_access_key_id = '';")
  DBI::dbExecute(con, "SET s3_secret_access_key = '';")
}

# helper function to download from GCS (uses public HTTPS URLs, no gcloud needed)
.cc_download_gcs_file <- function(gcs_path, local_path, overwrite = FALSE) {
  if (file.exists(local_path) && !overwrite) {
    return(local_path)
  }

  dir.create(dirname(local_path), recursive = TRUE, showWarnings = FALSE)


  # convert gs:// to https:// URL for public bucket access
  https_url <- gsub(
    "^gs://([^/]+)/(.*)$",
    "https://storage.googleapis.com/\\1/\\2",
    gcs_path)

  # download using base R (works without extra dependencies)
  tryCatch({
    utils::download.file(
      url      = https_url,
      destfile = local_path,
      mode     = "wb",
      quiet    = TRUE)
  }, error = function(e) {
    stop(glue::glue("Failed to download {gcs_path}: {e$message}"))
  })

  if (!file.exists(local_path)) {
    stop(glue::glue("Failed to download: {gcs_path}"))
  }

  local_path
}

# ─── dm with relationships ────────────────────────────────────────────────────

#' Get CalCOFI Database as dm Object with Relationships
#'
#' Returns a \code{dm} object with primary keys and foreign keys applied from
#' the \code{relationships.json} sidecar file included in frozen releases.
#' This enables schema visualization via \code{dm::dm_draw()} and relationship-
#' aware operations.
#'
#' @param version Version string (e.g., "v2026.02") or "latest" (default)
#' @param con Optional existing DuckDB connection. If NULL (default),
#'   calls \code{cc_get_db(version)} to create one.
#'
#' @return A \code{dm} object with PKs and FKs applied
#' @export
#' @concept database
#'
#' @examples
#' \dontrun{
#' dm <- cc_get_dm()
#' dm::dm_draw(dm, rankdir = "LR", view_type = "all")
#'
#' # use existing connection
#' con <- cc_get_db()
#' dm <- cc_get_dm(con = con)
#' }
#' @importFrom glue glue
cc_get_dm <- function(version = "latest", con = NULL) {
  if (!requireNamespace("dm", quietly = TRUE)) {
    stop("Package 'dm' is required. Install with: install.packages('dm')")
  }

  # get or create connection
  if (is.null(con)) {
    con <- cc_get_db(version)
  }

  # resolve version for GCS URL
  if (version == "latest") {
    cache_dir <- file.path(tempdir(), "calcofi4r_cache")
    dir.create(cache_dir, showWarnings = FALSE)
    version <- tryCatch({
      .cc_download_gcs_file(
        "gs://calcofi-db/ducklake/releases/latest.txt",
        file.path(cache_dir, "latest_dm.txt"),
        overwrite = TRUE)
      readLines(file.path(cache_dir, "latest_dm.txt"))[1]
    }, error = function(e) "v2026.02")
  }

  # build dm from connection (no learned keys)
  d <- dm::dm_from_con(con, learn_keys = FALSE)

  # download and apply relationships.json
  rels_url <- glue::glue(
    "gs://calcofi-db/ducklake/releases/{version}/relationships.json")
  rels_local <- file.path(tempdir(), glue::glue("relationships_{version}.json"))

  tryCatch({
    .cc_download_gcs_file(rels_url, rels_local, overwrite = TRUE)
    d <- .apply_relationships(d, rels_local)
    message(glue::glue("Applied relationships from {version}"))
  }, error = function(e) {
    message(glue::glue(
      "No relationships.json found for {version}, returning dm without keys"))
  })

  d
}

# internal helper: apply PKs/FKs from relationships.json to a dm object
.apply_relationships <- function(dm, rels_path) {
  rels <- jsonlite::fromJSON(rels_path, simplifyVector = FALSE)
  dm_tables <- dm::dm_get_tables(dm) |> names()

  # apply primary keys
  for (tbl in names(rels$primary_keys)) {
    if (tbl %in% dm_tables) {
      col <- rels$primary_keys[[tbl]]
      dm <- tryCatch(
        dm::dm_add_pk(dm, !!tbl, !!col),
        error = function(e) dm)
    }
  }

  # apply foreign keys
  for (fk in rels$foreign_keys) {
    if (fk$table %in% dm_tables && fk$ref_table %in% dm_tables) {
      dm <- tryCatch(
        dm::dm_add_fk(dm, !!fk$table, !!fk$column, !!fk$ref_table, !!fk$ref_column),
        error = function(e) dm)
    }
  }

  dm
}

# ─── spatial helpers ──────────────────────────────────────────────────────────

#' Read Spatial Table from DuckDB as sf Object
#'
#' Reads a DuckDB table containing geometry columns into an sf object.
#' Handles the `ST_AsWKB()` conversion required by DuckDB Spatial automatically.
#'
#' @param con DuckDB connection
#' @param table_name Name of the table to read
#' @param geom_col Name of the geometry column (default: "geom"). If NULL,
#'   auto-detects the first GEOMETRY column.
#' @param crs Coordinate reference system to set (default: 4326 for WGS84).
#'   DuckDB Spatial does not store CRS metadata, so this must be specified.
#'
#' @return An sf object with geometry column
#' @export
#' @concept database
#'
#' @examples
#' \dontrun{
#' con <- cc_get_db()
#'
#' # read site points
#' sites_sf <- cc_read_sf(con, "site")
#'
#' # read with specific geometry column
#' grid_sf <- cc_read_sf(con, "grid", geom_col = "geom_ctr")
#'
#' # read with different CRS
#' sites_sf <- cc_read_sf(con, "site", crs = 4326)
#' }
#' @importFrom DBI dbGetQuery dbListFields
#' @importFrom glue glue
#' @importFrom sf read_sf st_set_crs
cc_read_sf <- function(
    con,
    table_name,
    geom_col = NULL,
    crs      = 4326) {

  flds <- DBI::dbListFields(con, table_name)

  # auto-detect geometry column if not specified
  if (is.null(geom_col)) {
    col_types <- DBI::dbGetQuery(con, glue::glue(
      "SELECT column_name, data_type FROM information_schema.columns
       WHERE table_name = '{table_name}' AND data_type = 'GEOMETRY'"))

    if (nrow(col_types) == 0) {
      stop(glue::glue("No GEOMETRY column found in table '{table_name}'"))
    }

    geom_col <- col_types$column_name[1]
    if (nrow(col_types) > 1) {
      message(glue::glue(
        "Multiple geometry columns found: {paste(col_types$column_name, collapse = ', ')}. ",
        "Using '{geom_col}'. Specify geom_col to choose a different one."))
    }
  }

  # build query: select non-geom columns as-is, convert geom with ST_AsWKB
  other_cols <- setdiff(flds, geom_col)
  select_clause <- paste(c(
    other_cols,
    glue::glue("ST_AsWKB({geom_col}) as {geom_col}")),
    collapse = ", ")

  query <- glue::glue("SELECT {select_clause} FROM {table_name}")

  # read into sf
  result <- sf::read_sf(con, query = query, geometry_column = geom_col) |>
    sf::st_set_crs(crs)

  result
}

#' Access a CalCOFI Database Table
#'
#' Unified interface for reading any table from the CalCOFI database.
#' Returns a lazy `dplyr::tbl()` reference for non-spatial tables, or
#' an `sf` object for tables with geometry columns.
#'
#' For the `_spatial` table, automatically pivots attributes from
#' `_spatial_attr` wide and returns an sf object filtered to the
#' requested layer.
#'
#' @param con DuckDB connection (from [cc_get_db()])
#' @param table_name Name of the table
#' @param layer Required when `table_name = "_spatial"`. Character string
#'   specifying which spatial layer to return.
#' @param geom_col Name of the geometry column for spatial tables
#'   (default: "geom"). Use this to select alternate geometry columns,
#'   e.g. `"geom_ctr"` for grid centroids.
#' @param crs CRS to assign to the returned sf object (default: 4326)
#'
#' @return For non-spatial tables: a lazy `dplyr::tbl()` reference.
#'   For spatial tables: an `sf` object with geometry.
#' @export
#' @concept database
#'
#' @examples
#' \dontrun{
#' con <- cc_get_db()
#'
#' # non-spatial: returns lazy dbplyr reference
#' cc_tbl(con, "ichthyo")
#'
#' # spatial: returns sf with default geom column
#' cc_tbl(con, "grid")
#'
#' # spatial: select alternate geometry
#' cc_tbl(con, "grid", geom_col = "geom_ctr")
#'
#' # _spatial: returns sf with pivoted attributes for a layer
#' cc_tbl(con, "_spatial", layer = "CA Counties")
#' }
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue
#' @importFrom sf read_sf st_set_crs
cc_tbl <- function(
    con,
    table_name,
    layer    = NULL,
    geom_col = "geom",
    crs      = 4326) {

  # detect geometry columns
  geom_cols <- DBI::dbGetQuery(con, glue::glue(
    "SELECT column_name FROM information_schema.columns
     WHERE table_name = '{table_name}'
       AND data_type LIKE 'GEOMETRY%'"))$column_name

  # non-spatial: return lazy tbl
  if (length(geom_cols) == 0) {
    return(dplyr::tbl(con, table_name))
  }

  # _spatial table: require layer, pivot attributes wide

  if (table_name == "_spatial") {
    if (is.null(layer)) {
      stop("layer argument is required for the _spatial table")
    }

    # read geometry for the requested layer
    all_geom <- paste(geom_cols, collapse = ", ")
    exclude_clause <- paste(geom_cols, collapse = ", ")
    query <- glue::glue(
      "SELECT * EXCLUDE({exclude_clause}),
              ST_AsWKB({geom_col}) AS {geom_col}
       FROM {table_name}
       WHERE layer = '{layer}'")
    sf_data <- sf::read_sf(
      con, query = query,
      geometry_column = geom_col) |>
      sf::st_set_crs(crs)

    # read and pivot attributes
    attrs <- DBI::dbGetQuery(con, glue::glue(
      "SELECT id, fld,
              COALESCE(
                CAST(val_dbl AS VARCHAR),
                CAST(val_int AS VARCHAR),
                val_chr,
                CAST(val_date AS VARCHAR),
                CAST(val_lgl AS VARCHAR)
              ) AS val
       FROM _spatial_attr
       WHERE layer = '{layer}'"))

    if (nrow(attrs) > 0) {
      attrs_wide <- tidyr::pivot_wider(
        attrs, names_from = fld, values_from = val)
      sf_data <- dplyr::left_join(sf_data, attrs_wide, by = "id")
    }

    return(sf_data)
  }

  # regular spatial table: return sf via EXCLUDE + ST_AsWKB
  exclude_clause <- paste(geom_cols, collapse = ", ")
  query <- glue::glue(
    "SELECT * EXCLUDE({exclude_clause}),
            ST_AsWKB({geom_col}) AS {geom_col}
     FROM {table_name}")
  sf::read_sf(
    con, query = query,
    geometry_column = geom_col) |>
    sf::st_set_crs(crs)
}

# ─── derived views ─────────────────────────────────────────────────────────────

# internal: prebaked view templates
.view_templates <- list(
  casts_extra = list(
    base_table = "casts",
    view_name  = "casts_extra",
    columns    = c(
      year        = "EXTRACT(YEAR FROM datetime_utc)::SMALLINT",
      month       = "EXTRACT(MONTH FROM datetime_utc)::SMALLINT",
      quarter     = "EXTRACT(QUARTER FROM datetime_utc)::SMALLINT",
      julian_day  = "EXTRACT(DOY FROM datetime_utc)::SMALLINT",
      julian_date = "(datetime_utc::DATE - DATE '1899-12-30')",
      lat_deg     = "FLOOR(ABS(lat_dec))::SMALLINT",
      lat_min     = "(ABS(lat_dec) - FLOOR(ABS(lat_dec))) * 60",
      lat_hem     = "CASE WHEN lat_dec >= 0 THEN 'N' ELSE 'S' END",
      lon_deg     = "FLOOR(ABS(lon_dec))::SMALLINT",
      lon_min     = "(ABS(lon_dec) - FLOOR(ABS(lon_dec))) * 60",
      lon_hem     = "CASE WHEN lon_dec >= 0 THEN 'E' ELSE 'W' END",
      cruise      = "STRFTIME(datetime_utc, '%Y%m')",
      db_sta_key  = "REPLACE(REPLACE(site_key, '.', ''), ' ', '')")))

#' Create a Derived VIEW in the Database
#'
#' Creates a SQL VIEW with derived columns on top of base tables.
#' Supports prebaked templates (e.g., "casts_extra") or custom
#' column definitions specified as named SQL expressions.
#'
#' @param con DBI connection to DuckDB
#' @param template Character. Name of a prebaked view template.
#'   Available: "casts_extra". If provided, view_name and
#'   column_definitions are taken from the template (but can be
#'   overridden).
#' @param view_name Character. Name for the VIEW. Defaults to the
#'   template name if using a template.
#' @param base_table Character. Base table name. Required if no
#'   template is provided.
#' @param column_definitions Named character vector. Names are new
#'   column names, values are DuckDB SQL expressions. Appended as
#'   `expression AS column_name` to `SELECT *, ...`.
#' @return A lazy dbplyr table reference to the created VIEW.
#' @export
#' @concept database
#' @importFrom DBI dbExecute
#' @importFrom dplyr tbl
#' @importFrom glue glue
cc_make_view <- function(
    con,
    template           = NULL,
    view_name          = template,
    base_table         = NULL,
    column_definitions = NULL) {

  # resolve template
  if (!is.null(template)) {
    if (!template %in% names(.view_templates))
      stop(glue::glue(
        "Unknown template '{template}'. Available: ",
        "{paste(names(.view_templates), collapse = ', ')}"))

    tmpl <- .view_templates[[template]]

    if (is.null(base_table))
      base_table <- tmpl$base_table
    if (is.null(view_name))
      view_name <- tmpl$view_name
    if (is.null(column_definitions))
      column_definitions <- tmpl$columns
  }

  stopifnot(
    !is.null(view_name),
    !is.null(base_table),
    !is.null(column_definitions),
    length(column_definitions) > 0)

  # build derived column expressions
  derived_cols <- paste(
    column_definitions, "AS", names(column_definitions),
    collapse = ",\n    ")

  sql <- glue::glue("
    CREATE OR REPLACE VIEW {view_name} AS
    SELECT *,
    {derived_cols}
    FROM {base_table}")

  DBI::dbExecute(con, sql)
  message(glue::glue(
    "Created VIEW '{view_name}' on '{base_table}' with ",
    "{length(column_definitions)} derived columns"))

  dplyr::tbl(con, view_name)
}

#' List Available View Templates
#'
#' Returns the names of prebaked view templates that can be used
#' with \code{\link{cc_make_view}}.
#'
#' @return Character vector of template names
#' @export
#' @concept database
cc_list_view_templates <- function() {
  names(.view_templates)
}

# ─── postgresql connection (deprecated) ───────────────────────────────────────

#' Connect to the CalCOFI PostgreSQL database (Admin only) - DEPRECATED
#'
#' @description
#' \lifecycle{deprecated}
#'
#' This function is deprecated. Please use \code{\link{cc_get_db}} instead for
#' connecting to the new DuckDB-based database.
#'
#' Note that you must either be running this from the CalCOFI server or have a
#' [tunnelled SSH
#' connection](https://github.com/calcofi/server#ssh-tunnel-connection-to-postgis-db)
#' with a user account on the server and the password in a file located at
#' `"~/.calcofi_db_pass.txt"`.
#'
#' @param path_pw path to password file with one line containing the database
#'   password; default: `"~/.calcofi_db_pass.txt"`
#'
#' @return a `DBI::dbConnect()` object
#' @import DBI
#' @importFrom RPostgres Postgres
#' @export
#' @concept database
#'
#' @examples
#' \dontrun{
#' con <- cc_db_connect()
#' DBI::dbListTables(con)
#' }
cc_db_connect <- function(path_pw = "~/.calcofi_db_pass.txt"){

  # deprecation warning
  if (requireNamespace("lifecycle", quietly = TRUE)) {
    lifecycle::deprecate_warn(
      "1.0.0",
      "cc_db_connect()",
      "cc_get_db()",
      details = "PostgreSQL is being phased out. Use cc_get_db() for DuckDB access.")
  } else {
    warning("cc_db_connect() is deprecated. Use cc_get_db() instead for DuckDB access.")
  }

  is_server <- Sys.info()[["sysname"]] == "Linux"
  host <- ifelse(
    is_server,
    "postgis",   # from rstudio to postgis docker container on server
    "localhost") # from laptop to locally tunneled connection to db container on server
  # for localhost db, see: https://github.com/calcofi/server#ssh-tunnel-connection-to-postgis-db

  # database connect ----
  stopifnot(file.exists(path_pw))

  DBI::dbConnect(
    RPostgres::Postgres(),
    dbname   = "gis",
    host     = host,
    port     = 5432,
    user     = "admin",
    password = readLines(path_pw))
}

#' Show CalCOFI database catalog as interactive table
#'
#' Renders an interactive `DT::datatable()` of every table and column in a
#' CalCOFI database release, with descriptions and units sourced from the
#' release `metadata.json` sidecar at
#' `gs://calcofi-db/ducklake/releases/{version}/metadata.json`. Column data
#' types come from DuckDB `information_schema.columns`.
#'
#' For a richer point-and-click view with ERD diagram, dataset
#' provenance, and the canonical measurement-type registry, see the
#' [CalCOFI Schema explorer](https://calcofi.io/schema/).
#'
#' @param tables Optional character vector of table names to filter to.
#'   Default: \code{NULL} (show all).
#' @param version Database version. Default: \code{"latest"}.
#'
#' @return A `DT::datatable()` of tables and columns.
#'
#' @seealso [cc_describe_table()] for per-table schema as a tibble.
#'   [CalCOFI Schema explorer](https://calcofi.io/schema/) for the
#'   web-based browser with ERD and measurement-type registry.
#' @importFrom DT datatable
#' @importFrom htmltools div em span strong HTML
#' @importFrom markdown mark
#' @importFrom purrr pmap_chr
#' @importFrom dplyr left_join filter select mutate arrange
#' @importFrom tibble tibble
#' @importFrom DBI dbGetQuery dbListTables
#' @importFrom glue glue
#' @export
#' @concept database
#'
#' @examples
#' \dontrun{
#' cc_db_catalog()
#' cc_db_catalog(tables = c("bottle", "ichthyo"))
#' cc_db_catalog(version = "v2026.05.14")
#' }
cc_db_catalog <- function(tables = NULL, version = "latest"){

  con <- cc_get_db(version = version)
  all_tbls <- DBI::dbListTables(con)
  if (!is.null(tables)) {
    missing_tbls <- setdiff(tables, all_tbls)
    if (length(missing_tbls) > 0)
      warning(glue::glue(
        "cc_db_catalog: requested tables not in release: ",
        "{paste(missing_tbls, collapse = ', ')}"))
    all_tbls <- intersect(all_tbls, tables)
  }

  # information_schema → column types
  schema <- DBI::dbGetQuery(con, glue::glue("
    SELECT table_name AS \"table\", column_name AS \"column\",
           data_type AS column_type
    FROM information_schema.columns
    WHERE table_name IN ({paste(sprintf(\"'%s'\", all_tbls), collapse = ',')})
    ORDER BY table_name, ordinal_position"))

  # metadata.json sidecar → descriptions + units
  meta <- tryCatch(
    .cc_release_metadata(version = version),
    error = function(e) {
      warning(glue::glue(
        "cc_db_catalog: metadata.json not available for {version}; ",
        "descriptions and units will be blank ({conditionMessage(e)})"))
      NULL
    })

  if (!is.null(meta)) {
    tbl_desc <- tibble::tibble(
      table              = names(meta$tables),
      table_description  = vapply(meta$tables, function(x) x$description_md %||% NA_character_, character(1)))
    col_desc <- tibble::tibble(
      key                = names(meta$columns),
      table              = sub("\\..*$", "", key),
      column             = sub("^[^.]+\\.", "", key),
      column_description = vapply(meta$columns, function(x) x$description_md %||% NA_character_, character(1)),
      units              = vapply(meta$columns, function(x) x$units          %||% NA_character_, character(1)))
    col_desc <- col_desc[, c("table", "column", "column_description", "units")]
  } else {
    tbl_desc <- tibble::tibble(table = character(), table_description = character())
    col_desc <- tibble::tibble(table = character(), column = character(),
                               column_description = character(), units = character())
  }

  d <- schema |>
    dplyr::left_join(tbl_desc, by = "table") |>
    dplyr::left_join(col_desc, by = c("table", "column"))

  d |>
    dplyr::mutate(
      Table = purrr::pmap_chr(
        list(table, table_description),
        function(table, table_description, ...){
          if (is.na(table_description) || table_description == "") {
            as.character(htmltools::div(htmltools::strong(table)))
          } else {
            as.character(htmltools::div(
              htmltools::strong(table),
              htmltools::div(
                style = "font-size: 0.75rem; font-weight: 400;",
                htmltools::HTML(markdown::mark(table_description)))))
          }
        }),
      Column = purrr::pmap_chr(
        list(column, column_type, column_description, units),
        function(column, column_type, column_description, units, ...){
          type_span <- htmltools::span(
            style = "font-weight: 400;", "(", htmltools::em(column_type),
            if (!is.na(units) && units != "") paste0("; ", units) else NULL, ")")
          if (is.na(column_description) || column_description == "") {
            as.character(htmltools::div(htmltools::strong(column), type_span))
          } else {
            as.character(htmltools::div(
              htmltools::strong(column), type_span,
              htmltools::div(
                style = "font-size: 0.75rem; font-weight: 400;",
                htmltools::HTML(markdown::mark(column_description)))))
          }
        })) |>
    dplyr::select(table, column, Table, Column) |>
    DT::datatable(
      extensions = c("RowGroup"),
      options = list(
        dom        = "lfrtip",
        pageLength = 10,
        lengthMenu = c(10, 100, 1000),
        rowGroup   = list(dataSrc = c(0, 1)),
        columnDefs = list(list(visible = FALSE, targets = c(0, 1)))),
      escape   = FALSE,
      rownames = FALSE)
}


#' Create index in database
#'
#' @param con database connection object from `DBI::dbConnect()`, e.g. from `cc_db_connect()`
#' @param tbl table name as character
#' @param flds character vector of fields in table used for index
#' @param is_geom logical (default: FALSE) whether geometry field, so create GIST() index
#' @param is_unique logical (default: FALSE) whether to impose a unique constraint, to prevent duplicates; default: FALSE
#' @param overwrite logical (default: FALSE) whether to overwrite existing index
#' @param show logical (default: FALSE) whether to show SQL statement
#' @param exec logical (default: TRUE) whether to execute SQL statement
#'
#' @return nothing
#' @import DBI
#' @importFrom glue glue
#' @export
#' @concept database
#'
#' @examples
#' \dontrun{
#' con <- cc_db_connect()
#' create_index(con, "ctd_casts", "geom", is_geom=T)
#' create_index(con, "ctd_casts", "cast_count", is_unique=T)
#' }
create_index <- function(con, tbl, flds, is_geom=F, is_unique=F, overwrite=F, show=F, exec=T){
  # tbl = "taxa"; flds = c("tbl_orig", "aphia_id"); is_unique = T; is_geom=F
  stopifnot(!(is_geom == T & length(flds) != 1))
  sfx <- ifelse(
    is_geom,
    glue::glue(" USING GIST ({flds})"),
    glue::glue("({paste(flds, collapse=', ')})"))
  idx <- glue("{tbl}_{paste(flds, collapse='_')}_idx")

  if (overwrite)
    dbSendQuery(con, glue("DROP INDEX IF EXISTS {idx}"))

  sql <- glue::glue(
    "CREATE {ifelse(is_unique, 'UNIQUE','')} INDEX IF NOT EXISTS {idx} ON {tbl}{sfx}")
  if (show)
    message(sql)
  if (exec)
    DBI::dbSendQuery(con, sql)
}

