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
#'
#' @return DuckDB connection object
#' @export
#' @concept database
#'
#' @details
#' The connection points to Parquet files from the frozen release, which are
#' registered as views in DuckDB. This allows querying the data without
#' downloading the entire database.
#'
#' Data is stored at \code{gs://calcofi-db/ducklake/releases/{version}/}.
#'
#' @examples
#' \dontrun{
#' # connect to latest release
#' con <- cc_get_db()
#' DBI::dbListTables(con)
#'
#' # connect to specific version
#' con <- cc_get_db(version = "v2026.02")
#'
#' # query data
#' DBI::dbGetQuery(con, "SELECT COUNT(*) FROM larvae")
#' }
#' @importFrom glue glue
cc_get_db <- function(
    version     = "latest",
    local_cache = TRUE,
    cache_dir   = NULL,
    refresh     = FALSE) {

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
    stop("Version must be in format vYYYY.MM (e.g., 'v2026.02')")
  }

  # create DuckDB connection
  db_path <- if (local_cache) {
    file.path(cache_dir, glue::glue("calcofi_{version}.duckdb"))
  } else {
    ":memory:"
  }

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)

  # check if already initialized
  tables <- DBI::dbListTables(con)
  if (length(tables) > 0 && !refresh) {
    message(glue::glue("Using cached database: {version}"))
    return(con)
  }

  # get catalog for this version
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

  # create views for each table pointing to parquet files
  message(glue::glue("Loading {nrow(catalog$tables)} tables from {version}..."))

  for (i in seq_len(nrow(catalog$tables))) {
    tbl_name <- catalog$tables$name[i]

    # use httpfs to read directly from GCS (public bucket)
    parquet_url <- glue::glue(
      "https://storage.googleapis.com/calcofi-db/ducklake/releases/{version}/parquet/{tbl_name}.parquet")

    tryCatch({
      DBI::dbExecute(con, glue::glue(
        "CREATE OR REPLACE VIEW {tbl_name} AS SELECT * FROM read_parquet('{parquet_url}')"))
    }, error = function(e) {
      warning(glue::glue("Failed to load table {tbl_name}: {e$message}"))
    })
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
      db_sta_key  = "REPLACE(REPLACE(sta_key, '.', ''), ' ', '')")))

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
#' Read the tables and columns from the following [CalCOFI API](https://api.calcofi.io)
#' and display as an interactive table (as `DT::datatable()`):
#' - [api.calcofi.io/db_tables](https://api.calcofi.io/db_tables)
#' - [api.calcofi.io/db_columns](https://api.calcofi.io/db_columns)
#'
#' @param tables optional character vector of table names to filter; default: `NULL`
#'
#' @return DT::datatable() of tables and columns
#' @importFrom DT datatable
#' @importFrom htmltools div em span strong HTML
#' @importFrom markdown mark
#' @importFrom purrr pmap_chr
#' @importFrom readr read_csv
#' @importFrom dplyr left_join filter select
#' @importFrom glue glue
#' @export
#' @concept database
#'
#' @examples
#' # full catalog
#' cc_db_catalog()
#' # only certain tables
#' cc_db_catalog(tables = c("larvae_counts","nets","tows","stations","cruises"))
cc_db_catalog <- function(tables = NULL){

  d_tbls <- readr::read_csv(
    "https://api.calcofi.io/db_tables", show_col_types = F)
  d_cols <- readr::read_csv(
    "https://api.calcofi.io/db_columns", show_col_types = F)

  d <- d_tbls |>
    dplyr::left_join(
      d_cols,
      by = c("schema","table_type","table"))

  if (!is.null(tables))
    d <- d |>
      dplyr::filter(table %in% tables)

  d |>
    mutate(
      Table = purrr::pmap_chr(
        list(table_type, table, table_description),
        function(table_type, table, table_description, ...){
          ifelse(
            is.na(table_description),
            htmltools::div(
              htmltools::span(style = "font-weight: 400;", htmltools::em(table_type), ":"), htmltools::strong(table)) |>
              as.character(),
            htmltools::div(
              htmltools::span(style = "font-weight: 400;", htmltools::em(table_type), ":"), htmltools::strong(table),
              htmltools::div(
                style = "font-size: 0.75rem; font-weight: 400;",
                htmltools::HTML(markdown::mark(table_description)))) |>
              as.character() ) } ),
      Column = purrr::pmap_chr(
        list(column, column_type, column_description),
        function(column, column_type, column_description, ...){
          ifelse(
            is.na(column_description),
            htmltools::div(
              htmltools::strong(column), htmltools::span(style = "font-weight: 400;", "(", htmltools::em(column_type), ")")) |>
              as.character(),
            htmltools::div(
              htmltools::strong(column), htmltools::span(style = "font-weight: 400;", "(", htmltools::em(column_type), ")"),
              htmltools::div(
                style = "font-size: 0.75rem; font-weight: 400;",
                htmltools::HTML(markdown::mark(column_description)))) |>
              as.character() ) } ) ) |>
    dplyr::select(Table, Column) |>
    DT::datatable(
      # extensions = c("RowGroup", "Buttons"),
      extensions = c("RowGroup"),
      options = list(
        # dom      = "Blfrtip",
        # buttons = c("copy", "csv", "excel", "pdf", "print"),
        dom      = "lfrtip",
        pageLength = 10,
        lengthMenu = c(10, 100, 1000),
        rowGroup = list(
          dataSrc = c(0, 1) ),
        columnDefs = list(
          list(
            visible = F,
            targets = c(0,1) ) ) ),
      escape   = F,
      rownames = F)
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

