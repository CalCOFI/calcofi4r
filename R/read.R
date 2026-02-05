#' Get variables from CalCOFI API
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated because the CalCOFI API is being phased out
#' in favor of direct DuckDB database access. Use [cc_list_measurement_types()]
#' to see available oceanographic measurement types, or query the database
#' directly with [cc_get_db()].
#'
#' @return data frame with columns:
#' - `category`: category, e.g. "Oceanographic"
#' - `table_field`: unique identifier of the variable given by the form of table.field
#' - `plot_title`: title used in a typical time series plot output
#' - `plot_label`: y-axis label used in a typical time series plot output
#' @export
#' @import httr2 readr
#' @concept read
#' @examples
#' \dontrun{
#' # deprecated - use instead:
#' cc_list_measurement_types()
#' }
get_variables <- function(){
  lifecycle::deprecate_warn(
    "1.1.0",
    "get_variables()",
    "cc_list_measurement_types()",
    details = "The CalCOFI API is being phased out. Use DuckDB access via cc_get_db() instead.")

  req_vars <- req_perform(request("https://api.calcofi.io/variables"))
  stopifnot(!req_vars %>% httr2::resp_is_error())
  req_vars %>% resp_body_raw() %>% read_csv(show_col_types = F)
}

#' Get cruises from CalCOFI API
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated because the CalCOFI API is being phased out
#' in favor of direct DuckDB database access. Use [cc_read_cruise()] to
#' read cruise data, or query the database directly with [cc_get_db()].
#'
#' @return data frame with columns:
#' - `cruise_id`: unique identifier for cruise
#' - `date_beg`: date cruise began
#' - `date_end`: date cruise ended
#' - `lon_min`: longitude, minimum (as in for bounding box of cruise)
#' - `lon_max`: longitude, minimum (as in for bounding box of cruise)
#' - `lat_min`: latitude, minimum (as in for bounding box of cruise)
#' - `lat_max`: latitude, minimum (as in for bounding box of cruise)
#' - `n_casts`: number of oceanographic CTD casts made for given cruise
#' @export
#' @import httr2 readr
#' @concept read
#' @examples
#' \dontrun{
#' # deprecated - use instead:
#' cc_read_cruise()
#' }
get_cruises <- function(){
  lifecycle::deprecate_warn(
    "1.1.0",
    "get_cruises()",
    "cc_read_cruise()",
    details = "The CalCOFI API is being phased out. Use DuckDB access via cc_get_db() instead.")

  req_vars <- req_perform(request("https://api.calcofi.io/cruises"))
  stopifnot(!req_vars %>% httr2::resp_is_error())
  req_vars %>% resp_body_raw() %>% read_csv(show_col_types = F)
}

#' Get raster of interpolated values from CalCOFI API
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated because the CalCOFI API is being phased out
#' in favor of direct DuckDB database access. Query bottle measurement data
#' with [cc_read_measurements()] and [cc_read_casts()], then use
#' [pts_to_rast_idw()] for spatial interpolation.
#'
#' @param variable Variable to fetch from the CalCOFI API. One of `table_field` values from `get_variables()`.
#'   Default is `"ctdcast_bottle.t_deg_c"`.
#' @param cruise_id Cruise identifier. One of `cruise_id` values from `get_cruises()`.
#'   Default is the first cruise "1949-03-01-C-31CR".
#' @param depth_m_min Minimum depth range in meters, e.g. 0. Default is `NULL`, as in not filtered.
#' @param depth_m_max Maximum depth range in meters, e.g. 5351. Default is `NULL`, as in not filtered.
#' @param out_tif Output path to write raster (*.tif)
#' @concept read
#' @import httr2
#' @importFrom raster raster
#' @return path to output raster in GeoTIFF format (*.tif)
#' @export
#'
#' @examples
#' \dontrun{
#' # deprecated - use DuckDB queries + pts_to_rast_idw() instead:
#' con <- cc_get_db()
#' d <- DBI::dbGetQuery(con, "
#'   SELECT c.lon_dec, c.lat_dec, bm.measurement_value
#'   FROM bottle_measurement bm
#'   JOIN bottle b ON bm.bottle_id = b.bottle_id
#'   JOIN casts c ON b.cast_id = c.cast_id
#'   WHERE bm.measurement_type = 'temperature'")
#' }
get_raster <- function(
  variable = "ctdcast_bottle.t_deg_c",
  cruise_id = "1949-03-01-C-31CR",
  depth_m_min = NULL, depth_m_max = NULL,
  out_tif){

  lifecycle::deprecate_warn(
    "1.1.0",
    "get_raster()",
    details = c(
      "The CalCOFI API is being phased out.",
      "i" = "Query data with cc_get_db() and use pts_to_rast_idw() for interpolation."))

  req <- request("https://api.calcofi.io")
  resp <- try(req %>%
    req_url_path_append("raster") %>%
    req_url_query(
      variable    = variable,
      cruise_id   = cruise_id,
      depth_m_min = depth_m_min,
      depth_m_max = depth_m_max) %>%
    req_perform())

  if ("try-error" %in% class(resp)){
    last_response() %>%
      resp_body_json() %>%
      .$message %>%
      stop()
  }
  resp %>%
    resp_body_raw() %>%
    writeBin(out_tif)
  out_tif
}

#' Get timeseries summary from CalCOFI API
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated because the CalCOFI API is being phased out
#' in favor of direct DuckDB database access. Query bottle measurement data
#' with [cc_get_db()] and aggregate using dplyr for time series analysis.
#'
#' @param variable Variable to fetch from the CalCOFI API. One of `table_field` values from `get_variables()`.
#'   Default is `"ctdcast_bottle.t_deg_c"`.
#' @param aoi_wkt Area of interest (AOI), spatially described as
#'   [well-known text (WKT)](https://en.wikipedia.org/wiki/Well-known_text_representation_of_geometry).
#' @param depth_m_min Minimum depth range in meters, e.g. 0. Default is `NULL`, as in not filtered.
#' @param depth_m_max Maximum depth range in meters, e.g. 5351. Default is `NULL`, as in not filtered.
#' @param date_beg Beginning of date range, e.g."1949-02-28". Default is `NULL`, as in not filtered.
#' @param date_end End of date range, e.g. "2020-01-26". Default is `NULL`, as in not filtered.
#' @param time_step Time step over which to summarize. One of: a sequential
#'   increment ("decade","year","year.quarter","year.month","year.week","date"),
#'   or a climatology ("quarter","month","week","julianday","hour").
#'   Default is `"year"`.
#' @param stats Statistics to show per `date_step`. Acceptable values include any combination of:
#'   "avg", "median", "min", "max", "sd" or "p#"
#'   where "sd" is the standard deviation and
#'   "p#" represents the percentile value 0 to 100 within available range of values.
#'   Default is `c("p10", "mean", "p90")`.
#' @concept read
#' @return data frame of values
#' @export
#'
#' @examples
#' \dontrun{
#' # deprecated - use DuckDB queries instead:
#' con <- cc_get_db()
#' d <- DBI::dbGetQuery(con, "
#'   SELECT EXTRACT(YEAR FROM c.datetime_utc) AS year,
#'          AVG(bm.measurement_value) AS avg_temp
#'   FROM bottle_measurement bm
#'   JOIN bottle b ON bm.bottle_id = b.bottle_id
#'   JOIN casts c ON b.cast_id = c.cast_id
#'   WHERE bm.measurement_type = 'temperature'
#'   GROUP BY year ORDER BY year")
#' }
get_timeseries <- function(
  variable = "ctdcast_bottle.t_deg_c",
  aoi_wkt = NULL,
  depth_m_min = NULL, depth_m_max = NULL,
  date_beg = NULL, date_end = NULL,
  time_step = "year",
  stats = c("p10", "mean", "p90")){

  lifecycle::deprecate_warn(
    "1.1.0",
    "get_timeseries()",
    details = c(
      "The CalCOFI API is being phased out.",
      "i" = "Query data with cc_get_db() and aggregate with dplyr for time series."))

  req <- request("https://api.calcofi.io") |>
    req_url_path_append("timeseries") |>
    req_url_query(
      variable    = variable,
      aoi_wkt     = aoi_wkt,
      depth_m_min = depth_m_min,
      depth_m_max = depth_m_max,
      date_beg    = date_beg,
      date_end    = date_end,
      time_step   = time_step,
      stats       = paste(stats, collapse=","))
  resp <- try(req_perform(req))

  if ("try-error" %in% class(resp)){
    last_response() %>%
      resp_body_json() %>%
      .$message %>%
      stop()
  }
  resp %>%
    resp_body_raw() %>%
    read_csv(show_col_types = F)
}

# ─── duckdb convenience functions ─────────────────────────────────────────────

#' Read CalCOFI ichthyoplankton (larvae) data
#'
#' Convenience function to read ichthyoplankton (fish larvae) data from the
#' CalCOFI database. The data is stored in the `ichthyo` table.
#'
#' @param version Database version (default: "latest")
#' @param collect If TRUE, collect results into memory. If FALSE, return
#'   lazy dbplyr table (default: TRUE)
#' @param ... Additional filter expressions passed to \code{dplyr::filter()}
#'
#' @return Tibble of ichthyo data (if collect=TRUE) or lazy table
#'
#' @export
#' @concept read
#'
#' @examples
#' \dontrun{
#' # get first 100 ichthyo records
#' ichthyo <- cc_read_ichthyo() |> head(100)
#'
#' # get specific species (lazy query)
#' anchovy <- cc_read_ichthyo(species_id == 19, collect = FALSE)
#' }
#' @importFrom dplyr tbl filter collect
cc_read_ichthyo <- function(..., version = "latest", collect = TRUE) {
  con   <- cc_get_db(version = version)
  table <- dplyr::tbl(con, "ichthyo")

  # apply filters if provided
  dots <- rlang::enquos(...)
  if (length(dots) > 0) {
    table <- dplyr::filter(table, !!!dots)
  }

  if (collect) {
    dplyr::collect(table)
  } else {
    table
  }
}

#' @rdname cc_read_ichthyo
#' @export
cc_read_larvae <- cc_read_ichthyo

#' Read CalCOFI bottle data
#'
#' Convenience function to read bottle sample data from the CalCOFI database.
#'
#' @param version Database version (default: "latest")
#' @param collect If TRUE, collect results into memory. If FALSE, return
#'   lazy dbplyr table (default: TRUE)
#' @param ... Additional filter expressions passed to \code{dplyr::filter()}
#'
#' @return Tibble of bottle data (if collect=TRUE) or lazy table
#'
#' @export
#' @concept read
#'
#' @examples
#' \dontrun{
#' # get all bottle data
#' bottles <- cc_read_bottle()
#'
#' # filter by depth
#' shallow <- cc_read_bottle(depth_m < 100)
#' }
#' @importFrom dplyr tbl filter collect
cc_read_bottle <- function(..., version = "latest", collect = TRUE) {
  con   <- cc_get_db(version = version)
  table <- dplyr::tbl(con, "bottle")

  # apply filters if provided
  dots <- rlang::enquos(...)
  if (length(dots) > 0) {
    table <- dplyr::filter(table, !!!dots)
  }

  if (collect) {
    dplyr::collect(table)
  } else {
    table
  }
}

#' Read CalCOFI cast data
#'
#' Convenience function to read CTD/bottle cast data from the CalCOFI database.
#' The data is stored in the `casts` table.
#'
#' @param version Database version (default: "latest")
#' @param collect If TRUE, collect results into memory. If FALSE, return
#'   lazy dbplyr table (default: TRUE)
#' @param ... Additional filter expressions passed to \code{dplyr::filter()}
#'
#' @return Tibble of casts data (if collect=TRUE) or lazy table
#'
#' @export
#' @concept read
#'
#' @examples
#' \dontrun{
#' # get first 100 casts
#' casts <- cc_read_casts() |> head(100)
#' }
#' @importFrom dplyr tbl filter collect
cc_read_casts <- function(..., version = "latest", collect = TRUE) {
  con   <- cc_get_db(version = version)
  table <- dplyr::tbl(con, "casts")

  # apply filters if provided
  dots <- rlang::enquos(...)
  if (length(dots) > 0) {
    table <- dplyr::filter(table, !!!dots)
  }

  if (collect) {
    dplyr::collect(table)
  } else {
    table
  }
}

#' @rdname cc_read_casts
#' @export
cc_read_cast <- cc_read_casts

#' Execute SQL query on CalCOFI database
#'
#' Convenience function to execute arbitrary SQL queries against
#' a frozen CalCOFI database release.
#'
#' @param sql SQL query string
#' @param version Database version (default: "latest")
#'
#' @return Tibble with query results
#'
#' @export
#' @concept read
#'
#' @examples
#' \dontrun{
#' results <- cc_query("SELECT * FROM ichthyo LIMIT 10")
#' results <- cc_query("SELECT species_id, COUNT(*) as n FROM ichthyo GROUP BY species_id")
#' }
#' @importFrom DBI dbGetQuery
#' @importFrom tibble as_tibble
cc_query <- function(sql, version = "latest") {
  con    <- cc_get_db(version = version)
  result <- DBI::dbGetQuery(con, sql)
  tibble::as_tibble(result)
}

#' List tables in CalCOFI database
#'
#' Lists all available tables in a CalCOFI database release.
#'
#' @param version Database version (default: "latest")
#'
#' @return Character vector of table names
#'
#' @export
#' @concept read
#'
#' @examples
#' \dontrun{
#' cc_list_tables()
#' }
#' @importFrom DBI dbListTables
cc_list_tables <- function(version = "latest") {
  con <- cc_get_db(version = version)
  DBI::dbListTables(con)
}

#' Describe a CalCOFI database table
#'
#' Returns schema information for a table including column names,
#' types, and descriptions (if available).
#'
#' @param table Table name
#' @param version Database version (default: "latest")
#'
#' @return Tibble with column metadata (column_name, data_type, is_nullable)
#'
#' @export
#' @concept read
#'
#' @examples
#' \dontrun{
#' cc_describe_table("ichthyo")
#' cc_describe_table("species")
#' cc_describe_table("casts")
#' }
#' @importFrom DBI dbGetQuery
#' @importFrom tibble as_tibble
#' @importFrom glue glue
cc_describe_table <- function(table, version = "latest") {
  con <- cc_get_db(version = version)

  # check table exists
  tables <- DBI::dbListTables(con)
  if (!table %in% tables) {
    stop(glue::glue("Table '{table}' not found. Use cc_list_tables() to see available tables."))
  }

  # get column info from information_schema
  result <- DBI::dbGetQuery(con, glue::glue("
    SELECT
      column_name,
      data_type,
      is_nullable
    FROM information_schema.columns
    WHERE table_name = '{table}'
    ORDER BY ordinal_position"))

  tibble::as_tibble(result)
}

#' Read CalCOFI species data
#'
#' Convenience function to read species taxonomy data from the CalCOFI database.
#'
#' @param version Database version (default: "latest")
#' @param collect If TRUE, collect results into memory. If FALSE, return
#'   lazy dbplyr table (default: TRUE)
#' @param ... Additional filter expressions passed to \code{dplyr::filter()}
#'
#' @return Tibble of species data (if collect=TRUE) or lazy table
#'
#' @export
#' @concept read
#'
#' @examples
#' \dontrun{
#' species <- cc_read_species()
#' }
#' @importFrom dplyr tbl filter collect
cc_read_species <- function(..., version = "latest", collect = TRUE) {
  con   <- cc_get_db(version = version)
  table <- dplyr::tbl(con, "species")

  dots <- rlang::enquos(...)
  if (length(dots) > 0) {
    table <- dplyr::filter(table, !!!dots)
  }

  if (collect) dplyr::collect(table) else table
}

#' Read CalCOFI cruise data
#'
#' Convenience function to read cruise metadata from the CalCOFI database.
#'
#' @param version Database version (default: "latest")
#' @param collect If TRUE, collect results into memory. If FALSE, return
#'   lazy dbplyr table (default: TRUE)
#' @param ... Additional filter expressions passed to \code{dplyr::filter()}
#'
#' @return Tibble of cruise data (if collect=TRUE) or lazy table
#'
#' @export
#' @concept read
#'
#' @examples
#' \dontrun{
#' cruises <- cc_read_cruise()
#' }
#' @importFrom dplyr tbl filter collect
cc_read_cruise <- function(..., version = "latest", collect = TRUE) {
  con   <- cc_get_db(version = version)
  table <- dplyr::tbl(con, "cruise")

  dots <- rlang::enquos(...)
  if (length(dots) > 0) {
    table <- dplyr::filter(table, !!!dots)
  }

  if (collect) dplyr::collect(table) else table
}

#' Read CalCOFI bottle measurements
#'
#' Convenience function to read oceanographic measurements from the CalCOFI
#' bottle database. Returns data in long format from `bottle_measurement` table.
#'
#' @param measurement_types Character vector of measurement types to include.
#'   Default is NULL (all types). Use \code{cc_list_measurement_types()} to
#'   see available types.
#' @param version Database version (default: "latest")
#' @param collect If TRUE, collect results into memory. If FALSE, return
#'   lazy dbplyr table (default: TRUE)
#' @param ... Additional filter expressions passed to \code{dplyr::filter()}
#'
#' @return Tibble of measurement data (if collect=TRUE) or lazy table
#'
#' @export
#' @concept read
#'
#' @examples
#' \dontrun{
#' # get temperature and salinity measurements
#' temp_sal <- cc_read_measurements(c("temperature", "salinity"))
#' }
#' @importFrom dplyr tbl filter collect
cc_read_measurements <- function(
    ...,
    measurement_types = NULL,
    version = "latest",
    collect = TRUE) {
  con   <- cc_get_db(version = version)
  table <- dplyr::tbl(con, "bottle_measurement")

  # filter by measurement type if specified
  if (!is.null(measurement_types)) {
    table <- dplyr::filter(table, measurement_type %in% measurement_types)
  }

  dots <- rlang::enquos(...)
  if (length(dots) > 0) {
    table <- dplyr::filter(table, !!!dots)
  }

  if (collect) dplyr::collect(table) else table
}

#' List available measurement types
#'
#' Returns all available measurement types in the CalCOFI bottle database.
#'
#' @param version Database version (default: "latest")
#'
#' @return Tibble with measurement_type, description, and units
#'
#' @export
#' @concept read
#'
#' @examples
#' \dontrun{
#' cc_list_measurement_types()
#' }
#' @importFrom DBI dbGetQuery
#' @importFrom tibble as_tibble
cc_list_measurement_types <- function(version = "latest") {
  con <- cc_get_db(version = version)
  result <- DBI::dbGetQuery(con,
    "SELECT measurement_type, description, units
     FROM measurement_type
     ORDER BY measurement_type")
  tibble::as_tibble(result)
}

#' Read CalCOFI tow data
#'
#' Convenience function to read net tow data from the CalCOFI database.
#'
#' @param version Database version (default: "latest")
#' @param collect If TRUE, collect results into memory. If FALSE, return
#'   lazy dbplyr table (default: TRUE)
#' @param ... Additional filter expressions passed to \code{dplyr::filter()}
#'
#' @return Tibble of tow data (if collect=TRUE) or lazy table
#'
#' @export
#' @concept read
#'
#' @examples
#' \dontrun{
#' tows <- cc_read_tow()
#' }
#' @importFrom dplyr tbl filter collect
cc_read_tow <- function(..., version = "latest", collect = TRUE) {
  con   <- cc_get_db(version = version)
  table <- dplyr::tbl(con, "tow")

  dots <- rlang::enquos(...)
  if (length(dots) > 0) {
    table <- dplyr::filter(table, !!!dots)
  }

  if (collect) dplyr::collect(table) else table
}

#' Read CalCOFI site data
#'
#' Convenience function to read sampling site data from the CalCOFI database.
#'
#' @param version Database version (default: "latest")
#' @param collect If TRUE, collect results into memory. If FALSE, return
#'   lazy dbplyr table (default: TRUE)
#' @param ... Additional filter expressions passed to \code{dplyr::filter()}
#'
#' @return Tibble of site data (if collect=TRUE) or lazy table
#'
#' @export
#' @concept read
#'
#' @examples
#' \dontrun{
#' sites <- cc_read_site()
#' }
#' @importFrom dplyr tbl filter collect
cc_read_site <- function(..., version = "latest", collect = TRUE) {
  con   <- cc_get_db(version = version)
  table <- dplyr::tbl(con, "site")

  dots <- rlang::enquos(...)
  if (length(dots) > 0) {
    table <- dplyr::filter(table, !!!dots)
  }

  if (collect) dplyr::collect(table) else table
}
