# ─── bio ↔ env matching helpers ───────────────────────────────────────────────
#
# These functions relate biological observations (net-tow ichthyoplankton or
# zooplankton biomass) to environmental observations (CTD-bottle measurements)
# by matching them on time and space, on the fly, against the Parquet files of
# a frozen CalCOFI release on Google Cloud Storage (GCS).
#
# They supersede the retired Postgres Plumber API endpoints `zooplankton_biomass`,
# `itis_ichthyodata` and `ichthyodata`, which relied on pre-built `uunet2ctd*`
# match tables that are not part of the DuckDB/GCS releases. The matching SQL is
# modeled on the proven on-the-fly matcher in `int-app` (`prep_splot()`).
#
# The emitted SQL reads directly from public GCS Parquet URLs
# (`read_parquet('https://storage.googleapis.com/calcofi-db/.../{table}.parquet')`),
# so it is fully portable: `return_sql = TRUE` hands back the exact, interpolated
# query, which anyone can re-run in DuckDB (R, Python, or the CLI) and get
# identical rows. That reproducibility hook is the single source of truth for the
# `int-app` download bundle.

# internal: resolve "latest" to a concrete release version (e.g. "v2026.05.14")
.cc_resolve_version <- function(version = "latest") {
  if (!identical(version, "latest")) {
    if (!grepl("^v\\d{4}\\.\\d{2}", version))
      stop("Version must be 'latest' or in format vYYYY.MM[.DD] (e.g. 'v2026.05.14')")
    return(version)
  }
  tryCatch(
    trimws(readLines(
      "https://storage.googleapis.com/calcofi-db/ducklake/releases/latest.txt",
      warn = FALSE)[1]),
    error = function(e) stop(
      "Could not resolve 'latest' release version. ",
      "Pass an explicit version like 'v2026.05.14'."))
}

# internal: base URL for a release's single-file Parquet tables
.cc_parquet_base <- function(version) {
  glue::glue(
    "https://storage.googleapis.com/calcofi-db/ducklake/releases/{version}/parquet")
}

# internal: get a DuckDB connection with httpfs + spatial loaded.
# returns list(con=, created=) so the caller can disconnect a con it created.
.cc_match_con <- function(con = NULL) {
  created <- FALSE
  if (is.null(con)) {
    if (!requireNamespace("duckdb", quietly = TRUE))
      stop("Package 'duckdb' is required. Install with: install.packages('duckdb')")
    con     <- DBI::dbConnect(duckdb::duckdb())
    created <- TRUE
  }
  # idempotent — needed for https read_parquet() and ST_Distance_Sphere()
  DBI::dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
  DBI::dbExecute(con, "INSTALL spatial; LOAD spatial;")
  list(con = con, created = created)
}

# internal: pull the distinct read_parquet() source URLs out of a SQL string
.cc_extract_source_urls <- function(sql) {
  hits <- regmatches(sql, gregexpr("read_parquet\\('[^']+'", sql))[[1]]
  sort(unique(gsub("read_parquet\\('|'$", "", hits)))
}

# internal: assemble the full matching SQL from bio + env subqueries
.cc_build_match_sql <- function(bio, env, max_dist_km, max_time_hr, join_method) {

  # method picks which "nearest" rows survive before the per-bio average;
  # "average" keeps every env observation inside the window (no filter)
  where_nearest <- switch(
    join_method,
    nearest_time = "WHERE time_diff_hr = mn_time_diff_hr",
    nearest_dist = "WHERE dist_km = mn_dist_km",
    average      = "")

  glue::glue(
    "WITH bio AS (
{bio}
),
env AS (
{env}
),
matched AS (
  -- temporal interval join: every env observation within ± max_time_hr
  SELECT
    bio.*,
    env.* EXCLUDE (env_lon, env_lat),
    abs(epoch(bio.bio_datetime) - epoch(env.env_datetime)) / 3600.0 AS time_diff_hr,
    ST_Distance_Sphere(
      ST_Point(bio.bio_lon, bio.bio_lat),
      ST_Point(env.env_lon, env.env_lat)) / 1000.0                  AS dist_km
  FROM bio
  JOIN env
    ON env.env_datetime BETWEEN bio.bio_datetime - INTERVAL '{max_time_hr} hours'
                            AND bio.bio_datetime + INTERVAL '{max_time_hr} hours'
),
within AS (
  -- spatial filter: keep pairs within max_dist_km
  SELECT * FROM matched
  WHERE dist_km <= {max_dist_km}
),
ranked AS (
  SELECT
    *,
    min(time_diff_hr) OVER (PARTITION BY bio_id) AS mn_time_diff_hr,
    min(dist_km)      OVER (PARTITION BY bio_id) AS mn_dist_km
  FROM within
)
-- one row per bio observation (× measurement_type): env values aggregated
SELECT
  * EXCLUDE (
    env_id, env_value, env_datetime, env_depth_m,
    time_diff_hr, dist_km, mn_time_diff_hr, mn_dist_km),
  count(*)                                            AS n_env,
  avg(env_value)                                      AS env_value,
  CASE WHEN count(*) = 1 THEN 0
       ELSE coalesce(stddev_samp(env_value), 0) END   AS env_value_sd,
  avg(env_depth_m)                                    AS env_depth_m,
  min(env_datetime)                                   AS env_datetime_min,
  max(env_datetime)                                   AS env_datetime_max,
  avg(dist_km)                                        AS dist_km,
  avg(time_diff_hr)                                   AS time_diff_hr
FROM ranked
{where_nearest}
GROUP BY ALL
ORDER BY bio_id",
    .trim = FALSE)
}

# internal: build the env (CTD-bottle) subquery for a given measurement type
.cc_env_sql <- function(
    env_var,
    version,
    depth_m_min = NULL,
    depth_m_max = NULL,
    date_min    = NULL,
    date_max    = NULL,
    pad_hours   = 0) {

  base <- .cc_parquet_base(version)

  filt <- c(
    glue::glue("bm.measurement_type = '{env_var}'"),
    "bm.measurement_value IS NOT NULL",
    "c.datetime_utc IS NOT NULL",
    "c.lon_dec IS NOT NULL",
    "c.lat_dec IS NOT NULL")

  if (!is.null(depth_m_min))
    filt <- c(filt, glue::glue("b.depth_m >= {depth_m_min}"))
  if (!is.null(depth_m_max))
    filt <- c(filt, glue::glue("b.depth_m <= {depth_m_max}"))
  # pad the env date window by the match tolerance so boundary pairs survive
  if (!is.null(date_min))
    filt <- c(filt, glue::glue(
      "c.datetime_utc >= TIMESTAMP '{date_min}' - INTERVAL '{pad_hours} hours'"))
  if (!is.null(date_max))
    filt <- c(filt, glue::glue(
      "c.datetime_utc <= TIMESTAMP '{date_max}' + INTERVAL '{pad_hours} hours'"))

  glue::glue(
    "  SELECT
    bm.bottle_measurement_id AS env_id,
    c.datetime_utc           AS env_datetime,
    c.lon_dec                AS env_lon,
    c.lat_dec                AS env_lat,
    bm.measurement_value     AS env_value,
    b.depth_m                AS env_depth_m,
    bm.measurement_type      AS measurement_type
  FROM read_parquet('{base}/bottle_measurement.parquet') bm
  JOIN read_parquet('{base}/bottle.parquet') b ON bm.bottle_id = b.bottle_id
  JOIN read_parquet('{base}/casts.parquet')  c ON b.cast_id    = c.cast_id
  WHERE {filt_sql}",
    filt_sql = paste(filt, collapse = "\n    AND "))
}

# internal: build the ichthyoplankton bio subquery (shared by name + taxon wrappers)
.cc_bio_sql_ichthyo <- function(
    version,
    species_where,        # SQL boolean fragment filtering sp.* / i.* (or NULL)
    taxon_cte   = NULL,    # optional "WITH RECURSIVE ..." prefix string
    life_stage  = NULL,
    date_min    = NULL,
    date_max    = NULL) {

  base <- .cc_parquet_base(version)

  filt <- c(
    "i.tally IS NOT NULL",
    "i.measurement_type IS NULL",   # NULL measurement_type == count (tally) rows
    "t.time_start IS NOT NULL",
    "s.longitude IS NOT NULL",
    "s.latitude IS NOT NULL")
  if (!is.null(species_where))
    filt <- c(filt, species_where)
  if (!is.null(life_stage))
    filt <- c(filt, glue::glue(
      "i.life_stage IN ({vals})",
      vals = paste0("'", gsub("'", "''", life_stage), "'", collapse = ", ")))
  if (!is.null(date_min))
    filt <- c(filt, glue::glue("t.time_start >= TIMESTAMP '{date_min}'"))
  if (!is.null(date_max))
    filt <- c(filt, glue::glue("t.time_start <= TIMESTAMP '{date_max}'"))

  prefix <- if (is.null(taxon_cte)) "" else paste0(taxon_cte, "\n  ")

  glue::glue(
    "  {prefix}SELECT
    i.ichthyo_uuid::VARCHAR AS bio_id,
    t.time_start            AS bio_datetime,
    s.longitude             AS bio_lon,
    s.latitude              AS bio_lat,
    n.std_haul_factor * i.tally / nullif(n.prop_sorted, 0) AS bio_value,
    sp.scientific_name,
    sp.common_name,
    sp.worms_id,
    i.life_stage,
    i.tally
  FROM read_parquet('{base}/ichthyo.parquet') i
  JOIN read_parquet('{base}/species.parquet') sp ON i.species_id = sp.species_id
  JOIN read_parquet('{base}/net.parquet')     n  ON i.net_uuid   = n.net_uuid
  JOIN read_parquet('{base}/tow.parquet')     t  ON n.tow_uuid   = t.tow_uuid
  JOIN read_parquet('{base}/site.parquet')    s  ON t.site_uuid  = s.site_uuid
  WHERE {filt_sql}",
    filt_sql = paste(filt, collapse = "\n    AND "))
}

#' Match biological to environmental observations
#'
#' Core engine relating biological observations to environmental observations by
#' matching them on time and space. Builds a single DuckDB SQL string — a
#' temporal interval join (within `max_time_hr`) plus a spatial filter
#' (`ST_Distance_Sphere` within `max_dist_km`) — that reads directly from the
#' Parquet files of a frozen CalCOFI release on Google Cloud Storage, runs it,
#' and attaches the fully-interpolated SQL and query metadata as attributes.
#'
#' Most users want a wrapper ([cc_match_ichthyo_by_name()],
#' [cc_match_ichthyo_by_taxon()] or [cc_match_zooplankton_biomass()]) rather
#' than calling `cc_match_bio_env()` directly.
#'
#' @param bio SQL `SELECT` string producing the biological side. **Must** yield
#'   columns `bio_id` (unique per observation), `bio_datetime` (`TIMESTAMP`),
#'   `bio_lon`, `bio_lat` (decimal degrees) and `bio_value` (`DOUBLE`). Any
#'   additional columns (e.g. `scientific_name`) are carried through to the
#'   output as grouping keys.
#' @param env SQL `SELECT` string producing the environmental side. **Must**
#'   yield exactly `env_id`, `env_datetime` (`TIMESTAMP`), `env_lon`, `env_lat`,
#'   `env_value` (`DOUBLE`), `env_depth_m` (`DOUBLE`) and `measurement_type`.
#' @param max_dist_km Maximum match distance in kilometers (default: 2).
#' @param max_time_hr Maximum match time difference in hours (default: 6).
#' @param join_method One of `"nearest_time"` (default — keep the env
#'   observation(s) closest in time, averaging ties), `"nearest_dist"` (closest
#'   in space) or `"average"` (average every env observation in the window).
#' @param con Optional DuckDB connection. If `NULL` (default) a temporary
#'   in-memory connection is created (and closed again when `collect = TRUE`).
#' @param version Release version string (e.g. `"v2026.05.14"`) or `"latest"`
#'   (default). Recorded in the query metadata; the actual table URLs come from
#'   `bio` / `env`.
#' @param collect If `TRUE` (default) execute and return a tibble; if `FALSE`
#'   return a lazy `dplyr::tbl()` reference.
#' @param return_sql If `TRUE`, return the interpolated SQL string (with
#'   `query_meta` attached) **without executing**. Default: `FALSE`.
#'
#' @return When `return_sql = TRUE`, a length-1 character vector of SQL with
#'   `attr(., "query_meta")`. Otherwise a tibble (or lazy `tbl`) of one row per
#'   biological observation per `measurement_type`, with the matched
#'   `env_value`, `env_value_sd`, `env_depth_m`, `n_env`, `dist_km` and
#'   `time_diff_hr`, plus all `bio` columns. The result carries `attr(., "sql")`
#'   and `attr(., "query_meta")` (package + release version, parameters and GCS
#'   source URLs).
#' @export
#' @concept match
#'
#' @examples
#' \dontrun{
#' # build bio + env subqueries yourself, or use a wrapper
#' d <- cc_match_ichthyo_by_name("Sardinops sagax", env_var = "temperature")
#'
#' # the exact, portable SQL that produced it
#' cat(attr(d, "sql"))
#' str(attr(d, "query_meta"))
#' }
#' @importFrom glue glue
#' @importFrom DBI dbConnect dbExecute dbGetQuery dbDisconnect
#' @importFrom tibble as_tibble
#' @importFrom dplyr tbl sql
cc_match_bio_env <- function(
    bio,
    env,
    max_dist_km = 2,
    max_time_hr = 6,
    join_method = c("nearest_time", "nearest_dist", "average"),
    con         = NULL,
    version     = "latest",
    collect     = TRUE,
    return_sql  = FALSE) {

  join_method <- match.arg(join_method)
  stopifnot(
    is.character(bio), length(bio) == 1L, nzchar(bio),
    is.character(env), length(env) == 1L, nzchar(env),
    is.numeric(max_dist_km), max_dist_km > 0,
    is.numeric(max_time_hr), max_time_hr > 0)

  # strip trailing semicolons / whitespace so the subqueries nest cleanly
  bio <- sub(";\\s*$", "", trimws(bio))
  env <- sub(";\\s*$", "", trimws(env))

  sql <- as.character(
    .cc_build_match_sql(bio, env, max_dist_km, max_time_hr, join_method))

  query_meta <- list(
    package_version = as.character(utils::packageVersion("calcofi4r")),
    release_version = .cc_resolve_version(version),
    params          = list(
      max_dist_km = max_dist_km,
      max_time_hr = max_time_hr,
      join_method = join_method),
    source_urls     = .cc_extract_source_urls(sql),
    generated_at    = format(Sys.time(), tz = "UTC", usetz = TRUE))

  if (return_sql) {
    attr(sql, "query_meta") <- query_meta
    return(sql)
  }

  cc <- .cc_match_con(con)
  # only disconnect a con we created, and only once we've materialized results;
  # a lazy tbl (collect = FALSE) must keep its connection alive
  on.exit(
    if (cc$created && collect) DBI::dbDisconnect(cc$con, shutdown = TRUE),
    add = TRUE)

  if (collect) {
    res <- tibble::as_tibble(DBI::dbGetQuery(cc$con, sql))
    query_meta$n_rows       <- nrow(res)
    attr(res, "sql")        <- sql
    attr(res, "query_meta") <- query_meta
    return(res)
  }

  res <- dplyr::tbl(cc$con, dplyr::sql(sql))
  attr(res, "sql")        <- sql
  attr(res, "query_meta") <- query_meta
  if (cc$created)
    attr(res, "con") <- cc$con   # keep a handle so the connection isn't GC'd
  res
}

#' Match ichthyoplankton to environmental data by scientific name
#'
#' Relates net-tow ichthyoplankton (fish egg / larva counts, standardized to
#' `std_tally`) to CTD-bottle environmental measurements, filtering the
#' biological side by scientific name. Supersedes the retired `/ichthyodata`
#' Plumber endpoint.
#'
#' @param scientific_name Character vector of scientific names to match against
#'   `species.scientific_name`.
#' @param env_var Environmental `measurement_type` from the `bottle_measurement`
#'   table (default: `"temperature"`). See [cc_list_measurement_types()].
#' @param exact_match If `TRUE` (default) match names exactly; if `FALSE` match
#'   case-insensitively as substrings (`ILIKE '%name%'`).
#' @param life_stage Optional character vector restricting `ichthyo.life_stage`
#'   (e.g. `"larva"`, `"egg"`).
#' @param date_min,date_max Optional date bounds (`Date` or `"YYYY-MM-DD"`
#'   string) on the tow start time.
#' @param depth_m_min,depth_m_max Optional depth bounds (meters) on the bottle
#'   environmental observations.
#' @param max_dist_km,max_time_hr Match tolerances. Default to 2 km / 6 hr, or
#'   5 km / 72 hr when `relax_matching = TRUE`; an explicit value always wins.
#' @param relax_matching Convenience flag mirroring the old API's
#'   `relax_matching`: widens the default tolerances to 5 km / 72 hr.
#' @param join_method,con,version,collect,return_sql Passed to
#'   [cc_match_bio_env()].
#'
#' @return See [cc_match_bio_env()].
#' @export
#' @concept match
#'
#' @examples
#' \dontrun{
#' # Pacific sardine larvae vs. temperature, Q1 2018, relaxed matching
#' # (note: CTD-bottle env data ends 2021-05, so Q1 2018 is used as the
#' #  recurring worked example across calcofi4r, int-app and the docs book)
#' d <- cc_match_ichthyo_by_name(
#'   "Sardinops sagax",
#'   env_var        = "temperature",
#'   life_stage     = "larva",
#'   date_min       = "2018-01-01",
#'   date_max       = "2018-03-31",
#'   relax_matching = TRUE)
#' }
cc_match_ichthyo_by_name <- function(
    scientific_name,
    env_var        = "temperature",
    exact_match    = TRUE,
    life_stage     = NULL,
    date_min       = NULL,
    date_max       = NULL,
    depth_m_min    = NULL,
    depth_m_max    = NULL,
    max_dist_km    = NULL,
    max_time_hr    = NULL,
    relax_matching = FALSE,
    join_method    = c("nearest_time", "nearest_dist", "average"),
    con            = NULL,
    version        = "latest",
    collect        = TRUE,
    return_sql     = FALSE) {

  join_method <- match.arg(join_method)
  stopifnot(is.character(scientific_name), length(scientific_name) >= 1L)
  if (is.null(max_dist_km)) max_dist_km <- if (relax_matching) 5  else 2
  if (is.null(max_time_hr)) max_time_hr <- if (relax_matching) 72 else 6
  version <- .cc_resolve_version(version)

  nm <- gsub("'", "''", scientific_name)
  species_where <- if (isTRUE(exact_match)) {
    glue::glue(
      "sp.scientific_name IN ({vals})",
      vals = paste0("'", nm, "'", collapse = ", "))
  } else {
    paste0(
      "(",
      paste0("sp.scientific_name ILIKE '%", nm, "%'", collapse = " OR "),
      ")")
  }

  bio_sql <- .cc_bio_sql_ichthyo(
    version, species_where,
    life_stage = life_stage, date_min = date_min, date_max = date_max)
  env_sql <- .cc_env_sql(
    env_var, version,
    depth_m_min = depth_m_min, depth_m_max = depth_m_max,
    date_min = date_min, date_max = date_max, pad_hours = max_time_hr)

  cc_match_bio_env(
    bio_sql, env_sql,
    max_dist_km = max_dist_km, max_time_hr = max_time_hr,
    join_method = join_method, con = con, version = version,
    collect = collect, return_sql = return_sql)
}

#' Match ichthyoplankton to environmental data by WoRMS taxon subtree
#'
#' Relates net-tow ichthyoplankton to CTD-bottle environmental measurements,
#' filtering the biological side to a taxon and all of its descendants. The
#' subtree is resolved with a recursive walk of `taxon.parentNameUsageID` over
#' the WoRMS authority. Supersedes the retired `/itis_ichthyodata` endpoint,
#' replacing the dead ITIS `path` regex with the WoRMS `worms_id` hierarchy.
#'
#' @param worms_id Integer vector of WoRMS `taxonID`(s); the match includes each
#'   id and every descendant taxon.
#' @param env_var Environmental `measurement_type` (default: `"temperature"`).
#' @param life_stage Optional character vector restricting `ichthyo.life_stage`.
#' @param date_min,date_max Optional date bounds on the tow start time.
#' @param depth_m_min,depth_m_max Optional depth bounds (meters) on the bottle
#'   environmental observations.
#' @param max_dist_km,max_time_hr Match tolerances. Default to 2 km / 6 hr, or
#'   5 km / 72 hr when `relax_matching = TRUE`; an explicit value always wins.
#' @param relax_matching Widen the default tolerances to 5 km / 72 hr.
#' @param join_method,con,version,collect,return_sql Passed to
#'   [cc_match_bio_env()].
#'
#' @return See [cc_match_bio_env()].
#' @export
#' @concept match
#'
#' @examples
#' \dontrun{
#' # Engraulis (anchovies) — WoRMS taxonID 125620 — and all descendants
#' d <- cc_match_ichthyo_by_taxon(125620, env_var = "salinity")
#' }
cc_match_ichthyo_by_taxon <- function(
    worms_id,
    env_var        = "temperature",
    life_stage     = NULL,
    date_min       = NULL,
    date_max       = NULL,
    depth_m_min    = NULL,
    depth_m_max    = NULL,
    max_dist_km    = NULL,
    max_time_hr    = NULL,
    relax_matching = FALSE,
    join_method    = c("nearest_time", "nearest_dist", "average"),
    con            = NULL,
    version        = "latest",
    collect        = TRUE,
    return_sql     = FALSE) {

  join_method <- match.arg(join_method)
  worms_id <- suppressWarnings(as.integer(worms_id))
  stopifnot(length(worms_id) >= 1L, !anyNA(worms_id))
  if (is.null(max_dist_km)) max_dist_km <- if (relax_matching) 5  else 2
  if (is.null(max_time_hr)) max_time_hr <- if (relax_matching) 72 else 6
  version <- .cc_resolve_version(version)
  base    <- .cc_parquet_base(version)

  # recursive walk of the WoRMS taxon tree: seed taxa + every descendant
  taxon_cte <- glue::glue(
    "WITH RECURSIVE taxon_tree AS (
      SELECT taxonID
      FROM read_parquet('{base}/taxon.parquet')
      WHERE authority = 'WoRMS' AND taxonID IN ({ids})
    UNION ALL
      SELECT t.taxonID
      FROM read_parquet('{base}/taxon.parquet') t
      JOIN taxon_tree tt ON t.parentNameUsageID = tt.taxonID
      WHERE t.authority = 'WoRMS'
  )",
    ids = paste(worms_id, collapse = ", "))

  bio_sql <- .cc_bio_sql_ichthyo(
    version,
    species_where = "sp.worms_id IN (SELECT taxonID FROM taxon_tree)",
    taxon_cte     = taxon_cte,
    life_stage    = life_stage, date_min = date_min, date_max = date_max)
  env_sql <- .cc_env_sql(
    env_var, version,
    depth_m_min = depth_m_min, depth_m_max = depth_m_max,
    date_min = date_min, date_max = date_max, pad_hours = max_time_hr)

  cc_match_bio_env(
    bio_sql, env_sql,
    max_dist_km = max_dist_km, max_time_hr = max_time_hr,
    join_method = join_method, con = con, version = version,
    collect = collect, return_sql = return_sql)
}

#' Match zooplankton biomass to environmental data
#'
#' Relates net-tow zooplankton displacement-volume biomass (`net.totalplankton`
#' or `net.smallplankton`) to CTD-bottle environmental measurements. Supersedes
#' the retired `/zooplankton_biomass` endpoint.
#'
#' @param env_var Environmental `measurement_type` (default: `"temperature"`).
#' @param biomass_type Which net biomass column to use: `"totalplankton"`
#'   (default) or `"smallplankton"`.
#' @param date_min,date_max Optional date bounds on the tow start time.
#' @param depth_m_min,depth_m_max Optional depth bounds (meters) on the bottle
#'   environmental observations.
#' @param max_dist_km,max_time_hr Match tolerances. Default to 2 km / 6 hr, or
#'   5 km / 72 hr when `relax_matching = TRUE`; an explicit value always wins.
#' @param relax_matching Widen the default tolerances to 5 km / 72 hr.
#' @param join_method,con,version,collect,return_sql Passed to
#'   [cc_match_bio_env()].
#'
#' @return See [cc_match_bio_env()]. `bio_value` is the selected biomass column.
#' @export
#' @concept match
#'
#' @examples
#' \dontrun{
#' d <- cc_match_zooplankton_biomass(env_var = "temperature")
#' }
cc_match_zooplankton_biomass <- function(
    env_var        = "temperature",
    biomass_type   = c("totalplankton", "smallplankton"),
    date_min       = NULL,
    date_max       = NULL,
    depth_m_min    = NULL,
    depth_m_max    = NULL,
    max_dist_km    = NULL,
    max_time_hr    = NULL,
    relax_matching = FALSE,
    join_method    = c("nearest_time", "nearest_dist", "average"),
    con            = NULL,
    version        = "latest",
    collect        = TRUE,
    return_sql     = FALSE) {

  biomass_type <- match.arg(biomass_type)
  join_method  <- match.arg(join_method)
  if (is.null(max_dist_km)) max_dist_km <- if (relax_matching) 5  else 2
  if (is.null(max_time_hr)) max_time_hr <- if (relax_matching) 72 else 6
  version <- .cc_resolve_version(version)
  base    <- .cc_parquet_base(version)

  filt <- c(
    glue::glue("n.{biomass_type} IS NOT NULL"),
    "t.time_start IS NOT NULL",
    "s.longitude IS NOT NULL",
    "s.latitude IS NOT NULL")
  if (!is.null(date_min))
    filt <- c(filt, glue::glue("t.time_start >= TIMESTAMP '{date_min}'"))
  if (!is.null(date_max))
    filt <- c(filt, glue::glue("t.time_start <= TIMESTAMP '{date_max}'"))

  bio_sql <- glue::glue(
    "  SELECT
    n.net_uuid::VARCHAR AS bio_id,
    t.time_start        AS bio_datetime,
    s.longitude         AS bio_lon,
    s.latitude          AS bio_lat,
    n.{biomass_type}    AS bio_value,
    '{biomass_type}'    AS biomass_type,
    n.side,
    t.tow_type_key
  FROM read_parquet('{base}/net.parquet') n
  JOIN read_parquet('{base}/tow.parquet')  t ON n.tow_uuid  = t.tow_uuid
  JOIN read_parquet('{base}/site.parquet') s ON t.site_uuid = s.site_uuid
  WHERE {filt_sql}",
    filt_sql = paste(filt, collapse = "\n    AND "))

  env_sql <- .cc_env_sql(
    env_var, version,
    depth_m_min = depth_m_min, depth_m_max = depth_m_max,
    date_min = date_min, date_max = date_max, pad_hours = max_time_hr)

  cc_match_bio_env(
    bio_sql, env_sql,
    max_dist_km = max_dist_km, max_time_hr = max_time_hr,
    join_method = join_method, con = con, version = version,
    collect = collect, return_sql = return_sql)
}
