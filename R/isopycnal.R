# conditions on isopycnals: where a CTD cast reaches a sigma-theta surface, and what the water on
# that surface is like, from the full-resolution 1 m bins (obs_ctd_full) ----------------------------
#
# Rasmus Swalethorp asked (email "Missing data", 2026-11-12) for "conditions and their anomalies
# along defined isopycnals ... calculated at the 1-m bin resolution": an anomaly at a fixed depth
# mixes a displaced pycnocline with a change in the water itself, and on a density surface the two
# separate. The rules here mirror calcofi4db's (one rule, two languages, as cc_climatology() mirrors
# build_climatology()):
# - sigma-theta is the sensor-pair average by the provider's flags (calcofi4db::ctd_sigma_theta_ave()
#   = combine_sensor_pair(): 8/9 dropped, a 1 or a 2 selects that sensor, otherwise the mean);
# - a surface's depth is calcofi4db::profile_threshold_depth(direction = "above"), the tested rule
#   behind the nitracline: the shallowest bin whose sigma-theta reaches the surface, interpolated
#   linearly with the bin above it;
# - a property on the surface is its own 1 m profile interpolated linearly to that depth
#   (stats::approx(rule = 1)), after its flagged values (cc_qual_ok_sql()) are dropped.

#' Conditions on isopycnals, from the 1 m CTD casts
#'
#' For each CTD cast and each potential-density surface in `sigma` (sigma-theta, kg m^-3): the
#' depth at which the cast reaches it and the value of each of `variables` on it, from the
#' full-resolution 1 m bins of `obs_ctd_full`. Below the mixed layer an anomaly at a fixed depth
#' is mostly the pycnocline moving up or down; on a density surface that displacement is the
#' surface's depth, and what is left in temperature, salinity, oxygen or nitrate is a change in
#' the water mass itself. On one surface temperature and salinity move together (warmer is
#' saltier), so their departure there is the spice of the water.
#'
#' **Rules** (the same as `calcofi4db`'s, which builds the release's derived products):
#'
#' - **sigma-theta** is the sensor-pair average of `sigma_theta_1` and `sigma_theta_2` by the
#'   provider's flags (`calcofi4db::ctd_sigma_theta_ave()`): a sensor flagged 8 or 9 is dropped, a
#'   1 or a 2 selects that sensor, otherwise the mean. Repeated rows for one cast and depth are
#'   averaged first.
#' - **depth of a surface** is `calcofi4db::profile_threshold_depth(direction = "above")`, the rule
#'   behind the nitracline: walking down the cast, the first 1 m bin whose sigma-theta reaches the
#'   surface (within 1e-4), the depth interpolated linearly between it and the bin above.
#'   Only the shallowest crossing is reported, so a density inversion below it does not move the
#'   surface. `status` says why a surface has no depth: `"at_surface"` (the cast's first bin,
#'   within `surface_max_m` of the surface, is already as dense: the surface outcrops),
#'   `"no_bracket"` (the same, but the first bin is deeper), `"not_reached"` (the cast never gets
#'   this dense: it is too shallow, or the surface lies below it) or `"no_data"` (fewer than two
#'   good bins). Only `"ok"` carries a depth and values.
#' - **a property on the surface** is its own profile interpolated linearly to that depth, after
#'   values its provider flags (8/9, [cc_qual_ok_sql()]) are dropped; `NA` where the property has no
#'   bin on one side of the depth.
#' - **casts**: the down cast of each station occupation, the up cast only where there is none
#'   (`direction = "down"`, the release's thinned series makes the same choice), or every cast.
#'
#' The cruise's month and year are its median cast date (`cruise.month_median`, release
#' v2026.11+), the rule every CalCOFI baseline files a cast under; see [cc_cruise_month()].
#'
#' @param con DuckDB connection to a release opened with the supplemental tables:
#'   `cc_get_db(supplemental = TRUE)`.
#' @param sigma numeric: the surfaces, sigma-theta in kg m^-3. The default, 24.0 to 27.0 by 0.2,
#'   is the provider's list (25.0 to 27.0 by 0.2, Rasmus Swalethorp, September 2026) extended to
#'   24.0, where the upper thermocline sits in fall; 27.0 is reached by about a quarter of the down
#'   casts (the median density at the bottom of a 500 m cast is 26.98).
#' @param cruise_key optional cruise key(s) (`YYYY-MM-NODC`) to restrict to.
#' @param site_key optional station(s) (`sample.site_key`, e.g. `"093.3 030.0"`).
#' @param variables `obs_ctd_full` measurement types to carry onto each surface; the default is the
#'   sensor-pair averages the release publishes (the station-corrected oxygen and the bottle-fitted
#'   nitrate estimate, which exist only on casts with bottle data).
#' @param direction `"down"` (default): one cast per station occupation, the down cast where
#'   there is one; `"both"`: every cast.
#' @param surface_max_m,min_n as in `calcofi4db::profile_threshold_depth()`: the deepest first bin
#'   that counts as the surface (m) and the fewest bins a cast needs for `"at_surface"`.
#' @return Tibble, one row per cast x surface x variable: `sample_key`, `cruise_key`, `site_key`,
#'   `grid_key`, `longitude`, `latitude`, `datetime`, `data_stage`, `year` and `month` (the
#'   cruise's), `sigma_theta`, `status`, `variable` and `value`. The surface's depth is the row
#'   with `variable = "depth_m"`, present for every cast and surface; a property's row is present
#'   only where `status = "ok"` and the property has a value there.
#' @seealso [cc_isopycnal_anomaly()] for the departure from a baseline on the same surface.
#' @examples
#' \dontrun{
#' con <- cc_get_db(supplemental = TRUE)
#' iso <- cc_isopycnal(con, sigma = c(25, 25.5, 26), cruise_key = "2026-11-33P4",
#'                     site_key = "093.3 030.0")
#' tidyr::pivot_wider(iso, names_from = variable, values_from = value)
#' }
#' @export
#' @concept isopycnal
cc_isopycnal <- function(
  con,
  sigma         = seq(24, 27, by = 0.2),
  cruise_key    = NULL,
  site_key      = NULL,
  variables     = c("temperature_ave", "salinity_ave_corr", "oxygen_ml_l_ave_sta_corr",
                    "est_nitrate_sta_corr"),
  direction     = c("down", "both"),
  surface_max_m = 20,
  min_n         = 3
) {
  direction <- match.arg(direction)
  .cc_isopycnal_check(con, sigma, cruise_key, site_key, variables, surface_max_m, min_n)
  casts <- .cc_isopycnal_casts(con, cruise_key, site_key, direction)
  .cc_isopycnal_compute(con, casts, sigma, unique(variables), surface_max_m, min_n)
}

#' Anomalies on isopycnals, against a baseline on the same surfaces
#'
#' [cc_isopycnal()] for the casts asked for, beside a baseline built the way the release builds
#' its `climatology` (`calcofi4db::build_climatology()`), but on density surfaces instead of 10 m
#' depth bins: for each station (`site_key`), the cruise's month (or its season, `season = TRUE`:
#' DJF / MAM / JJA / SON), each surface and each variable, the mean over the casts of cruises in
#' `years`, kept where at least `min_cruises` distinct cruises contribute. `anomaly = value -
#' clim_mean`; a cast with no baseline cell comes back `NA`, never 0. For `variable = "depth_m"`
#' the anomaly is the surface's displacement in metres, positive deeper.
#'
#' The baseline is computed from `obs_ctd_full` at the stations of the casts asked for, so it
#' reads every cruise of the window at those stations (on the 1 m bins, as the anomaly is).
#'
#' @inheritParams cc_isopycnal
#' @param years two-element baseline range of cruise years, inclusive (default 1993-2013, the
#'   release's window).
#' @param season `FALSE` (default) for the monthly baseline; `TRUE` for the seasonal one.
#' @param min_cruises minimum distinct cruises behind a baseline cell (the release keeps >= 5).
#' @return [cc_isopycnal()]'s columns plus `clim_mean`, `clim_sd`, `clim_n` (casts), `n_cruises`,
#'   `anomaly`, `anomaly_sd`; attributes `baseline` (the years), `period` (`"month"` or
#'   `"season"`) and `climatology` (the baseline cells: `site_key`, `month`, `sigma_theta`,
#'   `variable`, `clim_mean`, `clim_sd`, `clim_n`, `n_cruises`).
#' @examples
#' \dontrun{
#' con <- cc_get_db(supplemental = TRUE)
#' an  <- cc_isopycnal_anomaly(con, sigma = seq(24.4, 26.6, by = 0.2),
#'                             cruise_key = "2026-11-33P4", season = TRUE)
#' # the displacement of each surface and the temperature change on it, station by station
#' dplyr::filter(an, variable %in% c("depth_m", "temperature_ave"))
#' }
#' @export
#' @concept isopycnal
cc_isopycnal_anomaly <- function(
  con,
  sigma         = seq(24, 27, by = 0.2),
  cruise_key    = NULL,
  site_key      = NULL,
  variables     = c("temperature_ave", "salinity_ave_corr", "oxygen_ml_l_ave_sta_corr",
                    "est_nitrate_sta_corr"),
  years         = c(1993, 2013),
  season        = FALSE,
  min_cruises   = 5,
  direction     = c("down", "both"),
  surface_max_m = 20,
  min_n         = 3
) {
  direction <- match.arg(direction)
  stopifnot(is.numeric(years), length(years) == 2, years[1] <= years[2],
            is.logical(season), length(season) == 1, !is.na(season),
            is.numeric(min_cruises), length(min_cruises) == 1, min_cruises >= 1)
  .cc_isopycnal_check(con, sigma, cruise_key, site_key, variables, surface_max_m, min_n)
  target <- .cc_isopycnal_casts(con, cruise_key, site_key, direction)
  base_c <- if (nrow(target)) .cc_isopycnal_casts(con, NULL, unique(stats::na.omit(target$site_key)),
                                                  direction, years = years) else target[0, ]
  casts  <- dplyr::distinct(dplyr::bind_rows(target, base_c))
  iso    <- .cc_isopycnal_compute(con, casts, sigma, unique(variables), surface_max_m, min_n)

  per_of <- function(m) if (season) cc_season(m) else as.integer(m)
  clim <- iso |>
    dplyr::filter(sample_key %in% base_c$sample_key, !is.na(value)) |>
    dplyr::mutate(per = per_of(month)) |>
    dplyr::group_by(site_key, per, sigma_theta, variable) |>
    dplyr::summarize(
      clim_mean = mean(value), clim_sd = stats::sd(value),
      clim_n = dplyr::n(), n_cruises = dplyr::n_distinct(cruise_key), .groups = "drop") |>
    dplyr::filter(n_cruises >= min_cruises)

  out <- iso |>
    dplyr::filter(sample_key %in% target$sample_key) |>
    dplyr::mutate(per = per_of(month)) |>
    dplyr::left_join(clim, by = c("site_key", "per", "sigma_theta", "variable")) |>
    dplyr::select(-per) |>
    dplyr::mutate(
      clim_n     = as.integer(clim_n), n_cruises = as.integer(n_cruises),
      anomaly    = value - clim_mean,
      anomaly_sd = dplyr::if_else(!is.na(clim_sd) & clim_sd > 0, anomaly / clim_sd, NA_real_))

  # the baseline cells on the months they cover, as cc_climatology() returns them
  clim <- if (season) {
    dplyr::inner_join(clim, data.frame(per = rep(1:4, each = 3), month = c(12L, 1:11)),
                      by = "per", relationship = "many-to-many")
  } else dplyr::mutate(clim, month = per)
  clim <- clim |>
    dplyr::mutate(month = as.integer(month), clim_n = as.integer(clim_n),
                  n_cruises = as.integer(n_cruises)) |>
    dplyr::select(site_key, month, sigma_theta, variable, clim_mean, clim_sd, clim_n, n_cruises) |>
    dplyr::arrange(site_key, month, sigma_theta, variable)

  attr(out, "baseline")    <- years
  attr(out, "period")      <- if (season) "season" else "month"
  attr(out, "climatology") <- clim
  out
}

# internals ----

utils::globalVariables(c("sigma_theta", "status", "n_obs", "obs_year", "n_bins_sampled", ".v"))

# the arguments, and the tables the computation reads, with the error a reader can act on
.cc_isopycnal_check <- function(con, sigma, cruise_key, site_key, variables, surface_max_m, min_n) {
  stopifnot(is.numeric(sigma), length(sigma) >= 1, all(is.finite(sigma)),
            is.null(cruise_key) || is.character(cruise_key),
            is.null(site_key) || is.character(site_key),
            is.character(variables), !anyNA(variables), all(grepl("^[a-z0-9_]+$", variables)),
            !"depth_m" %in% variables,
            is.numeric(surface_max_m), length(surface_max_m) == 1, surface_max_m >= 0,
            is.numeric(min_n), length(min_n) == 1, min_n >= 1)
  tbls <- DBI::dbListTables(con)
  if (!"obs_ctd_full" %in% tbls)
    stop("cc_isopycnal(): the 1 m casts are the supplemental table `obs_ctd_full`; open the ",
         "release with cc_get_db(supplemental = TRUE).", call. = FALSE)
  miss <- setdiff("sample", tbls)
  if (length(miss))
    stop("cc_isopycnal(): the connection has no `", miss, "` table.", call. = FALSE)
  invisible(TRUE)
}

# the casts: one row per CTD cast with its station and its cruise's year and month
.cc_isopycnal_casts <- function(con, cruise_key = NULL, site_key = NULL, direction = "down",
                                years = NULL) {
  q   <- function(x) paste0("'", gsub("'", "''", x), "'", collapse = ", ")
  whr <- c("s.dataset_key = 'calcofi_ctd-cast'", "s.sample_type = 'cast'",
           if (length(cruise_key)) glue::glue("s.cruise_key IN ({q(cruise_key)})"),
           if (length(site_key))   glue::glue("s.site_key IN ({q(site_key)})"))
  has_cm <- "cruise" %in% DBI::dbListTables(con) &&
    all(c("month_median", "date_median") %in% DBI::dbListFields(con, "cruise"))
  # the cruise's year and month: its median cast date (v2026.11+), else the key's YYYY-MM, else the
  # cast's date (cc_cruise_month()'s rule)
  key_part <- function(part) {
    rng <- if (part == "month") "6, 2" else "1, 4"
    glue::glue("CASE WHEN regexp_matches(s.cruise_key, '^[0-9]{{4}}-(0[1-9]|1[0-2])-') ",
               "THEN CAST(substr(s.cruise_key, {rng}) AS INTEGER) ",
               "ELSE CAST({part}(s.datetime) AS INTEGER) END")
  }
  yr  <- if (has_cm) glue::glue("COALESCE(CAST(year(c.date_median) AS INTEGER), {key_part('year')})") else key_part("year")
  mon <- if (has_cm) glue::glue("COALESCE(CAST(c.month_median AS INTEGER), {key_part('month')})") else key_part("month")
  jn  <- if (has_cm) "LEFT JOIN cruise c ON c.cruise_key = s.cruise_key" else ""
  dir_sql <- if (direction == "down")
    "QUALIFY row_number() OVER (PARTITION BY regexp_replace(sample_key, '[du]$', '') ORDER BY right(sample_key, 1)) = 1" else ""
  has_stage <- "data_stage" %in% DBI::dbListFields(con, "sample")
  sql <- glue::glue("
    SELECT * FROM (
      SELECT s.sample_key, s.cruise_key, s.site_key, s.grid_key, s.longitude, s.latitude, s.datetime,
             {if (has_stage) 's.data_stage' else 'NULL::VARCHAR AS data_stage'},
             {yr} AS year, {mon} AS month
      FROM sample s {jn}
      WHERE {paste(whr, collapse = ' AND ')}) x
    {if (is.null(years)) '' else glue::glue('WHERE year BETWEEN {as.integer(years[1])} AND {as.integer(years[2])}')}
    {dir_sql}")
  out <- tibble::as_tibble(DBI::dbGetQuery(con, sql))
  out$year  <- as.integer(out$year)
  out$month <- as.integer(out$month)
  dplyr::arrange(out, cruise_key, sample_key)
}

# the sensor-pair rule as one SQL expression (calcofi4db::combine_sensor_pair_sql(), verbatim)
.cc_sensor_pair_sql <- function(v1, v2, q1, q2) {
  nq <- function(q) glue::glue("regexp_replace(CAST({q} AS VARCHAR), '\\.0+$', '')")
  n1 <- nq(q1); n2 <- nq(q2)
  a  <- glue::glue("CASE WHEN {n1} IN ('8', '9') THEN NULL ELSE {v1} END")
  b  <- glue::glue("CASE WHEN {n2} IN ('8', '9') THEN NULL ELSE {v2} END")
  say1 <- glue::glue("({n1} = '1' OR {n2} = '1')")
  say2 <- glue::glue("({n1} = '2' OR {n2} = '2')")
  as.character(glue::glue(
    "CASE WHEN COALESCE({say1}, FALSE) AND NOT COALESCE({say2}, FALSE) AND ({a}) IS NOT NULL THEN ({a})
          WHEN COALESCE({say2}, FALSE) AND NOT COALESCE({say1}, FALSE) AND ({b}) IS NOT NULL THEN ({b})
          WHEN ({a}) IS NOT NULL AND ({b}) IS NOT NULL THEN (({a}) + ({b})) / 2
          ELSE COALESCE(({a}), ({b})) END"))
}

# the computation, in DuckDB: casts x surfaces -> depth and properties
.cc_isopycnal_compute <- function(con, casts, sigma, variables, surface_max_m, min_n) {
  empty <- tibble::tibble(
    sample_key = character(), cruise_key = character(), site_key = character(),
    grid_key = character(), longitude = double(), latitude = double(),
    datetime = as.POSIXct(character(), tz = "UTC"), data_stage = character(),
    year = integer(), month = integer(), sigma_theta = double(), status = character(),
    variable = character(), value = double())
  if (!nrow(casts)) return(empty)

  tag <- paste0("cc_iso_", paste(sample(c(letters, 0:9), 8, replace = TRUE), collapse = ""))
  nm  <- function(x) paste0(tag, "_", x)
  on.exit(for (x in c("casts", "piv", "sig", "iso", "prop"))
    try(DBI::dbExecute(con, glue::glue("DROP TABLE IF EXISTS {nm(x)}")), silent = TRUE), add = TRUE)
  q   <- function(x) paste0("'", gsub("'", "''", x), "'", collapse = ", ")
  lst <- function(x) paste0("[", paste(format(x, digits = 15, scientific = FALSE, trim = TRUE),
                                       collapse = ", "), "]")

  DBI::dbWriteTable(con, nm("casts"), as.data.frame(casts[, "sample_key"]), temporary = TRUE)

  # one row per cast x depth: both sigma-theta sensors with their flags, each property averaged over
  # its unflagged rows (repeated rows for a cast and depth are averaged, as profile_threshold_depth()
  # averages duplicate depths)
  qok   <- cc_qual_ok_sql("f")
  nqual <- "regexp_replace(CAST(f.measurement_qual AS VARCHAR), '\\.0+$', '')"
  piv <- paste(c(
    glue::glue("avg(f.measurement_value) FILTER (WHERE f.measurement_type = 'sigma_theta_{1:2}') AS s{1:2}"),
    glue::glue("max({nqual}) FILTER (WHERE f.measurement_type = 'sigma_theta_{1:2}') AS q{1:2}"),
    glue::glue("avg(f.measurement_value) FILTER (WHERE f.measurement_type = '{variables}' AND {qok}) AS \"{variables}\"")),
    collapse = ",\n      ")
  cruise_sql <- glue::glue("f.cruise_key IN ({q(unique(casts$cruise_key))}) AND")
  DBI::dbExecute(con, glue::glue("
    CREATE TEMP TABLE {nm('piv')} AS
    SELECT f.sample_key, f.depth_min_m AS z,
      {piv}
    FROM obs_ctd_full f
    WHERE {cruise_sql}
      f.sample_key IN (SELECT sample_key FROM {nm('casts')})
      AND f.measurement_type IN ({q(c('sigma_theta_1', 'sigma_theta_2', variables))})
      AND f.depth_min_m IS NOT NULL AND f.depth_min_m >= 0
      AND f.measurement_value IS NOT NULL AND isfinite(f.measurement_value)
    GROUP BY f.sample_key, f.depth_min_m"))

  # the sigma-theta profile of each cast: the value, the bin above it and the running maximum, so the
  # first bin to reach a surface is the one where the running maximum first reaches it
  DBI::dbExecute(con, glue::glue("
    CREATE TEMP TABLE {nm('sig')} AS
    WITH p AS (
      SELECT sample_key, z, {.cc_sensor_pair_sql('s1', 's2', 'q1', 'q2')} AS sig FROM {nm('piv')}),
    w AS (
      SELECT sample_key, z, sig,
             lag(z)   OVER o AS z0,
             lag(sig) OVER o AS sig0,
             max(sig) OVER (PARTITION BY sample_key ORDER BY z ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS cm,
             row_number() OVER o AS i,
             count(*) OVER (PARTITION BY sample_key) AS n
      FROM p WHERE sig IS NOT NULL AND isfinite(sig)
      WINDOW o AS (PARTITION BY sample_key ORDER BY z))
    SELECT *, lag(cm) OVER (PARTITION BY sample_key ORDER BY z) AS cm0 FROM w"))

  # each cast x surface: the crossing bin, its status and its interpolated depth
  # (profile_threshold_depth(direction = 'above'), tolerance 1e-4, clamped to the bracket)
  DBI::dbExecute(con, glue::glue("
    CREATE TEMP TABLE {nm('iso')} AS
    WITH surf AS (SELECT unnest({lst(sort(unique(sigma)))}::DOUBLE[]) AS sigma_theta),
    cs  AS (SELECT c.sample_key, s.sigma_theta FROM {nm('casts')} c CROSS JOIN surf s),
    nn  AS (SELECT sample_key, count(*) AS n FROM {nm('sig')} GROUP BY sample_key),
    hit AS (
      SELECT w.sample_key, s.sigma_theta, w.i, w.n, w.z, w.z0, w.sig, w.sig0
      FROM {nm('sig')} w JOIN surf s
        ON w.cm >= s.sigma_theta - 1e-4 AND (w.cm0 IS NULL OR w.cm0 < s.sigma_theta - 1e-4))
    SELECT cs.sample_key, cs.sigma_theta,
      CASE WHEN COALESCE(nn.n, 0) < 2 THEN 'no_data'
           WHEN h.sample_key IS NULL THEN 'not_reached'
           WHEN h.i = 1 THEN CASE WHEN h.z <= {surface_max_m} AND h.n >= {min_n} THEN 'at_surface' ELSE 'no_bracket' END
           ELSE 'ok' END AS status,
      CASE WHEN COALESCE(nn.n, 0) >= 2 AND h.i > 1 THEN
        least(greatest(h.z0 + (cs.sigma_theta - h.sig0) * (h.z - h.z0) / (h.sig - h.sig0), h.z0), h.z) END AS depth_m
    FROM cs LEFT JOIN nn USING (sample_key) LEFT JOIN hit h USING (sample_key, sigma_theta)"))

  # each property's own profile, then interpolated linearly to the surface's depth (approx(rule = 1))
  DBI::dbExecute(con, glue::glue("
    CREATE TEMP TABLE {nm('prop')} AS
    SELECT sample_key, z, variable, val
    FROM (UNPIVOT (SELECT sample_key, z, {paste0('\"', variables, '\"', collapse = ', ')} FROM {nm('piv')})
          ON {paste0('\"', variables, '\"', collapse = ', ')} INTO NAME variable VALUE val)
    WHERE val IS NOT NULL AND isfinite(val)"))

  vals <- DBI::dbGetQuery(con, glue::glue("
    WITH x AS (
      SELECT i.sample_key, i.sigma_theta, i.depth_m, v.variable
      FROM {nm('iso')} i CROSS JOIN (SELECT unnest([{q(variables)}]) AS variable) v
      WHERE i.status = 'ok')
    SELECT x.sample_key, x.sigma_theta, x.variable,
      CASE WHEN lo.z = hi.z THEN lo.val
           WHEN lo.z IS NOT NULL AND hi.z IS NOT NULL
             THEN lo.val + (hi.val - lo.val) * (x.depth_m - lo.z) / (hi.z - lo.z) END AS value
    FROM x
    ASOF LEFT JOIN {nm('prop')} lo ON lo.sample_key = x.sample_key AND lo.variable = x.variable AND lo.z <= x.depth_m
    ASOF LEFT JOIN {nm('prop')} hi ON hi.sample_key = x.sample_key AND hi.variable = x.variable AND hi.z >= x.depth_m"))
  iso <- DBI::dbGetQuery(con, glue::glue("SELECT sample_key, sigma_theta, status, depth_m FROM {nm('iso')}"))

  status <- dplyr::select(iso, sample_key, sigma_theta, status)
  out <- dplyr::bind_rows(
    dplyr::transmute(iso, sample_key, sigma_theta, variable = "depth_m", value = depth_m),
    dplyr::filter(vals, !is.na(value))) |>
    dplyr::left_join(status, by = c("sample_key", "sigma_theta")) |>
    dplyr::inner_join(casts, by = "sample_key") |>
    dplyr::mutate(variable = factor(variable, levels = c("depth_m", variables))) |>
    dplyr::arrange(cruise_key, sample_key, sigma_theta, variable) |>
    dplyr::mutate(variable = as.character(variable)) |>
    dplyr::select(dplyr::all_of(names(empty)))
  tibble::as_tibble(out)
}
