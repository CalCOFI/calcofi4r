# anomalies in 10 m depth bins at stations, any dataset, against the release's own climatology ----
#
# Rasmus Swalethorp (email "Missing data", 2026-11-13): time series "using the bottle 10-m binned
# data going back to 1949, supplementing with 10-m binned data in the more recent years ... from
# the CTD profile dataset". Both series are in obs_env (the bottle database's samples at their
# depths; the CTD's thinned series) with the 10 m floor bin on every row, and the release's
# climatology holds a baseline for each dataset. Each series is differenced against its OWN
# baseline (calcofi_bottle against calcofi_bottle's): the two are never pooled into one normal.

#' Anomalies in 10 m depth bins, any dataset (bottle 1949-, CTD 1993-)
#'
#' One row per dataset x cruise x station x 10 m depth bin x variable: the mean of the
#' quality-ok observations in the bin (`obs_env`), beside the release's `climatology` cell for
#' the same dataset, station (`site_key`), the cruise's month (or season) and bin, and the
#' difference. This is how the release's apps compute an anomaly: the climatology is built from
#' these same rows (`calcofi4db::build_climatology()`), so a bin's value and its baseline are on
#' the same footing. (A 1 m value from `obs_ctd_full` against this baseline reads about 0.2 °C
#' cool in a stratified layer, because the thinned CTD samples sit toward the top of each bin.)
#'
#' The bottle series (`calcofi_bottle`, 1949-2021 in releases of 2026) and the CTD's
#' (`calcofi_ctd-cast`, 1993 on) overlap for 1993-2021, so the switch from one to the other can
#' be checked rather than assumed. Each is differenced against its own dataset's baseline, never a
#' pooled one. Bottle samples sit at standard depths (0, 10, 20, 30, 50, 75, 100 m ...), so a
#' bottle cast has no value in some bins the CTD fills (40 m, 60 m, ...); [cc_layer_mean()]
#' interpolates each cast in depth before averaging a layer.
#'
#' @param con DuckDB connection to a release ([cc_get_db()]), v2026.10 or later (`obs_env` with
#'   `depth_bin`, `qual_ok` and `cruise_month`; `climatology` grained on `site_key`).
#' @param variables named character: the measurement type of each dataset, the names being the
#'   `dataset_key`s; repeat a name for several types of one dataset. The default pairs bottle
#'   temperature with the CTD's sensor-pair average; salinity is `calcofi_bottle = "salinity"`
#'   with `"calcofi_ctd-cast" = "salinity_ave_corr"`, oxygen `"oxygen_ml_l"` with
#'   `"oxygen_ml_l_ave_sta_corr"`, nitrate `"nitrate"` with `"est_nitrate_sta_corr"`.
#' @param site_key optional station(s), e.g. `"093.3 030.0"`.
#' @param cruise_key optional cruise key(s).
#' @param depth_max deepest bin returned (m, its shallow edge).
#' @param season `FALSE` (default) for the monthly baseline; `TRUE` for the seasonal one (DJF /
#'   MAM / JJA / SON), which also covers a cruise whose month has fewer than five baseline cruises
#'   (fall 1997 is a September cruise).
#' @return Tibble: `dataset_key`, `cruise_key`, `site_key`, `grid_key`, `year` and `month` (the
#'   cruise's), `datetime` (the first observation in the bin), `depth_bin`, `variable`, `value`,
#'   `n_obs`, `clim_mean`, `clim_sd`, `n_cruises`, `anomaly`, `anomaly_sd`; attributes `baseline`
#'   and `period`. A bin with no baseline cell has `anomaly` `NA`, never 0.
#' @seealso [cc_layer_mean()] to average a layer per cast; [cc_isopycnal_anomaly()] for the same
#'   question on density surfaces.
#' @examples
#' \dontrun{
#' con <- cc_get_db()
#' scb <- c("093.3 026.7", "093.3 028.0", "093.3 030.0", "090.0 028.0", "090.0 030.0",
#'          "086.7 033.0", "086.7 035.0")
#' b <- cc_bin_anomaly(con, site_key = scb, depth_max = 200, season = TRUE)
#' cc_layer_mean(b, top = 0, bottom = 40)     # 0-50 m: the bins 0, 10, ..., 40
#' }
#' @export
#' @concept transect
cc_bin_anomaly <- function(
  con,
  variables  = c("calcofi_bottle" = "temperature", "calcofi_ctd-cast" = "temperature_ave"),
  site_key   = NULL,
  cruise_key = NULL,
  depth_max  = 500,
  season     = FALSE
) {
  stopifnot(is.character(variables), length(variables) >= 1, !is.null(names(variables)),
            all(nzchar(names(variables))), !anyNA(variables),
            is.null(site_key) || is.character(site_key),
            is.null(cruise_key) || is.character(cruise_key),
            is.numeric(depth_max), length(depth_max) == 1,
            is.logical(season), length(season) == 1, !is.na(season))
  tbls <- DBI::dbListTables(con)
  need <- c("obs_env", "climatology")
  if (length(setdiff(need, tbls)))
    stop("cc_bin_anomaly(): the connection has no `", paste(setdiff(need, tbls), collapse = "`, `"),
         "`; open a release of v2026.10 or later with cc_get_db().", call. = FALSE)
  flds <- DBI::dbListFields(con, "obs_env")
  miss <- setdiff(c("depth_bin", "qual_ok", "cruise_month", "site_key"), flds)
  if (length(miss))
    stop("cc_bin_anomaly(): this release's obs_env has no `", paste(miss, collapse = "`, `"),
         "` (releases of v2026.10 or later carry them).", call. = FALSE)

  qv  <- function(x) paste0("'", gsub("'", "''", x), "'")
  q   <- function(x) paste(qv(x), collapse = ", ")
  pairs <- paste(glue::glue("(o.dataset_key = {qv(names(variables))} AND o.measurement_type = {qv(unname(variables))})"),
                 collapse = " OR ")
  whr <- c(glue::glue("({pairs})"),
           "o.qual_ok", "o.value IS NOT NULL", "isfinite(o.value)",
           "o.depth_bin IS NOT NULL", "o.site_key IS NOT NULL",
           glue::glue("o.depth_bin <= {depth_max}"),
           if (length(site_key))   glue::glue("o.site_key IN ({q(site_key)})"),
           if (length(cruise_key)) glue::glue("o.cruise_key IN ({q(cruise_key)})"))
  has_cm <- "cruise" %in% tbls && "date_median" %in% DBI::dbListFields(con, "cruise")
  yr  <- if (has_cm) "COALESCE(CAST(year(c.date_median) AS INTEGER), CAST(b.obs_year AS INTEGER))" else "CAST(b.obs_year AS INTEGER)"
  jcr <- if (has_cm) "LEFT JOIN cruise c ON c.cruise_key = b.cruise_key" else ""
  cm  <- if (season) "k.season_mean" else "k.clim_mean"
  cs  <- if (season) "k.season_sd" else "k.clim_sd"
  cn  <- if (season) "k.season_n_cruises" else "k.n_cruises"

  sql <- glue::glue("
    WITH b AS (
      SELECT o.dataset_key, o.cruise_key, o.site_key, o.depth_bin, o.measurement_type AS variable,
             mode(o.grid_key) AS grid_key, any_value(o.cruise_month) AS month,
             min(o.year) AS obs_year, min(o.datetime) AS datetime,
             avg(o.value) AS value, count(*) AS n_obs
      FROM obs_env o
      WHERE {paste(whr, collapse = '\n        AND ')}
      GROUP BY o.dataset_key, o.cruise_key, o.site_key, o.depth_bin, o.measurement_type)
    SELECT b.dataset_key, b.cruise_key, b.site_key, b.grid_key, {yr} AS year, b.month,
           b.datetime, b.depth_bin, b.variable, b.value, b.n_obs,
           {cm} AS clim_mean, {cs} AS clim_sd, {cn} AS n_cruises
    FROM b {jcr}
    LEFT JOIN climatology k
      ON k.dataset_key = b.dataset_key AND k.measurement_type = b.variable AND k.site_key = b.site_key
     AND k.month = b.month AND k.depth_bin = b.depth_bin")
  out <- tibble::as_tibble(DBI::dbGetQuery(con, sql)) |>
    dplyr::mutate(
      year = as.integer(year), month = as.integer(month), depth_bin = as.integer(depth_bin),
      n_obs = as.integer(n_obs), n_cruises = as.integer(n_cruises),
      anomaly    = value - clim_mean,
      anomaly_sd = dplyr::if_else(!is.na(clim_sd) & clim_sd > 0, anomaly / clim_sd, NA_real_)) |>
    dplyr::arrange(dataset_key, cruise_key, site_key, variable, depth_bin)

  win <- DBI::dbGetQuery(con, "SELECT DISTINCT clim_yr_min, clim_yr_max FROM climatology")
  attr(out, "baseline") <- if (nrow(win) == 1) c(win$clim_yr_min, win$clim_yr_max) else NULL
  attr(out, "period")   <- if (season) "season" else "month"
  out
}

#' Layer means per cast from 10 m bins, interpolated in depth
#'
#' Averages a column of [cc_bin_anomaly()] (by default `anomaly`) over a layer of 10 m bins for
#' each dataset x cruise x station x variable. The profile is first interpolated linearly **in
#' depth** onto every bin of the layer, between bins that were sampled and no more than `max_gap`
#' apart (never beyond the shallowest or deepest sampled bin, never in time or between
#' stations), so a bottle cast at standard depths (0, 10, 20, 30, 50 m) and a CTD cast with every
#' bin are averaged over the same depths. A cast is returned only where every bin of the layer
#' has a value. This is the rule of the November 2026 temperature-anomaly page
#' (calcofi.io/workflows/2026-11_temperature-anomaly.html).
#'
#' @param x a tibble from [cc_bin_anomaly()].
#' @param top,bottom the layer's shallowest and deepest bin (their shallow edges, m): 0 and 40 is
#'   0-50 m.
#' @param value the column to average.
#' @param max_gap the widest span between sampled bins to interpolate across (m).
#' @return Tibble: `dataset_key`, `cruise_key`, `site_key`, `year`, `month`, `variable`,
#'   `n_bins_sampled` (bins of the layer that were measured rather than interpolated) and the
#'   layer mean under the name of `value`.
#' @export
#' @concept transect
cc_layer_mean <- function(x, top = 0, bottom = 40, value = "anomaly", max_gap = 60) {
  stopifnot(is.data.frame(x), all(c("dataset_key", "cruise_key", "site_key", "variable",
                                    "depth_bin", value) %in% names(x)),
            is.numeric(top), is.numeric(bottom), top <= bottom, max_gap >= 0)
  bins <- seq(top, bottom, by = 10)
  keys <- intersect(c("dataset_key", "cruise_key", "site_key", "year", "month", "variable"), names(x))
  x$.v <- x[[value]]
  out <- x[!is.na(x$.v), ] |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::group_modify(function(d, k) {
      z <- d$depth_bin; v <- d$.v
      o <- order(z); z <- z[o]; v <- v[o]
      vi <- vapply(bins, function(b) {
        if (b %in% z) return(v[match(b, z)])
        lo <- suppressWarnings(max(z[z < b])); hi <- suppressWarnings(min(z[z > b]))
        if (!is.finite(lo) || !is.finite(hi) || hi - lo > max_gap) return(NA_real_)
        v[match(lo, z)] + (v[match(hi, z)] - v[match(lo, z)]) * (b - lo) / (hi - lo)
      }, numeric(1))
      if (anyNA(vi)) return(tibble::tibble(n_bins_sampled = integer(), .v = double()))
      tibble::tibble(n_bins_sampled = sum(bins %in% z), .v = mean(vi))
    }) |>
    dplyr::ungroup()
  names(out)[names(out) == ".v"] <- value
  out
}
