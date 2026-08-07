# Cross-shelf transects along CalCOFI lines, plus the climatology and anomaly
# built from them.
#
# ONE implementation, shared by every app that draws a section: apps/ctd-viz
# (Shiny, interpolates server-side), CalCOFI/ctd-transects (static, pre-renders
# matrices). They had diverged into separate private helpers, so a fix to the
# ordering or the binning had to be made twice.
#
# These functions PREPARE data; they do not draw. Rendering stays with each app
# because the two have genuinely different constraints — ctd-viz can call
# MBA::mba.surf() at request time, ctd-transects ships JSON to a browser with no
# R behind it. What they must agree on is what a transect IS.
#
# NOT to be confused with buffer_transect(), which is db-viz-hex's arbitrary
# user-drawn line plus buffer corridor. This file is about CalCOFI lines and
# their numbered stations.

#' Stations along a CalCOFI line, ordered nearshore to offshore
#'
#' A transect here is **a CalCOFI line, ordered by station number ascending**,
#' which is nearshore → offshore (`calcofi4r` treats station > 60 as offshore;
#' station 100 on line 76.7 sits at roughly -124.3° lon). That is well defined
#' for every cruise without asking the user to pick endpoints, which is what
#' makes a whole archive of sections pre-renderable.
#'
#' `order_occ` — the order stations were occupied during the cruise — is
#' deliberately NOT used: it is the ship's track, so its direction is whichever
#' way the ship steamed, and it is NULL on roughly half the release's cast rows.
#'
#' @section The `x` argument:
#' `x = "occupied"` (default) measures distance between the stations this cruise
#' actually occupied, so the section fills the plot and shows the data at its
#' largest. `x = "line"` measures each station's distance along the FULL line
#' geometry, so two cruises that sampled different subsets are directly
#' comparable in width — at the cost of blank space where a cruise stopped short.
#' That matters: line 93.3 has not been sampled past station 90 since 2025-01,
#' though 113 of the 130 cruises before that reached station 120, so recent
#' sections silently span a shorter distance than historical ones.
#'
#' @param con DuckDB connection to a release (see [cc_get_db()]).
#' @param line CalCOFI line, e.g. `93.3`.
#' @param cruise_key cruise to restrict to; `NULL` for every cruise on the line.
#' @param dataset_key source dataset (default `"calcofi_ctd-cast"`).
#' @param x `"occupied"` (default) or `"line"` — see above.
#' @return Tibble: `cruise_key`, `grid_key`, `sta`, `lon`, `lat`, `datetime`,
#'   `data_stage`, `dist_km`.
#' @export
#' @concept transect
#' @importFrom dplyr tbl filter select mutate arrange group_by ungroup collect
#'   left_join distinct row_number lag if_else
cc_transect_stations <- function(
  con,
  line,
  cruise_key  = NULL,
  dataset_key = "calcofi_ctd-cast",
  x           = c("occupied", "line")
) {
  x <- match.arg(x)

  grid <- dplyr::tbl(con, "grid") |>
    dplyr::filter(!is.na(line), !is.na(station), line == !!line) |>
    dplyr::select(grid_key, sta = station) |>
    dplyr::collect()
  if (!nrow(grid))
    stop("no grid stations on line ", line, call. = FALSE)

  # Station positions are the MEAN of every cast ever recorded at that station,
  # taken from the release itself. Two alternatives were rejected:
  #
  #   * the release's `grid.geom_ctr` is WKB, so reading it needs ST_X/ST_Y and
  #     therefore the spatial extension — which a connection handed in by a
  #     caller may not have loaded (cc_get_db() does not), and silently LOADing
  #     one into someone else's connection is a side effect this has no business
  #     having;
  #   * the bundled `cc_grid` is a different vintage: its `sta_lin` is a
  #     TRUNCATED INTEGER (93 for line 93.3, 76 for 76.7), so it cannot be joined
  #     to the release's decimal lines without inventing a mapping.
  #
  # Deriving from the casts also keeps the geometry self-consistent with the data
  # being plotted, and every station that can appear on a transect necessarily
  # has casts.
  ctr <- DBI::dbGetQuery(con, glue::glue("
    SELECT grid_key,
           AVG(longitude) AS lon,
           AVG(latitude)  AS lat
    FROM sample
    WHERE dataset_key = '{dataset_key}' AND sample_type = 'cast'
      AND grid_key IN ({paste0(\"'\", grid$grid_key, \"'\", collapse = ',')})
      AND isfinite(longitude) AND isfinite(latitude)
    GROUP BY grid_key"))
  grid <- dplyr::left_join(grid, ctr, by = "grid_key") |>
    dplyr::filter(!is.na(lon)) |>
    dplyr::arrange(sta)
  if (!nrow(grid))
    stop("no cast positions for any station on line ", line, call. = FALSE)

  smp <- dplyr::tbl(con, "sample") |>
    dplyr::filter(dataset_key == !!dataset_key, sample_type == "cast",
                  grid_key %in% !!grid$grid_key)
  if (!is.null(cruise_key))
    smp <- dplyr::filter(smp, cruise_key %in% !!cruise_key)
  smp <- smp |>
    dplyr::select(sample_key, cruise_key, grid_key,
                  latitude, longitude, datetime, data_stage) |>
    dplyr::collect()
  if (!nrow(smp)) return(smp[0, ])

  # one cast per (cruise, station): obs is already single-direction, but a
  # re-occupied station would otherwise double the column
  smp <- smp |>
    dplyr::arrange(cruise_key, grid_key,
                   dplyr::desc(substr(sample_key, nchar(sample_key), nchar(sample_key)) == "d"),
                   datetime) |>
    dplyr::distinct(cruise_key, grid_key, .keep_all = TRUE)

  out <- smp |>
    dplyr::left_join(grid, by = "grid_key") |>
    dplyr::mutate(
      # prefer the ACTUAL cast position; fall back to the nominal station centre
      lon = dplyr::coalesce(longitude, lon),
      lat = dplyr::coalesce(latitude,  lat)) |>
    dplyr::arrange(cruise_key, sta)

  if (identical(x, "line")) {
    # distance along the whole line, so every cruise shares one ruler
    line_dist <- grid |>
      dplyr::mutate(dist_km = .cumdist(lon, lat)) |>
      dplyr::select(grid_key, dist_km)
    out <- dplyr::left_join(out, line_dist, by = "grid_key")
  } else {
    out <- out |>
      dplyr::group_by(cruise_key) |>
      dplyr::mutate(dist_km = .cumdist(lon, lat)) |>
      dplyr::ungroup()
  }

  out |>
    dplyr::select(cruise_key, grid_key, sta, lon, lat, datetime, data_stage,
                  dist_km)
}

#' Cumulative great-circle distance (km) along an ordered path
#' @noRd
.cumdist <- function(lon, lat) {
  n <- length(lon)
  if (n < 2) return(rep(0, n))
  r <- 6371.0088
  p1 <- lat[-n] * pi / 180; p2 <- lat[-1] * pi / 180
  dp <- p2 - p1
  dl <- (lon[-1] - lon[-n]) * pi / 180
  a  <- sin(dp / 2)^2 + cos(p1) * cos(p2) * sin(dl / 2)^2
  c(0, cumsum(2 * r * asin(pmin(1, sqrt(a)))))
}

#' Observations along a transect, binned by depth
#'
#' Long/tidy, one row per (station, depth bin, variable) — the shape both the
#' matrix builder and the climatology consume.
#'
#' Depth is binned because CTD sensors sample continuously (47.283 m, 47.916 m,
#' …), so grouping by exact depth deduplicates almost nothing and the
#' native-resolution profile is jagged with sensor precision rather than signal.
#'
#' @param con DuckDB connection to a release.
#' @param line,cruise_key,dataset_key as in [cc_transect_stations()].
#' @param variables `measurement_type`s to return.
#' @param depth_max deepest bin, m (default 500 — a handful of casts reach
#'   5000 m, and letting them set the axis squashes every standard cast into the
#'   top tenth of the plot).
#' @param depth_bin bin width, m (default 5).
#' @param x passed to [cc_transect_stations()].
#' @return Tibble: `cruise_key`, `sta`, `dist_km`, `depth_m`, `variable`, `value`.
#' @export
#' @concept transect
cc_transect_section <- function(
  con,
  line,
  cruise_key  = NULL,
  variables   = "temperature_ave",
  dataset_key = "calcofi_ctd-cast",
  depth_max   = 500,
  depth_bin   = 5,
  x           = c("occupied", "line")
) {
  sta <- cc_transect_stations(con, line, cruise_key, dataset_key, match.arg(x))
  if (!nrow(sta)) return(sta[0, ])

  obs <- dplyr::tbl(con, "obs") |>
    dplyr::filter(
      dataset_key      == !!dataset_key,
      measurement_type %in% !!variables,
      !is.na(measurement_value),
      !is.na(depth_min_m),
      depth_min_m      <= !!(depth_max + depth_bin),
      grid_key         %in% !!unique(sta$grid_key)) |>
    dplyr::select(cruise_key, grid_key, depth_min_m,
                  variable = measurement_type, value = measurement_value) |>
    dplyr::collect()
  if (!nrow(obs)) return(obs[0, ])

  obs |>
    dplyr::inner_join(
      dplyr::select(sta, cruise_key, grid_key, sta, dist_km),
      by = c("cruise_key", "grid_key")) |>
    dplyr::mutate(depth_m = round(depth_min_m / depth_bin) * depth_bin) |>
    dplyr::filter(depth_m <= depth_max) |>
    dplyr::group_by(cruise_key, sta, dist_km, depth_m, variable) |>
    dplyr::summarize(value = mean(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(cruise_key, sta, depth_m)
}

#' Seasonal climatology for one or more measurement types
#'
#' A baseline per (`grid_key`, depth bin, month), which is the finest grouping
#' the CalCOFI sampling design supports: quarterly-ish cruises over decades give
#' many years per calendar month at a station, but not many days.
#'
#' Deliberately a **plain monthly mean**, not a harmonic fit. Rudnick et al.
#' (2017) fit annual and semiannual harmonics for the CUGN glider climatology,
#' which suits near-continuous glider sampling; CalCOFI's is episodic and
#' unevenly spaced, and a monthly mean is both defensible and legible — someone
#' reading an anomaly can say exactly what it is a departure from. `n` is
#' returned so a thin cell can be filtered rather than silently trusted.
#'
#' @param con DuckDB connection to a release.
#' @param variables `measurement_type`s.
#' @param years two-element baseline range, inclusive, e.g. `c(1993, 2013)`.
#'   Recorded on the result as the `baseline` attribute so a plot can state it.
#' @param dataset_key,depth_max,depth_bin as in [cc_transect_section()].
#' @param min_n minimum observations for a cell to be returned (default 3).
#' @return Tibble: `grid_key`, `month`, `depth_m`, `variable`, `clim_mean`,
#'   `clim_sd`, `clim_n`; with attribute `baseline`.
#' @export
#' @concept transect
cc_climatology <- function(
  con,
  variables   = "temperature_ave",
  years       = c(1993, 2013),
  dataset_key = "calcofi_ctd-cast",
  depth_max   = 500,
  depth_bin   = 5,
  min_n       = 3
) {
  stopifnot(length(years) == 2, years[1] <= years[2])

  out <- dplyr::tbl(con, "obs") |>
    dplyr::filter(
      dataset_key      == !!dataset_key,
      measurement_type %in% !!variables,
      !is.na(measurement_value), !is.na(depth_min_m), !is.na(datetime),
      depth_min_m      <= !!(depth_max + depth_bin)) |>
    dplyr::mutate(
      yr  = as.integer(strftime(datetime, "%Y")),
      mon = as.integer(strftime(datetime, "%m"))) |>
    dplyr::filter(yr >= !!years[1], yr <= !!years[2]) |>
    dplyr::mutate(depth_m = round(depth_min_m / !!depth_bin) * !!depth_bin) |>
    dplyr::filter(depth_m <= !!depth_max) |>
    dplyr::group_by(grid_key, month = mon, depth_m,
                    variable = measurement_type) |>
    dplyr::summarize(
      clim_mean = mean(measurement_value, na.rm = TRUE),
      clim_sd   = stats::sd(measurement_value, na.rm = TRUE),
      clim_n    = dplyr::n(),
      .groups   = "drop") |>
    dplyr::collect() |>
    dplyr::filter(clim_n >= min_n)

  attr(out, "baseline") <- years
  out
}

#' Join a section to a climatology and difference it
#'
#' `anomaly = value - clim_mean`, matched on station, calendar month and depth
#' bin. Cells with no baseline come back `NA` rather than 0 — an unsampled
#' baseline is not a zero anomaly, and collapsing the two is how a map ends up
#' claiming "normal" for somewhere never measured.
#'
#' `anomaly_sd` expresses the departure in baseline standard deviations, which
#' is what makes a 1 °C anomaly interpretable: large in the deep, unremarkable
#' at the surface in spring.
#'
#' @param section from [cc_transect_section()].
#' @param clim from [cc_climatology()].
#' @param stations from [cc_transect_stations()] — supplies `grid_key` and the
#'   month, which `section` does not carry.
#' @return `section` plus `clim_mean`, `clim_sd`, `clim_n`, `anomaly`,
#'   `anomaly_sd`; `baseline` attribute carried through.
#' @export
#' @concept transect
cc_anomaly <- function(section, clim, stations) {
  key <- stations |>
    dplyr::mutate(month = as.integer(strftime(datetime, "%m"))) |>
    dplyr::select(cruise_key, sta, grid_key, month) |>
    dplyr::distinct()

  out <- section |>
    dplyr::left_join(key, by = c("cruise_key", "sta")) |>
    dplyr::left_join(clim, by = c("grid_key", "month", "depth_m", "variable")) |>
    dplyr::mutate(
      anomaly    = value - clim_mean,
      anomaly_sd = dplyr::if_else(!is.na(clim_sd) & clim_sd > 0,
                                  anomaly / clim_sd, NA_real_))

  attr(out, "baseline") <- attr(clim, "baseline")
  out
}

#' Pivot a section (or anomaly) to a station x depth matrix
#'
#' The shape a heatmap wants: `z[[depth]][[station]]`, with `x` the station
#' distances and `y` the depth bins. Returning it from here rather than from each
#' app is what lets ctd-transects ship a matrix as JSON and ctd-viz hand the same
#' numbers to an interpolator.
#'
#' @param section from [cc_transect_section()] or [cc_anomaly()].
#' @param value column to pivot (`"value"`, or `"anomaly"` / `"anomaly_sd"`).
#' @param depths optional depth bins to force (keeps matrices aligned across
#'   cruises); defaults to those present.
#' @return List of `x` (dist_km), `sta`, `y` (depth_m) and `z` (matrix rows =
#'   depths, cols = stations).
#' @export
#' @concept transect
#' @importFrom rlang %||%
cc_transect_matrix <- function(section, value = "value", depths = NULL) {
  stopifnot(value %in% names(section))
  stn <- section |>
    dplyr::distinct(sta, dist_km) |>
    dplyr::arrange(sta)
  y <- depths %||% sort(unique(section$depth_m))

  z <- lapply(y, function(d) {
    row <- section[section$depth_m == d, ]
    vapply(stn$sta, function(s) {
      v <- row[[value]][row$sta == s]
      if (length(v)) as.numeric(v[1]) else NA_real_
    }, numeric(1))
  })

  list(x = stn$dist_km, sta = stn$sta, y = y, z = z)
}

# Columns referenced via NSE inside dplyr verbs. Declared so R CMD check does not
# report each as an undefined global — they are data-frame columns, not objects.
utils::globalVariables(c(
  "station", "grid_key", "sta", "sample_key", "cruise_key", "latitude",
  "longitude", "datetime", "data_stage", "lon", "lat", "dist_km",
  "dataset_key", "sample_type", "measurement_type", "measurement_value",
  "depth_min_m", "depth_m", "variable", "value", "yr", "mon", "month",
  "clim_mean", "clim_sd", "clim_n", "anomaly", "cc_grid"))
