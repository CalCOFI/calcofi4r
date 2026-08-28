# Effort and denominators (plan D8, 2026-08-28). A bio observation is a count, a count with the effort
# that produced it, or a density a provider already standardized — and those are not one population.
# The Hexagon Explorer averaged count, count/10m2 and count/100m3 for the default sardine without saying
# so; a polygon summary chose the unit with the most rows, which was the bare CUFES count. So:
#   density_per_10m2   areal, depth-integrated: count * std_haul_factor / prop_sorted for oblique and
#                      vertical tows (C1, CB, CV, PV — never the surface manta MT), and published per-m2
#                      densities * 10
#   density_per_1000m3 volumetric: count / prop_sorted / volume_sampled_m3 * 1000 for any tow with a
#                      volume, and published per-1000 m3 densities as is
#   effort_class       count_with_effort | raw_count_no_effort | density_as_published | other_unit
# Areal and volumetric are deliberately NOT converted into each other (that needs the tow's integrated
# depth and a vertical-distribution assumption). The expression is the same bytes in calcofi4r,
# calcofi4py (density_sql) and the explorer's sql/density.sql; a fixture pins it.

#' Units the release publishes as densities (per unit area, per unit volume)
#' @export
#' @concept quality
CC_DENSITY_UNITS <- list(
  area   = c("count/m2", "numberPerMeterSquared"),
  volume = c("count/1000m3"))

#' Gears whose `std_haul_factor` standardizes to a depth-integrated 10 m2 (oblique / vertical tows)
#' @export
#' @concept quality
CC_AREAL_GEARS <- c("C1", "CB", "CV", "PV")

#' SQL deriving the two canonical densities and the effort class of a bio observation
#'
#' Returns the `SELECT`-list fragment that derives `density_per_10m2`, `density_per_1000m3` and
#' `effort_class` from a count, its units, the gear and the effort of its own sample
#' (`std_haul_factor`, `prop_sorted`, `volume_sampled_m3`). The release cuts these columns onto
#' `obs_bio` with it; a consumer that joins `obs` to `sample_measurement` itself applies the same
#' expression. `calcofi4py::density_sql()` emits identical bytes.
#'
#' @param alias table alias to prefix every column with (`"o"` -> `o.measurement_value`), or `NULL`.
#' @param value,units,tow_type,std_haul_factor,prop_sorted,volume_sampled_m3 column names.
#' @param as `TRUE` returns one string of three `expr AS name` clauses joined by `,\n`; `FALSE`
#'   returns the three bare expressions as a named character vector.
#' @return A single string (or a named character vector when `as = FALSE`).
#' @examples
#' cat(cc_density_sql("o"))
#' @export
#' @concept quality
cc_density_sql <- function(alias = NULL, value = "measurement_value", units = "units", tow_type = "tow_type",
                           std_haul_factor = "std_haul_factor", prop_sorted = "prop_sorted",
                           volume_sampled_m3 = "volume_sampled_m3", as = TRUE) {
  p   <- if (is.null(alias) || !nzchar(alias)) "" else paste0(alias, ".")
  v   <- paste0(p, value);           u   <- paste0(p, units);       tt  <- paste0(p, tow_type)
  shf <- paste0(p, std_haul_factor); ps  <- paste0(p, prop_sorted); vol <- paste0(p, volume_sampled_m3)
  q   <- function(x) paste0("'", x, "'", collapse = ", ")
  # prop_sorted: 0 or NULL means "all of it was sorted", never a division by zero or a NULL density
  sorted <- sprintf("COALESCE(NULLIF(%s, 0), 1)", ps)
  ex <- c(
    density_per_10m2 = sprintf(
      "CASE WHEN %s = 'count' AND %s IS NOT NULL AND %s IN (%s) THEN %s * %s / %s\n     WHEN %s IN (%s) THEN %s * 10\n     END",
      u, shf, tt, q(CC_AREAL_GEARS), v, shf, sorted, u, q(CC_DENSITY_UNITS$area), v),
    density_per_1000m3 = sprintf(
      "CASE WHEN %s = 'count' AND %s IS NOT NULL AND %s > 0 THEN %s / %s / %s * 1000\n     WHEN %s IN (%s) THEN %s\n     END",
      u, vol, vol, v, sorted, vol, u, q(CC_DENSITY_UNITS$volume), v),
    effort_class = sprintf(
      "CASE WHEN %s = 'count' AND %s IS NULL AND %s IS NULL THEN 'raw_count_no_effort'\n     WHEN %s = 'count' THEN 'count_with_effort'\n     WHEN %s IN (%s) THEN 'density_as_published'\n     ELSE 'other_unit' END",
      u, shf, vol, u, u, q(c(CC_DENSITY_UNITS$area, CC_DENSITY_UNITS$volume))))
  if (!as) return(ex)
  paste(sprintf("%s AS %s", ex, names(ex)), collapse = ",\n")
}

#' The picker's defaults: which life stage and which denominator open a taxon (D8 rule 4)
#'
#' Given the per-dataset x life-stage coverage of a taxon — one row per `(dataset_key, life_stage)`
#' with `n` rows, `n_10m2` rows that can be expressed per 10 m2 and `n_1000m3` per 1000 m3 — the
#' default stage is the one with the most rows carrying effort (tie: most rows), and the default
#' denominator is the one that covers the most datasets *with effort* for that stage — never
#' largest-n; `per_10m2` on a tie; `raw` only when nothing carries effort. Eggs and larvae are never
#' merged. The explorer's `state.ts` implements the same two functions.
#'
#' @param picker data frame with `dataset_key`, `life_stage`, `n`, `n_10m2`, `n_1000m3`.
#' @param stage a life stage in `picker$life_stage` (`NA` for rows without one).
#' @return `cc_default_stage()`: a life stage (or `NA`); `cc_default_denominator()`: one of
#'   `"per_10m2"`, `"per_1000m3"`, `"raw"`.
#' @examples
#' p <- data.frame(dataset_key = c("swfsc_ichthyo", "swfsc_ichthyo", "swfsc_cufes"),
#'                 life_stage = c("larva", "egg", "egg"), n = c(7420, 5906, 49572),
#'                 n_10m2 = c(6158, 4907, 0), n_1000m3 = c(7420, 5906, 0))
#' cc_default_stage(p)
#' cc_default_denominator(p, "larva")
#' @export
#' @concept quality
cc_default_stage <- function(picker) {
  stopifnot(all(c("dataset_key", "life_stage", "n", "n_10m2", "n_1000m3") %in% names(picker)))
  if (!nrow(picker)) return(NA_character_)
  key <- ifelse(is.na(picker$life_stage), "\r<NA>", picker$life_stage)
  eff <- tapply(pmax(picker$n_10m2, picker$n_1000m3), key, sum)
  n   <- tapply(picker$n, key, sum)
  o   <- order(-eff, -n, names(eff))
  best <- names(eff)[o[1]]
  if (identical(best, "\r<NA>")) NA_character_ else best
}

#' @rdname cc_default_stage
#' @export
cc_default_denominator <- function(picker, stage) {
  stopifnot(all(c("dataset_key", "life_stage", "n", "n_10m2", "n_1000m3") %in% names(picker)))
  sel  <- if (is.na(stage)) is.na(picker$life_stage) else !is.na(picker$life_stage) & picker$life_stage == stage
  rows <- picker[sel, , drop = FALSE]
  ds10   <- unique(rows$dataset_key[rows$n_10m2   > 0])
  ds1000 <- unique(rows$dataset_key[rows$n_1000m3 > 0])
  if (!length(ds10) && !length(ds1000)) return("raw")
  if (length(ds1000) > length(ds10)) "per_1000m3" else "per_10m2"
}
