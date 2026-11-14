# Anomalies on isopycnals, against a baseline on the same surfaces

[`cc_isopycnal()`](https://calcofi.io/calcofi4r/reference/cc_isopycnal.md)
for the casts asked for, beside a baseline built the way the release
builds its `climatology` (`calcofi4db::build_climatology()`), but on
density surfaces instead of 10 m depth bins: for each station
(`site_key`), the cruise's month (or its season, `season = TRUE`: DJF /
MAM / JJA / SON), each surface and each variable, the mean over the
casts of cruises in `years`, kept where at least `min_cruises` distinct
cruises contribute. `anomaly = value - clim_mean`; a cast with no
baseline cell comes back `NA`, never 0. For `variable = "depth_m"` the
anomaly is the surface's displacement in metres, positive deeper.

## Usage

``` r
cc_isopycnal_anomaly(
  con,
  sigma = c(seq(24, 27, by = 0.2), 25.5, 26.5),
  cruise_key = NULL,
  site_key = NULL,
  variables = c("temperature_ave", "salinity_ave_corr", "oxygen_ml_l_ave_sta_corr",
    "est_nitrate_sta_corr", "spiciness0"),
  years = c(1993, 2013),
  season = FALSE,
  min_cruises = 5,
  direction = c("down", "both"),
  surface_max_m = 20,
  min_n = 3,
  baseline = c("auto", "release", "compute")
)
```

## Arguments

- con:

  DuckDB connection to a release opened with the supplemental tables:
  `cc_get_db(supplemental = TRUE)`.

- sigma:

  numeric: the surfaces, sigma-theta in kg m^-3. The default is the
  provider's list (Rasmus Swalethorp, 2026-11-13): 24.0 to 27.0 by 0.2,
  plus 25.5 and 26.5, which sit near the cores of the California Current
  and the California Undercurrent. 27.0 is reached by about a quarter of
  the down casts (the median density at the bottom of a 500 m cast is
  26.98).

- cruise_key:

  optional cruise key(s) (`YYYY-MM-NODC`) to restrict to.

- site_key:

  optional station(s) (`sample.site_key`, e.g. `"093.3 030.0"`).

- variables:

  `obs_ctd_full` measurement types to carry onto each surface, and
  `"spiciness0"` for spice computed there (see the rules); the default
  is the sensor-pair averages the release publishes (the
  station-corrected oxygen and the bottle-fitted nitrate estimate, which
  exist only on casts with bottle data) and spice.

- years:

  two-element baseline range of cruise years, inclusive (default
  1993-2013, the release's window).

- season:

  `FALSE` (default) for the monthly baseline; `TRUE` for the seasonal
  one.

- min_cruises:

  minimum distinct cruises behind a baseline cell (the release keeps \>=
  5).

- direction:

  `"down"` (default): one cast per station occupation, the down cast
  where there is one; `"both"`: every cast.

- surface_max_m, min_n:

  as in `calcofi4db::profile_threshold_depth()`: the deepest first bin
  that counts as the surface (m) and the fewest bins a cast needs for
  `"at_surface"`.

- baseline:

  `"auto"` (default): the release's `climatology` when it carries this
  baseline, computed otherwise; `"release"`: the release's, an error
  when it does not carry it; `"compute"`: always computed from
  `obs_ctd_full`.

## Value

[`cc_isopycnal()`](https://calcofi.io/calcofi4r/reference/cc_isopycnal.md)'s
columns plus `clim_mean`, `clim_sd`, `clim_n` (casts), `n_cruises`,
`anomaly`, `anomaly_sd`; attributes `baseline` (the years), `period`
(`"month"` or `"season"`), `source` (`"release"` or `"computed"`) and
`climatology` (the baseline cells: `site_key`, `month`, `sigma_theta`,
`variable`, `clim_mean`, `clim_sd`, `clim_n`, `n_cruises`).

## Details

**Where the baseline comes from.** A release from v2026.11.14 on carries
the baseline on these surfaces in its `climatology` table
(`calcofi4db::build_climatology(isopycnal_tbl = )`, built from the
release's `isopycnal` table: the cells with a `sigma_theta`, types
[`cc_isopycnal_type()`](https://calcofi.io/calcofi4r/reference/cc_isopycnal_type.md)),
and that is what is subtracted (`baseline = "auto"`) whenever it is the
baseline asked for: the release's window (`years`), down casts and the
default `surface_max_m` / `min_n`, every surface in `sigma` and every
variable in `variables` present. Only the casts asked for are then
computed. Otherwise (an older release, another window,
`direction = "both"`, a surface the release does not carry) the baseline
is computed here from `obs_ctd_full` at the stations of the casts asked
for, reading every cruise of the window at those stations, by the same
rules. The release keeps cells with at least 5 cruises, so a
`min_cruises` below 5 still finds only those there, as
[`cc_climatology()`](https://calcofi.io/calcofi4r/reference/cc_climatology.md)
does. The two give the same anomalies (to the release's rounding of the
mean, 1e-6); `attr(, "source")` says which was used.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- cc_get_db(supplemental = TRUE)
an  <- cc_isopycnal_anomaly(con, sigma = seq(24.4, 26.6, by = 0.2),
                            cruise_key = "2026-11-33P4", season = TRUE)
# the displacement of each surface and the temperature change on it, station by station
dplyr::filter(an, variable %in% c("depth_m", "temperature_ave"))
} # }
```
