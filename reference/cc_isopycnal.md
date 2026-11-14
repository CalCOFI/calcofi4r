# Conditions on isopycnals, from the 1 m CTD casts

For each CTD cast and each potential-density surface in `sigma`
(sigma-theta, kg m^-3): the depth at which the cast reaches it and the
value of each of `variables` on it, from the full-resolution 1 m bins of
`obs_ctd_full`. Below the mixed layer an anomaly at a fixed depth is
mostly the pycnocline moving up or down; on a density surface that
displacement is the surface's depth, and what is left in temperature,
salinity, oxygen or nitrate is a change in the water mass itself. On one
surface temperature and salinity move together (warmer is saltier), so
their departure there is the spice of the water.

## Usage

``` r
cc_isopycnal(
  con,
  sigma = c(seq(24, 27, by = 0.2), 25.5, 26.5),
  cruise_key = NULL,
  site_key = NULL,
  variables = c("temperature_ave", "salinity_ave_corr", "oxygen_ml_l_ave_sta_corr",
    "est_nitrate_sta_corr", "spiciness0"),
  direction = c("down", "both"),
  surface_max_m = 20,
  min_n = 3
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

- direction:

  `"down"` (default): one cast per station occupation, the down cast
  where there is one; `"both"`: every cast.

- surface_max_m, min_n:

  as in `calcofi4db::profile_threshold_depth()`: the deepest first bin
  that counts as the surface (m) and the fewest bins a cast needs for
  `"at_surface"`.

## Value

Tibble, one row per cast x surface x variable: `sample_key`,
`cruise_key`, `site_key`, `grid_key`, `longitude`, `latitude`,
`datetime`, `data_stage`, `year` and `month` (the cruise's),
`sigma_theta`, `status`, `variable` and `value`. The surface's depth is
the row with `variable = "depth_m"`, present for every cast and surface;
a property's row is present only where `status = "ok"` and the property
has a value there.

## Details

**Rules** (the same as `calcofi4db`'s, which builds the release's
derived products):

- **sigma-theta** is the sensor-pair average of `sigma_theta_1` and
  `sigma_theta_2` by the provider's flags
  (`calcofi4db::ctd_sigma_theta_ave()`): a sensor flagged 8 or 9 is
  dropped, a 1 or a 2 selects that sensor, otherwise the mean. Repeated
  rows for one cast and depth are averaged first.

- **depth of a surface** is
  `calcofi4db::profile_threshold_depth(direction = "above")`, the rule
  behind the nitracline: walking down the cast, the first 1 m bin whose
  sigma-theta reaches the surface (within 1e-4), the depth interpolated
  linearly between it and the bin above. Only the shallowest crossing is
  reported, so a density inversion below it does not move the surface.
  `status` says why a surface has no depth: `"at_surface"` (the cast's
  first bin, within `surface_max_m` of the surface, is already as dense:
  the surface outcrops), `"no_bracket"` (the same, but the first bin is
  deeper), `"not_reached"` (the cast never gets this dense: it is too
  shallow, or the surface lies below it) or `"no_data"` (fewer than two
  good bins). Only `"ok"` carries a depth and values.

- **a property on the surface** is its own profile interpolated linearly
  to that depth, after values its provider flags (8/9,
  [`cc_qual_ok_sql()`](https://calcofi.io/calcofi4r/reference/cc_qual_ok_sql.md))
  are dropped; `NA` where the property has no bin on one side of the
  depth.

- **spice** (`spiciness0`) is computed on the surface from the
  temperature (`temperature_ave`) and salinity (`salinity_ave_corr`)
  interpolated there, by `calcofi4db::ctd_spice()`'s recipe (Rasmus
  Swalethorp, 2026-09-22): TEOS-10 absolute salinity and conservative
  temperature at the surface's pressure (`gsw_p_from_z()` of its depth
  and the cast's latitude), then `gsw_spiciness0()` (McDougall & Krzysik
  2015). The release carries `spiciness0` only on the thinned series in
  `obs_env`, so it is not read from `obs_ctd_full`.

- **casts**: the down cast of each station occupation, the up cast only
  where there is none (`direction = "down"`, the release's thinned
  series makes the same choice), or every cast.

The cruise's month and year are its median cast date
(`cruise.month_median`, release v2026.11+), the rule every CalCOFI
baseline files a cast under; see
[`cc_cruise_month()`](https://calcofi.io/calcofi4r/reference/cc_cruise_month.md).

## See also

[`cc_isopycnal_anomaly()`](https://calcofi.io/calcofi4r/reference/cc_isopycnal_anomaly.md)
for the departure from a baseline on the same surface.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- cc_get_db(supplemental = TRUE)
iso <- cc_isopycnal(con, sigma = c(25, 25.5, 26), cruise_key = "2026-11-33P4",
                    site_key = "093.3 030.0")
tidyr::pivot_wider(iso, names_from = variable, values_from = value)
} # }
```
