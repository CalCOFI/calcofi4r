# Per-cast anomalies of the derived CTD products

A cast's mixed-layer depth, the depth and value of its chlorophyll
maximum and its integrated chlorophyll are **one value per cast**
(`sample_measurement`, dataset `calcofi_ctd-derived`), with no depth.
This returns each cast's value beside the release's baseline for its
station and its cruise's month (or quarter, `season = TRUE`) and the
difference. The baseline is the release's `climatology` table, where a
per-cast cell has `depth_bin` NULL (calcofi4db \>= 4.24.0, release
v2026.10.1x onward). As with
[`cc_anomaly()`](https://calcofi.io/calcofi4r/reference/cc_anomaly.md),
a cast with no baseline comes back `NA`, never 0.

## Usage

``` r
cc_cast_anomaly(
  con,
  variables = "mld_sigma_theta_002",
  cruise_key = NULL,
  season = FALSE,
  min_cruises = 5
)
```

## Arguments

- con:

  DuckDB connection to a release
  ([`cc_get_db()`](https://calcofi.io/calcofi4r/reference/cc_get_db.md)).

- variables:

  per-cast `measurement_type`s, e.g. `"mld_sigma_theta_002"`,
  `"chl_max_depth"`, `"chl_integrated"`.

- cruise_key:

  optional cruise key(s) (`YYYY-MM-NODC`) to restrict to.

- season:

  `TRUE` for the seasonal (quarter) baseline instead of the monthly one.

- min_cruises:

  minimum cruises behind a baseline cell (the release keeps \>= 5).

## Value

Tibble: `sample_key`, `cruise_key`, `site_key`, `grid_key`, `longitude`,
`latitude`, `datetime`, `month`, `variable`, `value`, `clim_mean`,
`clim_sd`, `n_cruises`, `anomaly`, `anomaly_sd`; attributes `baseline`
(the years) and `period` (`"month"` or `"season"`).

## Examples

``` r
if (FALSE) { # \dontrun{
con <- cc_get_db()
cc_cast_anomaly(con, "mld_sigma_theta_002", cruise_key = "2026-07-3322")
} # }
```
