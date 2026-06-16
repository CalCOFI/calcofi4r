# Create Dual-Panel Time Series Plot

Generates an interactive Highcharts plot with species abundance and
environmental data in separate panels, with synchronized x-axis zoom and
resolution-dependent date formatting.

## Usage

``` r
plot_ts(sp_ts, env_ts, ts_res, sel_env_var)
```

## Arguments

- sp_ts:

  data.frame from
  [`prep_ts_sp`](https://calcofi.io/calcofi4r/reference/prep_ts_sp.md)
  with columns: `time`, `name`, `avg`, `std`, `lwr`, `upr`

- env_ts:

  data.frame from
  [`prep_ts_env`](https://calcofi.io/calcofi4r/reference/prep_ts_env.md)
  with columns: `time`, `avg`, `std`, `lwr`, `upr`

- ts_res:

  Character string specifying temporal resolution: "year", "quarter",
  "month", "day", etc.

- sel_env_var:

  Character string of environmental variable column name (e.g.,
  "t_deg_c")

## Value

highchart object with dual y-axes, line + ribbon series, and zoom
capabilities

## Details

The function creates a two-panel plot with species on top and
environmental data on bottom. Date formatting adapts to `ts_res`.
Standard error ribbons are displayed as `arearange` series. Tooltips
show values ± standard error.

## See also

[`prep_ts_sp`](https://calcofi.io/calcofi4r/reference/prep_ts_sp.md) for
species time series data

[`prep_ts_env`](https://calcofi.io/calcofi4r/reference/prep_ts_env.md)
for environmental time series data

## Examples

``` r
if (FALSE) { # \dontrun{
sp_ts <- prep_ts_sp(df_sp, ts_res = "year")
env_ts <- prep_ts_env(df_env, ts_res = "year")
plot_ts(sp_ts, env_ts, ts_res = "year", sel_env_var = "t_deg_c")
} # }
```
