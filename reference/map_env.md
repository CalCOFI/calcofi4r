# Create Interactive Environmental Map with Hexagonal Binning

Generates a multi-resolution maplibre map displaying environmental data
aggregated into H3 hexagons with color-coded values and interactive
tooltips.

## Usage

``` r
map_env(
  env_hex_list,
  env_scale_list,
  env_stat_label,
  env_var_label,
  is_dark = T
)
```

## Arguments

- env_hex_list:

  List of sf objects, one per H3 resolution level, containing hexagonal
  geometries and aggregated environmental values

- env_scale_list:

  List of color scale specifications, one per resolution level (from
  [`scales::col_numeric()`](https://scales.r-lib.org/reference/col_numeric.html))

- env_stat_label:

  Character string describing the statistic (e.g., "Mean", "Median")

- env_var_label:

  Character string describing the variable (e.g., "Temperature (°C)")

## Value

maplibre object with multi-resolution hexagonal layers and legend

## Details

The map uses zoom-dependent layer visibility controlled by
`zoom_breaks`. Each resolution level displays at appropriate zoom ranges
to balance detail and performance.

## See also

[`prep_env_hex`](https://calcofi.io/calcofi4r/reference/prep_env_hex.md)
for data aggregation

[`get_env`](https://calcofi.io/calcofi4r/reference/get_env.md) for data
retrieval

## Examples

``` r
if (FALSE) { # \dontrun{
df_env <- get_env("t_deg_c", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"), min_depth = 0, max_depth = 100)
env_hex <- prep_env_hex(df_env, res_range = 3:5, env_stat = "mean")
env_scale <- lapply(env_hex, function(x) scales::col_numeric("viridis", domain = range(x$env.value)))
map_env(env_hex, env_scale, "Mean", "Temperature (°C)")
} # }
```
