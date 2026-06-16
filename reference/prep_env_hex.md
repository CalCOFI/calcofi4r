# Aggregate Environmental Data into H3 Hexagons

Converts environmental point data into multi-resolution H3 hexagonal
bins with aggregated statistics and geometries for mapping. Uses dbplyr
lazy evaluation to defer collection until after aggregation.

## Usage

``` r
prep_env_hex(df_env, res_range, env_stat)
```

## Arguments

- df_env:

  dbplyr lazy table with H3 index columns (`hex_h3res*`) and `qty`
  column

- res_range:

  Integer vector of H3 resolution levels to generate (e.g., 3:5)

- env_stat:

  Character string specifying aggregation function: "mean", "median",
  "min", "max", "sd"

## Value

List of sf objects, one per resolution level, each with columns:

- `resolution` - H3 resolution level

- `hexid` - H3 hexagon identifier

- `env.value` - aggregated environmental value

- `tooltip` - rounded value for display

- `geometry` - sf geometry (hexagon polygon)

## Details

This function uses dbplyr lazy evaluation to efficiently aggregate data
across multiple H3 resolutions via `union_all`. Geometries are joined
from a pre-computed sf object (`sf_hex`).

## See also

[`map_env`](https://calcofi.io/calcofi4r/reference/map_env.md) for
visualization

[`get_env`](https://calcofi.io/calcofi4r/reference/get_env.md) for data
retrieval

## Examples

``` r
if (FALSE) { # \dontrun{
df_env <- get_env("t_deg_c", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"), min_depth = 0, max_depth = 100)
env_hex <- prep_env_hex(df_env, res_range = 3:5, env_stat = "mean")
} # }
```
