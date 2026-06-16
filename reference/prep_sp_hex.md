# Aggregate Species Data into H3 Hexagons

Converts species occurrence/abundance data into multi-resolution H3
hexagonal bins with aggregated statistics and geometries for mapping.

## Usage

``` r
prep_sp_hex(df_sp, res_range)
```

## Arguments

- df_sp:

  dbplyr lazy table with columns: `hex_h3res*`, `std_tally`

- res_range:

  Integer vector of H3 resolution levels to generate (e.g., 3:5)

## Value

List of sf objects, one per resolution level, each with columns:

- `resolution` - H3 resolution level

- `hexid` - H3 hexagon identifier

- `sp.value` - mean standardized tally

- `tooltip` - rounded value for display

- `geometry` - sf geometry (hexagon polygon)

## Details

This function uses dbplyr lazy evaluation to efficiently aggregate data
across multiple H3 resolutions via `union_all`. Geometries are joined
from a pre-computed sf object (`sf_hex`).

## See also

[`map_sp`](https://calcofi.io/calcofi4r/reference/map_sp.md) for
visualization

[`get_sp`](https://calcofi.io/calcofi4r/reference/get_sp.md) for data
retrieval

## Examples

``` r
if (FALSE) { # \dontrun{
df_sp <- get_sp("Anchovy (Engraulis mordax)", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"))
sp_hex <- prep_sp_hex(df_sp, res_range = 3:5)
} # }
```
