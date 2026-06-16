# Create Interactive Species Distribution Map with Hexagonal Binning

Generates a multi-resolution maplibre map displaying species abundance
aggregated into H3 hexagons with color-coded values and interactive
tooltips.

## Usage

``` r
map_sp(sp_hex_list, sp_scale_list, is_dark = T)
```

## Arguments

- sp_hex_list:

  List of sf objects, one per H3 resolution level, containing hexagonal
  geometries and aggregated species abundance

- sp_scale_list:

  List of color scale specifications, one per resolution level (from
  [`scales::col_numeric()`](https://scales.r-lib.org/reference/col_numeric.html))

## Value

maplibre object with multi-resolution hexagonal layers, legend, and
scale control

## Details

The map uses zoom-dependent layer visibility controlled by
`zoom_breaks`. Each resolution level displays at appropriate zoom ranges
to balance detail and performance. Abundance values are standardized as
count per 10m² surface area.

## See also

[`prep_sp_hex`](https://calcofi.io/calcofi4r/reference/prep_sp_hex.md)
for data aggregation

[`get_sp`](https://calcofi.io/calcofi4r/reference/get_sp.md) for data
retrieval

## Examples

``` r
if (FALSE) { # \dontrun{
df_sp <- get_sp(sp_name = "Anchovy (Engraulis mordax)", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"))
sp_hex <- prep_sp_hex(df_sp, res_range = 3:5)
sp_scale <- lapply(sp_hex, function(x) scales::col_numeric("YlOrRd", domain = range(x$sp.value)))
map_sp(sp_hex, sp_scale)
} # }
```
