# Detect and Handle Dateline Crossing in Line Segments

Normalizes longitude coordinates when a line segment crosses the ±180°
dateline, preventing discontinuities in buffering and visualization.

## Usage

``` r
fix_dateline_crossing(segment)
```

## Arguments

- segment:

  sf linestring object representing a transect or track

## Value

sf linestring object with normalized coordinates (0-360° range if
dateline is crossed)

## Details

Dateline crossings are detected by checking for longitude jumps \> 180°.
When detected, negative longitudes are shifted to 0-360° range. The
segment is then segmentized to 1000m intervals for smooth buffering.

## See also

[`buffer_transect`](https://calcofi.io/calcofi4r/reference/buffer_transect.md)
for usage in buffering workflow

## Examples

``` r
if (FALSE) { # \dontrun{
# transect crossing the dateline
coords <- matrix(c(175, 30, -175, 35), ncol = 2, byrow = TRUE)
segment <- st_sf(st_sfc(st_linestring(coords), crs = 4326))
normalized <- fix_dateline_crossing(segment)
} # }
```
