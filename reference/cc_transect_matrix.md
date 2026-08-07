# Pivot a section (or anomaly) to a station x depth matrix

The shape a heatmap wants: `z[[depth]][[station]]`, with `x` the station
distances and `y` the depth bins. Returning it from here rather than
from each app is what lets ctd-transects ship a matrix as JSON and
ctd-viz hand the same numbers to an interpolator.

## Usage

``` r
cc_transect_matrix(section, value = "value", depths = NULL)
```

## Arguments

- section:

  from
  [`cc_transect_section()`](https://calcofi.io/calcofi4r/reference/cc_transect_section.md)
  or
  [`cc_anomaly()`](https://calcofi.io/calcofi4r/reference/cc_anomaly.md).

- value:

  column to pivot (`"value"`, or `"anomaly"` / `"anomaly_sd"`).

- depths:

  optional depth bins to force (keeps matrices aligned across cruises);
  defaults to those present.

## Value

List of `x` (dist_km), `sta`, `y` (depth_m) and `z` (matrix rows =
depths, cols = stations).
