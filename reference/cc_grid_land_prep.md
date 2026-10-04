# Prepare a land mask for the grid's coastline

Turns detailed land polygons (OpenStreetMap's, for the bundled
[cc_grid_land](https://calcofi.io/calcofi4r/reference/cc_grid_land.md))
into the mask
[`cc_grid_build()`](https://calcofi.io/calcofi4r/reference/cc_grid_build.md)
clips cells with: islets smaller than `islet_min_km2` are dropped (a
rock is not a hole in a cell), the coastline is pulled back `shrink_m`
so that a position at the water's edge (a ship at its berth, a nearshore
position rounded to the minute) still falls in a cell, and the result is
simplified to `simplify_m` so the cells stay light enough to draw in a
browser. A position less than `shrink_m - simplify_m` inland is
therefore inside the grid.

## Usage

``` r
cc_grid_land_prep(
  land,
  near = NULL,
  near_m = 10000,
  islet_min_km2 = 1,
  shrink_m = 300,
  simplify_m = 100,
  crs_m = 3310
)
```

## Arguments

- land:

  land polygons (`sf` or `sfc`, any CRS)

- near:

  optional `sf`/`sfc`: keep only land within `near_m` of it (e.g. the
  previous grid)

- near_m:

  distance for `near`, in metres (default 10 km)

- islet_min_km2:

  drop land polygons smaller than this (default 1 km2)

- shrink_m:

  pull the coastline back by this many metres (default 300)

- simplify_m:

  Douglas-Peucker tolerance, in metres (default 100)

- crs_m:

  projected CRS, in metres (default 3310)

## Value

`sfc` of polygons in EPSG:4326
