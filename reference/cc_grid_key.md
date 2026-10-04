# The grid cell a position falls in

The one rule that keys a position to a cell of the CalCOFI grid: the
cell whose polygon intersects the point, tested on longitude/latitude as
planar coordinates (no great-circle edges), and, for a point on an edge
shared by two or more cells, the cell whose key sorts first in byte
order. `calcofi4db::assign_grid_key()` applies the same rule in DuckDB
(`min(grid_key) ... WHERE ST_Intersects(point, geom)`), so R and the
database agree on every position, including one on an edge.

## Usage

``` r
cc_grid_key(lon, lat, grid = calcofi4r::cc_grid, key = "grid_key")
```

## Arguments

- lon, lat:

  longitude and latitude in decimal degrees (WGS 84); a missing or
  non-finite coordinate returns `NA`

- grid:

  an `sf` of cells in EPSG:4326 with a key column; default the bundled
  [cc_grid](https://calcofi.io/calcofi4r/reference/cc_grid.md)

- key:

  name of the key column in `grid` (default `"grid_key"`)

## Value

character vector of keys, `NA` where the position is in no cell (on
land, or outside the grid)

## Examples

``` r
# official station 90.0 37.0, and a point in Los Angeles (on land: NA)
cc_grid_key(lon = c(-118.38708, -118.25), lat = c(33.18462, 34.05))
#> [1] "st37-ln90" NA         
# the same station in the previous grid: another cell, under a name the new grid also uses
cc_grid_key(-118.38708, 33.18462, grid = cc_grid_v1)
#> [1] "st35-ln90"
```
