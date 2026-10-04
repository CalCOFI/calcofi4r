# The land mask the CalCOFI grid is clipped with

OpenStreetMap land polygons around the grid, prepared by
[`cc_grid_land_prep()`](https://calcofi.io/calcofi4r/reference/cc_grid_land_prep.md):
islets smaller than 1 km2 dropped, the coastline pulled back 300 m (so a
ship at its berth or a nearshore position rounded to the minute still
falls in a cell) and simplified to 100 m. It is the `land` argument of
[`cc_grid_build()`](https://calcofi.io/calcofi4r/reference/cc_grid_build.md),
bundled so the grid can be rebuilt, and a variant built, from the
package alone. It is not a coastline to draw.

## Usage

``` r
cc_grid_land
```

## Format

An `sf` of one multipolygon (EPSG:4326) with

- source:

  the source and its licence

- version:

  the date of the OpenStreetMap extract

- geom:

  the mask

## Source

<https://osmdata.openstreetmap.de/data/land-polygons.html>, (c)
OpenStreetMap contributors,
[ODbL](https://opendatacommons.org/licenses/odbl/)
