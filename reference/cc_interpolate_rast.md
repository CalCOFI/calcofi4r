# The surface from [`cc_interpolate()`](https://calcofi.io/calcofi4r/reference/cc_interpolate.md) as a `terra` raster in Web Mercator

The grid's rows are evenly spaced in Web-Mercator y, so the honest
raster is EPSG:3857 (regular there;
[`terra::project()`](https://rspatial.github.io/terra/reference/project.html)
it to 4326 if a lon/lat raster is wanted). Two layers: `value`, and `se`
when present.

## Usage

``` r
cc_interpolate_rast(s)
```

## Arguments

- s:

  the list
  [`cc_interpolate()`](https://calcofi.io/calcofi4r/reference/cc_interpolate.md)
  returns.

## Value

A `SpatRaster`.
