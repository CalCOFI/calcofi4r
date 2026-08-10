# Seafloor depth at points

Bilinear rather than nearest-cell, so a station on a steep slope is not
pinned to whichever 15 arc-second cell it happens to land in.

## Usage

``` r
cc_bathy_depth(lon, lat, bathy = cc_bathy())
```

## Arguments

- lon, lat:

  numeric vectors of equal length, decimal degrees.

- bathy:

  raster from
  [`cc_bathy()`](https://calcofi.io/calcofi4r/reference/cc_bathy.md);
  pass one explicitly to avoid re-reading it per call.

## Value

Numeric vector of depth in metres, `NA` outside the raster's extent.
Land reads 0, never negative.
