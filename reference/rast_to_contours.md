# Raster to contour polygons

Contour the input raster (using
[`stars::st_contour`](https://r-spatial.github.io/stars/reference/st_contour.html)).
Use "pretty" breaks (using
[`classInt::classIntervals()`](https://r-spatial.github.io/classInt/reference/classIntervals.html)).
Clip by area of interest.

## Usage

``` r
rast_to_contours(r, aoi, n_brks = 8)
```

## Arguments

- r:

  input raster as
  [`terra::rast`](https://rspatial.github.io/terra/reference/rast.html)

- aoi:

  area of interest as simple features
  [`sf::sf`](https://r-spatial.github.io/sf/reference/sf.html)

- n_brks:

  number of breaks

## Value

contour polygon simple features
([`sf::sf`](https://r-spatial.github.io/sf/reference/sf.html))
