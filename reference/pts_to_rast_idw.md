# Interpolate points to raster using Inverse-Distance Weighting (IDW)

Interpolate from points to raster using inverse-distance weighting
([`terra::interpIDW()`](https://rspatial.github.io/terra/reference/interpIDW.html)).
The further the input point from the destination pixel in the output
raster, the less its value is applied. The dimensions of the output
raster are automatically calculated based on approaching 100 pixels in
the maximum dimension (of longitude or latitude) based on the extent of
the area of interest.

## Usage

``` r
pts_to_rast_idw(
  pts,
  fld,
  aoi,
  radius = 1.5,
  power = 1.3,
  smooth = 0.2,
  maxPoints = Inf,
  minPoints = 1,
  near = FALSE,
  fill = NA,
  out_tif = NULL,
  verbose = F
)
```

## Arguments

- pts:

  input points as simple features
  [`sf::sf`](https://r-spatial.github.io/sf/reference/sf.html)

- fld:

  quoted string of field from `pts` with z-value to interpolate

- aoi:

  area of interest as simple features
  [`sf::sf`](https://r-spatial.github.io/sf/reference/sf.html)

- radius:

  argument for
  [`terra::interpIDW()`](https://rspatial.github.io/terra/reference/interpIDW.html);
  default = `1.5`

- power:

  argument for
  [`terra::interpIDW()`](https://rspatial.github.io/terra/reference/interpIDW.html);
  default = `1.3`

- smooth:

  argument for
  [`terra::interpIDW()`](https://rspatial.github.io/terra/reference/interpIDW.html);
  default = `0.2`

- maxPoints:

  argument for
  [`terra::interpIDW()`](https://rspatial.github.io/terra/reference/interpIDW.html);
  default = `Inf`

- minPoints:

  argument for
  [`terra::interpIDW()`](https://rspatial.github.io/terra/reference/interpIDW.html);
  default = `1`

- near:

  argument for
  [`terra::interpIDW()`](https://rspatial.github.io/terra/reference/interpIDW.html);
  default = `FALSE`

- fill:

  argument for
  [`terra::interpIDW()`](https://rspatial.github.io/terra/reference/interpIDW.html);
  default = `NA`

- out_tif:

  output path to write raster tif using
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html);
  default = `NULL`

- verbose:

  show messages of all variables used; default = FALSE

## Value

[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
object
