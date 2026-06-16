# Interpolate points to contours using a Generalized Additive Model (GAM)

Fit generalized additive model to a variable in space using a smoother
on latitude & longitude. Further clip by bounding polygon.

## Usage

``` r
pts_to_contours_gam(df, ply, gam_k = 60, grid_width = 0.1, n_breaks = 7)
```

## Arguments

- df:

  data frame containing the fields: `lon` (longitude), `lat` (latitude)
  and `v` (value)

- ply:

  polygon describing the study area mask

- gam_k:

  number of knots to use on the spatial smoother `s(lon, lat)`; default:
  `100`

- grid_width:

  cell width in decimal degrees to run interpolation; default: `0.1`

- n_breaks:

  number of breaks in values for creating contours; default: `7`

## Value

polygon simple features (`sf`) of contour
[`isoband::isobands()`](http://isoband.r-lib.org/reference/isobands.md)

## Examples

``` r
v_ply <- pts_to_contours_gam(bottle_temp_lonlat, area_calcofi_extended)
mapview::mapView(v_ply, zcol="v", layer.name="temp(C)")
```
