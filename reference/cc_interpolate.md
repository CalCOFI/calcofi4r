# Interpolate point values to a surface, exactly as the Explorer's Contours lens does

The same algorithm as `calcofi.io/explore` (`lens=contour`) and
`calcofi4py.interpolate()`, so a surface drawn in R matches the map cell
for cell: a grid of `cell_deg` degrees of longitude whose rows are
evenly spaced in Web-Mercator y (what the map stretches a bitmap over),
a local equirectangular km frame for the distances, and **no value
farther than `mask_km` from every point** — the surface never
extrapolates. Typically the points are the Explorer's station table
(`grid_key` centres with a summary), i.e. what *Share → Download data*
writes as `summary/station.csv`.

## Usage

``` r
cc_interpolate(
  pts,
  method = c("ok", "idw", "tps"),
  cell_deg = 0.06,
  mask_km = 60,
  se = TRUE,
  nmax = 0
)
```

## Arguments

- pts:

  a data frame with `lon`, `lat` and `z` columns (`NA` rows are
  dropped).

- method:

  `"ok"` ordinary kriging (default; the kriging SD is the error
  surface), `"idw"` inverse-distance weighting (power 1.3, radius 200
  km, 5 km smoothing — no error surface), or `"tps"` a thin-plate spline
  with the smoothing chosen by GCV (its standard error is the error
  surface).

- cell_deg:

  cell size in degrees of longitude (the Explorer uses 0.06).

- mask_km:

  cells farther than this from every point are `NA` (the Explorer uses
  60).

- se:

  compute the error surface (`ok`, `tps`; always `NULL` for `idw`). It
  is the slow part in the global mode.

- nmax:

  `0` (the station grid): every point in one system. `> 0` (the cast
  grain; the Explorer uses 32): the `nmax` nearest points per cell — one
  small solve each, which gives the value and its error together; the
  variogram then fits on at most 2,000 points and the leave-one-out
  error runs on at most 500, both drawn by a seeded generator shared
  with the browser; a neighbour is never farther than `3 * mask_km`. Not
  for `"tps"`.

## Value

A list: `grid` (`lon0`, `lon1`, `lat_s`, `lat_n`, `nx`, `ny`,
`cell_deg`), `values` (an `ny x nx` matrix, **row 1 = north**), `se`
(the same shape, or `NULL`), `fit` (`n`, `n_cells`, `loo` the
leave-one-out RMSE, `vg` the fitted variogram for `ok`, `edf` the
effective degrees of freedom for `tps`), and `method`. Turn it into a
raster with
[`cc_interpolate_rast()`](https://calcofi.io/calcofi4r/reference/cc_interpolate_rast.md).

## See also

[`cc_interpolate_rast()`](https://calcofi.io/calcofi4r/reference/cc_interpolate_rast.md),
[`pts_to_rast_idw()`](https://calcofi.io/calcofi4r/reference/pts_to_rast_idw.md)
(the superseded server-side IDW)

## Examples

``` r
set.seed(1)
pts <- data.frame(lon = runif(40, -122, -118), lat = runif(40, 31, 34))
pts$z <- 12 + 3 * sin(pts$lon + 122) + rnorm(40, 0, 0.3)
s <- cc_interpolate(pts, "ok", cell_deg = 0.25)
s$fit$loo; dim(s$values)
#> [1] 0.4763396
#> [1] 20 22
```
