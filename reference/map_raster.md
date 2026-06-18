# Map raster interactively

Map raster of interpolated oceanographic variable for a cruise.

## Usage

``` r
map_raster(r, legend_title = "Temperature (C)")
```

## Arguments

- r:

  raster of type
  [`raster::raster()`](https://rdrr.io/pkg/raster/man/raster.html)

- legend_title:

  title for legend of variable mapped

## Value

interactive plot of
[`leaflet::leaflet()`](https://rstudio.github.io/leaflet/reference/leaflet.html)

## Examples

``` r
(r_tif <- tempfile(fileext=".tif"))
#> [1] "/tmp/RtmpUrpVUE/file1f9424737ab3.tif"

# use second variable from previously fetched v
c(v$table_field[2], v$plot_label[2])
#> Error: object 'v' not found

# fetch interpolated raster from CalCOFI API
get_raster(
  variable = v$table_field[2],
  cruise_id = "2020-01-05-C-33RL",
  depth_m_min = 0, depth_m_max = 200,
  out_tif = r_tif)
#> Warning: `get_raster()` was deprecated in calcofi4r 1.1.0.
#> The CalCOFI API is being phased out.
#> ℹ Query data with cc_get_db() and use pts_to_rast_idw() for interpolation.
#> Error in eval(expr, envir) : object 'v' not found
#> Error in resp_body_json(.): Unexpected content type "text/html".
#> • Expecting type "application/json" or suffix "json".

r <- raster::raster(r_tif)
#> Warning: GDAL Error 4: /tmp/RtmpUrpVUE/file1f9424737ab3.tif: No such file or directory
#> Error in .rasterObjectFromFile(x, band = band, objecttype = "RasterLayer",     ...): Cannot create a RasterLayer object from this file. (file does not exist)

# plot raster
map_raster(r, v$plot_label[2])
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'extent': error in evaluating the argument 'x' in selecting a method for function 'raster': object 'r' not found
```
