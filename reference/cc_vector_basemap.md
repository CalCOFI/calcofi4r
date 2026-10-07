# Draw a map's CARTO basemaps as vector tiles

CARTO's raster basemaps — leaflet's `CartoDB.*` providers and the first
two of mapview's default basemaps — draw an "API KEY REQUIRED" watermark
on every tile since Sep 2026. This attaches
[`cc_vector_basemap_deps()`](https://calcofi.io/calcofi4r/reference/cc_vector_basemap_deps.md)
to a Leaflet or mapview map, so each CARTO raster layer on the page is
drawn instead from the same basemap's vector GL style
([`cc_basemap_style()`](https://calcofi.io/calcofi4r/reference/cc_basemap_style.md)).
Provider names, layer groups and the layer control are unchanged; other
tile layers (Esri, OpenStreetMap) pass through.

## Usage

``` r
cc_vector_basemap(map)
```

## Arguments

- map:

  a
  [`leaflet::leaflet()`](https://rstudio.github.io/leaflet/reference/leaflet.html)
  widget or a
  [`mapview::mapview()`](https://r-spatial.github.io/mapview/reference/mapView.html)
  object

## Value

`map`, with the dependencies attached

## Examples

``` r
library(leaflet)
leaflet() |>
  addProviderTiles("CartoDB.Positron") |>
  setView(-120.5, 33, zoom = 6) |>
  cc_vector_basemap()

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["CartoDB.Positron",null,null,{"errorTileUrl":"","noWrap":false,"detectRetina":false}]}],"setView":[[33,-120.5],6,[]]},"evals":[],"jsHooks":[]}
# mapview's default basemaps include CartoDB.Positron and CartoDB.DarkMatter
mapview::mapview(cc_grid_ctrs) |>
  cc_vector_basemap()
```
