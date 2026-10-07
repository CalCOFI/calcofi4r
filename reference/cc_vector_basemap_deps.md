# Dependencies that draw CARTO basemaps as vector tiles in Leaflet

The HTML dependencies behind
[`cc_vector_basemap()`](https://calcofi.io/calcofi4r/reference/cc_vector_basemap.md):
Leaflet and leaflet-providers (so they load first), maplibre-gl,
maplibre-gl-leaflet and `cc-vector-basemap.js`, which swaps each CARTO
raster layer on the page for its vector GL style. Use it page-wide where
piping each map is impractical:
[`cc_vector_basemap_page()`](https://calcofi.io/calcofi4r/reference/cc_vector_basemap_page.md)
in a notebook, or inside a Shiny `ui`.

## Usage

``` r
cc_vector_basemap_deps()
```

## Value

list of
[`htmltools::htmlDependency()`](https://rstudio.github.io/htmltools/reference/htmlDependency.html)
objects

## Examples

``` r
vapply(cc_vector_basemap_deps(), `[[`, "", "name")
#> [1] "leaflet"             "leaflet-providers"   "maplibre-gl"        
#> [4] "maplibre-gl-leaflet" "cc-vector-basemap"  
```
