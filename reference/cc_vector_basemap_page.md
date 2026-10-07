# Draw every CARTO basemap in a notebook as vector tiles

Call once in a knitr (R Markdown or Quarto) notebook, before or after
its maps: it adds
[`cc_vector_basemap_deps()`](https://calcofi.io/calcofi4r/reference/cc_vector_basemap_deps.md)
to the page, so every leaflet and mapview map on it draws its CARTO
basemaps from vector tiles without piping each one through
[`cc_vector_basemap()`](https://calcofi.io/calcofi4r/reference/cc_vector_basemap.md).

## Usage

``` r
cc_vector_basemap_page()
```

## Value

the dependencies, invisibly

## Examples

``` r
if (FALSE) { # \dontrun{
# in a notebook's setup chunk
calcofi4r::cc_vector_basemap_page()
} # }
```
