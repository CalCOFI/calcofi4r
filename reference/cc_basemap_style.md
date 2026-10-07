# CARTO vector basemap style

URL of a CARTO basemap's vector GL style, for a MapLibre map
(`mapgl::maplibre(style = ...)`, MapLibre GL JS, deck.gl) or Plotly's
`layout(mapbox = list(style = ...))`. CARTO's raster tiles of the same
basemaps need an API key since Sep 2026; these styles do not.

## Usage

``` r
cc_basemap_style(
  style = c("positron", "dark-matter", "voyager"),
  labels = TRUE
)
```

## Arguments

- style:

  one of `"positron"` (light grey), `"dark-matter"` or `"voyager"`

- labels:

  whether the style draws place labels; default `TRUE`

## Value

character URL of the style JSON

## Examples

``` r
cc_basemap_style()
#> [1] "https://basemaps.cartocdn.com/gl/positron-gl-style/style.json"
cc_basemap_style("dark-matter", labels = FALSE)
#> [1] "https://basemaps.cartocdn.com/gl/dark-matter-nolabels-gl-style/style.json"
```
