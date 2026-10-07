# ─── basemap: CARTO basemaps as vector tiles ──────────────────────────────────
#
# CARTO's raster basemaps (the `CartoDB.*` leaflet providers, mapview's first two
# default basemaps) answer every tile with an "API KEY REQUIRED" watermark since
# Sep 2026; its vector GL styles do not. MapLibre maps (mapgl, the Explorer) read
# the GL styles already. For a Leaflet map, cc-vector-basemap.js swaps each CARTO
# raster layer for the same style drawn by maplibre-gl-leaflet, so existing
# leaflet and mapview code keeps its provider names and gets a vector basemap.

# maplibre-gl and its leaflet bridge, from the CDN (maplibre-gl is ~1 MB)
.CC_MAPLIBRE_GL_VERSION      <- "5.24.0"
.CC_MAPLIBRE_LEAFLET_VERSION <- "0.1.4"

#' CARTO vector basemap style
#'
#' URL of a CARTO basemap's vector GL style, for a MapLibre map
#' (`mapgl::maplibre(style = ...)`, MapLibre GL JS, deck.gl) or Plotly's
#' `layout(mapbox = list(style = ...))`. CARTO's raster tiles of the same
#' basemaps need an API key since Sep 2026; these styles do not.
#'
#' @param style one of `"positron"` (light grey), `"dark-matter"` or `"voyager"`
#' @param labels whether the style draws place labels; default `TRUE`
#'
#' @return character URL of the style JSON
#' @concept visualize
#' @export
#'
#' @examples
#' cc_basemap_style()
#' cc_basemap_style("dark-matter", labels = FALSE)
cc_basemap_style <- function(
    style  = c("positron", "dark-matter", "voyager"),
    labels = TRUE) {
  style <- match.arg(style)
  stopifnot(is.logical(labels), length(labels) == 1, !is.na(labels))
  glue::glue(
    "https://basemaps.cartocdn.com/gl/{style}{if (labels) '' else '-nolabels'}-gl-style/style.json") |>
    as.character()
}

#' Dependencies that draw CARTO basemaps as vector tiles in Leaflet
#'
#' The HTML dependencies behind [cc_vector_basemap()]: Leaflet and its
#' providers plugin (so they load first), maplibre-gl, maplibre-gl-leaflet and
#' `cc-vector-basemap.js`, which swaps each CARTO raster layer on the page for
#' its vector GL style. Use it page-wide where piping each map is impractical:
#' `knitr::knit_meta_add(cc_vector_basemap_deps())` in a notebook, or inside a
#' Shiny `ui`.
#'
#' @return list of [htmltools::htmlDependency()] objects
#' @concept visualize
#' @importFrom htmltools htmlDependency
#' @importFrom leaflet leaflet addProviderTiles
#' @export
#'
#' @examples
#' vapply(cc_vector_basemap_deps(), `[[`, "", "name")
cc_vector_basemap_deps <- function() {
  # leaflet's own dependencies, by the same name and version a widget carries,
  # so a resolved page loads them before the bridge, which needs `L`
  m_leaflet <- leaflet::leaflet() |>
    leaflet::addProviderTiles("CartoDB.Positron")

  cdn <- "https://cdn.jsdelivr.net/npm"
  c(
    m_leaflet$dependencies,
    list(
      htmltools::htmlDependency(
        name       = "maplibre-gl",
        version    = .CC_MAPLIBRE_GL_VERSION,
        src        = c(href = glue::glue("{cdn}/maplibre-gl@{.CC_MAPLIBRE_GL_VERSION}/dist")),
        script     = "maplibre-gl.js",
        stylesheet = "maplibre-gl.css"),
      htmltools::htmlDependency(
        name    = "maplibre-gl-leaflet",
        version = .CC_MAPLIBRE_LEAFLET_VERSION,
        src     = c(href = glue::glue(
          "{cdn}/@maplibre/maplibre-gl-leaflet@{.CC_MAPLIBRE_LEAFLET_VERSION}")),
        script  = "leaflet-maplibre-gl.js"),
      htmltools::htmlDependency(
        name    = "cc-vector-basemap",
        version = as.character(utils::packageVersion("calcofi4r")),
        src     = "htmlwidgets/lib/cc-vector-basemap",
        package = "calcofi4r",
        script  = "cc-vector-basemap.js")))
}

#' Draw a map's CARTO basemaps as vector tiles
#'
#' CARTO's raster basemaps — leaflet's `CartoDB.*` providers and the first two
#' of mapview's default basemaps — draw an "API KEY REQUIRED" watermark on
#' every tile since Sep 2026. This attaches [cc_vector_basemap_deps()] to a
#' Leaflet or mapview map, so each CARTO raster layer on the page is drawn
#' instead from the same basemap's vector GL style ([cc_basemap_style()]).
#' Provider names, layer groups and the layer control are unchanged; other
#' tile layers (Esri, OpenStreetMap) pass through.
#'
#' @param map a `leaflet::leaflet()` widget or a `mapview::mapview()` object
#'
#' @return `map`, with the dependencies attached
#' @concept visualize
#' @export
#'
#' @examples
#' library(leaflet)
#' leaflet() |>
#'   addProviderTiles("CartoDB.Positron") |>
#'   setView(-120.5, 33, zoom = 6) |>
#'   cc_vector_basemap()
#'
#' # mapview's default basemaps include CartoDB.Positron and CartoDB.DarkMatter
#' mapview::mapview(cc_grid_ctrs) |>
#'   cc_vector_basemap()
cc_vector_basemap <- function(map) {
  if (inherits(map, "mapview")) {
    map@map <- cc_vector_basemap(map@map)
    return(map)
  }
  stopifnot(inherits(map, "leaflet"))
  map$dependencies <- c(map$dependencies, cc_vector_basemap_deps())
  map
}
