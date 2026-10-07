# CARTO basemaps as vector tiles: CARTO's raster tiles answer "API KEY REQUIRED" since Sep 2026

test_that("cc_basemap_style() names CARTO's GL styles", {
  expect_equal(
    cc_basemap_style(),
    "https://basemaps.cartocdn.com/gl/positron-gl-style/style.json")
  expect_equal(
    cc_basemap_style("dark-matter", labels = FALSE),
    "https://basemaps.cartocdn.com/gl/dark-matter-nolabels-gl-style/style.json")
  expect_equal(
    cc_basemap_style("voyager"),
    "https://basemaps.cartocdn.com/gl/voyager-gl-style/style.json")
  expect_error(cc_basemap_style("toner"))
  expect_error(cc_basemap_style(labels = NA))
})

test_that("cc_vector_basemap_deps() loads leaflet before the bridge that needs `L`", {
  d <- vapply(cc_vector_basemap_deps(), `[[`, "", "name")
  pos <- function(x) match(x, d)
  expect_false(anyNA(pos(c("leaflet", "leaflet-providers", "maplibre-gl",
                           "maplibre-gl-leaflet", "cc-vector-basemap"))))
  expect_lt(pos("leaflet"),             pos("maplibre-gl-leaflet"))
  expect_lt(pos("leaflet-providers"),   pos("cc-vector-basemap"))
  expect_lt(pos("maplibre-gl"),         pos("maplibre-gl-leaflet"))
  expect_lt(pos("maplibre-gl-leaflet"), pos("cc-vector-basemap"))
  # regression: leaflet's R binding needs htmlwidgets.js; a page-wide copy loaded
  # ahead of it ("Cannot read properties of undefined (reading 'widget')") blanked every map
  expect_false(any(c("leaflet-binding", "leaflet-providers-plugin", "htmlwidgets") %in% d))
  # the bridge ships in the package, on disk: Quarto refuses a CDN (href) dependency
  for (nm in c("maplibre-gl", "maplibre-gl-leaflet", "cc-vector-basemap")) {
    dep <- cc_vector_basemap_deps()[[pos(nm)]]
    expect_true(file.exists(system.file(
      dep$src$file, dep$script, package = "calcofi4r")), label = nm)
  }
})

test_that("cc_vector_basemap() attaches the dependencies to leaflet and mapview maps", {
  m <- leaflet::leaflet() |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    cc_vector_basemap()
  expect_s3_class(m, "leaflet")
  expect_true("cc-vector-basemap" %in% vapply(m$dependencies, `[[`, "", "name"))

  skip_if_not_installed("mapview")
  mv <- mapview::mapview(cc_grid_ctrs[1:3, ]) |>
    cc_vector_basemap()
  expect_true(inherits(mv, "mapview"))
  expect_true("cc-vector-basemap" %in% vapply(mv@map$dependencies, `[[`, "", "name"))

  expect_error(cc_vector_basemap(data.frame()))
})

# the swap rule itself is JavaScript; run it against a stub Leaflet
test_that("cc-vector-basemap.js swaps CARTO rasters and passes everything else through", {
  skip_if_not_installed("V8")
  ctx <- V8::v8()
  ctx$eval("
    var document = { createElement: function () { return {}; },
                     head: { appendChild: function () {} } };
    var window = {};
    var L = window.L = {
      maplibreGL: function (o) {
        return { kind: 'gl', options: o, on: function () {}, onRemove: function () {} }; },
      tileLayer: function (url, o) { return { kind: 'raster', url: url }; }
    };
    L.tileLayer.wms = function () { return { kind: 'wms' }; };
    L.tileLayer.provider = function (name) { return { kind: 'provider', name: name }; };")
  ctx$source(system.file(
    "htmlwidgets/lib/cc-vector-basemap/cc-vector-basemap.js", package = "calcofi4r"))

  style <- function(js) ctx$get(glue::glue("(function(l){{return l.kind === 'gl' ? l.options.style : l.kind}})({js})"))
  gl <- function(s) glue::glue("https://basemaps.cartocdn.com/gl/{s}-gl-style/style.json")

  # provider names (R leaflet addProviderTiles(), mapview's defaults)
  expect_equal(style("L.tileLayer.provider('CartoDB.Positron')"),           gl("positron"))
  expect_equal(style("L.tileLayer.provider('CartoDB')"),                    gl("positron"))
  expect_equal(style("L.tileLayer.provider('CartoDB.DarkMatter')"),         gl("dark-matter"))
  expect_equal(style("L.tileLayer.provider('CartoDB.VoyagerNoLabels')"),    gl("voyager-nolabels"))
  expect_equal(style("L.tileLayer.provider('CartoDB.PositronOnlyLabels')"), gl("positron"))
  expect_equal(style("L.tileLayer.provider('Esri.OceanBasemap')"),          "provider")

  # tile urls (plain Leaflet, R leaflet addTiles(), plotly.js 2's carto-positron)
  expect_equal(style("L.tileLayer('https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png')"),
               gl("positron"))
  expect_equal(style("L.tileLayer('https://{s}.basemaps.cartocdn.com/dark_nolabels/{z}/{x}/{y}.png')"),
               gl("dark-matter-nolabels"))
  expect_equal(style("L.tileLayer('https://{s}.basemaps.cartocdn.com/rastertiles/voyager_only_labels/{z}/{x}/{y}.png')"),
               gl("voyager"))
  expect_equal(style("L.tileLayer('https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png')"),
               gl("positron"))
  expect_equal(style("L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png')"), "raster")

  # the factory keeps its sub-factories; a labels-only layer is flagged for its symbol filter
  expect_equal(ctx$get("L.tileLayer.wms().kind"), "wms")
  expect_equal(ctx$get("L.tileLayer.provider('CartoDB.VoyagerOnlyLabels').ccCarto.labels"), "only")

  # regression: a GL layer carries a tile layer's zoom bounds; without maxZoom the map's
  # maxZoom is Infinity and Leaflet.markercluster fails ("reading '_addChild'")
  expect_equal(ctx$get("L.tileLayer.provider('CartoDB.Positron').options.maxZoom"), 20)
  expect_equal(ctx$get("L.tileLayer.provider('CartoDB.Positron', {maxZoom: 12}).options.maxZoom"), 12)
  # ... and registers them on the map as L.GridLayer does
  ctx$eval("var lim = []; var fake_map = { _addZoomLimit: function (l) { lim.push(l.options.maxZoom); } };
            L.tileLayer.provider('CartoDB.Positron').beforeAdd(fake_map);")
  expect_equal(ctx$get("lim"), 20)

  # idempotent: a second load on the same page does not wrap twice
  ctx$source(system.file(
    "htmlwidgets/lib/cc-vector-basemap/cc-vector-basemap.js", package = "calcofi4r"))
  expect_equal(style("L.tileLayer.provider('Esri.OceanBasemap')"), "provider")
})
