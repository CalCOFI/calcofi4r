# GEBCO seafloor bathymetry over the CalCOFI area

A
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
of GEBCO 2025 sub-ice bathymetry cropped to the CalCOFI grid, 15
arc-second (~390 m x 460 m at 33 degN), as **positive-down depth in
metres with land clamped to 0**. Downloaded once and cached, so the
first call in a session costs a 4.3 MB fetch and later ones cost
nothing.

## Usage

``` r
cc_bathy(
  path = NULL,
  cache_dir = NULL,
  refresh = FALSE,
  remote = FALSE,
  extent = c("calcofi", "full")
)
```

## Source

GEBCO Compilation Group (2025) GEBCO 2025 Grid,
<https://www.gebco.net/data_and_products/gridded_bathymetry_data/>.

## Arguments

- path:

  explicit raster to load; defaults to the option / env var above, then
  to the cached download.

- cache_dir:

  where to keep the download. Defaults to
  `rappdirs::user_cache_dir("calcofi4r")`.

- refresh:

  re-download even if cached (default `FALSE`).

- remote:

  read the published object in place over `/vsicurl/` instead of
  downloading it — GDAL then fetches only the blocks a query touches, so
  sampling a handful of points costs no download at all. Nothing is
  cached; the default stays download-and-cache (offline, deterministic).

- extent:

  `"calcofi"` (default) for the CalCOFI crop (lon −165 → −100 × lat 15 →
  56 — every released bottle / PIC / CUFES / dungeness / DIC /
  euphausiid position; ~129 MB), or `"full"` for the whole GEBCO source
  tile (lon −180 → −90 × lat 0 → 90; ~430 MB, so pair it with
  `remote = TRUE`). The full tile is published as raw GEBCO
  **elevation**; this function converts it on read to the same
  positive-down, land-0 `depth_m` convention as the crop, so callers
  never see the difference.

## Value

A
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html),
single layer `depth_m`.

## Details

The sign and land convention are baked into the published raster rather
than applied on read: a caller who sees `12.4` is 12.4 m under water,
and `0` is land. Nothing downstream has to remember to negate an
elevation.

## Using a local file instead

Set `options(calcofi4r.bathy = "/path/to.tif")` (or the `CALCOFI_BATHY`
environment variable) to bypass the download — how an app that already
ships its own crop, or a machine with no network, keeps working.

## The cache refreshes itself

When a cached copy exists, one cheap `HEAD` compares its size to the
published object's and re-downloads on a mismatch (once per session per
object) — so when the crop is re-cut nobody has to know to pass
`refresh = TRUE`. Offline, the `HEAD` fails quietly and the cache is
used as-is.
