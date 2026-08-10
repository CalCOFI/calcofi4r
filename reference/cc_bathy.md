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
cc_bathy(path = NULL, cache_dir = NULL, refresh = FALSE)
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
