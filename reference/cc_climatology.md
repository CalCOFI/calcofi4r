# Seasonal climatology for one or more measurement types

A baseline per (`grid_key`, depth bin, month), which is the finest
grouping the CalCOFI sampling design supports: quarterly-ish cruises
over decades give many years per calendar month at a station, but not
many days.

## Usage

``` r
cc_climatology(
  con,
  variables = "temperature_ave",
  years = c(1993, 2013),
  dataset_key = "calcofi_ctd-cast",
  depth_max = 500,
  depth_bin = 5,
  min_n = 3
)
```

## Arguments

- con:

  DuckDB connection to a release.

- variables:

  `measurement_type`s.

- years:

  two-element baseline range, inclusive, e.g. `c(1993, 2013)`. Recorded
  on the result as the `baseline` attribute so a plot can state it.

- dataset_key, depth_max, depth_bin:

  as in
  [`cc_transect_section()`](https://calcofi.io/calcofi4r/reference/cc_transect_section.md).

- min_n:

  minimum observations for a cell to be returned (default 3).

## Value

Tibble: `grid_key`, `month`, `depth_m`, `variable`, `clim_mean`,
`clim_sd`, `clim_n`; with attribute `baseline`.

## Details

Deliberately a **plain monthly mean**, not a harmonic fit. Rudnick et
al. (2017) fit annual and semiannual harmonics for the CUGN glider
climatology, which suits near-continuous glider sampling; CalCOFI's is
episodic and unevenly spaced, and a monthly mean is both defensible and
legible — someone reading an anomaly can say exactly what it is a
departure from. `n` is returned so a thin cell can be filtered rather
than silently trusted.
