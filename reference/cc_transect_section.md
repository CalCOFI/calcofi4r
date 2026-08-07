# Observations along a transect, binned by depth

Long/tidy, one row per (station, depth bin, variable) — the shape both
the matrix builder and the climatology consume.

## Usage

``` r
cc_transect_section(
  con,
  line,
  cruise_key = NULL,
  variables = "temperature_ave",
  dataset_key = "calcofi_ctd-cast",
  depth_max = 500,
  depth_bin = 5,
  x = c("occupied", "line")
)
```

## Arguments

- con:

  DuckDB connection to a release.

- line, cruise_key, dataset_key:

  as in
  [`cc_transect_stations()`](https://calcofi.io/calcofi4r/reference/cc_transect_stations.md).

- variables:

  `measurement_type`s to return.

- depth_max:

  deepest bin, m (default 500 — a handful of casts reach 5000 m, and
  letting them set the axis squashes every standard cast into the top
  tenth of the plot).

- depth_bin:

  bin width, m (default 5).

- x:

  passed to
  [`cc_transect_stations()`](https://calcofi.io/calcofi4r/reference/cc_transect_stations.md).

## Value

Tibble: `cruise_key`, `sta`, `dist_km`, `depth_m`, `variable`, `value`.

## Details

Depth is binned because CTD sensors sample continuously (47.283 m,
47.916 m, …), so grouping by exact depth deduplicates almost nothing and
the native-resolution profile is jagged with sensor precision rather
than signal.
