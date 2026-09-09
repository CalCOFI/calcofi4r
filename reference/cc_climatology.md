# Seasonal climatology for one or more measurement types

The baseline every CalCOFI anomaly is a departure from: a plain mean per
(`site_key` — the real station, calendar month, 10 m floor depth bin,
measurement type) across a window of years, kept where at least
`min_cruises` distinct cruises contribute. Calendar month is the finest
season CalCOFI's design supports — quarterly-ish cruises over decades
give many *years* per month at a station but only a handful of days —
and the coarsest that works: a mean over all months is a map of the
seasonal cycle, not an anomaly. A plain mean rather than harmonics:
Rudnick et al. (2017) fit annual and semiannual harmonics for the CUGN
glider climatology, which suits continuous glider sampling; CalCOFI's is
episodic, and a monthly mean is something a reader can state exactly.

## Usage

``` r
cc_climatology(
  con,
  variables = "temperature_ave",
  years = c(1993, 2013),
  dataset_key = "calcofi_ctd-cast",
  depth_max = 500,
  depth_bin = 10,
  min_cruises = 3
)
```

## Arguments

- con:

  DuckDB connection to a release
  ([`cc_get_db()`](https://calcofi.io/calcofi4r/reference/cc_get_db.md)).

- variables:

  `measurement_type`s.

- years:

  two-element baseline range, inclusive, e.g. `c(1993, 2013)`. Recorded
  on the result as the `baseline` attribute so a plot can state it.

- dataset_key, depth_max:

  as in
  [`cc_transect_section()`](https://calcofi.io/calcofi4r/reference/cc_transect_section.md).

- depth_bin:

  bin width, m (default 10; floor bins, labelled by the shallow edge).
  The release table is only used at 10.

- min_cruises:

  minimum distinct cruises for a cell to be returned (default 3). A
  floor in cruises rather than observations because a nearshore grid
  cell holds several stations' casts from one cruise.

## Value

Tibble: `site_key` (the station; absent only when reading a release
table from before v2026.09.2x, which was grained on the grid cell),
`grid_key` (the station's modal cell — the inshore cells hold 2–4
stations, so key on `site_key`), `month`, `depth_m`, `variable`,
`clim_mean`, `clim_sd`, `clim_n`, `n_cruises`; attributes `baseline`
(the years) and `source` (`"release"` or `"computed"`).

## Details

**Since the releases of 2026-09 the database ships this table** —
`climatology`, built by `calcofi4db::build_climatology()` at release
time with exactly this definition, the window stamped on every row — and
ctd-transects and the CalCOFI Explorer subtract it. When `con` holds
that table and `years` is its window, this returns its cells (`source`
attribute `"release"`), so an R user, the two apps and any notebook see
one baseline. Otherwise (an older release, or another window) it
computes the same thing from `obs` (`source` = `"computed"`).
