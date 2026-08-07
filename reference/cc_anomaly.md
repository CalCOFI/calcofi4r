# Join a section to a climatology and difference it

`anomaly = value - clim_mean`, matched on station, calendar month and
depth bin. Cells with no baseline come back `NA` rather than 0 — an
unsampled baseline is not a zero anomaly, and collapsing the two is how
a map ends up claiming "normal" for somewhere never measured.

## Usage

``` r
cc_anomaly(section, clim, stations)
```

## Arguments

- section:

  from
  [`cc_transect_section()`](https://calcofi.io/calcofi4r/reference/cc_transect_section.md).

- clim:

  from
  [`cc_climatology()`](https://calcofi.io/calcofi4r/reference/cc_climatology.md).

- stations:

  from
  [`cc_transect_stations()`](https://calcofi.io/calcofi4r/reference/cc_transect_stations.md)
  — supplies `grid_key` and the month, which `section` does not carry.

## Value

`section` plus `clim_mean`, `clim_sd`, `clim_n`, `anomaly`,
`anomaly_sd`; `baseline` attribute carried through.

## Details

`anomaly_sd` expresses the departure in baseline standard deviations,
which is what makes a 1 °C anomaly interpretable: large in the deep,
unremarkable at the surface in spring.
