# Read event-level effort measurements (`sample_measurement`)

Effort measurements that apply to all taxa in a sampling event — e.g.
net `volume_sampled` / `std_haul_factor` / `prop_sorted` / plankton
biomass, and bottle cast conditions — keyed by `sample_key`.

## Usage

``` r
cc_read_sample_measurement(..., version = "latest", collect = TRUE)
```

## Arguments

- ...:

  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  expressions applied to `sample_measurement`.

- version:

  database version (default `"latest"`).

- collect:

  if `TRUE` (default) return a tibble; else the lazy
  [`dplyr::tbl`](https://dplyr.tidyverse.org/reference/tbl.html).

## Value

Tibble of effort measurements (if `collect=TRUE`) or a lazy table.
