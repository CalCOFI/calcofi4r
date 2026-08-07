# Read sub-occurrence frequency distributions (`obs_freq`)

The `(bin, count)` distributions within a taxon-occurrence — e.g.
ichthyo larval `body_length` and developmental `stage` distributions —
linked to
[`cc_read_obs()`](https://calcofi.io/calcofi4r/reference/cc_read_obs.md)
by `sample_key` + `taxon_id` + `life_stage`.

## Usage

``` r
cc_read_obs_freq(..., version = "latest", collect = TRUE)
```

## Arguments

- ...:

  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  expressions applied to `obs_freq`.

- version:

  database version (default `"latest"`).

- collect:

  if `TRUE` (default) return a tibble; else the lazy
  [`dplyr::tbl`](https://dplyr.tidyverse.org/reference/tbl.html).

## Value

Tibble of frequency bins (if `collect=TRUE`) or a lazy table.
