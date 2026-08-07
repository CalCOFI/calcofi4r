# Read consolidated observations (`obs`)

The common occurrence-headline surface across every dataset: one scalar
per row, `realm` `'env'`/`'bio'`, provenance-stamped with `dataset_key`,
keyed to the sampling event via `sample_key`, and carrying
`grid_key`/`cruise_key`/ `hex_id` for rollups. Environmental CTD is
represented by the thinned `ctd_thin`; use
[`cc_read_ctd_full()`](https://calcofi.io/calcofi4r/reference/cc_read_ctd_full.md)
for full-resolution scans.

## Usage

``` r
cc_read_obs(
  ...,
  realm = NULL,
  datasets = NULL,
  measurement_types = NULL,
  version = "latest",
  collect = TRUE
)
```

## Arguments

- ...:

  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  expressions applied to `obs`.

- realm:

  optional `'env'` or `'bio'` to restrict the realm.

- datasets:

  optional vector of `dataset_key`s (e.g. `"calcofi_bottle"`).

- measurement_types:

  optional vector of `measurement_type`s.

- version:

  database version (default `"latest"`).

- collect:

  if `TRUE` (default) return a tibble; else the lazy
  [`dplyr::tbl`](https://dplyr.tidyverse.org/reference/tbl.html).

## Value

Tibble of observations (if `collect=TRUE`) or a lazy table.

## Examples

``` r
if (FALSE) { # \dontrun{
# larval abundance per station for one species
cc_read_obs(realm = "bio", measurement_types = "abundance",
            datasets = "swfsc_ichthyo")
} # }
```
