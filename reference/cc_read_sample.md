# Read the sampling-event dimension (`sample`)

One row per physical sampling event across every dataset, at its native
grain (`sample_type` =
site/tow/net/cast/bottle/underway/transect/region_pool), with
`parent_sample_key` + `root_sample_key` encoding the `site->tow->net`
and `cast->bottle` hierarchies. Count sampling events with
`dplyr::count(sample_type)` or `count(DISTINCT root_sample_key)`.

## Usage

``` r
cc_read_sample(
  ...,
  sample_types = NULL,
  datasets = NULL,
  version = "latest",
  collect = TRUE
)
```

## Arguments

- ...:

  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  expressions applied to `sample`.

- sample_types:

  optional vector of `sample_type`s.

- datasets:

  optional vector of `dataset_key`s.

- version:

  database version (default `"latest"`).

- collect:

  if `TRUE` (default) return a tibble; else the lazy
  [`dplyr::tbl`](https://dplyr.tidyverse.org/reference/tbl.html).

## Value

Tibble of sampling events (if `collect=TRUE`) or a lazy table.
