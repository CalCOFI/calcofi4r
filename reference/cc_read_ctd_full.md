# Read full-resolution CTD scans (`obs_ctd_full`, supplemental)

The supplemental full per-scan CTD table (~216M rows), same schema as
`obs`. The default `obs` already carries CTD via the thinned `ctd_thin`;
for full-resolution work filter CTD out of `obs` and union
`obs_ctd_full` — never sum both (`ctd_thin` is a subset of
`ctd_measurement`). Attached only when the release catalog lists it;
expect a large scan.

## Usage

``` r
cc_read_ctd_full(..., version = "latest", collect = TRUE)
```

## Arguments

- ...:

  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  expressions applied to `obs_ctd_full`.

- version:

  database version (default `"latest"`).

- collect:

  if `TRUE` (default) return a tibble; else the lazy
  [`dplyr::tbl`](https://dplyr.tidyverse.org/reference/tbl.html).

## Value

Tibble of full-resolution CTD observations or a lazy table.
