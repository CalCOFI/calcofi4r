# Read CalCOFI species data (deprecated)

**\[deprecated\]** The `species` table no longer exists: the taxon
consolidation replaced the ~7 per-dataset taxonomy tables with one
global
[`taxon`](https://calcofi.io/calcofi4r/reference/cc_read_taxon.md) keyed
`worms:<id>` / `itis:<id>`.

## Usage

``` r
cc_read_species(..., version = "latest", collect = TRUE)
```

## Arguments

- ...:

  Additional filter expressions passed to
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)

- version:

  Database version (default: "latest")

- collect:

  If TRUE, collect results into memory. If FALSE, return lazy dbplyr
  table (default: TRUE)

## Value

Tibble of taxa (if collect=TRUE) or lazy table

## Details

This called `tbl(con, "species")` and failed with "Can't query fields"
against any release from v2026.07 on. It now forwards to
[`cc_read_taxon()`](https://calcofi.io/calcofi4r/reference/cc_read_taxon.md)
with a warning, so existing scripts keep running — but the columns
differ (`scientific_name` and `common_name` survive; `species_id` is
replaced by `taxon_key`), so check any code that joined on `species_id`.
