# Read CalCOFI taxonomy

One row per taxon, keyed `worms:<id>` or `itis:<id>` (birds key ITIS,
because WoRMS bird taxonomy lags). Carries `worms_id` / `itis_id` /
`gbif_id`, `parent_taxon_key` and the flattened classification, so a
hierarchy rollup ("everything in Decapoda") resolves without a second
source.

## Usage

``` r
cc_read_taxon(..., version = "latest", collect = TRUE)
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

Replaces the per-dataset `species` table, which the taxon consolidation
removed — see
[`cc_read_species()`](https://calcofi.io/calcofi4r/reference/cc_read_species.md).

## Examples

``` r
if (FALSE) { # \dontrun{
taxa <- cc_read_taxon()
anchovy <- cc_read_taxon(scientific_name == "Engraulis mordax")
} # }
```
