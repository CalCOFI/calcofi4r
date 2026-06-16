# Read CalCOFI ichthyoplankton (larvae) data

Convenience function to read ichthyoplankton (fish larvae) data from the
CalCOFI database. The data is stored in the `ichthyo` table.

## Usage

``` r
cc_read_ichthyo(..., version = "latest", collect = TRUE)

cc_read_larvae(..., version = "latest", collect = TRUE)
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

Tibble of ichthyo data (if collect=TRUE) or lazy table

## Examples

``` r
if (FALSE) { # \dontrun{
# get first 100 ichthyo records
ichthyo <- cc_read_ichthyo() |> head(100)

# get specific species (lazy query)
anchovy <- cc_read_ichthyo(species_id == 19, collect = FALSE)
} # }
```
