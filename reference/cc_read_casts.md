# Read CalCOFI cast data

Convenience function to read CTD/bottle cast data from the CalCOFI
database. The data is stored in the `casts` table.

## Usage

``` r
cc_read_casts(..., version = "latest", collect = TRUE)

cc_read_cast(..., version = "latest", collect = TRUE)
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

Tibble of casts data (if collect=TRUE) or lazy table

## Examples

``` r
if (FALSE) { # \dontrun{
# get first 100 casts
casts <- cc_read_casts() |> head(100)
} # }
```
