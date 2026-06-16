# Read CalCOFI cruise data

Convenience function to read cruise metadata from the CalCOFI database.

## Usage

``` r
cc_read_cruise(..., version = "latest", collect = TRUE)
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

Tibble of cruise data (if collect=TRUE) or lazy table

## Examples

``` r
if (FALSE) { # \dontrun{
cruises <- cc_read_cruise()
} # }
```
