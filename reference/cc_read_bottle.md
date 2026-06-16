# Read CalCOFI bottle data

Convenience function to read bottle sample data from the CalCOFI
database.

## Usage

``` r
cc_read_bottle(..., version = "latest", collect = TRUE)
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

Tibble of bottle data (if collect=TRUE) or lazy table

## Examples

``` r
if (FALSE) { # \dontrun{
# get all bottle data
bottles <- cc_read_bottle()

# filter by depth
shallow <- cc_read_bottle(depth_m < 100)
} # }
```
