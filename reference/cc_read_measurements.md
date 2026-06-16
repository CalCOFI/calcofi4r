# Read CalCOFI bottle measurements

Convenience function to read oceanographic measurements from the CalCOFI
bottle database. Returns data in long format from `bottle_measurement`
table.

## Usage

``` r
cc_read_measurements(
  ...,
  measurement_types = NULL,
  version = "latest",
  collect = TRUE
)
```

## Arguments

- ...:

  Additional filter expressions passed to
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)

- measurement_types:

  Character vector of measurement types to include. Default is NULL (all
  types). Use
  [`cc_list_measurement_types()`](https://calcofi.io/calcofi4r/reference/cc_list_measurement_types.md)
  to see available types.

- version:

  Database version (default: "latest")

- collect:

  If TRUE, collect results into memory. If FALSE, return lazy dbplyr
  table (default: TRUE)

## Value

Tibble of measurement data (if collect=TRUE) or lazy table

## Examples

``` r
if (FALSE) { # \dontrun{
# get temperature and salinity measurements
temp_sal <- cc_read_measurements(c("temperature", "salinity"))
} # }
```
