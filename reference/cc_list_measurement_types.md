# List available measurement types

Returns all available measurement types in the CalCOFI bottle database.

## Usage

``` r
cc_list_measurement_types(version = "latest")
```

## Arguments

- version:

  Database version (default: "latest")

## Value

Tibble with measurement_type, description, and units

## Examples

``` r
if (FALSE) { # \dontrun{
cc_list_measurement_types()
} # }
```
