# List available measurement types

Returns all available measurement types in the CalCOFI bottle database.

## Usage

``` r
cc_list_measurement_types(version = "latest", con = NULL)
```

## Arguments

- version:

  Database version (default: "latest")

- con:

  Optional open connection from
  [`cc_get_db()`](https://calcofi.io/calcofi4r/reference/cc_get_db.md).
  When given it is used as is (no new connection); `version` is then
  ignored.

## Value

Tibble with measurement_type, description, and units

## Examples

``` r
if (FALSE) { # \dontrun{
cc_list_measurement_types()
} # }
```
