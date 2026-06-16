# Get variables from CalCOFI API

**\[deprecated\]**

This function is deprecated because the CalCOFI API is being phased out
in favor of direct DuckDB database access. Use
[`cc_list_measurement_types()`](https://calcofi.io/calcofi4r/reference/cc_list_measurement_types.md)
to see available oceanographic measurement types, or query the database
directly with
[`cc_get_db()`](https://calcofi.io/calcofi4r/reference/cc_get_db.md).

## Usage

``` r
get_variables()
```

## Value

data frame with columns:

- `category`: category, e.g. "Oceanographic"

- `table_field`: unique identifier of the variable given by the form of
  table.field

- `plot_title`: title used in a typical time series plot output

- `plot_label`: y-axis label used in a typical time series plot output

## Examples

``` r
if (FALSE) { # \dontrun{
# deprecated - use instead:
cc_list_measurement_types()
} # }
```
