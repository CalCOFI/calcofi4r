# Get cruises from CalCOFI API

**\[deprecated\]**

This function is deprecated because the CalCOFI API is being phased out
in favor of direct DuckDB database access. Use
[`cc_read_cruise()`](https://calcofi.io/calcofi4r/reference/cc_read_cruise.md)
to read cruise data, or query the database directly with
[`cc_get_db()`](https://calcofi.io/calcofi4r/reference/cc_get_db.md).

## Usage

``` r
get_cruises()
```

## Value

data frame with columns:

- `cruise_id`: unique identifier for cruise

- `date_beg`: date cruise began

- `date_end`: date cruise ended

- `lon_min`: longitude, minimum (as in for bounding box of cruise)

- `lon_max`: longitude, minimum (as in for bounding box of cruise)

- `lat_min`: latitude, minimum (as in for bounding box of cruise)

- `lat_max`: latitude, minimum (as in for bounding box of cruise)

- `n_casts`: number of oceanographic CTD casts made for given cruise

## Examples

``` r
if (FALSE) { # \dontrun{
# deprecated - use instead:
cc_read_cruise()
} # }
```
