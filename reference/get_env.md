# Retrieve Environmental Data from Database

Queries environmental bottle cast data with temporal, depth, and
variable filters. Returns a dbplyr lazy table for efficient downstream
processing.

## Usage

``` r
get_env(env_var, qtr, date_range, min_depth, max_depth)
```

## Arguments

- env_var:

  Character string of database column name for environmental variable
  (e.g., "t_deg_c", "salnty")

- qtr:

  Character or numeric vector of quarters to include (1-4)

- date_range:

  Date vector of length 2 (start date, end date)

- min_depth:

  Numeric minimum depth in meters

- max_depth:

  Numeric maximum depth in meters

## Value

dbplyr lazy table with columns:

- `date` - date of cast

- `time` - time of cast (seconds since midnight)

- `dtime` - datetime (computed via SQL CAST and INTERVAL)

- `depthm` - depth in meters

- `lat_dec` - latitude (decimal degrees)

- `lon_dec` - longitude (decimal degrees)

- `qty` - renamed environmental variable value

- `hex_h3res*` - H3 hexagon indices at multiple resolutions

## Details

The function joins `cast` and `bottle` tables, then joins with `site` to
obtain H3 spatial indices. Only records with non-NA values for the
selected variable are returned. Datetime is constructed from separate
date and time fields using DuckDB SQL.

## See also

[`prep_env_hex`](https://calcofi.io/calcofi4r/reference/prep_env_hex.md)
for spatial aggregation

[`prep_ts_env`](https://calcofi.io/calcofi4r/reference/prep_ts_env.md)
for temporal aggregation

## Examples

``` r
if (FALSE) { # \dontrun{
# retrieve temperature data for Q1-Q2 2010-2020
df_env <- get_env(
  env_var    = "t_deg_c",
  qtr        = c(1, 2),
  date_range = as.Date(c("2010-01-01", "2020-12-31")),
  min_depth  = 0,
  max_depth  = 100
)
df_env |> collect()
} # }
```
