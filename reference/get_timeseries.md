# Get timeseries summary from CalCOFI API

**\[deprecated\]**

This function is deprecated because the CalCOFI API is being phased out
in favor of direct DuckDB database access. Query bottle measurement data
with
[`cc_get_db()`](https://calcofi.io/calcofi4r/reference/cc_get_db.md) and
aggregate using dplyr for time series analysis.

## Usage

``` r
get_timeseries(
  variable = "ctdcast_bottle.t_deg_c",
  aoi_wkt = NULL,
  depth_m_min = NULL,
  depth_m_max = NULL,
  date_beg = NULL,
  date_end = NULL,
  time_step = "year",
  stats = c("p10", "mean", "p90")
)
```

## Arguments

- variable:

  Variable to fetch from the CalCOFI API. One of `table_field` values
  from
  [`get_variables()`](https://calcofi.io/calcofi4r/reference/get_variables.md).
  Default is `"ctdcast_bottle.t_deg_c"`.

- aoi_wkt:

  Area of interest (AOI), spatially described as [well-known text
  (WKT)](https://en.wikipedia.org/wiki/Well-known_text_representation_of_geometry).

- depth_m_min:

  Minimum depth range in meters, e.g. 0. Default is `NULL`, as in not
  filtered.

- depth_m_max:

  Maximum depth range in meters, e.g. 5351. Default is `NULL`, as in not
  filtered.

- date_beg:

  Beginning of date range, e.g."1949-02-28". Default is `NULL`, as in
  not filtered.

- date_end:

  End of date range, e.g. "2020-01-26". Default is `NULL`, as in not
  filtered.

- time_step:

  Time step over which to summarize. One of: a sequential increment
  ("decade","year","year.quarter","year.month","year.week","date"), or a
  climatology ("quarter","month","week","julianday","hour"). Default is
  `"year"`.

- stats:

  Statistics to show per `date_step`. Acceptable values include any
  combination of: "avg", "median", "min", "max", "sd" or "p#" where "sd"
  is the standard deviation and "p#" represents the percentile value 0
  to 100 within available range of values. Default is
  `c("p10", "mean", "p90")`.

## Value

data frame of values

## Examples

``` r
if (FALSE) { # \dontrun{
# deprecated - use DuckDB queries instead:
con <- cc_get_db()
d <- DBI::dbGetQuery(con, "
  SELECT EXTRACT(YEAR FROM c.datetime_utc) AS year,
         AVG(bm.measurement_value) AS avg_temp
  FROM bottle_measurement bm
  JOIN bottle b ON bm.bottle_id = b.bottle_id
  JOIN casts c ON b.cast_id = c.cast_id
  WHERE bm.measurement_type = 'temperature'
  GROUP BY year ORDER BY year")
} # }
```
