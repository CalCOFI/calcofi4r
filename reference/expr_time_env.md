# Generate Time Aggregation Expression for Environmental Data

Creates a SQL-based expression for temporal aggregation of environmental
time series using DuckDB date functions. Used internally by
[`prep_ts_env`](https://calcofi.io/calcofi4r/reference/prep_ts_env.md).

## Usage

``` r
expr_time_env(ts_res)
```

## Arguments

- ts_res:

  Character string specifying temporal resolution: "year", "quarter",
  "month", "day", "year_quarter", "year_month", or "year_day"

## Value

Expression object suitable for use in
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
with dbplyr

## Details

For seasonal aggregation (`ts_res = "quarter"`), all quarters are
normalized to year 2000 to enable cyclic plotting. Uses DuckDB's
`date_trunc()` and `extract()` functions for database-side computation.

## See also

[`prep_ts_env`](https://calcofi.io/calcofi4r/reference/prep_ts_env.md)
for usage context

## Examples

``` r
if (FALSE) { # \dontrun{
df_env |> mutate(time = !!expr_time_env("year"))
} # }
```
