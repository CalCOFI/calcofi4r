# Build Environmental Time Series Data

Aggregates environmental data by temporal resolution, computing mean and
standard error for visualization in time series plots. Uses dbplyr lazy
evaluation for efficient database queries.

## Usage

``` r
prep_ts_env(df_env, ts_res)
```

## Arguments

- df_env:

  dbplyr lazy table with columns: `dtime`, `qty`

- ts_res:

  Character string specifying temporal resolution: "year", "quarter",
  "month", "day", "year_quarter", "year_month", or "year_day"

## Value

data.frame with columns:

- `time` - aggregated time value

- `avg` - mean of `qty`

- `std` - standard error of `qty` (sd/n)

- `upr` - upper confidence bound (avg + std)

- `lwr` - lower confidence bound (avg - std)

## Details

For seasonal plots (`ts_res = "quarter"`), the function adds a wrapping
row to ensure visual continuity across the year boundary. Data is
collected from database only at the end of aggregation.

## See also

[`expr_time_env`](https://calcofi.io/calcofi4r/reference/expr_time_env.md)
for temporal transformation logic

[`plot_ts`](https://calcofi.io/calcofi4r/reference/plot_ts.md) for
visualization

## Examples

``` r
if (FALSE) { # \dontrun{
df_env <- get_env("t_deg_c", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"), min_depth = 0, max_depth = 100)
env_ts <- prep_ts_env(df_env, ts_res = "year")
} # }
```
