# Build Species Time Series Data

Aggregates species abundance data by temporal resolution, computing mean
and standard error for visualization in time series plots. Uses dbplyr
lazy evaluation for efficient database queries.

## Usage

``` r
prep_ts_sp(df_sp, ts_res)
```

## Arguments

- df_sp:

  dbplyr lazy table or data.frame with columns: `time_start`, `name`,
  `std_tally`

- ts_res:

  Character string specifying temporal resolution: "year", "quarter",
  "month", "day", "year_quarter", "year_month", or "year_day"

## Value

data.frame with columns:

- `time` - aggregated time value

- `name` - species name

- `avg` - mean standardized tally

- `std` - standard error (sd/n)

- `n` - number of observations

- `upr` - upper confidence bound (avg + std)

- `lwr` - lower confidence bound (avg - std)

## Details

For seasonal plots (`ts_res = "quarter"`), the function adds a wrapping
row to ensure visual continuity across the year boundary. Data is
collected from database before aggregation.

## See also

[`expr_time_sp`](https://calcofi.io/calcofi4r/reference/expr_time_sp.md)
for temporal transformation logic

[`plot_ts`](https://calcofi.io/calcofi4r/reference/plot_ts.md) for
visualization

## Examples

``` r
if (FALSE) { # \dontrun{
df_sp <- get_sp("Anchovy (Engraulis mordax)", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"))
sp_ts <- prep_ts_sp(df_sp, ts_res = "year")
} # }
```
