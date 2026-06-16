# Prepare Data for Species-Environment Scatterplot

Joins species and environmental data by matching observations that are
close in time and space, enabling correlation analysis between abundance
and environmental variables.

## Usage

``` r
prep_splot(
  df_sp,
  df_env,
  env_stat,
  method = "nearest_time",
  max_hours_diff = 72,
  max_meters_diff = 1000
)
```

## Arguments

- df_sp:

  dbplyr lazy table or data.frame with species data

- df_env:

  dbplyr lazy table or data.frame with environmental data

- env_stat:

  Character string specifying aggregation function (e.g., "mean",
  "median")

- max_hours_diff:

  Numeric maximum time difference (in hours) for matching observations
  (default: 72)

- max_meters_diff:

  Numeric maximum spatial distance (in meters) for matching observations
  (default: 1000)

## Value

data.frame with matched species-environment observations

## Details

This function performs a fuzzy join based on temporal proximity using
[`fuzzyjoin::difference_inner_join()`](https://rdrr.io/pkg/fuzzyjoin/man/difference_join.html).
For each species observation, the closest environmental measurement
(within `max_hours_diff`) is selected. Data is collected from database
before joining.

## See also

[`get_sp`](https://calcofi.io/calcofi4r/reference/get_sp.md) for species
data retrieval

[`get_env`](https://calcofi.io/calcofi4r/reference/get_env.md) for
environmental data retrieval

## Examples

``` r
if (FALSE) { # \dontrun{
df_sp <- get_sp("Anchovy (Engraulis mordax)", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"))
df_env <- get_env("t_deg_c", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"), min_depth = 0, max_depth = 100)
df_splot <- prep_splot(df_sp, df_env, env_stat = "mean")
} # }
```
