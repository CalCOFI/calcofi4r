# Plot interactive time series of an oceanographic variable

Plot interactive time series of an oceanographic variable

## Usage

``` r
plot_timeseries(
  d,
  fld_date,
  fld_avg,
  fld_sd,
  title = "Temperature",
  y_label = "Temperature (C)",
  color = "red"
)
```

## Arguments

- d:

  data frame with data

- fld_date:

  unquoted name of field containing the date, e.g. `year`

- fld_avg:

  unquoted name of field containing the average value, e.g.
  `t_deg_c_avg`

- fld_sd:

  unquoted name of field containing the standard deviation, e.g. `sd`

- title:

  quoted title, Default is `"Temperature"`.

- y_label:

  quoted label for y-axis. Default is `"Temperature (C)"`.

- color:

  quoted color. Should be one of
  [`grDevices::colors()`](https://rdrr.io/r/grDevices/colors.html).
  Default is `"red"`.

## Value

interactive plot of
[`dygraphs::dygraph()`](https://rdrr.io/pkg/dygraphs/man/dygraph.html)

## Examples

``` r
# get variables
(v <- get_variables())
#> Error in req_perform(request("https://api.calcofi.io/variables")): Failed to perform HTTP request.
#> Caused by error in `curl::curl_fetch_memory()`:
#> ! Timeout was reached [api.calcofi.io]:
#> Operation too slow. Less than 1 bytes/sec transferred the last 600 seconds

# get data for the first variable
(d <- get_timeseries(v$table_field[1]))
#> Warning: `get_timeseries()` was deprecated in calcofi4r 1.1.0.
#> The CalCOFI API is being phased out.
#> ℹ Query data with cc_get_db() and aggregate with dplyr for time series.
#> Error: object 'v' not found

# plot time series with the first variable
with(v[1,],
  plot_timeseries(
    # data and columns (from d)
    d, year, t_deg_c_avg, t_deg_c_sd,
    # plot attributes (from v)
    plot_title, plot_label, plot_color))
#> Error: object 'v' not found
```
