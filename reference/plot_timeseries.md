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
#> # A tibble: 3 × 6
#>   category      table_field       plot_title plot_label plot_color color_palette
#>   <chr>         <chr>             <chr>      <chr>      <chr>      <chr>        
#> 1 Oceanographic ctd_bottles.sali… Salinity   Salinity … purple     Purples      
#> 2 Oceanographic ctd_bottles.o2sat Oxygen Sa… Oxygen pe… blue       Blues        
#> 3 Oceanographic ctd_bottles.t_de… Temperatu… Temperatu… red        Reds         

# get data for the first variable
(d <- get_timeseries(v$table_field[1]))
#> Warning: `get_timeseries()` was deprecated in calcofi4r 1.1.0.
#> The CalCOFI API is being phased out.
#> ℹ Query data with cc_get_db() and aggregate with dplyr for time series.
#> # A tibble: 71 × 4
#>     year val_avg val_sd n_obs
#>    <dbl>   <dbl>  <dbl> <dbl>
#>  1  1949    33.8  0.550 28081
#>  2  1950    33.9  0.515 38298
#>  3  1951    34.0  0.512 40459
#>  4  1952    33.8  0.469 31690
#>  5  1953    33.9  0.438 29789
#>  6  1954    33.9  0.469 18874
#>  7  1955    34.0  0.475 23248
#>  8  1956    34.1  0.536 14274
#>  9  1957    34.2  0.552 19072
#> 10  1958    33.9  0.420 24324
#> # ℹ 61 more rows

# plot time series with the first variable
with(v[1,],
  plot_timeseries(
    # data and columns (from d)
    d, year, t_deg_c_avg, t_deg_c_sd,
    # plot attributes (from v)
    plot_title, plot_label, plot_color))
#> Error in dplyr::mutate(., `-sd` = {    {        fld_avg    }} - {    {        fld_sd    }}, `+sd` = {    {        fld_avg    }} + {    {        fld_sd    }}): ℹ In argument: `-sd = t_deg_c_avg - t_deg_c_sd`.
#> Caused by error:
#> ! object 't_deg_c_avg' not found
```
