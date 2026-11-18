# Plot interactive depth of an oceanographic variable

The plot initially has descending depth on y-axis and ascending variable
(e.g., Temperature) on x-axis with each line representing a unique CTD
cast across each depth recorded. If the plot is interactive, then on
hover the CTD cast (uniquely identified by `cast_count`) is highlighted
in red and others dimmed in gray.

## Usage

``` r
plot_depth(df, variable = "Temperature", interactive = TRUE)
```

## Arguments

- df:

  data frame with columns: `cast_count`, `depth_m`, `v`

- variable:

  variable (character), should be one of `plot_title` from
  [`get_variables()`](https://calcofi.io/calcofi4r/reference/get_variables.md);
  default: `"Temperature"`

- interactive:

  whether to render interactive plot; default: `TRUE`

## Value

interactive plot of type
[`plotly::ggplotly()`](https://rdrr.io/pkg/plotly/man/ggplotly.html) or
static plot of type
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

## Examples

``` r
# plot depth with the station data
plot_depth(df = bottle_temp_depth, variable = "Temperature")
#> Warning: `get_variables()` was deprecated in calcofi4r 1.1.0.
#> ℹ Please use `cc_list_measurement_types()` instead.
#> ℹ The CalCOFI API is being phased out. Use DuckDB access via cc_get_db()
#>   instead.
#> ℹ The deprecated feature was likely used in the calcofi4r package.
#>   Please report the issue at <https://github.com/calcofi/calcofi4r/issues>.
#> Error in req_perform(request("https://api.calcofi.io/variables")): Failed to perform HTTP request.
#> Caused by error in `curl::curl_fetch_memory()`:
#> ! Timeout was reached [api.calcofi.io]:
#> Operation too slow. Less than 1 bytes/sec transferred the last 600 seconds
```
