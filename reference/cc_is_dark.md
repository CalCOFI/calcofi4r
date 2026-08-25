# Is the app currently in dark mode?

Reads bslib's dark-mode switch (`input[[id]]`, `"dark"` or `"light"`),
for `is_dark` arguments such as
[`map_sp()`](https://calcofi.io/calcofi4r/reference/map_sp.md),
[`plot_ts()`](https://calcofi.io/calcofi4r/reference/plot_ts.md),
[`cc_plotly_theme()`](https://calcofi.io/calcofi4r/reference/cc_plotly_theme.md).
Before the switch has reported (first flush) it is `default`.

## Usage

``` r
cc_is_dark(input, id = "dark_toggle", default = TRUE)
```

## Arguments

- input:

  the Shiny `input` object

- id:

  the switch's id, as given to
  [`cc_brand_header()`](https://calcofi.io/calcofi4r/reference/cc_brand_header.md)

- default:

  value before the input exists

## Value

logical scalar
