# Theme a plotly figure for the current theme

Transparent paper/plot background (the page shows through), text in the
brand foreground, grid and zero lines in the border tone. Apply last,
after the plot's own
[`layout()`](https://rdrr.io/r/graphics/layout.html); merges rather than
replaces axis settings.

## Usage

``` r
cc_plotly_theme(p, is_dark = TRUE)
```

## Arguments

- p:

  a plotly object

- is_dark:

  logical

## Value

the plotly object
