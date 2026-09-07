# Plot colours for the current theme

The brand tokens a chart needs, so a plot on a dark page is not drawn
with black axis text: `fg` (text), `muted` (axis labels), `grid`,
`panel`, and a transparent `bg` so the plot inherits the page. The
values are brand v2's (UCSD navy on white; navy ground in dark) since
calcofi4r 1.18.0.

## Usage

``` r
cc_plot_colors(is_dark = FALSE)
```

## Arguments

- is_dark:

  logical

## Value

named list of colour strings

## Examples

``` r
cc_plot_colors(FALSE)$fg
#> [1] "#182b49"
```
