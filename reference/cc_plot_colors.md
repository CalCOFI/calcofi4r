# Plot colours for the current theme

The brand tokens a chart needs, so a plot on a dark page is not drawn
with black axis text: `fg` (text), `muted` (axis labels), `grid`,
`panel`, and a transparent `bg` so the plot inherits the page.

## Usage

``` r
cc_plot_colors(is_dark = TRUE)
```

## Arguments

- is_dark:

  logical

## Value

named list of colour strings

## Examples

``` r
cc_plot_colors(TRUE)$fg
#> [1] "#e6e9ed"
```
