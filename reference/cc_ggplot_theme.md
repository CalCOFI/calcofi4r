# A ggplot2 theme for the current theme

[`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
with transparent backgrounds and the brand text / grid colours, for a
static or `ggplotly()`-converted plot on a themed page.

## Usage

``` r
cc_ggplot_theme(is_dark = TRUE, base_size = 11)
```

## Arguments

- is_dark:

  logical

- base_size:

  passed to
  [`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)

## Value

a ggplot2 theme
