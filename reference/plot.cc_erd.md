# Plot a cc_erd object as an interactive diagram

Renders the Mermaid ERD in the Viewer pane via
[`DiagrammeR::mermaid()`](https://rich-iannone.github.io/DiagrammeR/reference/mermaid.html).
Colors are embedded as `themeCSS` in the Mermaid `%%{init}%%` directive
and render natively.

## Usage

``` r
# S3 method for class 'cc_erd'
plot(x, ...)
```

## Arguments

- x:

  A `cc_erd` object returned by
  [`cc_erd()`](https://calcofi.io/calcofi4r/reference/cc_erd.md)

- ...:

  Ignored

## Value

The
[`DiagrammeR::mermaid()`](https://rich-iannone.github.io/DiagrammeR/reference/mermaid.html)
htmlwidget (invisibly)
