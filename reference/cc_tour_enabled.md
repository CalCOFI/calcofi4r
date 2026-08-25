# Should the guided tour run? (`?tour=off`)

The contract's one URL parameter besides `?theme=`:
`tour=off|false|0|no` suppresses a first-visit tour or welcome modal so
a screenshot — or a colleague following a link — sees the interface.
Everything else (absent, `on`, `1`, …) leaves the app's own first-visit
logic in charge.

## Usage

``` r
cc_tour_enabled(query = NULL, session = shiny::getDefaultReactiveDomain())
```

## Arguments

- query:

  a query string (`"?tour=off"`) or a parsed list; `NULL` reads the
  session's URL via
  [`shiny::getQueryString()`](https://rdrr.io/pkg/shiny/man/getQueryString.html)
  (reactive context required)

- session:

  Shiny session, for the `NULL` case

## Value

`TRUE` unless the URL switched the tour off

## Examples

``` r
cc_tour_enabled("?tour=off")
#> [1] FALSE
cc_tour_enabled("?cruise=2026-04-3322")
#> [1] TRUE
```
