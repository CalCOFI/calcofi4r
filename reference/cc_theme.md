# Resolve the theme a Shiny request asks for

The server-side twin of `theme.js`'s resolution, for
`ui <- function(request)`: `?theme=dark|light` in the query string, else
the `cc_theme` cookie (`Domain=.calcofi.io`, set by any CalCOFI site's
toggle) — honoured only beside its `cc_theme_src=user` marker, i.e. when
the visitor chose it (brand v2's persistence rule: a v1 page's default
can never leak in) — else `default`. Pass the result as `mode` to
[`cc_brand_header()`](https://calcofi.io/calcofi4r/reference/cc_brand_header.md)
so bslib's switch starts in the right state and the page never flashes
the other colour.

## Usage

``` r
cc_theme(request = NULL, default = c("light", "dark"))
```

## Arguments

- request:

  the Rook request Shiny hands a `ui` function (`NULL` → `default`)

- default:

  theme when neither the URL nor a cookie says: `"light"`, the
  calcofi.io convention since brand v2 (2026-09-04)

## Value

`"dark"` or `"light"`

## Examples

``` r
cc_theme(list(QUERY_STRING = "?theme=light"))
#> [1] "light"
cc_theme(list(HTTP_COOKIE = "cc_theme=dark; cc_theme_src=user"))
#> [1] "dark"
cc_theme(NULL)
#> [1] "light"
```
