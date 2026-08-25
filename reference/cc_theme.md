# Resolve the theme a Shiny request asks for

The server-side twin of `theme.js`'s resolution, for
`ui <- function(request)`: `?theme=dark|light` in the query string, else
the `cc_theme` cookie (`Domain=.calcofi.io`, set by any CalCOFI site's
toggle), else `default`. Pass the result as `mode` to
[`cc_brand_header()`](https://calcofi.io/calcofi4r/reference/cc_brand_header.md)
so bslib's switch starts in the right state and the page never flashes
the other colour.

## Usage

``` r
cc_theme(request = NULL, default = c("dark", "light"))
```

## Arguments

- request:

  the Rook request Shiny hands a `ui` function (`NULL` → `default`)

- default:

  theme when neither the URL nor a cookie says: `"dark"`, the calcofi.io
  convention

## Value

`"dark"` or `"light"`

## Examples

``` r
cc_theme(list(QUERY_STRING = "?theme=light"))
#> [1] "light"
cc_theme(list(HTTP_COOKIE = "theme=x; cc_theme=light"))
#> [1] "light"
cc_theme(NULL)
#> [1] "dark"
```
