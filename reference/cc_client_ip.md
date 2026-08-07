# Best-effort client IP from a Shiny request

Reads `X-Forwarded-For` (set by the Caddy reverse proxy the apps sit
behind) and falls back to the direct `REMOTE_ADDR`. Never errors.

## Usage

``` r
cc_client_ip(x)
```

## Arguments

- x:

  a Shiny `session`, or the `req` environment handed to a `ui` function
  (anything carrying the request fields directly)

## Value

character scalar, or `NA_character_` if unavailable

## Details

**Pass the `req` of a `ui` function, not a `session`, when you can.**
shiny-server does not proxy the websocket upgrade — it opens a fresh
localhost connection to the R worker — so a session's `request` carries
no `X-Forwarded-For` and its `REMOTE_ADDR` is always `127.0.0.1`. The
page's HTTP request, which `ui = function(req)` receives, is the only
place the real client IP survives. See the `ip` argument of
[`cc_ga_js()`](https://calcofi.io/calcofi4r/reference/cc_ga_js.md).

## Examples

``` r
cc_client_ip(list(request = list(HTTP_X_FORWARDED_FOR = "203.0.113.7, 10.0.0.1")))
#> [1] "203.0.113.7"
cc_client_ip(list(HTTP_X_FORWARDED_FOR = "203.0.113.7"))   # a ui(req)
#> [1] "203.0.113.7"
```
