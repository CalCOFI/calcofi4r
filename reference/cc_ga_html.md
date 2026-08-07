# Write the analytics snippet to a standalone HTML file

For apps that do **not** already depend on calcofi4r. Loading the
calcofi4r namespace costs several seconds of cold start (its imports
pull in sf, terra, stars, …), which a small app should not pay just to
be counted — so generate the snippet to a file and keep the app's
existing `includeHTML("google-analytics.html")`, with no runtime
dependency at all.

## Usage

``` r
cc_ga_html(
  path,
  app,
  content_group = app,
  app_version = "",
  measurement_id = .CC_GA_ID,
  log_url = ""
)
```

## Arguments

- path:

  file to write, e.g. `"google-analytics.html"`

- app:

  short app id recorded on every event, e.g. `"db-viz-hex"`

- content_group:

  GA4 content group for reporting; defaults to `app`

- app_version:

  version string recorded on every event, e.g. a git SHA

- measurement_id:

  GA4 measurement ID; defaults to the CalCOFI apps property

- log_url:

  Apps Script `/exec` endpoint. Defaults to `""` (GA4 only) — unlike
  [`cc_ga_js()`](https://calcofi.io/calcofi4r/reference/cc_ga_js.md), a
  generated file does not read the environment, since the result is
  committed to a repo.

## Value

`path`, invisibly

## Details

Apps that already attach calcofi4r should call
[`cc_ga_head()`](https://calcofi.io/calcofi4r/reference/cc_ga_head.md)
directly instead: it costs them nothing and can never go stale.

The file is **generated**. It carries a banner naming the exact call
that produced it, so when the shared snippet changes it is regenerated
rather than hand-patched in each repo.

A generated file is static, so it cannot carry the per-request `ip` of
[`cc_ga_js()`](https://calcofi.io/calcofi4r/reference/cc_ga_js.md) — the
Sheet leg is off by default here for that reason. GA4 is unaffected:
gtag resolves the client IP in the browser.

## Examples

``` r
f <- tempfile(fileext = ".html")
cc_ga_html(f, app = "marmam")
```
