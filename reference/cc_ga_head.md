# Analytics `<head>` snippet as a Shiny tag

[`cc_ga_js()`](https://calcofi.io/calcofi4r/reference/cc_ga_js.md)
wrapped in
[`htmltools::HTML()`](https://rstudio.github.io/htmltools/reference/HTML.html)
for use inside `tags$head()`.

## Usage

``` r
cc_ga_head(
  app,
  content_group = app,
  app_version = "",
  ip = "",
  measurement_id = .CC_GA_ID,
  log_url = Sys.getenv("CALCOFI_LOG_URL", "")
)
```

## Arguments

- app:

  short app id recorded on every event, e.g. `"db-viz-hex"`

- content_group:

  GA4 content group for reporting; defaults to `app`

- app_version:

  version string recorded on every event, e.g. a git SHA

- ip:

  client IP stamped on every logged row. Behind shiny-server this is the
  **only** way to record a real one: the websocket handshake the R
  session sees is a fresh localhost connection with no
  `X-Forwarded-For`, so make the app's `ui` a `function(req)` and pass
  `cc_client_ip(req)` here. Left empty, the `ip` column falls back to
  whatever
  [`cc_track_session()`](https://calcofi.io/calcofi4r/reference/cc_track_session.md)
  reports (`127.0.0.1` behind a proxy).

- measurement_id:

  GA4 measurement ID; defaults to the CalCOFI apps property

- log_url:

  Apps Script `/exec` endpoint for the Sheet log. Defaults to the
  `CALCOFI_LOG_URL` environment variable; empty means the Sheet leg is a
  silent no-op (GA4 still receives events).

## Value

an `html` object

## Examples

``` r
if (FALSE) { # \dontrun{
ui <- bslib::page_sidebar(
  tags$head(calcofi4r::cc_ga_head("db-viz-hex", app_version = APP_VERSION)), ...)
} # }
```
