# Analytics `<head>` snippet (GA4 + batched Sheet beacon)

Generates the self-contained HTML/JS installed once per page: the GA4
gtag loader, the client-side event queue that beacons to the usage-log
Sheet, the `Shiny.addCustomMessageHandler()` handlers that
[`cc_track()`](https://calcofi.io/calcofi4r/reference/cc_track.md) and
[`cc_track_session()`](https://calcofi.io/calcofi4r/reference/cc_track_session.md)
depend on, and a `window.ccTrack(event, params, metrics)` helper for
page JS.

## Usage

``` r
cc_ga_js(
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

character scalar of raw HTML (`<script>` tags)

## Details

The same snippet serves every CalCOFI app; `content_group` (and the
`app` column of the Sheet) is what separates them in reporting, so no
app needs its own measurement ID.

## Examples

``` r
substr(cc_ga_js(app = "db-viz-hex"), 1, 60)
#> <!-- Google tag (gtag.js) + CalCOFI usage log -->
#> <script as
```
