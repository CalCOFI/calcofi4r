# Send a tracking event from the Shiny server to the browser

Pushes an event over the session's existing websocket; the client-side
handler installed by
[`cc_ga_js()`](https://calcofi.io/calcofi4r/reference/cc_ga_js.md)
forwards it to GA4 and queues it for the Sheet log. **Non-blocking** —
no HTTP request is made on the R side, so a slow or unreachable log
endpoint can never stall a reactive.

## Usage

``` r
cc_track(session, event, ...)
```

## Arguments

- session:

  the Shiny `session` object

- event:

  event name, passed to
  [`cc_event()`](https://calcofi.io/calcofi4r/reference/cc_event.md)

- ...:

  named event parameters, passed to
  [`cc_event()`](https://calcofi.io/calcofi4r/reference/cc_event.md);
  `n_rows`, `ms`, `status` and `error` land in their own Sheet columns

## Value

the payload, invisibly

## Details

Use it for facts only the server knows (the taxa behind a picker value,
a query's row count, an error); pure UI interactions are better tracked
client-side with `window.ccTrack()` (see
[`cc_ga_js()`](https://calcofi.io/calcofi4r/reference/cc_ga_js.md)).

## Examples

``` r
if (FALSE) { # \dontrun{
cc_track(session, "map_query_sp", taxa = sel_name, n_rows = 1234, ms = 842)
} # }
```
