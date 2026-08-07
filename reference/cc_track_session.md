# Hand the browser the session facts only the server knows

The client IP and the Shiny session token cannot be read in JavaScript,
so the server pushes them once at session start; the client then stamps
them on every queued row (the `ip` and `session` columns of
[`cc_log_header()`](https://calcofi.io/calcofi4r/reference/cc_log_header.md)).
Call it at the top of the `server` function, before any
[`cc_track()`](https://calcofi.io/calcofi4r/reference/cc_track.md) call,
so no event is written without them.

## Usage

``` r
cc_track_session(session)
```

## Arguments

- session:

  the Shiny `session` object

## Value

the sent list, invisibly

## Details

The token is authoritative, the IP is only a **fallback**: behind
shiny-server a session sees `127.0.0.1`, so an `ip` already baked into
the page by
[`cc_ga_js()`](https://calcofi.io/calcofi4r/reference/cc_ga_js.md) wins
and this one is ignored.

## Examples

``` r
if (FALSE) { # \dontrun{
server <- function(input, output, session) {
  cc_track_session(session)
  ...
}
} # }
```
