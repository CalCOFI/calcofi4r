# Build a tracking-event payload

The pure payload constructor shared by
[`cc_track()`](https://calcofi.io/calcofi4r/reference/cc_track.md) and
the tests. Normalises the event name to GA4's rules (lowercase,
`[a-z0-9_]`, leading letter, truncated to 40 characters) and drops
`NULL`/`NA`/empty parameters so the Sheet's `params` column stays
readable.

## Usage

``` r
cc_event(event, ...)
```

## Arguments

- event:

  event name, e.g. `"map_query_sp"`; coerced to GA4-safe form

- ...:

  named event parameters; `NULL`/`NA`/`""` are dropped and multi-value
  parameters are collapsed to a comma-separated string

## Value

list with `event` (character scalar), `params` (named list of character)
and `metrics` (named list of the reserved column values)

## Details

The names in
[`cc_log_header()`](https://calcofi.io/calcofi4r/reference/cc_log_header.md)
that have their own column — `n_rows`, `ms`, `status`, `error` — are
**reserved**: pass them like any other parameter and they are hoisted
out of `params` into `metrics`, keeping row counts and durations numeric
and chartable in the Sheet.

Parameter *values* are NOT truncated here — the Sheet leg wants the full
string (a full species name, a list of taxa). The client truncates to
100 characters for the gtag leg only.

## Examples

``` r
cc_event("Map Query", taxa = "Sardinops sagax", n_rows = 1234, ms = 842)
#> $event
#> [1] "map_query"
#> 
#> $params
#> $params$taxa
#> [1] "Sardinops sagax"
#> 
#> 
#> $metrics
#> $metrics$n_rows
#> [1] 1234
#> 
#> $metrics$ms
#> [1] 842
#> 
#> 
cc_event("download", file = NULL, format = "csv")   # NULL dropped
#> $event
#> [1] "download"
#> 
#> $params
#> $params$format
#> [1] "csv"
#> 
#> 
#> $metrics
#> list()
#> 
```
