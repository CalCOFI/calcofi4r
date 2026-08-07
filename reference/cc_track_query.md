# Time a query, log it, and return its result

Wraps a (usually lazy) query expression: times it, records the row count
when the result is a data frame, tracks the outcome via
[`cc_track()`](https://calcofi.io/calcofi4r/reference/cc_track.md), and
returns the result unchanged. An error is tracked as `status = "error"`
with its message and then re-raised, so instrumentation changes nothing
about the app's behaviour.

## Usage

``` r
cc_track_query(session, event, params = list(), expr)
```

## Arguments

- session:

  the Shiny `session` object

- event:

  event name, e.g. `"map_query_sp"`

- params:

  named list of query parameters (taxa, dates, depths, …)

- expr:

  the query expression; evaluated once, lazily

## Value

the value of `expr`

## Examples

``` r
if (FALSE) { # \dontrun{
df_sp <- cc_track_query(
  session, "map_query_sp", list(taxa = sel_name, quarters = sel_qtr),
  get_sp(sel_name, sel_qtr, sel_date_range, ck_children))
} # }
```
