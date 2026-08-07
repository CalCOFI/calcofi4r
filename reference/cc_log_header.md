# Column header for the usage-log Sheet

The exact first row the Google Sheet must carry for
[`cc_apps_script()`](https://calcofi.io/calcofi4r/reference/cc_apps_script.md)
to append into. Kept here so the Sheet, the Apps Script, and the client
payload cannot drift.

## Usage

``` r
cc_log_header()
```

## Value

character vector of column names, in order

## Details

The first ten columns are the original `db-viz-hex` query log, so rows
written before the non-blocking rewrite keep their meaning; the rest
were appended for the browser-driven channel (`app` identifies which app
wrote the row, so several apps can share one Sheet).

## Examples

``` r
cc_log_header()
#>  [1] "timestamp"   "ip"          "session"     "event"       "params"     
#>  [6] "n_rows"      "ms"          "status"      "error"       "app_version"
#> [11] "app"         "client_id"   "session_id"  "page"        "referrer"   
#> [16] "user_agent" 
```
