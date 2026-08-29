# Column header for the feedback Sheet

The exact first row of the `feedback` tab that
[`cc_feedback_script()`](https://calcofi.io/calcofi4r/reference/cc_feedback_script.md)
appends into, so the Sheet, the script and the client payload cannot
drift. The submitter's `email` lives here and nowhere else — the public
issue never carries it.

## Usage

``` r
cc_feedback_header()
```

## Value

character vector of column names, in order

## Examples

``` r
cc_feedback_header()
#>  [1] "ts"         "app"        "url"        "release"    "viewport"  
#>  [6] "theme"      "text"       "email"      "image_url"  "issue_url" 
#> [11] "id"         "user_agent" "status"    
```
