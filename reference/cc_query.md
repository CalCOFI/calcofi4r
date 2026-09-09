# Execute SQL query on CalCOFI database

Convenience function to execute arbitrary SQL queries against a frozen
CalCOFI database release.

## Usage

``` r
cc_query(sql, version = "latest")
```

## Arguments

- sql:

  SQL query string

- version:

  Database version (default: "latest")

## Value

Tibble with query results

## Examples

``` r
if (FALSE) { # \dontrun{
results <- cc_query("SELECT * FROM obs_bio LIMIT 10")
results <- cc_query("
  SELECT taxon_key, COUNT(*) AS n FROM obs_bio
  WHERE dataset_key = 'swfsc_ichthyo' GROUP BY taxon_key ORDER BY n DESC LIMIT 10")
} # }
```
