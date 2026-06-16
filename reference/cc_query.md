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
results <- cc_query("SELECT * FROM ichthyo LIMIT 10")
results <- cc_query("SELECT species_id, COUNT(*) as n FROM ichthyo GROUP BY species_id")
} # }
```
