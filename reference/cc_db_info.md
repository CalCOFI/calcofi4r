# Get CalCOFI database information

Returns metadata about a specific database release including table
schemas and statistics.

## Usage

``` r
cc_db_info(version = "latest")
```

## Arguments

- version:

  Version string or "latest" (default)

## Value

List with release metadata, table info, and schema

## Examples

``` r
if (FALSE) { # \dontrun{
cc_db_info()
cc_db_info("v2026.02")
} # }
```
