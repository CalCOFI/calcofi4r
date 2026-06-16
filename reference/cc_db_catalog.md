# Show CalCOFI database catalog as interactive table

Renders an interactive
[`DT::datatable()`](https://rdrr.io/pkg/DT/man/datatable.html) of every
table and column in a CalCOFI database release, with descriptions and
units sourced from the release `metadata.json` sidecar at
`gs://calcofi-db/ducklake/releases/{version}/metadata.json`. Column data
types come from DuckDB `information_schema.columns`.

## Usage

``` r
cc_db_catalog(tables = NULL, version = "latest")
```

## Arguments

- tables:

  Optional character vector of table names to filter to. Default: `NULL`
  (show all).

- version:

  Database version. Default: `"latest"`.

## Value

A [`DT::datatable()`](https://rdrr.io/pkg/DT/man/datatable.html) of
tables and columns.

## Details

For a richer point-and-click view with ERD diagram, dataset provenance,
and the canonical measurement-type registry, see the [CalCOFI Schema
explorer](https://calcofi.io/schema/).

## See also

[`cc_describe_table()`](https://calcofi.io/calcofi4r/reference/cc_describe_table.md)
for per-table schema as a tibble. [CalCOFI Schema
explorer](https://calcofi.io/schema/) for the web-based browser with ERD
and measurement-type registry.

## Examples

``` r
if (FALSE) { # \dontrun{
cc_db_catalog()
cc_db_catalog(tables = c("bottle", "ichthyo"))
cc_db_catalog(version = "v2026.05.14")
} # }
```
