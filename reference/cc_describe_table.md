# Describe a CalCOFI database table

Returns schema information for a table by joining DuckDB
`information_schema.columns` with the release `metadata.json` sidecar.
Adds `name_long`, `units`, and `description_md` per column when
available. The table-level description is attached as the
`description_md` attribute on the result.

## Usage

``` r
cc_describe_table(table, version = "latest")
```

## Arguments

- table:

  Table name.

- version:

  Database version (default: `"latest"`).

## Value

Tibble with one row per column: `column_name`, `data_type`,
`is_nullable`, `name_long`, `units`, `description_md`. The table
description is in `attr(<result>, "description_md")`.

## Details

For a point-and-click view of the same metadata (ERD, sortable
tables/columns, measurement-type registry), see the [CalCOFI Schema
explorer](https://calcofi.io/schema/).

## See also

[`cc_db_catalog()`](https://calcofi.io/calcofi4r/reference/cc_db_catalog.md)
for a DT::datatable() of every table and column in a release. [CalCOFI
Schema explorer](https://calcofi.io/schema/) for a web view of the same
metadata.

## Examples

``` r
if (FALSE) { # \dontrun{
cc_describe_table("ichthyo")
cc_describe_table("species")
cc_describe_table("casts")
attr(cc_describe_table("bottle"), "description_md")
} # }
```
