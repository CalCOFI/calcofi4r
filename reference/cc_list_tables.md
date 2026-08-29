# List tables in CalCOFI database

Lists all available tables in a CalCOFI database release.

## Usage

``` r
cc_list_tables(version = "latest", con = NULL)
```

## Arguments

- version:

  Database version (default: "latest")

- con:

  Optional open connection from
  [`cc_get_db()`](https://calcofi.io/calcofi4r/reference/cc_get_db.md).
  When given it is used as is (no new connection); `version` is then
  ignored.

## Value

Character vector of table names

## Examples

``` r
if (FALSE) { # \dontrun{
cc_list_tables()

# reuse a connection
con <- cc_get_db()
cc_list_tables(con = con)
} # }
```
