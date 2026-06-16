# Create index in database

Create index in database

## Usage

``` r
create_index(
  con,
  tbl,
  flds,
  is_geom = F,
  is_unique = F,
  overwrite = F,
  show = F,
  exec = T
)
```

## Arguments

- con:

  database connection object from
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html),
  e.g. from
  [`cc_db_connect()`](https://calcofi.io/calcofi4r/reference/cc_db_connect.md)

- tbl:

  table name as character

- flds:

  character vector of fields in table used for index

- is_geom:

  logical (default: FALSE) whether geometry field, so create GIST()
  index

- is_unique:

  logical (default: FALSE) whether to impose a unique constraint, to
  prevent duplicates; default: FALSE

- overwrite:

  logical (default: FALSE) whether to overwrite existing index

- show:

  logical (default: FALSE) whether to show SQL statement

- exec:

  logical (default: TRUE) whether to execute SQL statement

## Value

nothing

## Examples

``` r
if (FALSE) { # \dontrun{
con <- cc_db_connect()
create_index(con, "ctd_casts", "geom", is_geom=T)
create_index(con, "ctd_casts", "cast_count", is_unique=T)
} # }
```
