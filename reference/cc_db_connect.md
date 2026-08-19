# Connect to the CalCOFI PostgreSQL database (Admin only) - DEPRECATED

deprecated

This function is deprecated. For the public releases use
[`cc_get_db`](https://calcofi.io/calcofi4r/reference/cc_get_db.md)
(DuckDB over Parquet); for the multi-user PostgreSQL database (CTD
QA/QC, database `calcofi`) use
[`cc_pg_connect`](https://calcofi.io/calcofi4r/reference/cc_pg_connect.md),
which reads your role and password from `~/.pgpass` instead of a
password file.

Note that you must either be running this from the CalCOFI server or
have a [tunnelled SSH
connection](https://github.com/calcofi/server#ssh-tunnel-connection-to-postgis-db)
with a user account on the server and the password in a file located at
`"~/.calcofi_db_pass.txt"`.

## Usage

``` r
cc_db_connect(path_pw = "~/.calcofi_db_pass.txt")
```

## Arguments

- path_pw:

  path to password file with one line containing the database password;
  default: `"~/.calcofi_db_pass.txt"`

## Value

a [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)
object

## Examples

``` r
if (FALSE) { # \dontrun{
con <- cc_db_connect()
DBI::dbListTables(con)
} # }
```
