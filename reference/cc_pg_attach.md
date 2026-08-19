# Attach the CalCOFI PostgreSQL database inside a DuckDB connection

Loads DuckDB's `postgres` extension and `ATTACH`es the PostgreSQL
database, so one DuckDB query can join the public release tables (from
[`cc_get_db()`](https://calcofi.io/calcofi4r/reference/cc_get_db.md))
with the team's PostgreSQL tables (`pg.ctd.flag`, `pg.work.*`, …). Host
/ port / user default exactly as in
[`cc_pg_connect()`](https://calcofi.io/calcofi4r/reference/cc_pg_connect.md);
the password is read by libpq from `~/.pgpass`.

## Usage

``` r
cc_pg_attach(
  con,
  alias = "pg",
  dbname = "calcofi",
  host = NULL,
  port = NULL,
  user = NULL,
  read_only = TRUE
)
```

## Arguments

- con:

  a DuckDB connection, e.g. from
  [`cc_get_db()`](https://calcofi.io/calcofi4r/reference/cc_get_db.md)
  or `DBI::dbConnect(duckdb::duckdb())`

- alias:

  catalog name inside DuckDB; default `"pg"`

- dbname:

  database name; default `"calcofi"` (`"gis"` is the legacy 2022 db)

- host:

  host name; default described above

- port:

  port; default `5432` (`PGPORT` overrides; use `15432` if your tunnel
  maps there)

- user:

  role name; default described above

- read_only:

  attach read-only (default `TRUE`)

## Value

`con` (invisibly)

## Details

With `read_only = FALSE` you can also write *into* PostgreSQL from
DuckDB (`INSERT INTO pg.work.my_table …`,
`CREATE TABLE pg.work.x AS SELECT …`), which is how bulk loads from
Parquet are done.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- cc_get_db()
cc_pg_attach(con)
DBI::dbGetQuery(con, "
  SELECT s.cruise_key, count(*) AS n_flags
  FROM pg.ctd.flag f JOIN sample s ON s.sample_key = f.sample_key
  GROUP BY 1 ORDER BY 2 DESC LIMIT 10")
} # }
```
