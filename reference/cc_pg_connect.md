# Connect to the CalCOFI PostgreSQL database

Opens a `DBI` connection (via `RPostgres`) to the multi-user CalCOFI
PostgreSQL database used by the CTD team for QA/QC (database `calcofi`,
schemas `ctd` / `work` / your own). Every argument has a sensible
default:

## Usage

``` r
cc_pg_connect(
  dbname = "calcofi",
  host = NULL,
  port = NULL,
  user = NULL,
  password = NULL,
  tunnel = FALSE,
  ...
)
```

## Arguments

- dbname:

  database name; default `"calcofi"` (`"gis"` is the legacy 2022 db)

- host:

  host name; default described above

- port:

  port; default `5432` (`PGPORT` overrides; use `15432` if your tunnel
  maps there)

- user:

  role name; default described above

- password:

  password; default `NULL` = use `~/.pgpass`

- tunnel:

  if `TRUE`, start an SSH tunnel with
  [`cc_pg_tunnel()`](https://calcofi.io/calcofi4r/reference/cc_pg_tunnel.md)
  first (only meaningful off-server)

- ...:

  passed to
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

## Value

a `DBI` connection; disconnect with
[`DBI::dbDisconnect()`](https://dbi.r-dbi.org/reference/dbDisconnect.html)

## Details

- **host**: `"postgis"` when running on the CalCOFI server (RStudio
  Server at rstudio.calcofi.io, Shiny), otherwise `"localhost"` — i.e.
  the local end of your SSH tunnel (`ssh -N calcofi`, or
  [`cc_pg_tunnel()`](https://calcofi.io/calcofi4r/reference/cc_pg_tunnel.md)).
  `PGHOST` overrides.

- **user**: `PGUSER` if set, else the role name found in your
  `~/.pgpass` for this host/port/database (the file you copied from the
  server), else your OS user name.

- **password**: `NULL` — libpq reads `~/.pgpass` (Windows:
  `%APPDATA%\\postgresql\\pgpass.conf`). Pass one only for throw-away
  use.

## See also

[`cc_pg_tunnel()`](https://calcofi.io/calcofi4r/reference/cc_pg_tunnel.md),
[`cc_pg_attach()`](https://calcofi.io/calcofi4r/reference/cc_pg_attach.md),
[`cc_get_db()`](https://calcofi.io/calcofi4r/reference/cc_get_db.md) for
the public releases

## Examples

``` r
if (FALSE) { # \dontrun{
con <- cc_pg_connect()                  # tunnel already running, ~/.pgpass in place
DBI::dbListObjects(con, DBI::Id(schema = "ctd"))
dplyr::tbl(con, I("ctd.cast"))

con <- cc_pg_connect(tunnel = TRUE)     # also opens `ssh -N calcofi` for you
} # }
```
