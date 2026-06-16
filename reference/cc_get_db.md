# Connect to CalCOFI Database

Connects to a frozen CalCOFI DuckLake release. Downloads and caches the
database locally for fast subsequent access.

## Usage

``` r
cc_get_db(
  version = "latest",
  local_cache = TRUE,
  cache_dir = NULL,
  refresh = FALSE,
  local_data = FALSE,
  tables = NULL
)
```

## Arguments

- version:

  Version string (e.g., "v2026.02") or "latest" (default)

- local_cache:

  Use local cache if available (default: TRUE)

- cache_dir:

  Directory for local cache. Default uses
  `rappdirs::user_cache_dir("calcofi4r")` if rappdirs is installed,
  otherwise a temp directory.

- refresh:

  Force re-download even if cached (default: FALSE)

- local_data:

  Download parquet files locally and create tables instead of remote
  views (default: FALSE). Useful for apps that need fast local queries
  without network overhead.

- tables:

  Character vector of table names to include. NULL (default) includes
  all tables. Use to exclude large tables like CTD data.

## Value

DuckDB connection object

## Details

The frozen releases contain clean, stable data without provenance
columns, suitable for analysis and visualization. Use
[`cc_list_versions()`](https://calcofi.io/calcofi4r/reference/cc_list_versions.md)
to see available releases.

When `local_data = FALSE` (default), the connection points to Parquet
files from the frozen release registered as views in DuckDB. This allows
querying without downloading the entire database.

When `local_data = TRUE`, parquet files are downloaded to
`cache_dir/parquet/{version}/` and loaded as local tables for faster
queries. Files are only downloaded if missing or if `refresh = TRUE`.

Data is stored at `gs://calcofi-db/ducklake/releases/{version}/`.

## Examples

``` r
if (FALSE) { # \dontrun{
# connect to latest release (remote views)
con <- cc_get_db()
DBI::dbListTables(con)

# connect with local data (downloads parquets)
con <- cc_get_db(local_data = TRUE, cache_dir = "data")

# exclude CTD tables
con <- cc_get_db(
  local_data = TRUE,
  tables     = setdiff(cc_db_info()$tables$name,
    c("ctd_cast", "ctd_measurement", "ctd_summary")))
} # }
```
