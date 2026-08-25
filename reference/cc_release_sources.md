# Resolve where a release table's parquet bytes live

Since the v2026.09 releases the database is **content-addressed**: each
table (or each partition of a partitioned table) is one immutable object
under `gs://calcofi-db/ducklake/tables/{table}/{content_hash}/…`, and a
release's `catalog.json` lists those objects per table in `objects[]`. A
release that changed three tables shares every other object with the
release before it, so a pinned consumer that has already fetched a table
need not fetch it again — and the archive can drop a retired version's
own copies without breaking anyone who reads through the catalog.

## Usage

``` r
cc_release_sources(
  catalog,
  table,
  base_https = "https://storage.googleapis.com/calcofi-db"
)
```

## Arguments

- catalog:

  a release catalog as returned by
  [`cc_catalog()`](https://calcofi.io/calcofi4r/reference/cc_catalog.md)
  (a list; a `jsonlite`-simplified data frame form is accepted too)

- table:

  table name

- base_https:

  https root of the bucket (default the public GCS host)

## Value

list with `urls` (character; https, or one `s3://` glob for a legacy
partitioned table), `hive` (logical: pass `hive_partitioning = true`),
`canonical` (logical: from `objects[]`), `hashes` (content hashes, or
`NA`), `local_paths` (relative cache paths, one per url,
content-addressed so a table unchanged between releases is cached once)
and `single_file` (the whole-table file a partitioned table may also
publish — `obs` does — for https-only readers that cannot take a list;
`NA` otherwise. Read one or the other, never both)

## Details

This is the one place a consumer turns a catalog entry into URLs. Read
the catalog with
[`cc_catalog()`](https://calcofi.io/calcofi4r/reference/cc_catalog.md)
(or `jsonlite::fromJSON(simplifyVector = FALSE)`) and never build a
`releases/{version}/parquet/…` path by hand: that path is only
guaranteed to answer for the promoted and consolidated versions.

Rules, in order:

1.  entry has `objects[]` → one https URL per object, in catalog order
    (partition files carry their `key=value` segment, so DuckDB's
    `hive_partitioning = true` recovers the partition column from the
    URL);

2.  otherwise (releases before v2026.09) → the legacy per-release path:
    `…/releases/{version}/parquet/{table}.parquet`, or an `s3://` glob
    for a partitioned table (DuckDB cannot glob over https).

## Examples

``` r
if (FALSE) { # \dontrun{
src <- cc_release_sources(cc_catalog("latest"), "obs")
con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbExecute(con, paste0("CREATE VIEW obs AS SELECT * FROM ", cc_read_parquet_sql(src)))
} # }
```
