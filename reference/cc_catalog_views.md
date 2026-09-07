# Views a release catalog carries beside its tables

Since the v2026.09 releases (calcofi4db 3.31.0, pre-release plan D-S1)
`catalog.json` may carry a top-level `views` map: view name → SQL over
`{{table}}` tokens, one token per table the view reads. `obs` is the
first: the UNION ALL over `obs_bio` and `obs_env` that reconstructs its
18 columns under their original names, so `FROM obs` keeps working while
the observation rows ship once, as the pair. The table a view replaces
is marked `deprecated` in `tables[]` (with `replaced_by` and
`removed_in`) for the release it still ships in.

## Usage

``` r
cc_catalog_views(catalog)

cc_view_tables(sql)

cc_view_sql(catalog, name, rp = function(table) paste0("\"", table, "\""))
```

## Arguments

- catalog:

  a release catalog as returned by
  [`cc_catalog()`](https://calcofi.io/calcofi4r/reference/cc_catalog.md)
  (either jsonlite form)

- sql:

  a view's SQL carrying `{{table}}` tokens

- name:

  the view's name

- rp:

  `function(table) -> character(1)`

## Value

`cc_catalog_views()`: a named list of SQL strings; `cc_view_tables()`:
the distinct table names in order of first appearance; `cc_view_sql()`:
a length-one SQL string.

## Details

`cc_catalog_views()` lists the views (an empty list for a catalog
without any); `cc_view_tables()` the tables one reads; `cc_view_sql()`
its SQL with every token replaced by `rp(table)` — a quoted identifier
by default (the tables exist in the connection, as
[`cc_get_db()`](https://calcofi.io/calcofi4r/reference/cc_get_db.md)
arranges), or a `read_parquet(...)` from
[`cc_release_sources()`](https://calcofi.io/calcofi4r/reference/cc_release_sources.md) +
[`cc_read_parquet_sql()`](https://calcofi.io/calcofi4r/reference/cc_read_parquet_sql.md)
for a connection that has none. Wrap the result in parentheses to use it
in a `FROM`.

## Examples

``` r
if (FALSE) { # \dontrun{
cat_ <- cc_catalog("latest")
names(cc_catalog_views(cat_))
rp  <- function(t) cc_read_parquet_sql(cc_release_sources(cat_, t))
sql <- paste0("SELECT count(*) FROM (", cc_view_sql(cat_, "obs", rp), ")")
} # }
```
