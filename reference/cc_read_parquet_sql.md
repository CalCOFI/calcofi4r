# The `read_parquet(...)` SQL for a resolved source

The `read_parquet(...)` SQL for a resolved source

## Usage

``` r
cc_read_parquet_sql(src, paths = NULL, prefer_single_file = FALSE)
```

## Arguments

- src:

  result of
  [`cc_release_sources()`](https://calcofi.io/calcofi4r/reference/cc_release_sources.md)

- paths:

  override the paths to read (e.g. local downloads); default `src$urls`

- prefer_single_file:

  when `TRUE` and `src` has a `single_file` twin, read that one object
  instead of the partition list (for https-only readers that cannot
  expand a glob). Ignored when `paths` is given.

## Value

a length-one character SQL fragment
