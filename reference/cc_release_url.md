# Where a database release is documented

The schema browser at <https://calcofi.io/db-schema/> opens on a
version's ERD, with its tables, columns, measurement types and release
notes — the one place a release chip should send someone.

## Usage

``` r
cc_release_url(version)
```

## Arguments

- version:

  release version, `"v2026.08.25"`

## Value

a URL

## Examples

``` r
cc_release_url("v2026.08.25")
#> [1] "https://calcofi.io/db-schema/#erd?v=v2026.08.25"
```
