# SQL predicate keeping only observations whose quality flag is not bad

Returns a boolean SQL fragment over `dataset_key` and `measurement_qual`
that is `TRUE` for unflagged rows (NULL), for datasets without a flag
vocabulary, and for codes not in
[CC_QUAL_EXCLUDE](https://calcofi.io/calcofi4r/reference/CC_QUAL_EXCLUDE.md);
`FALSE` for suspect / bad / missing. Bottle codes were written as
`"8.0"` through v2026.08.14, so the trailing `.0` is stripped before
comparison. Append it to any `WHERE` over `obs`, `obs_ctd_full`,
`sample_measurement` or `ctd_thin`.

## Usage

``` r
cc_qual_ok_sql(alias = NULL)
```

## Arguments

- alias:

  Optional table alias to prefix the columns with (e.g. `"o"`).

## Value

A single string.

## Examples

``` r
cc_qual_ok_sql("o")
#> [1] "COALESCE(NOT ((o.dataset_key = 'calcofi_bottle' AND regexp_replace(o.measurement_qual, '\\.0+$', '') IN ('8', '9')) OR (o.dataset_key = 'calcofi_ctd-cast' AND regexp_replace(o.measurement_qual, '\\.0+$', '') IN ('8', '9')) OR (o.dataset_key = 'calcofi_dic' AND regexp_replace(o.measurement_qual, '\\.0+$', '') IN ('3', '4', '9'))), TRUE)"
```
