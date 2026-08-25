# Is each observation's quality flag acceptable? (vectorised, in R)

The in-memory twin of
[`cc_qual_ok_sql()`](https://calcofi.io/calcofi4r/reference/cc_qual_ok_sql.md),
for data already collected.

## Usage

``` r
cc_qual_ok(dataset_key, measurement_qual)
```

## Arguments

- dataset_key, measurement_qual:

  Character vectors of equal length.

## Value

Logical, `TRUE` where the row should be kept.
