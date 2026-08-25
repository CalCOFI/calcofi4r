# Quality codes a consumer should exclude, per dataset

Named list: `dataset_key` -\> the `measurement_qual` codes that mean
suspect, bad or missing in that dataset's vocabulary. Datasets not
listed carry no usable flag and are never excluded.

## Usage

``` r
CC_QUAL_EXCLUDE
```

## Format

An object of class `list` of length 3.
