# The `https://calcofi.io/datasets/{dataset_key}/` page URL for a dataset

The one place a URL is built from a `dataset_key` — every consumer that
names a dataset should call this (or its equivalent in another language)
rather than hard-code the pattern.

## Usage

``` r
cc_dataset_page_url(dataset_key)
```

## Arguments

- dataset_key:

  one or more dataset keys

## Value

character vector of page URLs, same length as `dataset_key`

## Examples

``` r
cc_dataset_page_url("calcofi_bottle")
#> [1] "https://calcofi.io/datasets/calcofi_bottle/"
```
