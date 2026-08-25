# Read a release catalog

Read a release catalog

## Usage

``` r
cc_catalog(
  version = "latest",
  base_https = "https://storage.googleapis.com/calcofi-db"
)
```

## Arguments

- version:

  release version (`"latest"` resolves through `latest.txt`)

- base_https:

  https root of the bucket

## Value

the catalog as a nested list (not simplified — `objects[]` stays a list
of one record per object)
