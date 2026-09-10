# List available CalCOFI database versions

Lists all available frozen CalCOFI database releases by reading the
versions manifest from the public GCS bucket.

## Usage

``` r
cc_list_versions()
```

## Value

Tibble with columns: version, release_date, tables, total_rows, size_mb,
is_latest, doi (NA until Zenodo mints one) and consolidated; always
present, whatever the manifest carries

## Examples

``` r
if (FALSE) { # \dontrun{
cc_list_versions()
} # }
```
