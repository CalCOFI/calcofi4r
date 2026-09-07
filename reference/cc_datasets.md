# List CalCOFI datasets (the dataset-catalog record)

Reads a release's `datasets.json` — the record every dataset page,
ERDDAP `infoUrl`, and
[`cc_cite()`](https://calcofi.io/calcofi4r/reference/cc_cite.md)'s page
line are built from (`calcofi4db::build_dataset_catalog()`, calcofi4db
\>= 4.1.0) — into a data frame, one row per record. `distributions`,
`registrations`, `keywords`, `tables`, `objects` and
`coverage$years`/`coverage$variables` arrive as list-columns (one list
per row); `provider`, `category`, `attribution`, `links`, `coverage`,
`status` arrive as nested data frames (`jsonlite`'s `simplifyVector`
behavior) —
[`tidyr::unnest_wider()`](https://tidyr.tidyverse.org/reference/unnest_wider.html)
flattens one when needed.

## Usage

``` r
cc_datasets(
  version = "latest",
  what = c("datasets", "holdings", "reference"),
  base_https = "https://storage.googleapis.com/calcofi-db"
)
```

## Arguments

- version:

  release version (default `"latest"`)

- what:

  which table of the record to return: `"datasets"` (default, the 16+
  integrated datasets), `"holdings"` (datasets known but not yet in the
  database) or `"reference"` (cruises, stations, spatial layers,
  bathymetry — not datasets, but linked the same way)

- base_https:

  https root of the bucket

## Value

a
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html),
one row per record

## Details

A release frozen before the dataset catalog (calcofi4db \< 4.1.0, before
2026-09) carries no `datasets.json`; this errors naming the version
rather than returning an empty table, so a caller does not mistake "no
catalog yet" for "no datasets".

## Examples

``` r
if (FALSE) { # \dontrun{
ds <- cc_datasets()
ds[, c("dataset_key", "dataset_name_short")]
ds$links$page  # the dataset page URL, straight from the record
cc_datasets(what = "holdings")
} # }
```
