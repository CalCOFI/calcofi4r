# Cite CalCOFI data

Every CalCOFI release cites itself (`catalog.json`'s `citation`, set by
`calcofi4db::add_release_citation()`) and every dataset in it carries a
checked `citation_main`, a registered `license` and, where the source
gives one, a `doi` and `acknowledgement`
(`calcofi4db::check_dataset_citation()`, calcofi4db \>= 3.30.0, the
attribution contract). `cc_cite()` is the one place that formats them
for a paper, a data-management plan or a `.bib` file — read the
`dataset` table off `con`, do not build a citation string by hand.

## Usage

``` r
cc_cite(
  x = NULL,
  version = "latest",
  format = c("text", "bibtex", "csl"),
  con = NULL,
  resolve = FALSE
)
```

## Arguments

- x:

  `NULL` (every dataset), a character vector of `dataset_key`, or a data
  frame/tibble carrying a `dataset_key` column.

- version:

  release version (default `"latest"`). Only consulted for the
  release-level citation (`cc_catalog(version)`) — with `con` supplied,
  pass the version `con` was opened on if it is not `"latest"`, the same
  caveat as
  [`cc_describe_table()`](https://calcofi.io/calcofi4r/reference/cc_describe_table.md).

- format:

  `"text"` (a character vector, release citation first), `"bibtex"` (one
  string, every `@misc{...}` entry concatenated) or `"csl"` (a list of
  CSL-JSON items, one per entry).

- con:

  optional open connection from
  [`cc_get_db()`](https://calcofi.io/calcofi4r/reference/cc_get_db.md);
  when given it is used as is (no new connection).

- resolve:

  `format = "bibtex"` only: fetch the DOI's own BibTeX from `doi.org`
  for any entry with a DOI, instead of building it offline (default
  `FALSE`).

## Value

See `format`. The result carries a `source` attribute (`"release"` or
`"computed"`) describing where the release-level citation came from.

## Details

Every call returns the **release citation first**, then one entry per
dataset. With `x = NULL` (default) that is every dataset in the release,
alphabetical by `dataset_key`; a character vector of `dataset_key` or a
data frame/tibble carrying a `dataset_key` column (so
`cc_cite(cc_read_obs(...))` works directly on a query result) cites just
those, de-duplicated, in the order given. A `dataset_key` that does not
exist in the release is an error naming it.

Each dataset entry always carries its `citation_main`; `format = "text"`
appends a `License: <id>` line (plus the URL, for a `custom` license), a
`DOI:` line when the dataset has one, an `Acknowledgement:` line when
the source requires one, and (2026-09-05) always a `Page:` line linking
`https://calcofi.io/datasets/{dataset_key}/` — the dataset-catalog
record's own page
([`cc_datasets()`](https://calcofi.io/calcofi4r/reference/cc_datasets.md));
the release citation gets the same line for
`https://calcofi.io/datasets/release/`. `format = "bibtex"` and
`format = "csl"` fold license and acknowledgement into one `note`/`note`
field instead, since neither format has a natural place for more than
one, and do not carry the page line.

`format = "bibtex"` builds every `@misc{...}` entry **offline**, from
the fields already on `dataset` and in the catalog — nothing here calls
the network by default. `resolve = TRUE` instead fetches
`https://doi.org/<doi>` with `Accept: application/x-bibtex` for any
entry that has a DOI (falling back to the offline entry for one that
does not, or if the fetch fails), which is closer to what many reference
managers expect but is slower and requires a live connection.

A release frozen before the attribution contract (2026-09-03) carries no
`citation` in its catalog; `cc_cite()` computes the same wording
`calcofi4db::release_citation()` would have written
(`source = "computed"` on the result's `source` attribute, mirroring
[`cc_climatology()`](https://calcofi.io/calcofi4r/reference/cc_climatology.md)'s
`source`), rather than erroring or citing nothing.

The **software** itself is cited separately — `citation("calcofi4r")`
(from `DESCRIPTION`'s `Authors@R`) for R, `calcofi4py.__citation__` for
Python; `cc_cite()` is for the *data*.

## Examples

``` r
if (FALSE) { # \dontrun{
cc_cite()
cc_cite("calcofi_dic")
cc_cite(format = "bibtex") |> cat()
cc_cite(cc_read_obs(datasets = "calcofi_dic"))
} # }
```
