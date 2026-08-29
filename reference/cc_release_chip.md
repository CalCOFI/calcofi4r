# The integrated-database release chip

`release <b>v2026.08.25</b>` in the brand header, right after the title
— so it survives a collapsed sidebar and every tab switch, and travels
with a screenshot. Links to
[`cc_release_url()`](https://calcofi.io/calcofi4r/reference/cc_release_url.md).
[`cc_brand_header()`](https://calcofi.io/calcofi4r/reference/cc_brand_header.md)
emits it from its `release` argument; call this directly where a
framework owns the bar (`page_sidebar()`'s title, `page_navbar()`).

## Usage

``` r
cc_release_chip(version, href = cc_release_url(version))
```

## Arguments

- version:

  release version, `"v2026.08.25"`; `NULL`/`NA`/`""` → no chip

- href:

  where the chip links; default
  [`cc_release_url()`](https://calcofi.io/calcofi4r/reference/cc_release_url.md)

## Value

an `<a class="cc-release">` tag, or `NULL`

## Details

Show the release the page's data was **built from** (a sidecar the app's
`prep_db.R` wrote), never "latest" fetched at load: the two diverge
between a release and the next redeploy, and a figure is only
reproducible if the release that produced it travelled with it.

## Examples

``` r
cc_release_chip("v2026.08.25")
#> <a class="cc-release" href="https://calcofi.io/db-schema/#erd?v=v2026.08.25" title="CalCOFI integrated database release v2026.08.25 — every value shown comes from this frozen release; schema and release notes">
#>   release
#>   <b>v2026.08.25</b>
#> </a>
```
