# Which release is currently `latest`

Resolves the promoted release to a concrete version string, e.g.
`"v2026.08.07"`. Reads the same `latest.txt` every other CalCOFI
consumer reads, so it never disagrees with them.

## Usage

``` r
cc_latest_version()
```

## Value

Character scalar, the current release version.

## Details

Use it when you are about to pin. An analysis that passes
`version = "latest"` is not reproducible — it silently follows the
promoted release — but you cannot pin to a version you have not looked
up. Record the value beside your results, along with the package version
that read it: immutable data is only half of reproducibility, since a
later `calcofi4r` may expect a schema an older release does not carry.

## Examples

``` r
if (FALSE) { # \dontrun{
rel <- cc_latest_version()
c(release = rel,
  calcofi4r = as.character(utils::packageVersion("calcofi4r")))
} # }
```
