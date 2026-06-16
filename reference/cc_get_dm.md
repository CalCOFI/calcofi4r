# Get CalCOFI Database as dm Object with Relationships

Returns a `dm` object with primary keys and foreign keys applied from
the `relationships.json` sidecar file included in frozen releases. This
enables schema visualization via
[`dm::dm_draw()`](https://dm.cynkra.com/reference/dm_draw.html) and
relationship- aware operations.

## Usage

``` r
cc_get_dm(version = "latest", con = NULL)
```

## Arguments

- version:

  Version string (e.g., "v2026.02") or "latest" (default)

- con:

  Optional existing DuckDB connection. If NULL (default), calls
  `cc_get_db(version)` to create one.

## Value

A `dm` object with PKs and FKs applied

## Examples

``` r
if (FALSE) { # \dontrun{
dm <- cc_get_dm()
dm::dm_draw(dm, rankdir = "LR", view_type = "all")

# use existing connection
con <- cc_get_db()
dm <- cc_get_dm(con = con)
} # }
```
