# Match biological to environmental observations

Core engine relating biological observations to environmental
observations by matching them on time and space. Builds a single DuckDB
SQL string — a temporal interval join (within `max_time_hr`) plus a
spatial filter (`ST_Distance_Sphere` within `max_dist_km`) — that reads
directly from the Parquet files of a frozen CalCOFI release on Google
Cloud Storage, runs it, and attaches the fully-interpolated SQL and
query metadata as attributes.

## Usage

``` r
cc_match_bio_env(
  bio,
  env,
  max_dist_km = 2,
  max_time_hr = 6,
  join_method = c("nearest_time", "nearest_dist", "average"),
  con = NULL,
  version = "latest",
  collect = TRUE,
  return_sql = FALSE
)
```

## Arguments

- bio:

  SQL `SELECT` string producing the biological side. **Must** yield
  columns `bio_id` (unique per observation), `bio_datetime`
  (`TIMESTAMP`), `bio_lon`, `bio_lat` (decimal degrees) and `bio_value`
  (`DOUBLE`). Any additional columns (e.g. `scientific_name`) are
  carried through to the output as grouping keys.

- env:

  SQL `SELECT` string producing the environmental side. **Must** yield
  exactly `env_id`, `env_datetime` (`TIMESTAMP`), `env_lon`, `env_lat`,
  `env_value` (`DOUBLE`), `env_depth_m` (`DOUBLE`) and
  `measurement_type`.

- max_dist_km:

  Maximum match distance in kilometers (default: 2).

- max_time_hr:

  Maximum match time difference in hours (default: 6).

- join_method:

  One of `"nearest_time"` (default — keep the env observation(s) closest
  in time, averaging ties), `"nearest_dist"` (closest in space) or
  `"average"` (average every env observation in the window).

- con:

  Optional DuckDB connection. If `NULL` (default) a temporary in-memory
  connection is created (and closed again when `collect = TRUE`).

- version:

  Release version string (e.g. `"v2026.05.14"`) or `"latest"` (default).
  Recorded in the query metadata; the actual table URLs come from `bio`
  / `env`.

- collect:

  If `TRUE` (default) execute and return a tibble; if `FALSE` return a
  lazy [`dplyr::tbl()`](https://dplyr.tidyverse.org/reference/tbl.html)
  reference.

- return_sql:

  If `TRUE`, return the interpolated SQL string (with `query_meta`
  attached) **without executing**. Default: `FALSE`.

## Value

When `return_sql = TRUE`, a length-1 character vector of SQL with
`attr(., "query_meta")`. Otherwise a tibble (or lazy `tbl`) of one row
per biological observation per `measurement_type`, with the matched
`env_value`, `env_value_sd`, `env_depth_m`, `n_env`, `dist_km` and
`time_diff_hr`, plus all `bio` columns. The result carries
`attr(., "sql")` and `attr(., "query_meta")` (package + release version,
parameters and GCS source URLs).

## Details

Most users want a wrapper
([`cc_match_ichthyo_by_name()`](https://calcofi.io/calcofi4r/reference/cc_match_ichthyo_by_name.md),
[`cc_match_ichthyo_by_taxon()`](https://calcofi.io/calcofi4r/reference/cc_match_ichthyo_by_taxon.md)
or
[`cc_match_zooplankton_biomass()`](https://calcofi.io/calcofi4r/reference/cc_match_zooplankton_biomass.md))
rather than calling `cc_match_bio_env()` directly.

## Examples

``` r
if (FALSE) { # \dontrun{
# build bio + env subqueries yourself, or use a wrapper
d <- cc_match_ichthyo_by_name("Sardinops sagax", env_var = "temperature")

# the exact, portable SQL that produced it
cat(attr(d, "sql"))
str(attr(d, "query_meta"))
} # }
```
