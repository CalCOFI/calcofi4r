# Match ichthyoplankton to environmental data by WoRMS taxon subtree

Relates net-tow ichthyoplankton to CTD-bottle environmental
measurements, filtering the biological side to a taxon and all of its
descendants. The subtree is resolved with a recursive walk of
`taxon.parentNameUsageID` over the WoRMS authority. Supersedes the
retired `/itis_ichthyodata` endpoint, replacing the dead ITIS `path`
regex with the WoRMS `worms_id` hierarchy.

## Usage

``` r
cc_match_ichthyo_by_taxon(
  worms_id,
  env_var = "temperature",
  life_stage = NULL,
  date_min = NULL,
  date_max = NULL,
  depth_m_min = NULL,
  depth_m_max = NULL,
  max_dist_km = NULL,
  max_time_hr = NULL,
  relax_matching = FALSE,
  join_method = c("nearest_time", "nearest_dist", "average"),
  con = NULL,
  version = "latest",
  collect = TRUE,
  return_sql = FALSE
)
```

## Arguments

- worms_id:

  Integer vector of WoRMS `taxonID`(s); the match includes each id and
  every descendant taxon.

- env_var:

  Environmental `measurement_type` (default: `"temperature"`).

- life_stage:

  Optional character vector restricting `ichthyo.life_stage`.

- date_min, date_max:

  Optional date bounds on the tow start time.

- depth_m_min, depth_m_max:

  Optional depth bounds (meters) on the bottle environmental
  observations.

- max_dist_km, max_time_hr:

  Match tolerances. Default to 2 km / 6 hr, or 5 km / 72 hr when
  `relax_matching = TRUE`; an explicit value always wins.

- relax_matching:

  Widen the default tolerances to 5 km / 72 hr.

- join_method, con, version, collect, return_sql:

  Passed to
  [`cc_match_bio_env()`](https://calcofi.io/calcofi4r/reference/cc_match_bio_env.md).

## Value

See
[`cc_match_bio_env()`](https://calcofi.io/calcofi4r/reference/cc_match_bio_env.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Engraulis (anchovies) — WoRMS taxonID 125620 — and all descendants
d <- cc_match_ichthyo_by_taxon(125620, env_var = "salinity")
} # }
```
