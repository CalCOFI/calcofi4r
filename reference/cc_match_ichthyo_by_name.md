# Match ichthyoplankton to environmental data by scientific name

Relates net-tow ichthyoplankton (fish egg / larva counts, standardized
to `std_tally`) to CTD-bottle environmental measurements, filtering the
biological side by scientific name. Supersedes the retired
`/ichthyodata` Plumber endpoint.

## Usage

``` r
cc_match_ichthyo_by_name(
  scientific_name,
  env_var = "temperature",
  exact_match = TRUE,
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

- scientific_name:

  Character vector of scientific names to match against
  `species.scientific_name`.

- env_var:

  Environmental `measurement_type` from the `bottle_measurement` table
  (default: `"temperature"`). See
  [`cc_list_measurement_types()`](https://calcofi.io/calcofi4r/reference/cc_list_measurement_types.md).

- exact_match:

  If `TRUE` (default) match names exactly; if `FALSE` match
  case-insensitively as substrings (`ILIKE '%name%'`).

- life_stage:

  Optional character vector restricting `ichthyo.life_stage` (e.g.
  `"larva"`, `"egg"`).

- date_min, date_max:

  Optional date bounds (`Date` or `"YYYY-MM-DD"` string) on the tow
  start time.

- depth_m_min, depth_m_max:

  Optional depth bounds (meters) on the bottle environmental
  observations.

- max_dist_km, max_time_hr:

  Match tolerances. Default to 2 km / 6 hr, or 5 km / 72 hr when
  `relax_matching = TRUE`; an explicit value always wins.

- relax_matching:

  Convenience flag mirroring the old API's `relax_matching`: widens the
  default tolerances to 5 km / 72 hr.

- join_method, con, version, collect, return_sql:

  Passed to
  [`cc_match_bio_env()`](https://calcofi.io/calcofi4r/reference/cc_match_bio_env.md).

## Value

See
[`cc_match_bio_env()`](https://calcofi.io/calcofi4r/reference/cc_match_bio_env.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Pacific sardine larvae vs. temperature, Q1 2018, relaxed matching
# (note: CTD-bottle env data ends 2021-05, so Q1 2018 is used as the
#  recurring worked example across calcofi4r, int-app and the docs book)
d <- cc_match_ichthyo_by_name(
  "Sardinops sagax",
  env_var        = "temperature",
  life_stage     = "larva",
  date_min       = "2018-01-01",
  date_max       = "2018-03-31",
  relax_matching = TRUE)
} # }
```
