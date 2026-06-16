# Match zooplankton biomass to environmental data

Relates net-tow zooplankton displacement-volume biomass
(`net.totalplankton` or `net.smallplankton`) to CTD-bottle environmental
measurements. Supersedes the retired `/zooplankton_biomass` endpoint.

## Usage

``` r
cc_match_zooplankton_biomass(
  env_var = "temperature",
  biomass_type = c("totalplankton", "smallplankton"),
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

- env_var:

  Environmental `measurement_type` (default: `"temperature"`).

- biomass_type:

  Which net biomass column to use: `"totalplankton"` (default) or
  `"smallplankton"`.

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
`bio_value` is the selected biomass column.

## Examples

``` r
if (FALSE) { # \dontrun{
d <- cc_match_zooplankton_biomass(env_var = "temperature")
} # }
```
