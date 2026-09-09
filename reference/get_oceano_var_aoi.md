# Get oceanographic variable for area of interest

**\[deprecated\]**

This function is deprecated because it relies on legacy data structures
(`var_lookup`) that are not compatible with the new DuckDB database.
Query bottle measurement data directly with
[`cc_get_db()`](https://calcofi.io/calcofi4r/reference/cc_get_db.md) and
[`cc_read_measurements()`](https://calcofi.io/calcofi4r/reference/cc_read_measurements.md),
then use sf for spatial filtering.

## Usage

``` r
get_oceano_var_aoi(
  var,
  aoi,
  date_step = c("year", "day", "week", "month", "quarter", "decade"),
  depth_min = 0,
  depth_max = 10
)
```

## Arguments

- var:

  variable of interest (TODO: see keys)

- aoi:

  area of interest

- date_step:

  eg month, quarter, year

- depth_min:

  depth minimum

- depth_max:

  depth maximum

## Value

data frame of values

## Examples

``` r
if (FALSE) { # \dontrun{
# deprecated - use DuckDB queries instead:
con <- cc_get_db()
d <- DBI::dbGetQuery(con, "
  SELECT s.longitude, s.latitude, o.datetime, o.depth_min_m,
         o.measurement_value, o.measurement_type
  FROM obs_env o
  JOIN sample s USING (sample_key)
  WHERE o.dataset_key = 'calcofi_bottle' AND o.measurement_type = 'oxygen_ml_l'")
# then filter spatially with sf::st_filter(d_sf, aoi)
} # }
```
