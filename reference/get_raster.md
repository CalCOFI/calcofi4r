# Get raster of interpolated values from CalCOFI API

**\[deprecated\]**

This function is deprecated because the CalCOFI API is being phased out
in favor of direct DuckDB database access. Query bottle measurement data
with
[`cc_read_measurements()`](https://calcofi.io/calcofi4r/reference/cc_read_measurements.md)
and
[`cc_read_casts()`](https://calcofi.io/calcofi4r/reference/cc_read_casts.md),
then use
[`pts_to_rast_idw()`](https://calcofi.io/calcofi4r/reference/pts_to_rast_idw.md)
for spatial interpolation.

## Usage

``` r
get_raster(
  variable = "ctdcast_bottle.t_deg_c",
  cruise_id = "1949-03-01-C-31CR",
  depth_m_min = NULL,
  depth_m_max = NULL,
  out_tif
)
```

## Arguments

- variable:

  Variable to fetch from the CalCOFI API. One of `table_field` values
  from
  [`get_variables()`](https://calcofi.io/calcofi4r/reference/get_variables.md).
  Default is `"ctdcast_bottle.t_deg_c"`.

- cruise_id:

  Cruise identifier. One of `cruise_id` values from
  [`get_cruises()`](https://calcofi.io/calcofi4r/reference/get_cruises.md).
  Default is the first cruise "1949-03-01-C-31CR".

- depth_m_min:

  Minimum depth range in meters, e.g. 0. Default is `NULL`, as in not
  filtered.

- depth_m_max:

  Maximum depth range in meters, e.g. 5351. Default is `NULL`, as in not
  filtered.

- out_tif:

  Output path to write raster (\*.tif)

## Value

path to output raster in GeoTIFF format (\*.tif)

## Examples

``` r
if (FALSE) { # \dontrun{
# deprecated - use DuckDB queries + pts_to_rast_idw() instead:
con <- cc_get_db()
d <- DBI::dbGetQuery(con, "
  SELECT c.lon_dec, c.lat_dec, bm.measurement_value
  FROM bottle_measurement bm
  JOIN bottle b ON bm.bottle_id = b.bottle_id
  JOIN casts c ON b.cast_id = c.cast_id
  WHERE bm.measurement_type = 'temperature'")
} # }
```
