# Great-circle distance in kilometres, as a DuckDB SQL expression

Builds a haversine expression over four longitude/latitude columns
(decimal degrees) on a sphere of mean Earth radius 6,371.0088 km. It is
plain SQL, so it needs no spatial extension and returns the same answer
on every DuckDB version. `ST_Distance_Sphere(ST_Point(lon, lat), …)`
does not: DuckDB 1.x reads its points as `[latitude, longitude]`, which
made one degree of latitude 55.6 km instead of 111.2 km at 34°N, and
DuckDB 2.0 refuses the call unless `geometry_always_xy` is set.

## Usage

``` r
cc_distance_km_sql(lon1, lat1, lon2, lat2)
```

## Arguments

- lon1, lat1, lon2, lat2:

  Character. SQL expressions (usually column names) for the two
  positions' longitude and latitude in decimal degrees.

## Value

Character scalar, a SQL expression evaluating to kilometres.

## Examples

``` r
cc_distance_km_sql("bio_lon", "bio_lat", "env_lon", "env_lat")
#> (2 * 6371.0088 * asin(sqrt(pow(sin(radians((env_lat) - (bio_lat)) / 2), 2) + cos(radians(bio_lat)) * cos(radians(env_lat)) * pow(sin(radians((env_lon) - (bio_lon)) / 2), 2))))
if (FALSE) { # \dontrun{
con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbGetQuery(con, paste(
  "SELECT", cc_distance_km_sql("-120", "34", "-120", "35"), "AS km"))  # 111.2
} # }
```
