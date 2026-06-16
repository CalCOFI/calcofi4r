# Read Spatial Table from DuckDB as sf Object

Reads a DuckDB table containing geometry columns into an sf object.
Handles the `ST_AsWKB()` conversion required by DuckDB Spatial
automatically.

## Usage

``` r
cc_read_sf(con, table_name, geom_col = NULL, crs = 4326)
```

## Arguments

- con:

  DuckDB connection

- table_name:

  Name of the table to read

- geom_col:

  Name of the geometry column (default: "geom"). If NULL, auto-detects
  the first GEOMETRY column.

- crs:

  Coordinate reference system to set (default: 4326 for WGS84). DuckDB
  Spatial does not store CRS metadata, so this must be specified.

## Value

An sf object with geometry column

## Examples

``` r
if (FALSE) { # \dontrun{
con <- cc_get_db()

# read site points
sites_sf <- cc_read_sf(con, "site")

# read with specific geometry column
grid_sf <- cc_read_sf(con, "grid", geom_col = "geom_ctr")

# read with different CRS
sites_sf <- cc_read_sf(con, "site", crs = 4326)
} # }
```
