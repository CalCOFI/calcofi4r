# Access a CalCOFI Database Table

Unified interface for reading any table from the CalCOFI database.
Returns a lazy
[`dplyr::tbl()`](https://dplyr.tidyverse.org/reference/tbl.html)
reference for non-spatial tables, or an `sf` object for tables with
geometry columns.

## Usage

``` r
cc_tbl(con, table_name, layer = NULL, geom_col = "geom", crs = 4326)
```

## Arguments

- con:

  DuckDB connection (from
  [`cc_get_db()`](https://calcofi.io/calcofi4r/reference/cc_get_db.md))

- table_name:

  Name of the table

- layer:

  Required when `table_name = "_spatial"`. Character string specifying
  which spatial layer to return.

- geom_col:

  Name of the geometry column for spatial tables (default: "geom"). Use
  this to select alternate geometry columns, e.g. `"geom_ctr"` for grid
  centroids.

- crs:

  CRS to assign to the returned sf object (default: 4326)

## Value

For non-spatial tables: a lazy
[`dplyr::tbl()`](https://dplyr.tidyverse.org/reference/tbl.html)
reference. For spatial tables: an `sf` object with geometry.

## Details

For the `_spatial` table, automatically pivots attributes from
`_spatial_attr` wide and returns an sf object filtered to the requested
layer.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- cc_get_db()

# non-spatial: returns lazy dbplyr reference
cc_tbl(con, "ichthyo")

# spatial: returns sf with default geom column
cc_tbl(con, "grid")

# spatial: select alternate geometry
cc_tbl(con, "grid", geom_col = "geom_ctr")

# _spatial: returns sf with pivoted attributes for a layer
cc_tbl(con, "_spatial", layer = "CA Counties")
} # }
```
