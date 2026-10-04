# The previous CalCOFI grid (through calcofi4r 1.24 and release v2026.10.01)

The grid as `cc_grid` shipped it through calcofi4r 1.24.2 and as every
database release through v2026.10.01 carries it in `grid`: Voronoi
polygons of an idealized lattice (5, 10 and 20 station units) in
`+proj=calcofi` coordinates, clipped by Natural Earth land. Kept because
the current [cc_grid](https://calcofi.io/calcofi4r/reference/cc_grid.md)
is built from it (its cells beyond the official pattern are kept as they
were, and the station cells fill the ones they replace), and because the
release's `grid_crosswalk` is the overlap of the two.

## Usage

``` r
cc_grid_v1
```

## Format

An `sf` of 218 polygons and multipolygons (EPSG:4326) with

- grid_key:

  the release key through v2026.10.01 (`st{station}-ln{line}`, `_hist`
  for the historical pattern)

- sta_key:

  "`line`,`station`"; not unique (`"90,120"` is a standard and a
  historical cell)

- sta_lin, sta_pos:

  line and station of the cell's label

- sta_dpos, sta_shore, sta_pattern, zone_key:

  as in [cc_grid](https://calcofi.io/calcofi4r/reference/cc_grid.md)

- lon_ctr, lat_ctr:

  the cell's centre as released (`grid.geom_ctr`): the centroid of its
  largest polygon

- geom:

  the cell (EPSG:4326)

## Source

calcofi4r 1.24.2 `data/cc_grid.rda` and `data/cc_grid_ctrs.rda`
(`data-raw/cc_grid_v1.R`)
