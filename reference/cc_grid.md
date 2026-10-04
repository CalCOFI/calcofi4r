# CalCOFI grid: one cell per official station

The cells of the CalCOFI grid. 113 **station cells**: the Voronoi
tessellation of the official station positions
([cc_station_positions](https://calcofi.io/calcofi4r/reference/cc_station_positions.md)),
confined to the cells of the previous grid
([cc_grid_v1](https://calcofi.io/calcofi4r/reference/cc_grid_v1.md)) it
replaces, so an outer station's cell stops where the previous grid
stopped. And 112 **kept cells**: the previous grid's cells beyond 20
nautical miles of the official pattern, with their own boundaries and
keys, so a historical position keys as it always did. Both end at the
coastline
([cc_grid_land](https://calcofi.io/calcofi4r/reference/cc_grid_land.md)).
Every station cell holds its own station, and the release's `grid` table
is these cells (`calcofi4db::build_grid_reference()`). Built by
[`cc_grid_build()`](https://calcofi.io/calcofi4r/reference/cc_grid_build.md)
in `data-raw/cc_grid.R`; the rules are documented there.

## Usage

``` r
cc_grid
```

## Format

An `sf` of 225 cells (EPSG:4326) with

- grid_key:

  the release key, `st{station}-ln{line}`, with `_hist` for a cell of
  the historical pattern (e.g. `st26.7-ln93.3`, `st120-ln110_hist`)

- sta_key:

  station key in the form "`line`,`station`" (e.g. `"93.3,26.7"`)

- sta_lin:

  line (alongshore) in the CalCOFI coordinate system (double: `76.7`,
  `93.4`)

- sta_pos:

  station (offshore position) in the CalCOFI coordinate system (double:
  `26.7`)

- sta_dpos:

  nominal spacing class in station units: 5 (nearshore), 10 (offshore)
  or 20 (historical pattern)

- sta_shore:

  "nearshore" (station 60 or less) or "offshore"

- sta_pattern:

  "standard" (lines 76.7 and south: the 75-station pattern), "extended"
  (lines north of 76.7) or "historical" (kept from the previous grid)

- zone_key:

  `"{sta_shore}-{sta_pattern}"`, the key of
  [cc_grid_zones](https://calcofi.io/calcofi4r/reference/cc_grid_zones.md)

- sta_type:

  the station's type on the official list: "ROS" (rosette) or "SCCOOS"
  (the nine ~20 m inshore stations); `NA` for a cell kept from the
  previous grid

- sta_source:

  "official" (a station of the official list) or "previous" (a cell kept
  from
  [cc_grid_v1](https://calcofi.io/calcofi4r/reference/cc_grid_v1.md))

- geom:

  the cell (EPSG:4326); edges are straight in longitude/latitude. A
  station cell is one polygon (but `st53-ln60`, which keeps 21 km2 of
  Tomales Bay that no station cell reaches by water); a kept cell is the
  previous cell, in as many pieces as it was (13 are in several)

## Source

[Station Positions -
CalCOFI](https://calcofi.org/sampling-info/station-positions); coastline
(c) OpenStreetMap contributors (ODbL)

## Details

Before this grid, cells were Voronoi polygons of an idealized lattice
and an inshore cell held several real stations
([cc_grid_v1](https://calcofi.io/calcofi4r/reference/cc_grid_v1.md)).
**84 of the 113 station cells reuse a key whose polygon changed**, so
never map a key between the two grids by name: use the release's
`grid_crosswalk` table (`calcofi4db::build_grid_crosswalk()`), and
[`cc_grid_key()`](https://calcofi.io/calcofi4r/reference/cc_grid_key.md)
to key a position.
