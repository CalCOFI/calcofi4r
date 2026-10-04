# Build the CalCOFI grid: one cell per official station, inside the previous grid

The cells of the CalCOFI grid, built from the official station positions
(<https://calcofi.org/sampling-info/station-positions/>) so that every
station is the generator of its own cell and sits on its line by
construction (CalCOFI/workflows#130). The bundled
[cc_grid](https://calcofi.io/calcofi4r/reference/cc_grid.md),
[cc_grid_ctrs](https://calcofi.io/calcofi4r/reference/cc_grid_ctrs.md)
and
[cc_grid_zones](https://calcofi.io/calcofi4r/reference/cc_grid_zones.md)
are this function's output (`data-raw/cc_grid.R`); it is exported so the
build can be tested and a variant (another set of stations) measured
beside it.

## Usage

``` r
cc_grid_build(
  stations = calcofi4r::cc_station_positions,
  grid_prev = calcofi4r::cc_grid_v1,
  land = calcofi4r::cc_grid_land,
  keep_dist_m = 20 * CC_NMI_M,
  edge_m = 5000,
  gap_m = 250,
  crs_m = 3310,
  verbose = FALSE
)
```

## Arguments

- stations:

  data frame of generators: `line`, `station`, `longitude`, `latitude`
  and `sta_type` (e.g. `"ROS"`, `"SCCOOS"`); default the bundled
  [cc_station_positions](https://calcofi.io/calcofi4r/reference/cc_station_positions.md)

- grid_prev:

  the previous grid: `sf` polygons in EPSG:4326 with `sta_key`
  (`"line,station"`), `sta_pattern`, `sta_shore`, `sta_dpos` and,
  optionally, each cell's centre as `lon_ctr` and `lat_ctr` (else the
  centroid of its largest polygon); default the bundled
  [cc_grid_v1](https://calcofi.io/calcofi4r/reference/cc_grid_v1.md)

- land:

  land polygons (`sf` or `sfc`, any CRS), used as given; default the
  bundled
  [cc_grid_land](https://calcofi.io/calcofi4r/reference/cc_grid_land.md),
  which is
  [`cc_grid_land_prep()`](https://calcofi.io/calcofi4r/reference/cc_grid_land_prep.md)
  of OpenStreetMap's land polygons

- keep_dist_m:

  a previous cell is kept when its label is farther than this from the
  convex hull of `stations`, in metres (default 20 nautical miles)

- edge_m:

  longest Voronoi edge segment, in metres (default 5000)

- gap_m:

  spacing of the points along a gap's edges that decide which cell gap
  water is nearest to, in metres (default 250)

- crs_m:

  projected CRS, in metres, for the tessellation and the areas (default
  3310, California Albers)

- verbose:

  print what the build did

## Value

a list: `grid` (`sf`, EPSG:4326: `grid_key`, `sta_key`, `sta_lin`,
`sta_pos`, `sta_dpos`, `sta_shore`, `sta_pattern`, `zone_key`,
`sta_type`, `sta_source` and `geom`), `ctrs` (the same rows with each
cell's site as `geom`), `zones` (cells dissolved by pattern and shore),
`pockets` (`sf`, EPSG:4326: every piece that changed cell under rule 5,
with `key_from`, `key_to`, `area_km2`), `dropped` (`sf`: the gap water
left out under rule 6), `prev` (one row per previous cell: `grid_key`,
`sta_key`, `sta_pattern`, `in_zone`, `fate` (`"kept"`, `"replaced"`, or
`"no water"` for a kept cell that is all land), `site_at` (`"label"`,
`"centre"` or `"water"`) and `gap_km2`, the gap water it gained),
`cases` (a data frame of what rule 5 could not attach and of a replaced
region in several pieces: `case`, `grid_key`, `area_km2`, `note`) and
`report` (named counts)

## Details

The rules, in order:

1.  **Extent.** The grid covers the outer hull of the previous grid (the
    union of its cells with interior holes filled) minus `land`.

2.  **Kept or replaced.** A previous cell whose labelled (line, station)
    lies farther than `keep_dist_m` (20 nautical miles) from the convex
    hull of `stations` is **kept**; the others are **replaced**. Where
    two previous cells overlap, the overlap belongs to the key that
    sorts first, which is the cell
    [`cc_grid_key()`](https://calcofi.io/calcofi4r/reference/cc_grid_key.md)
    gave a position there.

3.  **Kept cells are the previous cells.** A kept cell keeps its key,
    its attributes and its own boundaries, toward the other kept cells
    and toward the replaced region, so a position that was in a kept
    cell is still in it. Only its coast side changes: it ends at `land`.

4.  **Station cells.** The Voronoi tessellation of `stations` in metres
    (`crs_m`), confined to the replaced region `R` (the union of the
    replaced cells): an outer station's cell stops where the previous
    grid stopped. Voronoi edges get a vertex every `edge_m` so the
    lon/lat polygon follows the metric edge (to well under a metre at 5
    km).

5.  **Pockets.** A piece of a station's cell that land separates from
    the piece holding the station joins the neighbouring station cell it
    shares the longest water edge with (ties: the key that sorts first),
    repeated until every piece is attached. A pocket never joins a kept
    cell. A piece of `R` that no station's cell reaches by water stays
    with the station it is nearest to, as a detached part, and is
    reported in `cases`.

6.  **Gap water.** Water of the extent that no previous cell covered
    (the previous grid was clipped by a coarser coastline) joins the
    cell whose edge it is nearest to, kept or replaced; inside `R` it is
    then part of the station tessellation. Gap water that touches no
    cell is left out of the grid.

7.  **Sites.** The site of a station cell (`ctrs`, the release's
    `grid.geom_ctr`) is the station. The site of a kept cell is its
    label under `+proj=calcofi` when the cell holds it, else the
    previous cell's centre, else a point on the cell.

8.  **Attributes.** A station's cell is `nearshore` at station 60 or
    less and `offshore` beyond; `standard` on lines 76.7 and south (the
    75-station pattern) and `extended` north of it; spacing class 5
    nearshore and 10 offshore.

It stops when a station lies on land, outside the previous grid's hull,
or outside the replaced region `R`: a station inside a kept cell has no
cell to be the station of.
