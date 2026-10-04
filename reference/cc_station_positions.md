# Official CalCOFI station positions

The 113 stations of the "Station Position (Lat/Lon), Depth, and Type"
table on <https://calcofi.org/sampling-info/station-positions/> (fetched
2026-10-02): the generators of the
[cc_grid](https://calcofi.io/calcofi4r/reference/cc_grid.md) cells.

## Usage

``` r
cc_station_positions
```

## Format

A tibble of 113 rows with

- station_key:

  `"LLL.L SSS.S"`, the form of the database's `site_key` (e.g.
  `"093.3 026.7"`)

- grid_key:

  the key of the station's cell in
  [cc_grid](https://calcofi.io/calcofi4r/reference/cc_grid.md)

- order_occ:

  order of occupation on a cruise

- line, station:

  line and station in the CalCOFI coordinate system

- longitude, latitude:

  the listed position, decimal degrees (WGS 84)

- depth_est_m:

  estimated bottom depth, m

- sta_type:

  "ROS" (rosette) or "SCCOOS" (the nine ~20 m inshore stations)

- in_75:

  in the 75-station pattern (lines 76.7 to 93.4), the standard pattern
  since 1984

- navy_ops_area:

  the operations area, for a station Navy operations may close

## Source

<https://calcofi.org/sampling-info/station-positions/>
