# CalCOFI grid sites: the station of each cell

The site of each
[cc_grid](https://calcofi.io/calcofi4r/reference/cc_grid.md) cell: the
official station position for a station cell (on its line by
construction), and for a cell kept from the previous grid its labelled
(line, station) under `+proj=calcofi` where the cell holds it, else the
previous cell's centre, else a point on the cell. The release's
`grid.geom_ctr`.

## Usage

``` r
cc_grid_ctrs
```

## Format

An `sf` of 225 points (EPSG:4326) with the columns of
[cc_grid](https://calcofi.io/calcofi4r/reference/cc_grid.md) and

- geom:

  the site (EPSG:4326)

## Source

[Station Positions -
CalCOFI](https://calcofi.org/sampling-info/station-positions)
