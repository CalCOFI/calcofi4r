# CalCOFI Grid for Extracting Effort

A grid for calculating effort by station using Voronoi diagram to fetch
nearest station, additionally clipped by land
([`rnaturalearthhires::states10`](https://docs.ropensci.org/rnaturalearthhires/reference/states.html)).

## Usage

``` r
cc_grid
```

## Format

A `sf` spatial feature set with

- site_key:

  site key in the form of "`lin`,`pos`"

- sta_lin:

  alongshore line in CalCOFI coordinate system

- sta_pos:

  offshore position in CalCOFI coordinate system

- sta_dpos:

  difference in position, from 5 (nearshore), 10 (offshore) to 20
  (outside 113 station extended area)

- sta_pattern:

  the CalCOFI station pattern; one of: "standard", "extended" or
  "historical"

- sta_shore:

  the position wrt shore; one of: "nearshore" or "offshore"

- geom:

  station voronoi polygon with latitude and longitude in decimal degree
  geographic coordinates (SRID 4326)

## Source

[Station Positions -
CalCOFI](https://calcofi.org/sampling-info/station-positions)
