# CalCOFI Grid Centroids for Extracting Effort

A set of centroids for the grid to calculate effort by station using
Voronoi diagram to fetch nearest station, additionally clipped by land
([`rnaturalearthhires::states10`](https://docs.ropensci.org/rnaturalearthhires/reference/states.html)).

## Usage

``` r
cc_grid_ctrs
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

- geom:

  station latitude and longitude in decimal degree geographic
  coordinates (SRID 4326)

## Source

<https://calcofi.org/sampling-info/station-positions/>
