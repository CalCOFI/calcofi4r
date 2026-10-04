# CalCOFI Grid Zones

The six zones
[cc_grid](https://calcofi.io/calcofi4r/reference/cc_grid.md) dissolves
into by position relative to shore (`sta_shore`: "nearshore" or
"offshore") and station pattern (`sta_pattern`: "standard", "extended"
or "historical");
[`cc_grid_zones_build()`](https://calcofi.io/calcofi4r/reference/cc_grid_zones_build.md)
of [cc_grid](https://calcofi.io/calcofi4r/reference/cc_grid.md).

## Usage

``` r
cc_grid_zones
```

## Format

An `sf` of 6 rows x 9 columns:

- zone_key:

  unique zone key of the form `"{sta_shore}-{sta_pattern}"`

- sta_pattern:

  the CalCOFI station pattern; one of: "standard", "extended" or
  "historical"

- sta_shore:

  the position wrt shore; one of: "nearshore" or "offshore"

- sta_dpos:

  the spacing class: 5 (nearshore), 10 (offshore) or 20 (historical)

- sta_lin_min:

  the minimum `sta_lin` of the zone's cells

- sta_lin_max:

  the maximum `sta_lin` of the zone's cells

- sta_pos_min:

  the minimum `sta_pos` of the zone's cells

- sta_pos_max:

  the maximum `sta_pos` of the zone's cells

- geom:

  the dissolved zone (EPSG:4326)

## Source

[Station Positions -
CalCOFI](https://calcofi.org/sampling-info/station-positions)
