# CalCOFI Grid Zones

A set of zones based on dissolving `cc_grid` for differentiating
position wrt the shore (`sta_shore`: "nearshore" or "offshore") and
station patterns ( `sta_pattern`: "standard", "extended" or
"historical").

## Usage

``` r
cc_grid_zones
```

## Format

A `sf` spatial feature set with 6 rows x 9 columns:

- zone_key:

  unique zone key of the form `"{sta_pattern}-{sta_shore}"`

- sta_pattern:

  the CalCOFI station pattern; one of: "standard", "extended" or
  "historical"

- sta_shore:

  the position wrt shore; one of: "nearshore" or "offshore"

- sta_dpos:

  the difference in position: 5 (nearshore), 10 (offshore) or 20
  (historical)

- sta_lin_min:

  the minimum value of dissolved `sta_lin` from `cc_grid`

- sta_lin_max:

  the maximum value of dissolved `sta_lin` from `cc_grid`

- sta_pos_min:

  the minimum value of dissolved `sta_pos` from `cc_grid`

- sta_pos_max:

  the maximum value of dissolved `sta_pos` from `cc_grid`

- geom:

  geometry of dissolved zone from `cc_grid` with latitude and longitude
  in decimal degree geographic coordinates (EPSG:4326)

## Source

[Station Positions -
CalCOFI](https://calcofi.org/sampling-info/station-positions)
