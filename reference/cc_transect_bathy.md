# Seafloor profile along a transect, sampled at a regular interval

Samples the seafloor **between** the supplied positions, not only at
them.

## Usage

``` r
cc_transect_bathy(
  lon,
  lat,
  dist_km = NULL,
  interval_m = 500,
  bathy = cc_bathy(),
  land_depth_m = 0
)
```

## Arguments

- lon, lat:

  positions to sample between, in order along the transect.

- dist_km:

  optional ruler value at each position — the section's own x-axis.
  Interpolated linearly within each leg, so the profile lands on the
  caller's axis and is anchored exactly at every station. Defaults to
  cumulative great-circle distance from the first position.

- interval_m:

  spacing along the track, metres (default 500).

- bathy:

  raster from
  [`cc_bathy()`](https://calcofi.io/calcofi4r/reference/cc_bathy.md).

- land_depth_m:

  at or below this depth a sample is reported `on_land` (default 0, the
  raster's land clamp).

## Value

Tibble of `dist_km`, `lon`, `lat`, `depth_m`, `on_land`, one row per
sample, ordered along the transect. Positions outside the raster carry
`NA` depth; the caller decides whether that is a gap or an error.

## Why the interval matters

Sampling at stations alone and joining those soundings with straight
lines does not simplify the terrain, it invents different terrain. Line
86.7 is the case: station 50 sits on a Channel Islands bank at 80 m
between neighbours at 1,654 m and 1,190 m that are 37 km away on either
side, so station-only sampling drew that bank as one triangle 74 km wide
rising 1.5 km off the seafloor — at the exact depths where someone is
reading the thermocline.

Too coarse an interval fails the same way, more quietly. At 2 km,
Fortymile Bank on line 93.3 is four soundings (385, 344, 238, 370 m) and
draws as a spike; at 500 m it is what it really is, a ~14 km rise from
652 m to a 178 m crest with flanks on both sides. The default sits just
above GEBCO's ~390 m cell: fine enough to keep every cell the track
crosses, coarse enough not to imply detail the grid does not have.

## Crossing land

A cruise track — as opposed to a CalCOFI line — zigzags, so a leg
between two consecutive casts can cross an island. Those samples come
back at depth 0 with `on_land = TRUE` rather than being dropped or
smoothed away: the track really did cross land, and a consumer that
draws a filled silhouette should break the polygon there instead of
drawing a wall to the surface.
