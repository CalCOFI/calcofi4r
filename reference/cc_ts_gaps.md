# Insert NA rows at time steps with no observations

So a line chart BREAKS at an unsampled step instead of drawing straight
through it.

## Usage

``` r
cc_ts_gaps(d, ts_res)
```

## Arguments

- d:

  data frame from
  [`prep_ts_sp()`](https://calcofi.io/calcofi4r/reference/prep_ts_sp.md):
  `time`, `name`, `avg`, `std`, `n`, `upr`, `lwr`.

- ts_res:

  temporal resolution, as passed to
  [`prep_ts_sp()`](https://calcofi.io/calcofi4r/reference/prep_ts_sp.md).

## Value

`d` with NA-valued rows added at missing steps, ordered by name/time.

## Details

Highcharts connects consecutive points, and a species series is mostly
zeros, so an unsampled stretch renders as a flat line along zero — which
reads as "we looked and found none" when the truth is "nobody looked".
Those are different facts and the chart was showing the wrong one.

`cdfw_dungeness-crab` is the worked example: its sorted-archive effort
exists in only nine years (1984, 1988, 1998, 2004-2009), because the
sorting log records which archived jars have been examined and most have
not. The chart drew a continuous zero from 1984 to 2008, asserting
measured absence across roughly 20 years in which not one jar was
opened.

Gaps are NA on `avg`/`std`/`upr`/`lwr` and `n = 0`, never `0` — zero is
a measurement, and collapsing the two is the bug. `n` tells them apart.

Only applied to resolutions that are a real time AXIS.
`"quarter"`/`"month"`/ `"day"` are climatology CYCLES — every bin is
populated by construction and a gap there means something different — so
they are left alone.

Exported because `db-viz-hex` carries its own copy of
[`prep_ts_sp()`](https://calcofi.io/calcofi4r/reference/prep_ts_sp.md);
both call this, so the rule cannot drift into two versions.
