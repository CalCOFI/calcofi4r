# Layer means per cast from 10 m bins, interpolated in depth

Averages a column of
[`cc_bin_anomaly()`](https://calcofi.io/calcofi4r/reference/cc_bin_anomaly.md)
(by default `anomaly`) over a layer of 10 m bins for each dataset x
cruise x station x variable. The profile is first interpolated linearly
**in depth** onto every bin of the layer, between bins that were sampled
and no more than `max_gap` apart (never beyond the shallowest or deepest
sampled bin, never in time or between stations), so a bottle cast at
standard depths (0, 10, 20, 30, 50 m) and a CTD cast with every bin are
averaged over the same depths. A cast is returned only where every bin
of the layer has a value. This is the rule of the November 2026
temperature-anomaly page
(calcofi.io/workflows/2026-11_temperature-anomaly.html).

## Usage

``` r
cc_layer_mean(x, top = 0, bottom = 40, value = "anomaly", max_gap = 60)
```

## Arguments

- x:

  a tibble from
  [`cc_bin_anomaly()`](https://calcofi.io/calcofi4r/reference/cc_bin_anomaly.md).

- top, bottom:

  the layer's shallowest and deepest bin (their shallow edges, m): 0 and
  40 is 0-50 m.

- value:

  the column to average.

- max_gap:

  the widest span between sampled bins to interpolate across (m).

## Value

Tibble: `dataset_key`, `cruise_key`, `site_key`, `year`, `month`,
`variable`, `n_bins_sampled` (bins of the layer that were measured
rather than interpolated) and the layer mean under the name of `value`.
