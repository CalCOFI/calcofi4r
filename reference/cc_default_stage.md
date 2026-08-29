# The picker's defaults: which life stage and which denominator open a taxon (D8 rule 4)

Given the per-dataset x life-stage coverage of a taxon — one row per
`(dataset_key, life_stage)` with `n` rows, `n_10m2` rows that can be
expressed per 10 m2 and `n_1000m3` per 1000 m3 — the default stage is
the one with the most rows carrying effort (tie: most rows), and the
default denominator is the one that covers the most datasets *with
effort* for that stage — never largest-n; `per_10m2` on a tie; `raw`
only when nothing carries effort. Eggs and larvae are never merged. The
explorer's `state.ts` implements the same two functions.

## Usage

``` r
cc_default_stage(picker)

cc_default_denominator(picker, stage)
```

## Arguments

- picker:

  data frame with `dataset_key`, `life_stage`, `n`, `n_10m2`,
  `n_1000m3`.

- stage:

  a life stage in `picker$life_stage` (`NA` for rows without one).

## Value

`cc_default_stage()`: a life stage (or `NA`);
`cc_default_denominator()`: one of `"per_10m2"`, `"per_1000m3"`,
`"raw"`.

## Examples

``` r
p <- data.frame(dataset_key = c("swfsc_ichthyo", "swfsc_ichthyo", "swfsc_cufes"),
                life_stage = c("larva", "egg", "egg"), n = c(7420, 5906, 49572),
                n_10m2 = c(6158, 4907, 0), n_1000m3 = c(7420, 5906, 0))
cc_default_stage(p)
#> [1] "larva"
cc_default_denominator(p, "larva")
#> [1] "per_10m2"
```
