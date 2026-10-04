# Dissolve grid cells into zones by station pattern and shore

The six zones of
[cc_grid_zones](https://calcofi.io/calcofi4r/reference/cc_grid_zones.md):
the cells of a grid dissolved by `sta_pattern` and `sta_shore`, with the
range of lines and stations each holds.

## Usage

``` r
cc_grid_zones_build(grid)
```

## Arguments

- grid:

  an `sf` of cells with `sta_pattern`, `sta_shore`, `sta_dpos`,
  `sta_lin`, `sta_pos` and `zone_key`, as
  [`cc_grid_build()`](https://calcofi.io/calcofi4r/reference/cc_grid_build.md)
  returns

## Value

`sf` with one row per zone: `zone_key`, `sta_pattern`, `sta_shore`,
`sta_dpos`, `sta_lin_min`, `sta_lin_max`, `sta_pos_min`, `sta_pos_max`,
`geom`
