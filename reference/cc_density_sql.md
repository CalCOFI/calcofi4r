# SQL deriving the two canonical densities and the effort class of a bio observation

Returns the `SELECT`-list fragment that derives `density_per_10m2`,
`density_per_1000m3` and `effort_class` from a count, its units, the
gear and the effort of its own sample (`std_haul_factor`, `prop_sorted`,
`volume_sampled_m3`). The release cuts these columns onto `obs_bio` with
it; a consumer that joins `obs` to `sample_measurement` itself applies
the same expression. `calcofi4py::density_sql()` emits identical bytes.

## Usage

``` r
cc_density_sql(
  alias = NULL,
  value = "measurement_value",
  units = "units",
  tow_type = "tow_type",
  std_haul_factor = "std_haul_factor",
  prop_sorted = "prop_sorted",
  volume_sampled_m3 = "volume_sampled_m3",
  as = TRUE
)
```

## Arguments

- alias:

  table alias to prefix every column with (`"o"` -\>
  `o.measurement_value`), or `NULL`.

- value, units, tow_type, std_haul_factor, prop_sorted,
  volume_sampled_m3:

  column names.

- as:

  `TRUE` returns one string of three `expr AS name` clauses joined by
  `,\n`; `FALSE` returns the three bare expressions as a named character
  vector.

## Value

A single string (or a named character vector when `as = FALSE`).

## Examples

``` r
cat(cc_density_sql("o"))
#> CASE WHEN o.units = 'count' AND o.std_haul_factor IS NOT NULL AND o.tow_type IN ('C1', 'CB', 'CV', 'PV') THEN o.measurement_value * o.std_haul_factor / COALESCE(NULLIF(o.prop_sorted, 0), 1)
#>      WHEN o.units IN ('count/m2', 'numberPerMeterSquared') THEN o.measurement_value * 10
#>      END AS density_per_10m2,
#> CASE WHEN o.units = 'count' AND o.volume_sampled_m3 IS NOT NULL AND o.volume_sampled_m3 > 0 THEN o.measurement_value / COALESCE(NULLIF(o.prop_sorted, 0), 1) / o.volume_sampled_m3 * 1000
#>      WHEN o.units IN ('count/1000m3') THEN o.measurement_value
#>      END AS density_per_1000m3,
#> CASE WHEN o.units = 'count' AND o.std_haul_factor IS NULL AND o.volume_sampled_m3 IS NULL THEN 'raw_count_no_effort'
#>      WHEN o.units = 'count' THEN 'count_with_effort'
#>      WHEN o.units IN ('count/m2', 'numberPerMeterSquared', 'count/1000m3') THEN 'density_as_published'
#>      ELSE 'other_unit' END AS effort_class
```
