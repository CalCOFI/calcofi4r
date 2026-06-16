# Retrieve Species Larval Abundance Data from Database

Queries species, larva, net, tow, and site tables with temporal filters,
computing standardized tally values. Returns a dbplyr lazy table for
efficient downstream processing.

## Usage

``` r
get_sp(sp_name, qtr, date_range, ck_children = T)
```

## Arguments

- sp_name:

  Character vector of species names (format: "Common Name (Scientific
  Name)")

- qtr:

  Numeric vector of quarters to include (1-4)

- date_range:

  Date vector of length 2 (start date, end date)

- ck_children:

  Boolean (TRUE or FALSE) whether to include taxonomic children

## Value

dbplyr lazy table with columns:

- `name` - species name (common + scientific)

- `tally` - raw larval count

- `std_tally` - standardized tally (adjusted for haul factor and sorting
  proportion)

- `time_start` - tow start datetime

- `longitude`, `latitude` - spatial coordinates

- `quarter` - quarter (1-4)

- `hex_h3res*` - H3 hexagon indices at multiple resolutions

## Details

The standardized tally accounts for differences in haul efficiency and
subsampling: `std_tally = std_haul_factor * tally / prop_sorted`. Only
records with non-NA tally values are returned.

## See also

[`prep_sp_hex`](https://calcofi.io/calcofi4r/reference/prep_sp_hex.md)
for spatial aggregation

[`prep_ts_sp`](https://calcofi.io/calcofi4r/reference/prep_ts_sp.md) for
temporal aggregation

## Examples

``` r
if (FALSE) { # \dontrun{
# retrieve anchovy data for all quarters 2010-2020
df_sp <- get_sp(
  sp_name    = "Anchovy (Engraulis mordax)",
  qtr        = 1:4,
  date_range = as.Date(c("2010-01-01", "2020-12-31"))
)
df_sp |> collect()
} # }
```
