# Build an ERD color map from table → dataset metadata

Produces the `colors` argument
[`cc_erd()`](https://calcofi.io/calcofi4r/reference/cc_erd.md) expects
(a named list mapping each color to the tables it applies to) from
authoritative table → dataset membership, so ERD coloring is derived
from metadata rather than a hard-coded grouping. Tables owned by a
single dataset take that dataset's color; tables shared across datasets
(or owned by more than one) take the `neutral` color; per-table
`overrides` always win.

## Usage

``` r
cc_erd_color_map(
  table_dataset,
  dataset_colors,
  overrides = NULL,
  neutral = "#d0d0d0"
)
```

## Arguments

- table_dataset:

  Named list (or named character vector) keyed by table name. Each value
  is one `provider_dataset` identifier, or a character vector of them
  for shared tables.

- dataset_colors:

  Named list (or named character vector) keyed by `provider_dataset`,
  with a color name or hex code per dataset.

- overrides:

  Optional named list keyed by table name with a color name or hex code.
  Highest priority; use for common/shared tables.

- neutral:

  Color for shared/multi-dataset/unknown tables (default `"#d0d0d0"`).

## Value

A named list mapping color (hex) → character vector of table names,
suitable for `cc_erd(colors = ...)`.

## Examples

``` r
if (FALSE) { # \dontrun{
color_map <- cc_erd_color_map(
  table_dataset  = list(
    casts            = "calcofi_bottle",
    ctd_thin         = "calcofi_ctd-cast",
    measurement_type = c("calcofi_bottle", "calcofi_ctd-cast", "calcofi_dic")),
  dataset_colors = list(
    calcofi_bottle   = "#cfe3f7",
    `calcofi_ctd-cast` = "#e6d7f2",
    calcofi_dic      = "#ffd9c2"),
  overrides = list(measurement_type = "#e0e0e0"))
cc_erd(con, colors = color_map)
} # }
```
