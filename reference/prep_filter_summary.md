# Build Filter Summary for Display

Creates a formatted list of filter criteria for display in the UI.
Summarizes species, environmental variables, temporal filters, depth
ranges, and spatial constraints into human-readable markdown strings.

## Usage

``` r
prep_filter_summary(
  sel_name,
  sel_env_var,
  sel_qtr,
  sel_date_range,
  sel_depth_range,
  drawn_polygon,
  selected_grid_zones
)
```

## Arguments

- sel_name:

  Character vector of selected species names (format: "Common Name
  (Scientific Name)")

- sel_env_var:

  Character string of selected environmental variable (e.g., "t_deg_c")

- sel_qtr:

  Numeric vector of selected quarters (1-4)

- sel_date_range:

  Date vector of length 2 (start date, end date)

- sel_depth_range:

  Numeric vector of length 2 (min depth, max depth) in meters

- drawn_polygon:

  sf object or data.frame representing user-drawn polygon (or NULL)

## Value

Character vector of markdown-formatted filter descriptions

## See also

[`modal_data`](https://calcofi.io/calcofi4r/reference/modal_data.md) for
the modal dialog that captures these filters

## Examples

``` r
prep_filter_summary(
  sel_name        = c("Anchovy (Engraulis mordax)", "Sardine (Sardinops sagax)"),
  sel_env_var     = "t_deg_c",
  sel_qtr         = c(1, 2),
  sel_date_range  = as.Date(c("2000-01-01", "2020-12-31")),
  sel_depth_range = c(0, 100),
  drawn_polygon   = NULL
)
#> Error in prep_filter_summary(sel_name = c("Anchovy (Engraulis mordax)",     "Sardine (Sardinops sagax)"), sel_env_var = "t_deg_c", sel_qtr = c(1,     2), sel_date_range = as.Date(c("2000-01-01", "2020-12-31")),     sel_depth_range = c(0, 100), drawn_polygon = NULL): object 'env_var_choices' not found
```
