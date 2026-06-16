# Data Selection Modal Dialog

Creates a multi-tabbed modal dialog for selecting species, environmental
variables, temporal filters, depth ranges, and spatial regions.

## Usage

``` r
modal_data()
```

## Value

Shiny modal dialog object with four tabs:

- Species - selectizeInput for multiple species selection

- Environmental - variable and depth range selection

- Temporal - quarter and date range selection

- Spatial - interactive map for polygon drawing

## Details

The modal dialog uses
[`bslib::navset_tab()`](https://rstudio.github.io/bslib/reference/navset.html)
for tab organization and `shiny::input_task_button()` for submission
handling. Spatial filtering is implemented via `maplibre` with drawing
capabilities.

## See also

[`prep_filter_summary`](https://calcofi.io/calcofi4r/reference/prep_filter_summary.md)
for filter summary generation

## Examples

``` r
if (FALSE) { # \dontrun{
# in server.R
observeEvent(input$show_filters, {
  showModal(modal_data())
})
} # }
```
