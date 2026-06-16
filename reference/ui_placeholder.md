# Create Placeholder Message UI

Generates a centered placeholder message for empty or loading states in
the Shiny UI. Useful for displaying instructions or status messages when
no data is available.

## Usage

``` r
ui_placeholder(title, message)
```

## Arguments

- title:

  Character string for heading text

- message:

  Character string for body text

## Value

shiny.tag div element with centered, styled placeholder content

## Examples

``` r
if (FALSE) { # \dontrun{
output$map_placeholder <- renderUI({
  ui_placeholder("No Data Selected", "Please select species from the filter menu.")
})
} # }
```
