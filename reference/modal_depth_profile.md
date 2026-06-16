# Depth Profile Modal Dialog

Creates a modal dialog for defining a transect line segment and buffer
distance to generate environmental depth profiles.

## Usage

``` r
modal_depth_profile(map_sp)
```

## Arguments

- map_sp:

  maplibre object (currently unused in implementation, retained for
  future enhancement)

## Value

Shiny modal dialog object with transect drawing interface and buffer
distance input

## Details

Users draw a line segment on the map to define a transect. The buffer
distance controls the width of the corridor around the transect for data
aggregation. Default buffer is 5 km.

## See also

[`buffer_transect`](https://calcofi.io/calcofi4r/reference/buffer_transect.md)
for transect buffer generation

## Examples

``` r
if (FALSE) { # \dontrun{
# in server.R
observeEvent(input$create_profile, {
  showModal(modal_depth_profile(map_sp = NULL))
})
} # }
```
