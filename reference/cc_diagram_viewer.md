# Zoomable Mermaid diagrams in a Quarto page

An HTML dependency that gives every Mermaid diagram Quarto draws on the
page (each `{mermaid}` block, and each
[`cc_erd()`](https://calcofi.io/calcofi4r/reference/cc_erd.md)) the look
and controls of the figures in the CalCOFI.io docs book: Source Sans 3
labels and curved edges, then a viewer with +/-/reset controls, drag to
pan, double-click to zoom and a full-screen button (Esc closes; the
mouse wheel zooms only in full screen). Quarto still draws the diagrams
in the browser, so a page is the same whichever machine rendered it and
no headless browser runs at render time.

## Usage

``` r
cc_diagram_viewer()
```

## Value

An
[`htmltools::htmlDependency()`](https://rstudio.github.io/htmltools/reference/htmlDependency.html)
(it renders nothing visible).

## Details

[`cc_erd()`](https://calcofi.io/calcofi4r/reference/cc_erd.md) attaches
it by itself; call `cc_diagram_viewer()` in a chunk of a page that has
`{mermaid}` blocks but no ERD. Attaching it more than once is harmless.

## Examples

``` r
if (FALSE) { # \dontrun{
# in a Quarto notebook chunk
calcofi4r::cc_diagram_viewer()
} # }
```
