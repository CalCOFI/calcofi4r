# Brand `<head>` tags for a Shiny app

Everything the contract puts in `<head>`: the page `<title>`, the
CalCOFI favicon set, the inline pre-paint theme snippet, `theme.css`,
`theme.js`, the bslib bridge (see the source header), and — if `ga_app`
is given — the analytics snippet via
[`cc_ga_head()`](https://calcofi.io/calcofi4r/reference/cc_ga_head.md).

## Usage

``` r
cc_brand_head(title = NULL, ga_app = NULL, ..., brand_url = .CC_BRAND_URL)
```

## Arguments

- title:

  the browser-tab title (`NULL` to leave the app's own)

- ga_app:

  app slug for
  [`cc_ga_head()`](https://calcofi.io/calcofi4r/reference/cc_ga_head.md);
  `NULL` for no analytics

- ...:

  passed to
  [`cc_ga_head()`](https://calcofi.io/calcofi4r/reference/cc_ga_head.md)

- brand_url:

  where the assets live; the default is the only value a published app
  should use

## Value

a
[`htmltools::tagList()`](https://rstudio.github.io/htmltools/reference/tagList.html)
for `tags$head()`

## Examples

``` r
if (FALSE) { # \dontrun{
ui <- function(request) bslib::page_fillable(
  tags$head(cc_brand_head("CalCOFI CTD Explorer", ga_app = "ctd-viz")),
  cc_brand_header("CTD Explorer", mode = cc_theme(request)),
  ...)
} # }
```
