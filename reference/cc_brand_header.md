# The brand header bar for a Shiny app

The `.cc-header` chrome: CalCOFI logo far left linking to
<https://calcofi.io>, the app's `title` (linking to `href`, its own
root), a spacer, the app's own controls in `...`, and bslib's dark-mode
switch.

## Usage

``` r
cc_brand_header(
  title,
  ...,
  subtitle = NULL,
  release = NULL,
  href = "./",
  toggle_id = "dark_toggle",
  mode = c("dark", "light"),
  brand_url = .CC_BRAND_URL
)
```

## Arguments

- title:

  the app's name, shown beside the logo

- ...:

  the app's own header controls (selects, buttons, help)

- subtitle:

  small muted text after the title

- release:

  the database release the app's data was built from, shown as
  [`cc_release_chip()`](https://calcofi.io/calcofi4r/reference/cc_release_chip.md)
  after the title; `NULL` for an app not on the database

- href:

  where the title links: the app's root

- toggle_id:

  id of the dark-mode switch (read it with
  [`cc_is_dark()`](https://calcofi.io/calcofi4r/reference/cc_is_dark.md));
  `NULL` for no switch

- mode:

  initial theme of the switch — pass
  [`cc_theme()`](https://calcofi.io/calcofi4r/reference/cc_theme.md)`(request)`

- brand_url:

  where the assets live; the default is the only value a published app
  should use

## Value

a `<header class="cc-header">` tag
