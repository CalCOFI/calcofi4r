# ─── brand: the theme + header + favicon contract every CalCOFI product wears ──
#
# The contract lives at https://calcofi.io/brand/v2/ (source:
# CalCOFI.github.io/brand/v2/README.md; v2 — the SIO look, light by default,
# the horizontal lockup, the app scale — in force since 2026-09-04; v1 is
# superseded and still served). These helpers are its R half, so a
# Shiny app declares against it instead of re-implementing it — before this,
# three apps carried the same 30 lines (logo pair, `[data-bs-theme]` CSS block,
# `input_dark_mode()`), only one had a favicon, and none could be put into a
# given theme from a URL.
#
# HOW THE THEME REACHES A SHINY APP, without a flash of the wrong colour:
#   1. `ui <- function(request)` resolves it SERVER-SIDE with [cc_theme()]
#      (`?theme=` → `cc_theme` cookie, if the visitor chose it → light) and hands it to
#      [cc_brand_header()] as the initial `mode` of bslib's dark-mode switch —
#      the same resolution theme.js does in the browser, so the two agree.
#   2. [cc_brand_head()] emits the inline pre-paint snippet (first paint is the
#      right colour) plus theme.css / theme.js.
#   3. bslib's `<bslib-input-dark-mode>` writes `data-bs-theme` on `<html>`
#      when clicked; the bridge in [cc_brand_head()] observes that attribute and
#      pushes the change into `ccTheme.set()`, which persists the cookie across
#      *.calcofi.io. Our toggle and theirs cannot disagree: theme.js also sets
#      `data-bs-theme`, and the component observes it.

# where the assets are served from; a consumer never vendors them
.CC_BRAND_URL <- "https://calcofi.io/brand/v2"

# brand/v2/head.html's inline pre-paint, verbatim: resolves the theme before the
# first paint (light unless the visitor chose otherwise — a stored choice counts
# only beside the `cc_theme_src=user` marker, so a v1 default never leaks in) and
# copies <meta name="cc-scale"> onto <html data-cc-scale>. Must be inline (a
# deferred script is too late).
.CC_BRAND_PREPAINT <- paste0(
  '(function(){var d=document.documentElement,m=/[?&]theme=(dark|light)\\b/.exec(location.search),',
  'k=/(?:^|;\\s*)cc_theme_src=user\\b/.test(document.cookie),',
  'c=k&&/(?:^|;\\s*)cc_theme=(dark|light)\\b/.exec(document.cookie),s=null;',
  'try{if(localStorage.getItem("cc_theme_src")==="user")s=localStorage.getItem("cc_theme")}catch(e){}',
  'var t=(m&&m[1])||(c&&c[1])||(s==="dark"||s==="light"?s:null)||"light";',
  'd.dataset.theme=t;d.setAttribute("data-bs-theme",t);',
  'd.setAttribute("data-md-color-scheme",t==="dark"?"slate":"default");d.style.colorScheme=t;',
  'var sc=document.querySelector(\'meta[name="cc-scale"]\');',
  'if(sc&&sc.content)d.setAttribute("data-cc-scale",sc.content)})();')

# bslib's dark-mode switch → the site-wide choice (see the header comment)
.CC_BRAND_BSLIB_BRIDGE <- paste0(
  '(function(){var r=document.documentElement,last=r.getAttribute("data-bs-theme");',
  'new MutationObserver(function(){var t=r.getAttribute("data-bs-theme");',
  'if(t===last||(t!=="dark"&&t!=="light"))return;last=t;',
  'if(window.ccTheme&&ccTheme.get()!==t)ccTheme.set(t)})',
  '.observe(r,{attributes:true,attributeFilter:["data-bs-theme"]})})();')

# the `.cc-header` inside a bslib page: v2's bar is self-contained (its own font,
# size and colours), so only bslib's switch and controls need dressing
.CC_BRAND_SHINY_CSS <- paste(
  ".cc-header bslib-input-dark-mode { --text-1: var(--fg); --text-2: var(--muted); }",
  ".cc-header .form-select, .cc-header .btn { font-size: 0.85rem; }",
  sep = "\n")

#' Resolve the theme a Shiny request asks for
#'
#' The server-side twin of `theme.js`'s resolution, for `ui <- function(request)`:
#' `?theme=dark|light` in the query string, else the `cc_theme` cookie
#' (`Domain=.calcofi.io`, set by any CalCOFI site's toggle) — honoured only
#' beside its `cc_theme_src=user` marker, i.e. when the visitor chose it (brand
#' v2's persistence rule: a v1 page's default can never leak in) — else `default`.
#' Pass the result as `mode` to [cc_brand_header()] so bslib's switch starts in
#' the right state and the page never flashes the other colour.
#'
#' @param request the Rook request Shiny hands a `ui` function (`NULL` → `default`)
#' @param default theme when neither the URL nor a cookie says: `"light"`, the
#'   calcofi.io convention since brand v2 (2026-09-04)
#' @return `"dark"` or `"light"`
#' @examples
#' cc_theme(list(QUERY_STRING = "?theme=light"))
#' cc_theme(list(HTTP_COOKIE = "cc_theme=dark; cc_theme_src=user"))
#' cc_theme(NULL)
#' @export
#' @concept brand
cc_theme <- function(request = NULL, default = c("light", "dark")) {
  default <- match.arg(default)
  if (is.null(request)) return(default)

  q <- shiny::parseQueryString(request$QUERY_STRING %||% "")
  t <- q[["theme"]]
  if (!is.null(t) && t %in% c("dark", "light")) return(t)

  ck <- request$HTTP_COOKIE %||% ""
  m  <- regmatches(ck, regexpr("(^|;\\s*)cc_theme=(dark|light)\\b", ck, perl = TRUE))
  if (length(m) && nzchar(m) && grepl("(^|;\\s*)cc_theme_src=user\\b", ck, perl = TRUE))
    return(sub(".*cc_theme=", "", m))

  default
}

#' Is the app currently in dark mode?
#'
#' Reads bslib's dark-mode switch (`input[[id]]`, `"dark"` or `"light"`), for
#' `is_dark` arguments such as [map_sp()], [plot_ts()], [cc_plotly_theme()].
#' Before the switch has reported (first flush) it is `default`.
#'
#' @param input the Shiny `input` object
#' @param id the switch's id, as given to [cc_brand_header()]
#' @param default value before the input exists
#' @return logical scalar
#' @export
#' @concept brand
cc_is_dark <- function(input, id = "dark_toggle", default = FALSE) {
  v <- input[[id]]
  if (is.null(v)) default else identical(v, "dark")
}

#' Should the guided tour run? (`?tour=off`)
#'
#' The contract's one URL parameter besides `?theme=`: `tour=off|false|0|no`
#' suppresses a first-visit tour or welcome modal so a screenshot — or a
#' colleague following a link — sees the interface. Everything else (absent,
#' `on`, `1`, …) leaves the app's own first-visit logic in charge.
#'
#' @param query a query string (`"?tour=off"`) or a parsed list; `NULL` reads the
#'   session's URL via [shiny::getQueryString()] (reactive context required)
#' @param session Shiny session, for the `NULL` case
#' @return `TRUE` unless the URL switched the tour off
#' @examples
#' cc_tour_enabled("?tour=off")
#' cc_tour_enabled("?cruise=2026-04-3322")
#' @export
#' @concept brand
cc_tour_enabled <- function(query = NULL, session = shiny::getDefaultReactiveDomain()) {
  if (is.null(query)) {
    if (is.null(session)) return(TRUE)
    query <- shiny::getQueryString(session)
  } else if (is.character(query)) {
    query <- shiny::parseQueryString(query)
  }
  v <- tolower(trimws(query[["tour"]] %||% ""))
  !v %in% c("off", "false", "0", "no")
}

#' Where a database release is documented
#'
#' The schema browser at <https://calcofi.io/db-schema/> opens on a version's
#' ERD, with its tables, columns, measurement types and release notes — the one
#' place a release chip should send someone.
#'
#' @param version release version, `"v2026.08.25"`
#' @return a URL
#' @examples
#' cc_release_url("v2026.08.25")
#' @export
#' @concept brand
cc_release_url <- function(version)
  paste0("https://calcofi.io/db-schema/#erd?v=", version)

#' The integrated-database release chip
#'
#' `release <b>v2026.08.25</b>` in the brand header, right after the title — so it
#' survives a collapsed sidebar and every tab switch, and travels with a
#' screenshot. Links to [cc_release_url()]. [cc_brand_header()] emits it from its
#' `release` argument; call this directly where a framework owns the bar
#' (`page_sidebar()`'s title, `page_navbar()`).
#'
#' Show the release the page's data was **built from** (a sidecar the app's
#' `prep_db.R` wrote), never "latest" fetched at load: the two diverge between a
#' release and the next redeploy, and a figure is only reproducible if the
#' release that produced it travelled with it.
#'
#' @param version release version, `"v2026.08.25"`; `NULL`/`NA`/`""` → no chip
#' @param href where the chip links; default [cc_release_url()]
#' @return an `<a class="cc-release">` tag, or `NULL`
#' @examples
#' cc_release_chip("v2026.08.25")
#' @export
#' @concept brand
cc_release_chip <- function(version, href = cc_release_url(version)) {
  if (is.null(version) || length(version) != 1 || is.na(version) || !nzchar(version))
    return(NULL)
  htmltools::tags$a(
    class = "cc-release", href = href,
    title = paste0(
      "CalCOFI integrated database release ", version,
      " \u2014 every value shown comes from this frozen release; ",
      "schema and release notes"),
    "release", htmltools::tags$b(version))
}

#' Brand `<head>` tags for a Shiny app
#'
#' Everything the contract puts in `<head>`: the page `<title>`, the app-scale
#' meta (`<meta name="cc-scale" content="app">` — brand v2's compact scale), the
#' CalCOFI favicon set, the font preloads, the inline pre-paint theme snippet,
#' `fonts.css`, `theme.css`, `theme.js`, the bslib bridge (see the source
#' header), and — if `ga_app` is given — the analytics snippet via [cc_ga_head()].
#'
#' @param title the browser-tab title (`NULL` to leave the app's own)
#' @param ga_app app slug for [cc_ga_head()]; `NULL` for no analytics
#' @param ... passed to [cc_ga_head()]
#' @param brand_url where the assets live; the default is the only value a
#'   published app should use
#' @param scale `"app"` (the default for a Shiny app) or `"page"` (the reading
#'   scale: larger type and spacing)
#' @return a [htmltools::tagList()] for `tags$head()`
#' @examples
#' \dontrun{
#' ui <- function(request) bslib::page_fillable(
#'   tags$head(cc_brand_head("CalCOFI CTD Explorer", ga_app = "ctd-viz")),
#'   cc_brand_header("CTD Explorer", mode = cc_theme(request)),
#'   ...)
#' }
#' @export
#' @concept brand
cc_brand_head <- function(title = NULL, ga_app = NULL, ..., brand_url = .CC_BRAND_URL,
                          scale = c("app", "page")) {
  scale <- match.arg(scale)
  u <- function(f) paste0(brand_url, "/", f)
  htmltools::tagList(
    if (!is.null(title)) htmltools::tags$title(title),
    if (scale == "app") htmltools::tags$meta(name = "cc-scale", content = "app"),
    htmltools::tags$link(rel = "icon", type = "image/x-icon", href = u("favicon.ico")),
    htmltools::tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = u("favicon-32x32.png")),
    htmltools::tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = u("favicon-16x16.png")),
    htmltools::tags$link(rel = "apple-touch-icon", sizes = "180x180", href = u("apple-touch-icon.png")),
    htmltools::tags$link(rel = "preload", href = u("fonts/SourceSans3-VF.woff2"), as = "font", type = "font/woff2", crossorigin = NA),
    htmltools::tags$link(rel = "preload", href = u("fonts/Teko-VF.woff2"), as = "font", type = "font/woff2", crossorigin = NA),
    htmltools::tags$script(htmltools::HTML(.CC_BRAND_PREPAINT)),
    htmltools::tags$link(rel = "stylesheet", href = u("fonts.css")),
    htmltools::tags$link(rel = "stylesheet", href = u("theme.css")),
    htmltools::tags$script(defer = NA, src = u("theme.js")),
    htmltools::tags$script(htmltools::HTML(.CC_BRAND_BSLIB_BRIDGE)),
    htmltools::tags$style(htmltools::HTML(.CC_BRAND_SHINY_CSS)),
    if (!is.null(ga_app)) cc_ga_head(ga_app, ...))
}

#' The brand header bar for a Shiny app
#'
#' The `.cc-header` chrome: the CalCOFI lockup (brand v2's horizontal mark +
#' wordmark) far left linking to <https://calcofi.io>, the app's `title` (linking to `href`, its own root),
#' a spacer, the app's own controls in `...`, and bslib's dark-mode switch.
#'
#' @param title the app's name, shown beside the logo
#' @param ... the app's own header controls (selects, buttons, help)
#' @param subtitle small muted text after the title
#' @param release the database release the app's data was built from, shown as
#'   [cc_release_chip()] after the title; `NULL` for an app not on the database
#' @param href where the title links: the app's root
#' @param toggle_id id of the dark-mode switch (read it with [cc_is_dark()]);
#'   `NULL` for no switch
#' @param mode initial theme of the switch — pass [cc_theme()]`(request)`
#' @inheritParams cc_brand_head
#' @return a `<header class="cc-header">` tag
#' @export
#' @concept brand
cc_brand_header <- function(title, ..., subtitle = NULL, release = NULL, href = "./",
                            toggle_id = "dark_toggle", mode = c("light", "dark"),
                            brand_url = .CC_BRAND_URL) {
  mode <- match.arg(mode)
  u <- function(f) paste0(brand_url, "/", f)
  htmltools::tags$header(
    class = "cc-header",
    htmltools::tags$a(
      class = "cc-home", href = "https://calcofi.io", `aria-label` = "CalCOFI.io home",
      htmltools::tags$img(class = "cc-logo-dark",  src = u("logo_calcofi_h.svg"),       alt = "CalCOFI"),
      htmltools::tags$img(class = "cc-logo-light", src = u("logo_calcofi_h_light.svg"), alt = "CalCOFI")),
    htmltools::tags$a(
      class = "cc-title", href = href, title,
      if (!is.null(subtitle)) htmltools::tags$small(subtitle)),
    cc_release_chip(release),
    htmltools::tags$span(class = "cc-spacer"),
    ...,
    if (!is.null(toggle_id)) bslib::input_dark_mode(id = toggle_id, mode = mode))
}

#' Plot colours for the current theme
#'
#' The brand tokens a chart needs, so a plot on a dark page is not drawn with
#' black axis text: `fg` (text), `muted` (axis labels), `grid`, `panel`, and a
#' transparent `bg` so the plot inherits the page. The values are brand v2's
#' (UCSD navy on white; navy ground in dark) since calcofi4r 1.18.0.
#'
#' @param is_dark logical
#' @return named list of colour strings
#' @examples
#' cc_plot_colors(FALSE)$fg
#' @export
#' @concept brand
cc_plot_colors <- function(is_dark = FALSE) {
  if (is_dark)
    list(fg = "#e9edf3", muted = "#9fb0c8", grid = "#34486b", panel = "#182b49",
         accent = "#4fb6e6", bg = "rgba(0,0,0,0)")
  else
    list(fg = "#182b49", muted = "#66686a", grid = "#dddddd", panel = "#f5f5f5",
         accent = "#00629b", bg = "rgba(0,0,0,0)")
}

#' Theme a plotly figure for the current theme
#'
#' Transparent paper/plot background (the page shows through), text in the
#' brand foreground, grid and zero lines in the border tone. Apply last, after
#' the plot's own `layout()`; merges rather than replaces axis settings.
#'
#' @param p a plotly object
#' @inheritParams cc_plot_colors
#' @return the plotly object
#' @export
#' @concept brand
#' @importFrom plotly layout
cc_plotly_theme <- function(p, is_dark = FALSE) {
  k <- cc_plot_colors(is_dark)
  ax <- list(gridcolor = k$grid, zerolinecolor = k$grid, linecolor = k$grid,
             tickcolor = k$muted, tickfont = list(color = k$muted),
             title = list(font = list(color = k$fg)))
  plotly::layout(
    p,
    paper_bgcolor = k$bg, plot_bgcolor = k$bg,
    font   = list(color = k$fg),
    xaxis  = ax, yaxis = ax,
    legend = list(font = list(color = k$fg), bgcolor = k$bg),
    hoverlabel = list(font = list(color = k$fg), bgcolor = k$panel, bordercolor = k$grid))
}

#' A ggplot2 theme for the current theme
#'
#' [ggplot2::theme_minimal()] with transparent backgrounds and the brand text /
#' grid colours, for a static or `ggplotly()`-converted plot on a themed page.
#'
#' @inheritParams cc_plot_colors
#' @param base_size passed to [ggplot2::theme_minimal()]
#' @return a ggplot2 theme
#' @export
#' @concept brand
cc_ggplot_theme <- function(is_dark = FALSE, base_size = 11) {
  k <- cc_plot_colors(is_dark)
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background   = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.grid.major  = ggplot2::element_line(colour = k$grid),
      panel.grid.minor  = ggplot2::element_blank(),
      text              = ggplot2::element_text(colour = k$fg),
      axis.text         = ggplot2::element_text(colour = k$muted),
      strip.text        = ggplot2::element_text(colour = k$fg),
      plot.title        = ggplot2::element_text(colour = k$fg))
}
