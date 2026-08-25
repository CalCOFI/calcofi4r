# the R half of the calcofi.io brand contract (https://calcofi.io/brand/v1/)

test_that("cc_theme resolves ?theme= over the cookie over the default", {
  expect_equal(cc_theme(NULL), "dark")
  expect_equal(cc_theme(NULL, default = "light"), "light")
  expect_equal(cc_theme(list()), "dark")
  # the URL wins
  expect_equal(cc_theme(list(QUERY_STRING = "?theme=light", HTTP_COOKIE = "cc_theme=dark")), "light")
  expect_equal(cc_theme(list(QUERY_STRING = "?cruise=x&theme=dark", HTTP_COOKIE = "cc_theme=light")), "dark")
  # then the cookie, wherever it sits in the header
  expect_equal(cc_theme(list(HTTP_COOKIE = "cc_theme=light")), "light")
  expect_equal(cc_theme(list(HTTP_COOKIE = "_ga=GA1.2; cc_theme=light; theme=dark")), "light")
  # a cookie that merely ends in the name is not ours
  expect_equal(cc_theme(list(HTTP_COOKIE = "xcc_theme=light")), "dark")
  # garbage values fall through
  expect_equal(cc_theme(list(QUERY_STRING = "?theme=blue")), "dark")
  expect_equal(cc_theme(list(QUERY_STRING = "?theme=blue", HTTP_COOKIE = "cc_theme=light")), "light")
})

test_that("cc_tour_enabled honours ?tour=off|false|0|no and nothing else", {
  expect_true(cc_tour_enabled("?cruise=2026-04-3322"))
  expect_true(cc_tour_enabled(""))
  expect_true(cc_tour_enabled("?tour=on"))
  for (v in c("off", "false", "0", "no", "OFF", "False"))
    expect_false(cc_tour_enabled(paste0("?theme=dark&tour=", v)), info = v)
  # a parsed list works too
  expect_false(cc_tour_enabled(list(tour = "off")))
  # no session, no query -> the tour runs
  expect_true(cc_tour_enabled(NULL, session = NULL))
})

test_that("cc_is_dark reads bslib's switch, defaulting before it reports", {
  expect_true(cc_is_dark(list()))
  expect_false(cc_is_dark(list(), default = FALSE))
  expect_true(cc_is_dark(list(dark_toggle = "dark")))
  expect_false(cc_is_dark(list(dark_toggle = "light")))
  expect_false(cc_is_dark(list(my_switch = "light"), id = "my_switch"))
})

test_that("cc_brand_head emits the favicon set, pre-paint, theme.css/js and bridge", {
  h <- as.character(cc_brand_head("CalCOFI CTD Explorer"))
  expect_match(h, "<title>CalCOFI CTD Explorer</title>", fixed = TRUE)
  for (f in c("favicon.ico", "favicon-32x32.png", "favicon-16x16.png", "apple-touch-icon.png",
              "theme.css", "theme.js"))
    expect_match(h, paste0("https://calcofi.io/brand/v1/", f), fixed = TRUE, info = f)
  expect_match(h, 'defer src="https://calcofi.io/brand/v1/theme.js"', fixed = TRUE)
  # pre-paint sets every framework attribute
  expect_match(h, "data-bs-theme", fixed = TRUE)
  expect_match(h, "data-md-color-scheme", fixed = TRUE)
  # the bslib bridge
  expect_match(h, "MutationObserver", fixed = TRUE)
  # no analytics unless asked
  expect_no_match(h, "googletagmanager")
  expect_match(as.character(cc_brand_head(ga_app = "ctd-viz")), "googletagmanager", fixed = TRUE)
  expect_no_match(as.character(cc_brand_head()), "<title>")
})

test_that("cc_brand_header puts the logo→calcofi.io far left and the title→own root", {
  h <- as.character(cc_brand_header("CTD Explorer", subtitle = "v2026.08.25", mode = "light"))
  expect_match(h, '<header class="cc-header">', fixed = TRUE)
  # logo link comes before the title link
  expect_lt(regexpr('class="cc-home" href="https://calcofi.io"', h, fixed = TRUE),
            regexpr('class="cc-title" href="./"', h, fixed = TRUE))
  expect_match(h, "cc-logo-dark", fixed = TRUE)
  expect_match(h, "cc-logo-light", fixed = TRUE)
  expect_match(h, "<small>v2026.08.25</small>", fixed = TRUE)
  expect_match(h, 'bslib-input-dark-mode id="dark_toggle"', fixed = TRUE)
  expect_match(h, 'mode="light"', fixed = TRUE)
  # no switch when asked
  expect_no_match(as.character(cc_brand_header("x", toggle_id = NULL)), "bslib-input-dark-mode")
  # the app's controls land between the spacer and the switch
  h2 <- as.character(cc_brand_header("x", htmltools::tags$button(id = "btn_help", "?")))
  expect_lt(regexpr("cc-spacer", h2, fixed = TRUE), regexpr("btn_help", h2, fixed = TRUE))
  expect_lt(regexpr("btn_help", h2, fixed = TRUE), regexpr("bslib-input-dark-mode", h2, fixed = TRUE))
})

test_that("plot colours are the brand tokens and the ggplot/plotly themes apply them", {
  expect_equal(cc_plot_colors(TRUE)$fg,  "#e6e9ed")
  expect_equal(cc_plot_colors(FALSE)$fg, "#212529")
  expect_equal(cc_plot_colors(TRUE)$bg,  "rgba(0,0,0,0)")
  expect_s3_class(cc_ggplot_theme(TRUE), "theme")
  expect_equal(cc_ggplot_theme(FALSE)$axis.text$colour, "#6c757d")
  p <- cc_plotly_theme(plotly::plot_ly(x = 1:3, y = 1:3, type = "scatter", mode = "lines"), is_dark = TRUE)
  l <- p$x$layoutAttrs[[length(p$x$layoutAttrs)]]
  expect_equal(l$paper_bgcolor, "rgba(0,0,0,0)")
  expect_equal(l$font$color, "#e6e9ed")
  expect_equal(l$xaxis$gridcolor, "#3a3f44")
})

test_that("the release chip names the version, links to its schema, and is absent when unknown", {
  h <- as.character(cc_release_chip("v2026.08.25"))
  expect_match(h, '<a class="cc-release" href="https://calcofi.io/db-schema/#erd?v=v2026.08.25"', fixed = TRUE)
  expect_match(h, "release\\s*<b>v2026.08.25</b>")
  expect_null(cc_release_chip(NULL))
  expect_null(cc_release_chip(NA_character_))
  expect_null(cc_release_chip(""))
  # in the header it sits between the title and the spacer
  h2 <- as.character(cc_brand_header("CTD Explorer", release = "v2026.08.25"))
  expect_lt(regexpr("cc-title", h2, fixed = TRUE), regexpr("cc-release", h2, fixed = TRUE))
  expect_lt(regexpr("cc-release", h2, fixed = TRUE), regexpr("cc-spacer", h2, fixed = TRUE))
  expect_no_match(as.character(cc_brand_header("x")), "cc-release")
})
