# a session stand-in that records what the app pushed down the websocket
fake_session <- function(request = list()) {
  sent <- list()
  list(
    request           = request,
    token             = "abc123",
    sendCustomMessage = function(type, message) {
      sent[[length(sent) + 1]] <<- list(type = type, message = message)
      invisible(TRUE)
    },
    sent = function() sent)
}

test_that("cc_event normalises event names to GA4 rules", {
  # lowercase + non-alphanumerics collapsed to a single underscore
  expect_equal(cc_event("Map Query")$event, "map_query")
  expect_equal(cc_event("download-CSV")$event, "download_csv")
  expect_equal(cc_event("map:get_sp")$event, "map_get_sp")
  # leading/trailing separators trimmed
  expect_equal(cc_event(" _map click_ ")$event, "map_click")
  # must start with a letter
  expect_equal(cc_event("404_error")$event, "e_404_error")
  # <= 40 chars
  expect_equal(nchar(cc_event(strrep("a", 60))$event), 40L)
  # already-valid names pass through untouched
  expect_equal(cc_event("select_tab")$event, "select_tab")
})

test_that("cc_event keeps parameters and drops absent ones", {
  p <- cc_event("map_query_sp",
                taxa    = "Pacific sardine (species: Sardinops sagax)",
                env_var = "temperature")$params
  expect_equal(names(p), c("taxa", "env_var"))
  expect_equal(p$env_var, "temperature")

  # NULL / NA / "" are dropped so the Sheet's params column stays readable
  p2 <- cc_event("download", file = NULL, fmt = NA, label = "", ver = "v8")$params
  expect_equal(names(p2), "ver")

  # no params at all -> empty list, not NULL (JSON encodes as {})
  expect_equal(cc_event("page_view")$params, list())

  # multi-value parameters collapse to one readable cell rather than an array
  p3 <- cc_event("filter_submit", quarters = 1:4,
                 date_range = as.Date(c("1949-01-01", "2021-05-01")))$params
  expect_equal(p3$quarters, "1, 2, 3, 4")
  expect_equal(p3$date_range, "1949-01-01, 2021-05-01")
})

test_that("cc_event hoists the reserved metrics into their own columns", {
  # REGRESSION: n_rows / ms must stay OUT of the params JSON and stay NUMERIC —
  # Apps Script setValues() writes a JS string as text, making the column
  # unchartable, which is the whole point of giving them their own column.
  e <- cc_event("map_query_sp", taxa = "Sardinops sagax",
                n_rows = 1234, ms = 842.37, status = "ok")
  expect_equal(names(e$params), "taxa")
  expect_equal(sort(names(e$metrics)), c("ms", "n_rows", "status"))
  expect_identical(e$metrics$n_rows, 1234L)
  expect_identical(e$metrics$ms, 842.4)          # rounded to 0.1 ms
  expect_identical(e$metrics$status, "ok")

  # an event with no metrics carries an empty list, not NULL
  expect_equal(cc_event("select_tab", tab = "Map")$metrics, list())
})

test_that("cc_event rejects bad input", {
  expect_error(cc_event(""))
  expect_error(cc_event(NA))
  expect_error(cc_event(c("a", "b")))
  expect_error(cc_event("ok", "unnamed"), "must be named")
})

test_that("cc_track pushes a ccTrack message without any network I/O", {
  s   <- fake_session()
  out <- cc_track(s, "Select Layer", layer = "sst", n_rows = 10)
  msg <- s$sent()[[1]]
  expect_equal(msg$type, "ccTrack")
  expect_equal(msg$message$event, "select_layer")
  expect_equal(msg$message$params$layer, "sst")
  expect_equal(msg$message$metrics$n_rows, 10L)
  expect_equal(out, msg$message)
})

test_that("cc_track is silent when the session cannot receive", {
  # REGRESSION: instrumentation must never take down the app. A closed session
  # (sendCustomMessage errors) has to be swallowed.
  bad <- list(sendCustomMessage = function(type, message) stop("session closed"))
  expect_silent(cc_track(bad, "select_layer", layer = "x"))
  expect_error(cc_track(list(), "select_layer"), NA)   # no handler at all
})

test_that("cc_client_ip reads a ui(req) as well as a session", {
  # THE reason this matters: shiny-server does not proxy the websocket upgrade,
  # it opens a fresh localhost connection to the R worker - so session$request
  # has no X-Forwarded-For and REMOTE_ADDR is 127.0.0.1. The page request handed
  # to `ui = function(req)` is the only one that still carries the real IP.
  expect_equal(cc_client_ip(list(HTTP_X_FORWARDED_FOR = "203.0.113.7")), "203.0.113.7")
  expect_equal(cc_client_ip(list(REMOTE_ADDR = "198.51.100.4")), "198.51.100.4")
  # an environment, which is what Shiny actually hands a ui function
  e <- new.env(); assign("HTTP_X_FORWARDED_FOR", "203.0.113.9, 10.0.0.1", e)
  expect_equal(cc_client_ip(e), "203.0.113.9")
})

test_that("cc_client_ip prefers X-Forwarded-For over REMOTE_ADDR", {
  # behind the shiny-server/nginx proxy the direct peer is the proxy itself,
  # so the left-most forwarded address is the real client
  expect_equal(
    cc_client_ip(list(request = list(
      HTTP_X_FORWARDED_FOR = "203.0.113.7, 10.0.0.1",
      REMOTE_ADDR          = "10.0.0.1"))),
    "203.0.113.7")
  expect_equal(
    cc_client_ip(list(request = list(REMOTE_ADDR = "198.51.100.4"))),
    "198.51.100.4")
  expect_true(is.na(cc_client_ip(list(request = list()))))
  expect_true(is.na(cc_client_ip(list())))            # never errors
})

test_that("cc_track_session hands the browser the server-only facts", {
  # the ip and the Shiny token cannot be read in JS; without this message the
  # Sheet's `ip` and `session` columns would go permanently empty.
  s <- fake_session(list(HTTP_X_FORWARDED_FOR = "203.0.113.7"))
  cc_track_session(s)
  msg <- s$sent()[[1]]
  expect_equal(msg$type, "ccTrackSession")
  expect_equal(msg$message$ip, "203.0.113.7")
  expect_equal(msg$message$session, "abc123")
})

test_that("cc_track_query returns the result and logs row count + duration", {
  s   <- fake_session()
  res <- cc_track_query(s, "map_query_sp", list(taxa = "Sardinops sagax"),
                        data.frame(x = 1:3))
  expect_equal(nrow(res), 3L)                          # value passes through
  m <- s$sent()[[1]]$message
  expect_equal(m$event, "map_query_sp")
  expect_equal(m$params$taxa, "Sardinops sagax")
  expect_equal(m$metrics$n_rows, 3L)
  expect_equal(m$metrics$status, "ok")
  expect_true(is.numeric(m$metrics$ms) && m$metrics$ms >= 0)
})

test_that("cc_track_query leaves a lazy query lazy", {
  # dbplyr tables are the normal input: the row count must NOT be forced (that
  # would run the query twice, defeating the point of returning it lazily).
  s   <- fake_session()
  lazy <- structure(list(), class = c("tbl_lazy", "tbl"))
  res  <- cc_track_query(s, "map_query_env", list(var = "temperature"), lazy)
  expect_identical(res, lazy)
  expect_null(s$sent()[[1]]$message$metrics$n_rows)    # empty cell, not a count
})

test_that("cc_track_query logs an error then re-raises it", {
  # instrumentation must not swallow failures: the app's own error handling
  # (and the user-facing notification) still has to run.
  s <- fake_session()
  expect_error(
    cc_track_query(s, "map_query_sp", list(taxa = "x"), stop("boom")),
    "boom")
  m <- s$sent()[[1]]$message
  expect_equal(m$metrics$status, "error")
  expect_match(m$metrics$error, "boom")
  expect_null(m$metrics$n_rows)
})

test_that("cc_track_query evaluates its expression exactly once", {
  s <- fake_session()
  n <- 0
  cc_track_query(s, "e", list(), { n <- n + 1; data.frame(a = 1) })
  expect_equal(n, 1)
})

test_that("cc_ga_js embeds the configured ids and app metadata", {
  js <- cc_ga_js(app = "db-viz-hex", app_version = "abc1234",
                 log_url = "https://script.google.com/macros/s/AAA/exec")
  expect_true(grepl("G-VV117EV9ZT", js, fixed = TRUE))          # default property
  expect_true(grepl('var APP      = "db-viz-hex"', js, fixed = TRUE))
  expect_true(grepl('var APP_VER  = "abc1234"', js, fixed = TRUE))
  expect_true(grepl("https://script.google.com/macros/s/AAA/exec", js, fixed = TRUE))
  # content_group defaults to app, and is what separates apps in one property
  expect_true(grepl('var GROUP    = "db-viz-hex"', js, fixed = TRUE))
  expect_true(grepl("content_group", js, fixed = TRUE))
  # the server -> browser bridges cc_track() / cc_track_session() depend on
  expect_true(grepl('addCustomMessageHandler("ccTrack"', js, fixed = TRUE))
  expect_true(grepl('addCustomMessageHandler("ccTrackSession"', js, fixed = TRUE))
  # REGRESSION: an empty R list serialises as [] not {} — without normalising,
  # the Sheet's params column reads "[]".
  expect_true(grepl("(!x || Array.isArray(x)) ? {} : x", js, fixed = TRUE))
})

test_that("cc_ga_js bakes in the page-request IP, which the session cannot override", {
  js <- cc_ga_js("db-viz-hex", ip = "203.0.113.7", log_url = "https://example.com/exec")
  expect_true(grepl('var SERVER_IP = "203.0.113.7"', js, fixed = TRUE))
  # REGRESSION: cc_track_session() reports the SESSION's ip, which behind
  # shiny-server is 127.0.0.1 - applying it unconditionally would overwrite the
  # real address the page supplied, putting the log right back where it started.
  expect_true(grepl("if (!SERVER_IP && m.ip) SERVER_IP = m.ip;", js, fixed = TRUE))

  # NA (cc_client_ip's "cannot tell") must not reach the page as the string "NA"
  expect_true(grepl('var SERVER_IP = ""', cc_ga_js("x", ip = NA), fixed = TRUE))
  expect_true(grepl('var SERVER_IP = ""', cc_ga_js("x"), fixed = TRUE))
})

test_that("cc_ga_js keeps the Sheet beacon a CORS-simple request", {
  # REGRESSION: an application/json body triggers a preflight OPTIONS, which an
  # Apps Script /exec endpoint does not answer -> every event silently dropped.
  js <- cc_ga_js("db-viz-hex", log_url = "https://example.com/exec")
  expect_true(grepl('{ type: "text/plain;charset=UTF-8" }', js, fixed = TRUE))
  expect_true(grepl('"Content-Type": "text/plain"', js, fixed = TRUE))
  expect_false(grepl('"Content-Type": "application/json"', js, fixed = TRUE))
  # batched, not one request per interaction (Apps Script execution quota)
  expect_true(grepl("navigator.sendBeacon", js, fixed = TRUE))
  expect_true(grepl("queue.length >= BATCH", js, fixed = TRUE))
  # flushed on backgrounding too — "pagehide" alone misses mobile
  expect_true(grepl("visibilitychange", js, fixed = TRUE))
})

test_that("cc_ga_js with no log_url still emits GA4 but no beacon target", {
  js <- withr::with_envvar(c(CALCOFI_LOG_URL = ""), cc_ga_js("db-viz-hex"))
  expect_true(grepl('var LOG_URL  = ""', js, fixed = TRUE))
  expect_true(grepl("G-VV117EV9ZT", js, fixed = TRUE))
  # the guard that makes the Sheet leg a silent no-op
  expect_true(grepl("if (!LOG_URL) return;", js, fixed = TRUE))
})

test_that("cc_ga_js JSON-encodes interpolated values", {
  # a quote in an app id must not break out of the JS string literal
  js <- cc_ga_js(app = 'a"b')
  expect_true(grepl('var APP      = "a\\"b"', js, fixed = TRUE))
})

test_that("cc_ga_js validates app", {
  expect_error(cc_ga_js(""))
  expect_error(cc_ga_js(c("a", "b")))
})

test_that("cc_ga_head wraps the script for tags$head()", {
  h <- cc_ga_head("db-viz-hex")
  expect_s3_class(h, "html")
})

test_that("cc_ga_html writes a self-describing, GA4-only file", {
  f <- withr::local_tempfile(fileext = ".html")
  expect_equal(cc_ga_html(f, app = "marmam"), f)
  txt <- paste(readLines(f), collapse = "\n")

  expect_true(grepl('var APP      = "marmam"', txt, fixed = TRUE))
  expect_true(grepl("G-VV117EV9ZT", txt, fixed = TRUE))
  # the banner is a runnable regeneration command, not a vague hint - these
  # files live in six repos and must never be hand-patched out of sync
  expect_true(grepl('cc_ga_html("', txt, fixed = TRUE))
  expect_true(grepl('"marmam"', txt, fixed = TRUE))
  # Sheet leg off by default: a generated file is static, so it can carry
  # neither a per-request ip nor an env-var-driven endpoint
  expect_true(grepl('var LOG_URL  = ""', txt, fixed = TRUE))
  expect_true(grepl('var SERVER_IP = ""', txt, fixed = TRUE))

  # non-default arguments make it into the regeneration command
  g <- withr::local_tempfile(fileext = ".html")
  cc_ga_html(g, app = "x", app_version = "v9", log_url = "https://e.com/exec")
  txt2 <- paste(readLines(g), collapse = "\n")
  expect_true(grepl('app_version = "v9"', txt2, fixed = TRUE))
  expect_true(grepl('log_url = "https://e.com/exec"', txt2, fixed = TRUE))
})

test_that("the Sheet header, Apps Script, and client payload agree", {
  hdr <- cc_log_header()
  expect_equal(hdr[1], "timestamp")
  # the original db-viz-hex query-log columns keep their positions, so rows
  # written before the non-blocking rewrite still line up
  expect_equal(
    hdr[1:10],
    c("timestamp", "ip", "session", "event", "params", "n_rows", "ms",
      "status", "error", "app_version"))
  expect_true(all(c("app", "client_id", "session_id") %in% hdr))

  # the Apps Script writes exactly cc_log_header()'s columns, in order
  gs <- cc_apps_script()
  expect_true(grepl(as.character(jsonlite::toJSON(hdr)), gs, fixed = TRUE))
  # batched write, not appendRow() per event (Apps Script quota)
  expect_true(grepl("setValues(values)", gs, fixed = TRUE))
  expect_false(grepl("sh.appendRow", gs, fixed = TRUE))
  # timestamp lands as a real Date, like the pre-rewrite rows
  expect_true(grepl("new Date(r.timestamp)", gs, fixed = TRUE))

  # a GET health check must exist: without doGet, opening the /exec URL returns
  # "Script function not found: doGet", which reads as a broken deployment and
  # costs real debugging time even though only POST is ever used.
  expect_true(grepl("function doGet(e)", gs, fixed = TRUE))
  expect_true(grepl("function doPost(e)", gs, fixed = TRUE))

  # every header column is populated by the client payload builder in cc_ga_js()
  js <- cc_ga_js("db-viz-hex", log_url = "https://example.com/exec")
  for (col in hdr)
    expect_true(grepl(paste0(col, ":"), js, fixed = TRUE),
                info = paste("cc_ga_js() queue row is missing column:", col))
})
