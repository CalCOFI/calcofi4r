# ─── usage analytics for the CalCOFI Shiny apps ───────────────────────────────
#
# TWO CHANNELS, one code path. Both are driven from the BROWSER, so the Shiny
# reactive thread never performs network I/O — a synchronous POST per query (the
# original `db-viz-hex/app/logging.R`) stalled the reactive for the whole Apps
# Script round-trip, on every submit and every download:
#
#   1. GA4 (gtag)  — aggregate, bounded-cardinality behaviour: page views, which
#      tab, which layer, which download.
#   2. Sheet log   — the per-query detail GA4 is bad at: the taxon names and
#      filter parameters GA4 buckets into "(other)" once its daily cardinality
#      limit is hit, plus row counts, timings and error text. A Google Apps
#      Script web app bound to a Sheet exposes a POST endpoint; the BROWSER posts
#      to it, batched, via `navigator.sendBeacon()`.
#
# WHY THE BROWSER SENDS BOTH: `sendBeacon` is fire-and-forget by definition, is
# queued by the browser rather than the page, and survives unload. Server-side R
# facts (the resolved taxa behind a picker value, a query's row count and
# duration, an error message) reach it via [cc_track()], which only pushes a
# small message down the websocket the session already has open — no `httr2`, no
# blocking, no background worker, no flush queue in R.
#
# BATCHING: events accumulate client-side and flush when the queue reaches
# `.CC_LOG_BATCH` events, every `.CC_LOG_INTERVAL_MS`, or when the page is
# hidden/unloaded. This keeps well inside the Apps Script execution quota, which
# a per-interaction POST would burn through.
#
# CORS: the beacon body is sent as `text/plain;charset=UTF-8` so it stays a CORS
# "simple request" and triggers no preflight — an Apps Script `/exec` endpoint
# does not answer `OPTIONS`, so an `application/json` body would be dropped.
#
# SETUP (one-time, by a maintainer):
#   1. Create a Sheet whose first row is exactly `cc_log_header()`.
#   2. Extensions > Apps Script, paste `cc_apps_script()`, Deploy > New
#      deployment > Web app, execute as "Me", access "Anyone". Copy the /exec URL.
#   3. Set `CALCOFI_LOG_URL` to that URL for the app. Unset => the Sheet leg is a
#      silent no-op and only GA4 receives events.

# the GA4 measurement ID for the CalCOFI apps property
.CC_GA_ID <- "G-VV117EV9ZT"

# client-side batching: flush at N queued events or after T ms, whichever first
.CC_LOG_BATCH       <- 10L
.CC_LOG_INTERVAL_MS <- 15000L

# GA4 hard limits — enforced client-side before the gtag leg (the Sheet leg
# always receives the untruncated value).
.CC_GA_MAX_EVENT_CHARS <- 40L
.CC_GA_MAX_PARAM_CHARS <- 100L

# query metrics that get their OWN Sheet column instead of going into the
# `params` JSON blob, so row counts and durations stay chartable. Names are
# reserved: passing any of them to cc_track() hoists it out of `params`.
.CC_LOG_METRICS <- c("n_rows", "ms", "status", "error")

#' Column header for the usage-log Sheet
#'
#' The exact first row the Google Sheet must carry for [cc_apps_script()] to
#' append into. Kept here so the Sheet, the Apps Script, and the client payload
#' cannot drift.
#'
#' The first ten columns are the original `db-viz-hex` query log, so rows written
#' before the non-blocking rewrite keep their meaning; the rest were appended for
#' the browser-driven channel (`app` identifies which app wrote the row, so
#' several apps can share one Sheet).
#'
#' @return character vector of column names, in order
#' @examples
#' cc_log_header()
#' @export
#' @concept analytics
cc_log_header <- function()
  c("timestamp", "ip", "session", "event", "params", "n_rows", "ms", "status",
    "error", "app_version", "app", "client_id", "session_id", "page",
    "referrer", "user_agent")

# internal: the shared payload builder behind cc_event() and cc_track_query().
# `params` is a plain named list rather than `...` so callers holding a list
# (a filter-parameter bundle) need no do.call() — which would collide with the
# `session`/`event` argument names.
.cc_payload <- function(event, params = list()) {
  stopifnot(length(event) == 1L, !is.na(event), nzchar(event))

  # GA4: lowercase, non-alphanumerics -> "_", must start with a letter, <= 40 chars
  nm <- tolower(as.character(event))
  nm <- gsub("[^a-z0-9_]+", "_", nm)
  nm <- gsub("_+", "_", nm)
  nm <- gsub("^_+|_+$", "", nm)
  if (!grepl("^[a-z]", nm)) nm <- paste0("e_", nm)
  nm <- substr(nm, 1L, .CC_GA_MAX_EVENT_CHARS)

  if (length(params) > 0) {
    if (is.null(names(params)) || any(!nzchar(names(params))))
      stop("all event parameters must be named")
    # drop NULL / NA / "" so absent facts don't clutter the Sheet
    keep <- vapply(
      params,
      function(v) length(v) >= 1L && !all(is.na(v)) && any(nzchar(as.character(v))),
      logical(1))
    params <- params[keep]
  }

  # hoist the reserved metric names into their own columns. n_rows / ms stay
  # NUMERIC in the payload: Apps Script `setValues()` writes a JS string as text,
  # which would make the column unchartable.
  metrics <- params[intersect(.CC_LOG_METRICS, names(params))]
  params  <- params[setdiff(names(params), .CC_LOG_METRICS)]
  if (!is.null(metrics$n_rows)) metrics$n_rows <- as.integer(metrics$n_rows)
  if (!is.null(metrics$ms))     metrics$ms     <- round(as.numeric(metrics$ms), 1)
  for (k in intersect(c("status", "error"), names(metrics)))
    metrics[[k]] <- as.character(metrics[[k]])

  # everything else is text: a multi-value parameter (the selected taxa, the
  # quarters) collapses to one readable cell rather than a nested JSON array.
  params <- lapply(params, function(v) paste(as.character(v), collapse = ", "))

  # drop the zero-length names that `[` leaves behind, so an empty bag is always
  # the same object (the client normalises either form to a JS object)
  if (length(params)  == 0) params  <- list()
  if (length(metrics) == 0) metrics <- list()

  list(event = nm, params = params, metrics = metrics)
}

#' Build a tracking-event payload
#'
#' The pure payload constructor shared by [cc_track()] and the tests. Normalises
#' the event name to GA4's rules (lowercase, `[a-z0-9_]`, leading letter,
#' truncated to 40 characters) and drops `NULL`/`NA`/empty parameters so the
#' Sheet's `params` column stays readable.
#'
#' The names in [cc_log_header()] that have their own column — `n_rows`, `ms`,
#' `status`, `error` — are **reserved**: pass them like any other parameter and
#' they are hoisted out of `params` into `metrics`, keeping row counts and
#' durations numeric and chartable in the Sheet.
#'
#' Parameter *values* are NOT truncated here — the Sheet leg wants the full
#' string (a full species name, a list of taxa). The client truncates to 100
#' characters for the gtag leg only.
#'
#' @param event event name, e.g. `"map_query_sp"`; coerced to GA4-safe form
#' @param ... named event parameters; `NULL`/`NA`/`""` are dropped and
#'   multi-value parameters are collapsed to a comma-separated string
#' @return list with `event` (character scalar), `params` (named list of
#'   character) and `metrics` (named list of the reserved column values)
#' @examples
#' cc_event("Map Query", taxa = "Sardinops sagax", n_rows = 1234, ms = 842)
#' cc_event("download", file = NULL, format = "csv")   # NULL dropped
#' @export
#' @concept analytics
cc_event <- function(event, ...) .cc_payload(event, list(...))

# internal: push a payload down the session websocket, swallowing everything.
# Instrumentation must never take down an app: a closed session, a missing
# handler, or a session object that is not a session at all is silently ignored.
.cc_send <- function(session, type, message) {
  try(session$sendCustomMessage(type, message), silent = TRUE)
  invisible(message)
}

#' Send a tracking event from the Shiny server to the browser
#'
#' Pushes an event over the session's existing websocket; the client-side
#' handler installed by [cc_ga_js()] forwards it to GA4 and queues it for the
#' Sheet log. **Non-blocking** — no HTTP request is made on the R side, so a slow
#' or unreachable log endpoint can never stall a reactive.
#'
#' Use it for facts only the server knows (the taxa behind a picker value, a
#' query's row count, an error); pure UI interactions are better tracked
#' client-side with `window.ccTrack()` (see [cc_ga_js()]).
#'
#' @param session the Shiny `session` object
#' @param event event name, passed to [cc_event()]
#' @param ... named event parameters, passed to [cc_event()]; `n_rows`, `ms`,
#'   `status` and `error` land in their own Sheet columns
#' @return the payload, invisibly
#' @examples
#' \dontrun{
#' cc_track(session, "map_query_sp", taxa = sel_name, n_rows = 1234, ms = 842)
#' }
#' @export
#' @concept analytics
cc_track <- function(session, event, ...)
  .cc_send(session, "ccTrack", .cc_payload(event, list(...)))

#' Best-effort client IP from a Shiny request
#'
#' Reads `X-Forwarded-For` (set by the Caddy reverse proxy the apps sit behind)
#' and falls back to the direct `REMOTE_ADDR`. Never errors.
#'
#' **Pass the `req` of a `ui` function, not a `session`, when you can.**
#' shiny-server does not proxy the websocket upgrade — it opens a fresh
#' localhost connection to the R worker — so a session's `request` carries no
#' `X-Forwarded-For` and its `REMOTE_ADDR` is always `127.0.0.1`. The page's
#' HTTP request, which `ui = function(req)` receives, is the only place the real
#' client IP survives. See the `ip` argument of [cc_ga_js()].
#'
#' @param x a Shiny `session`, or the `req` environment handed to a `ui`
#'   function (anything carrying the request fields directly)
#' @return character scalar, or `NA_character_` if unavailable
#' @examples
#' cc_client_ip(list(request = list(HTTP_X_FORWARDED_FOR = "203.0.113.7, 10.0.0.1")))
#' cc_client_ip(list(HTTP_X_FORWARDED_FOR = "203.0.113.7"))   # a ui(req)
#' @export
#' @concept analytics
cc_client_ip <- function(x) {
  tryCatch({
    # a session carries the fields under $request; a ui() req carries them itself
    req <- if (is.null(x$request)) x else x$request
    xff <- req[["HTTP_X_FORWARDED_FOR"]]
    if (!is.null(xff) && nzchar(xff)) trimws(strsplit(xff, ",")[[1]][1])
    else {
      addr <- req[["REMOTE_ADDR"]]
      if (is.null(addr) || !nzchar(addr)) NA_character_ else addr
    }
  }, error = function(e) NA_character_)
}

#' Hand the browser the session facts only the server knows
#'
#' The client IP and the Shiny session token cannot be read in JavaScript, so the
#' server pushes them once at session start; the client then stamps them on every
#' queued row (the `ip` and `session` columns of [cc_log_header()]). Call it at
#' the top of the `server` function, before any [cc_track()] call, so no event
#' is written without them.
#'
#' The token is authoritative, the IP is only a **fallback**: behind
#' shiny-server a session sees `127.0.0.1`, so an `ip` already baked into the
#' page by [cc_ga_js()] wins and this one is ignored.
#'
#' @param session the Shiny `session` object
#' @return the sent list, invisibly
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   cc_track_session(session)
#'   ...
#' }
#' }
#' @export
#' @concept analytics
cc_track_session <- function(session)
  .cc_send(session, "ccTrackSession", list(
    ip      = cc_client_ip(session),
    session = tryCatch(session$token, error = function(e) NA_character_)))

#' Time a query, log it, and return its result
#'
#' Wraps a (usually lazy) query expression: times it, records the row count when
#' the result is a data frame, tracks the outcome via [cc_track()], and returns
#' the result unchanged. An error is tracked as `status = "error"` with its
#' message and then re-raised, so instrumentation changes nothing about the app's
#' behaviour.
#'
#' @param session the Shiny `session` object
#' @param event event name, e.g. `"map_query_sp"`
#' @param params named list of query parameters (taxa, dates, depths, …)
#' @param expr the query expression; evaluated once, lazily
#' @return the value of `expr`
#' @examples
#' \dontrun{
#' df_sp <- cc_track_query(
#'   session, "map_query_sp", list(taxa = sel_name, quarters = sel_qtr),
#'   get_sp(sel_name, sel_qtr, sel_date_range, ck_children))
#' }
#' @export
#' @concept analytics
cc_track_query <- function(session, event, params = list(), expr) {
  t0  <- Sys.time()
  res <- tryCatch(force(expr), error = function(e) e)
  ms  <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000

  if (inherits(res, "error")) {
    .cc_send(session, "ccTrack", .cc_payload(event, c(
      params, list(ms = ms, status = "error", error = conditionMessage(res)))))
    stop(res)
  }

  # a lazy dbplyr table has no row count until it is collected — don't force one
  n <- tryCatch(if (is.data.frame(res)) nrow(res) else NA_integer_,
                error = function(e) NA_integer_)
  .cc_send(session, "ccTrack", .cc_payload(event, c(
    params, list(n_rows = n, ms = ms, status = "ok"))))
  res
}

#' Analytics `<head>` snippet (GA4 + batched Sheet beacon)
#'
#' Generates the self-contained HTML/JS installed once per page: the GA4 gtag
#' loader, the client-side event queue that beacons to the usage-log Sheet, the
#' `Shiny.addCustomMessageHandler()` handlers that [cc_track()] and
#' [cc_track_session()] depend on, and a `window.ccTrack(event, params, metrics)`
#' helper for page JS.
#'
#' The same snippet serves every CalCOFI app; `content_group` (and the `app`
#' column of the Sheet) is what separates them in reporting, so no app needs its
#' own measurement ID.
#'
#' @param app short app id recorded on every event, e.g. `"db-viz-hex"`
#' @param content_group GA4 content group for reporting; defaults to `app`
#' @param app_version version string recorded on every event, e.g. a git SHA
#' @param ip client IP stamped on every logged row. Behind shiny-server this is
#'   the **only** way to record a real one: the websocket handshake the R session
#'   sees is a fresh localhost connection with no `X-Forwarded-For`, so make the
#'   app's `ui` a `function(req)` and pass `cc_client_ip(req)` here. Left empty,
#'   the `ip` column falls back to whatever [cc_track_session()] reports
#'   (`127.0.0.1` behind a proxy).
#' @param measurement_id GA4 measurement ID; defaults to the CalCOFI apps property
#' @param log_url Apps Script `/exec` endpoint for the Sheet log. Defaults to the
#'   `CALCOFI_LOG_URL` environment variable; empty means the Sheet leg is a
#'   silent no-op (GA4 still receives events).
#' @return character scalar of raw HTML (`<script>` tags)
#' @examples
#' substr(cc_ga_js(app = "db-viz-hex"), 1, 60)
#' @export
#' @concept analytics
cc_ga_js <- function(app,
                     content_group  = app,
                     app_version    = "",
                     ip             = "",
                     measurement_id = .CC_GA_ID,
                     log_url        = Sys.getenv("CALCOFI_LOG_URL", "")) {
  stopifnot(length(app) == 1L, nzchar(app))
  # cc_client_ip() returns NA when it cannot tell; the page wants an empty string
  if (length(ip) != 1L || is.na(ip)) ip <- ""

  # JSON-encode every interpolated value so a stray quote can't break the script
  j <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE))

  glue::glue(
    '<!-- Google tag (gtag.js) + CalCOFI usage log -->
<script async src="https://www.googletagmanager.com/gtag/js?id=<<measurement_id>>"></script>
<script>
(function () {
  var GA_ID    = <<j(measurement_id)>>;
  var APP      = <<j(app)>>;
  var APP_VER  = <<j(app_version)>>;
  var GROUP    = <<j(content_group)>>;
  var LOG_URL  = <<j(log_url)>>;
  var BATCH    = <<.CC_LOG_BATCH>>;
  var INTERVAL = <<.CC_LOG_INTERVAL_MS>>;

  // ---- GA4 ----------------------------------------------------------------
  window.dataLayer = window.dataLayer || [];
  function gtag() { dataLayer.push(arguments); }
  window.gtag = window.gtag || gtag;
  gtag("js", new Date());
  gtag("config", GA_ID, { content_group: GROUP, app_name: APP, app_version: APP_VER });

  // ---- identity -----------------------------------------------------------
  // Independent of GA so the Sheet log still stitches sessions when gtag is
  // blocked. client_id persists across visits; session_id is per tab.
  function uid() {
    return (Date.now().toString(36) + Math.random().toString(36).slice(2, 10));
  }
  function stored(store, key) {
    try {
      var v = store.getItem(key);
      if (!v) { v = uid(); store.setItem(key, v); }
      return v;
    } catch (e) { return "na"; }   // private mode / storage disabled
  }
  var CLIENT_ID  = stored(window.localStorage,   "calcofi_client_id");
  var SESSION_ID = stored(window.sessionStorage, "calcofi_session_id");

  // the client IP and the Shiny session token are server-only facts. The IP is
  // baked in from the PAGE request (the only request that still carries
  // X-Forwarded-For — shiny-server rebuilds the websocket handshake as a fresh
  // localhost connection); the token arrives from cc_track_session().
  var SERVER_IP = <<j(ip)>>, SERVER_SESSION = "";

  // ---- Sheet log: queue + batched beacon -----------------------------------
  var queue = [];

  function flush() {
    if (!LOG_URL || !queue.length) return;
    var rows = queue.splice(0, queue.length);
    var body = JSON.stringify({ rows: rows });
    try {
      // text/plain keeps this a CORS "simple request": Apps Script /exec does
      // not answer OPTIONS, so an application/json preflight would be dropped.
      var blob = new Blob([body], { type: "text/plain;charset=UTF-8" });
      if (!(navigator.sendBeacon && navigator.sendBeacon(LOG_URL, blob))) {
        fetch(LOG_URL, { method: "POST", body: body, keepalive: true,
                         mode: "no-cors", headers: { "Content-Type": "text/plain" } })
          .catch(function () {});
      }
    } catch (e) { /* logging must never surface to the user */ }
  }

  setInterval(flush, INTERVAL);
  // sendBeacon survives unload; "hidden" also fires on mobile backgrounding,
  // which "unload" does not.
  document.addEventListener("visibilitychange", function () {
    if (document.visibilityState === "hidden") flush();
  });
  window.addEventListener("pagehide", flush);

  // ---- the one entry point -------------------------------------------------
  // Sends to BOTH legs: GA4 (truncated to its 100-char param limit) and the
  // Sheet queue (full values). `metrics` carries the reserved n_rows / ms /
  // status / error, which get their own Sheet columns.
  window.ccTrack = function (event, params, metrics) {
    params  = params  || {};
    metrics = metrics || {};
    try {
      var ga = { content_group: GROUP, app_name: APP, app_version: APP_VER };
      [params, metrics].forEach(function (o) {
        Object.keys(o).forEach(function (k) {
          var v = o[k];
          ga[k] = (typeof v === "string") ? v.slice(0, <<.CC_GA_MAX_PARAM_CHARS>>) : v;
        });
      });
      if (window.gtag) gtag("event", event, ga);
    } catch (e) {}

    if (!LOG_URL) return;
    function m(k) { return (metrics[k] === undefined || metrics[k] === null) ? "" : metrics[k]; }
    queue.push({
      timestamp:   new Date().toISOString(),
      ip:          SERVER_IP,
      session:     SERVER_SESSION,
      event:       event,
      params:      JSON.stringify(params),
      n_rows:      m("n_rows"),
      ms:          m("ms"),
      status:      m("status"),
      error:       m("error"),
      app_version: APP_VER,
      app:         APP,
      client_id:   CLIENT_ID,
      session_id:  SESSION_ID,
      page:        location.pathname + location.search,
      referrer:    document.referrer || "",
      user_agent:  navigator.userAgent || ""
    });
    if (queue.length >= BATCH) flush();
  };

  // ---- server -> browser (calcofi4r::cc_track) -----------------------------
  if (window.Shiny && Shiny.addCustomMessageHandler) {
    // an EMPTY R list serialises as [] rather than {}, which would land in the
    // params column of the Sheet as "[]" — normalise both bags to objects.
    function obj(x) { return (!x || Array.isArray(x)) ? {} : x; }
    Shiny.addCustomMessageHandler("ccTrack", function (m) {
      window.ccTrack(m.event, obj(m.params), obj(m.metrics));
    });
    Shiny.addCustomMessageHandler("ccTrackSession", function (m) {
      // the IP reported here is a FALLBACK, never an override: behind
      // shiny-server a session always sees 127.0.0.1, which would clobber the
      // real address the page request supplied.
      if (!SERVER_IP && m.ip) SERVER_IP = m.ip;
      SERVER_SESSION = m.session || "";
    });
  }
})();
</script>',
    .open = "<<", .close = ">>")
}

#' Analytics `<head>` snippet as a Shiny tag
#'
#' [cc_ga_js()] wrapped in [htmltools::HTML()] for use inside `tags$head()`.
#'
#' @inheritParams cc_ga_js
#' @return an `html` object
#' @examples
#' \dontrun{
#' ui <- bslib::page_sidebar(
#'   tags$head(calcofi4r::cc_ga_head("db-viz-hex", app_version = APP_VERSION)), ...)
#' }
#' @export
#' @concept analytics
cc_ga_head <- function(app,
                       content_group  = app,
                       app_version    = "",
                       ip             = "",
                       measurement_id = .CC_GA_ID,
                       log_url        = Sys.getenv("CALCOFI_LOG_URL", ""))
  htmltools::HTML(
    cc_ga_js(app, content_group, app_version, ip, measurement_id, log_url))

#' Write the analytics snippet to a standalone HTML file
#'
#' For apps that do **not** already depend on calcofi4r. Loading the calcofi4r
#' namespace costs several seconds of cold start (its imports pull in sf, terra,
#' stars, …), which a small app should not pay just to be counted — so generate
#' the snippet to a file and keep the app's existing
#' `includeHTML("google-analytics.html")`, with no runtime dependency at all.
#'
#' Apps that already attach calcofi4r should call [cc_ga_head()] directly
#' instead: it costs them nothing and can never go stale.
#'
#' The file is **generated**. It carries a banner naming the exact call that
#' produced it, so when the shared snippet changes it is regenerated rather than
#' hand-patched in each repo.
#'
#' A generated file is static, so it cannot carry the per-request `ip` of
#' [cc_ga_js()] — the Sheet leg is off by default here for that reason. GA4 is
#' unaffected: gtag resolves the client IP in the browser.
#'
#' @param path file to write, e.g. `"google-analytics.html"`
#' @inheritParams cc_ga_js
#' @param log_url Apps Script `/exec` endpoint. Defaults to `""` (GA4 only) —
#'   unlike [cc_ga_js()], a generated file does not read the environment, since
#'   the result is committed to a repo.
#' @return `path`, invisibly
#' @examples
#' f <- tempfile(fileext = ".html")
#' cc_ga_html(f, app = "marmam")
#' @export
#' @concept analytics
cc_ga_html <- function(path,
                       app,
                       content_group  = app,
                       app_version    = "",
                       measurement_id = .CC_GA_ID,
                       log_url        = "") {
  # spell the regeneration call with the non-default arguments actually used, so
  # the banner is a command that can be pasted, not a hint
  args <- sprintf('"%s", "%s"', basename(path), app)
  if (!identical(content_group, app))
    args <- paste0(args, sprintf(', content_group = "%s"', content_group))
  if (nzchar(app_version))
    args <- paste0(args, sprintf(', app_version = "%s"', app_version))
  if (!identical(measurement_id, .CC_GA_ID))
    args <- paste0(args, sprintf(', measurement_id = "%s"', measurement_id))
  if (nzchar(log_url))
    args <- paste0(args, sprintf(', log_url = "%s"', log_url))

  writeLines(c(
    "<!-- GENERATED by calcofi4r::cc_ga_html() — do not hand-edit.",
    sprintf("     Regenerate: Rscript -e 'calcofi4r::cc_ga_html(%s)'", args),
    "     Shared with every CalCOFI app; see calcofi4r::cc_ga_js(). -->",
    cc_ga_js(app, content_group, app_version, ip = "", measurement_id, log_url)),
    path)

  invisible(path)
}

#' Apps Script source for the usage-log Sheet
#'
#' The `doPost()` handler to paste into the Sheet's bound Apps Script project.
#' It appends a **batch** of rows in one `setValues()` call (the client sends
#' `{rows: [...]}`), which is what keeps the write cost — and the Apps Script
#' execution quota — flat regardless of interaction rate.
#'
#' Column order is taken from [cc_log_header()], so the Sheet, the script, and
#' the client payload cannot drift.
#'
#' @return character scalar of JavaScript source
#' @examples
#' cat(cc_apps_script())
#' @export
#' @concept analytics
cc_apps_script <- function() {
  cols <- cc_log_header()
  glue::glue(
    '// Code.gs — CalCOFI usage log (bound to the log Sheet).
// Generated by calcofi4r::cc_apps_script() — do not hand-edit.
// Deploy: Deploy > New deployment > type "Web app",
//         execute as "Me", who has access "Anyone". Copy the /exec URL into
//         the CALCOFI_LOG_URL environment variable for the Shiny apps.
//
// The client (calcofi4r::cc_ga_js) posts {rows:[{...}, ...]} as text/plain so
// the request stays CORS-simple (this endpoint does not answer OPTIONS).

var COLS = <<jsonlite::toJSON(cols)>>;

// Health check. Without this, opening the /exec URL in a browser returns
// "Script function not found: doGet", which looks like a broken or unauthorized
// deployment and sends you hunting through deployment settings. It is not — the
// client only ever POSTs. A GET now answers {ok:true,...} so the endpoint can be
// verified at a glance. Reports the row count so you can confirm writes land.
function doGet(e) {
  try {
    var sh = SpreadsheetApp.getActiveSpreadsheet().getSheets()[0];
    return ContentService
      .createTextOutput(JSON.stringify({ ok: true, endpoint: "calcofi-usage-log", rows: sh.getLastRow() - 1 }))
      .setMimeType(ContentService.MimeType.JSON);
  } catch (err) {
    return ContentService
      .createTextOutput(JSON.stringify({ ok: false, error: String(err) }))
      .setMimeType(ContentService.MimeType.JSON);
  }
}

function doPost(e) {
  try {
    var sh   = SpreadsheetApp.getActiveSpreadsheet().getSheets()[0];
    var body = JSON.parse(e.postData.contents);
    var rows = body.rows || [body];
    if (!rows.length) return _ok(0);

    // one setValues() for the whole batch — far cheaper than appendRow() per event
    var values = rows.map(function (r) {
      return COLS.map(function (c) {
        if (c === "timestamp")
          // a real Date, matching the rows written before the client-side
          // rewrite — an ISO string would land as unsortable text.
          return r.timestamp ? new Date(r.timestamp) : new Date();
        return r[c] === undefined ? "" : r[c];
      });
    });
    sh.getRange(sh.getLastRow() + 1, 1, values.length, COLS.length).setValues(values);
    return _ok(values.length);
  } catch (err) {
    return ContentService
      .createTextOutput(JSON.stringify({ ok: false, error: String(err) }))
      .setMimeType(ContentService.MimeType.JSON);
  }
}

function _ok(n) {
  return ContentService
    .createTextOutput(JSON.stringify({ ok: true, n: n }))
    .setMimeType(ContentService.MimeType.JSON);
}',
    .open = "<<", .close = ">>")
}
