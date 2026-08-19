# ─── CalCOFI PostgreSQL (multi-user CTD QA/QC database) ───────────────────────
#
# The frozen releases are public Parquet (see cc_get_db()). The *working*
# multi-user database the CTD team writes to is PostgreSQL on the CalCOFI server,
# reachable only through an SSH tunnel (laptop) or by service name from the
# server's own containers. These helpers make that a one-liner and keep secrets
# out of code: the password comes from libpq's ~/.pgpass, never from an argument
# you would commit. Instructions for getting an account, the tunnel and .pgpass:
# https://calcofi.io/docs/server-access.html

#' Connect to the CalCOFI PostgreSQL database
#'
#' Opens a `DBI` connection (via `RPostgres`) to the multi-user CalCOFI
#' PostgreSQL database used by the CTD team for QA/QC (database `calcofi`,
#' schemas `ctd` / `work` / your own). Every argument has a sensible default:
#'
#' * **host**: `"postgis"` when running on the CalCOFI server (RStudio Server at
#'   rstudio.calcofi.io, Shiny), otherwise `"localhost"` — i.e. the local end of
#'   your SSH tunnel (`ssh -N calcofi`, or [cc_pg_tunnel()]). `PGHOST` overrides.
#' * **user**: `PGUSER` if set, else the role name found in your `~/.pgpass` for
#'   this host/port/database (the file you copied from the server), else your OS
#'   user name.
#' * **password**: `NULL` — libpq reads `~/.pgpass` (Windows:
#'   `%APPDATA%\\postgresql\\pgpass.conf`). Pass one only for throw-away use.
#'
#' @param dbname database name; default `"calcofi"` (`"gis"` is the legacy 2022 db)
#' @param host host name; default described above
#' @param port port; default `5432` (`PGPORT` overrides; use `15432` if your
#'   tunnel maps there)
#' @param user role name; default described above
#' @param password password; default `NULL` = use `~/.pgpass`
#' @param tunnel if `TRUE`, start an SSH tunnel with [cc_pg_tunnel()] first
#'   (only meaningful off-server)
#' @param ... passed to [DBI::dbConnect()]
#' @return a `DBI` connection; disconnect with `DBI::dbDisconnect()`
#' @export
#' @concept database
#' @seealso [cc_pg_tunnel()], [cc_pg_attach()], [cc_get_db()] for the public releases
#' @examples
#' \dontrun{
#' con <- cc_pg_connect()                  # tunnel already running, ~/.pgpass in place
#' DBI::dbListObjects(con, DBI::Id(schema = "ctd"))
#' dplyr::tbl(con, I("ctd.cast"))
#'
#' con <- cc_pg_connect(tunnel = TRUE)     # also opens `ssh -N calcofi` for you
#' }
cc_pg_connect <- function(
    dbname   = "calcofi",
    host     = NULL,
    port     = NULL,
    user     = NULL,
    password = NULL,
    tunnel   = FALSE,
    ...) {

  host <- host %||% .nz(Sys.getenv("PGHOST")) %||% (if (cc_on_server()) "postgis" else "localhost")
  port <- port %||% .nz(Sys.getenv("PGPORT")) %||% 5432L
  port <- as.integer(port)

  if (isTRUE(tunnel) && host %in% c("localhost", "127.0.0.1"))
    cc_pg_tunnel(local_port = port)

  user <- user %||% .nz(Sys.getenv("PGUSER")) %||%
    cc_pgpass_user(host, port, dbname) %||% Sys.info()[["user"]]

  DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = dbname, host = host, port = port, user = user,
    password = password, ...)
}

#' Open an SSH tunnel to the CalCOFI server's PostgreSQL
#'
#' Runs `ssh -N -L <local_port>:localhost:<remote_port> <ssh_host>` in the
#' background with `processx`, so `cc_pg_connect()` can reach the database on
#' `localhost:<local_port>`. Uses your `~/.ssh/config` entry (the docs call it
#' `calcofi`: `HostName ssh.calcofi.io`, `User <you>`, `IdentityFile …`), so no
#' credentials are handled here. Windows 10+ has `ssh.exe` built in.
#'
#' The process is kept in a package-level registry and reused while alive; call
#' [cc_pg_tunnel_close()] to stop it. If something already listens on
#' `local_port` (an earlier tunnel, a local Postgres) the port is left alone and
#' a message says so — set `local_port = 15432` in both places in that case.
#'
#' @param ssh_host the `Host` alias from `~/.ssh/config` (or `user@ssh.calcofi.io`)
#' @param local_port local port to listen on; default `5432`
#' @param remote_port port on the server; default `5432`
#' @param wait seconds to wait for the port to come up; default `10`
#' @return the `processx::process` (invisibly), or `NULL` if the port was
#'   already open
#' @export
#' @concept database
cc_pg_tunnel <- function(
    ssh_host    = "calcofi",
    local_port  = 5432L,
    remote_port = 5432L,
    wait        = 10) {

  if (!requireNamespace("processx", quietly = TRUE))
    stop("cc_pg_tunnel() needs the 'processx' package: install.packages('processx')")

  key <- paste0(ssh_host, ":", local_port)
  p   <- .cc_tunnels[[key]]
  if (!is.null(p) && p$is_alive()) {
    message("tunnel already running (", key, ")")
    return(invisible(p))
  }
  if (cc_port_open("127.0.0.1", local_port)) {
    message("something already listens on localhost:", local_port,
            " — using it as-is (an existing tunnel or a local Postgres). ",
            "If that is not the CalCOFI server, use local_port = 15432.")
    return(invisible(NULL))
  }
  ssh <- Sys.which("ssh")
  if (!nzchar(ssh))
    stop("no `ssh` executable found on PATH (Windows: enable the OpenSSH Client optional feature)")

  p <- processx::process$new(
    ssh,
    c("-N", "-o", "ExitOnForwardFailure=yes", "-o", "BatchMode=yes",
      "-L", sprintf("%d:localhost:%d", as.integer(local_port), as.integer(remote_port)),
      ssh_host),
    stdout = "|", stderr = "|", cleanup = TRUE)

  t0 <- Sys.time()
  while (!cc_port_open("127.0.0.1", local_port)) {
    if (!p$is_alive()) {
      err <- paste(p$read_all_error_lines(), collapse = "\n")
      stop("ssh exited before the tunnel came up:\n", err,
           "\nCheck `ssh ", ssh_host, "` works in a terminal first (key loaded? host alias in ~/.ssh/config?).")
    }
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > wait)
      stop("tunnel did not open within ", wait, " s")
    Sys.sleep(0.25)
  }
  .cc_tunnels[[key]] <- p
  message("SSH tunnel up: localhost:", local_port, " -> ", ssh_host, ":", remote_port)
  invisible(p)
}

#' @rdname cc_pg_tunnel
#' @export
cc_pg_tunnel_close <- function(ssh_host = "calcofi", local_port = 5432L) {
  key <- paste0(ssh_host, ":", local_port)
  p <- .cc_tunnels[[key]]
  if (!is.null(p)) {
    if (p$is_alive()) p$kill()
    rm(list = key, envir = .cc_tunnels)
    message("tunnel closed (", key, ")")
  }
  invisible(NULL)
}

#' Attach the CalCOFI PostgreSQL database inside a DuckDB connection
#'
#' Loads DuckDB's `postgres` extension and `ATTACH`es the PostgreSQL database,
#' so one DuckDB query can join the public release tables (from [cc_get_db()])
#' with the team's PostgreSQL tables (`pg.ctd.flag`, `pg.work.*`, …). Host /
#' port / user default exactly as in [cc_pg_connect()]; the password is read by
#' libpq from `~/.pgpass`.
#'
#' With `read_only = FALSE` you can also write *into* PostgreSQL from DuckDB
#' (`INSERT INTO pg.work.my_table …`, `CREATE TABLE pg.work.x AS SELECT …`),
#' which is how bulk loads from Parquet are done.
#'
#' @param con a DuckDB connection, e.g. from [cc_get_db()] or
#'   `DBI::dbConnect(duckdb::duckdb())`
#' @param alias catalog name inside DuckDB; default `"pg"`
#' @param read_only attach read-only (default `TRUE`)
#' @inheritParams cc_pg_connect
#' @return `con` (invisibly)
#' @export
#' @concept database
#' @examples
#' \dontrun{
#' con <- cc_get_db()
#' cc_pg_attach(con)
#' DBI::dbGetQuery(con, "
#'   SELECT s.cruise_key, count(*) AS n_flags
#'   FROM pg.ctd.flag f JOIN sample s ON s.sample_key = f.sample_key
#'   GROUP BY 1 ORDER BY 2 DESC LIMIT 10")
#' }
cc_pg_attach <- function(
    con,
    alias     = "pg",
    dbname    = "calcofi",
    host      = NULL,
    port      = NULL,
    user      = NULL,
    read_only = TRUE) {

  host <- host %||% .nz(Sys.getenv("PGHOST")) %||% (if (cc_on_server()) "postgis" else "localhost")
  port <- as.integer(port %||% .nz(Sys.getenv("PGPORT")) %||% 5432L)
  user <- user %||% .nz(Sys.getenv("PGUSER")) %||%
    cc_pgpass_user(host, port, dbname) %||% Sys.info()[["user"]]

  DBI::dbExecute(con, "INSTALL postgres; LOAD postgres;")
  conn_str <- sprintf("dbname=%s host=%s port=%d user=%s", dbname, host, port, user)
  opts <- paste0("TYPE postgres", if (isTRUE(read_only)) ", READ_ONLY" else "")
  DBI::dbExecute(con, sprintf("ATTACH IF NOT EXISTS '%s' AS %s (%s)", conn_str, alias, opts))
  invisible(con)
}

# ─── internals ─────────────────────────────────────────────────────────────────

.cc_tunnels <- new.env(parent = emptyenv())

# `%||%` comes from rlang (imported package-wide)
.nz <- function(x) if (length(x) == 1 && !is.na(x) && nzchar(x)) x else NULL

# TRUE inside the CalCOFI server containers (rstudio/shiny), where the database
# is reachable as `postgis` on the compose network
cc_on_server <- function() {
  nzchar(Sys.getenv("CALCOFI_ON_SERVER")) ||
    (dir.exists("/share/github/CalCOFI") && Sys.info()[["sysname"]] == "Linux")
}

cc_port_open <- function(host, port, timeout = 1) {
  ok <- tryCatch({
    s <- suppressWarnings(socketConnection(host, as.integer(port), open = "r+", blocking = TRUE, timeout = timeout))
    close(s); TRUE
  }, error = function(e) FALSE)
  ok
}

# the role name recorded in ~/.pgpass for host:port:dbname (first matching line),
# so a user who copied the file from the server needs no PGUSER
cc_pgpass_user <- function(host, port, dbname) {
  f <- Sys.getenv("PGPASSFILE")
  if (!nzchar(f)) {
    f <- if (.Platform$OS.type == "windows")
      file.path(Sys.getenv("APPDATA"), "postgresql", "pgpass.conf")
    else
      path.expand("~/.pgpass")
  }
  if (!file.exists(f)) return(NULL)
  lines <- readLines(f, warn = FALSE)
  lines <- lines[nzchar(trimws(lines)) & !startsWith(trimws(lines), "#")]
  for (ln in lines) {
    # host:port:database:user:password  (password may contain ':' — split into 5)
    parts <- strsplit(ln, ":", fixed = TRUE)[[1]]
    if (length(parts) < 5) next
    h <- parts[1]; p <- parts[2]; d <- parts[3]; u <- parts[4]
    if ((h == "*" || h == host) && (p == "*" || p == as.character(port)) &&
        (d == "*" || d == dbname) && nzchar(u))
      return(u)
  }
  NULL
}
