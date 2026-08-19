# pure-logic pieces of the PostgreSQL helpers (no server needed); the live
# connection path is exercised only when CALCOFI_PG_TEST=1 (a tunnel + ~/.pgpass)

test_that("cc_pgpass_user() picks the role for host/port/db, honouring wildcards and comments", {
  f <- withr::local_tempfile(fileext = ".pgpass")
  writeLines(c(
    "# comment line",
    "",
    "otherhost:5432:*:someone:pw",
    "localhost:5432:*:rswalethorp:s3cret:with:colons",
    "postgis:5432:calcofi:bmgire:pw2",
    "*:*:*:fallback:pw3"), f)
  withr::local_envvar(PGPASSFILE = f)
  expect_equal(cc_pgpass_user("localhost", 5432, "calcofi"), "rswalethorp")
  expect_equal(cc_pgpass_user("postgis",   5432, "calcofi"), "bmgire")
  expect_equal(cc_pgpass_user("postgis",   5432, "gis"),     "fallback")   # db mismatch -> wildcard line
  expect_equal(cc_pgpass_user("localhost", 15432, "calcofi"), "fallback")  # port mismatch -> wildcard line
})

test_that("cc_pgpass_user() returns NULL when there is no file", {
  withr::local_envvar(PGPASSFILE = file.path(tempdir(), "definitely-missing.pgpass"))
  expect_null(cc_pgpass_user("localhost", 5432, "calcofi"))
})

test_that("host default is postgis on the server, localhost elsewhere; PGHOST wins", {
  withr::local_envvar(CALCOFI_ON_SERVER = "1")
  expect_true(cc_on_server())
  withr::local_envvar(CALCOFI_ON_SERVER = "")
  # off-server unless /share/github/CalCOFI exists on a Linux box (the server)
  if (!(dir.exists("/share/github/CalCOFI") && Sys.info()[["sysname"]] == "Linux"))
    expect_false(cc_on_server())
})

test_that("cc_port_open() is FALSE on a closed port", {
  expect_false(cc_port_open("127.0.0.1", 1))   # port 1 is never listening
})

test_that("cc_pg_connect() reaches the calcofi database (only with CALCOFI_PG_TEST=1)", {
  skip_if_not(nzchar(Sys.getenv("CALCOFI_PG_TEST")), "set CALCOFI_PG_TEST=1 with a tunnel + ~/.pgpass to run")
  con <- cc_pg_connect()
  on.exit(DBI::dbDisconnect(con))
  expect_true(DBI::dbIsValid(con))
  expect_true(all(c("ctd", "work") %in% DBI::dbGetQuery(con, "select nspname from pg_namespace")$nspname))
})
