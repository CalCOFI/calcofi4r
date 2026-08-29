# cc_get_db() must never hand back a database missing the release's tables.
# Through 1.13.0 a view that failed to bind was a warning() per table; a vignette
# rendered with warning = FALSE saw nothing until "Table 'obs' not found", and
# pkgdown CI failed on every run from the first content-addressed release
# (2026-08-25) — the day the last s3:// glob, whose setup happened to INSTALL
# httpfs, left the catalog.

# stand in for the GCS fetches cc_get_db() makes before it touches any parquet:
# catalog.json comes from a fixture, and no version is retired
mock_release_fetches <- function(catalog_fixture, .env = parent.frame()) {
  local_mocked_bindings(
    .cc_download_gcs_file = function(gcs_path, local_path, overwrite = FALSE) {
      if (!grepl("catalog\\.json$", gcs_path))
        stop("unexpected download in test: ", gcs_path)
      dir.create(dirname(local_path), recursive = TRUE, showWarnings = FALSE)
      file.copy(test_path("fixtures", catalog_fixture), local_path, overwrite = TRUE)
      local_path
    },
    .cc_stop_if_retired = function(version) invisible(TRUE),
    .env = .env)
}

test_that("a table whose object cannot be read fails cc_get_db() outright: no warning, no partial cache", {
  skip_if_not_installed("duckdb")
  # the fixture's objects are resolved under a local root instead of the bucket,
  # so this runs offline: `cruise` is a real parquet there, `nope` is not
  root  <- withr::local_tempdir()
  cache <- withr::local_tempdir()
  d <- file.path(root, "ducklake/tables/cruise/a1b2c3d4e5f60718293a4b5c")
  dir.create(d, recursive = TRUE)
  con0 <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbExecute(con0, sprintf(
    "COPY (SELECT 'x' AS cruise_key UNION ALL SELECT 'y') TO '%s' (FORMAT parquet)",
    file.path(d, "cruise.parquet")))
  DBI::dbDisconnect(con0, shutdown = TRUE)

  mock_release_fetches("catalog_bad_object.json")
  orig_sources <- cc_release_sources
  local_mocked_bindings(
    cc_release_sources = function(catalog, table, base_https = NULL)
      orig_sources(catalog, table, base_https = root),
    # the real INSTALL/LOAD is exercised by the network tests below; a machine
    # that has never installed httpfs cannot INSTALL it offline
    .cc_load_httpfs = function(con) invisible(con))

  expect_no_warning(
    expect_error(
      cc_get_db(version = "v2026.09.01", cache_dir = cache),
      "failed to load table 'nope' of v2026.09.01 as a remote view"))

  # `cruise` bound before `nope` failed; the transaction must have rolled it
  # back, or the next call finds one "existing table" and returns it as cached
  db <- file.path(cache, "calcofi_v2026.09.01.duckdb")
  expect_true(file.exists(db))
  con <- DBI::dbConnect(duckdb::duckdb(dbdir = db))
  # withr::defer, never a bare on.exit(): without add = TRUE it REPLACES the
  # handlers already on the test frame — including the mock restores above,
  # which then leak into every later test file
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  expect_equal(DBI::dbListTables(con), character(0))
})

test_that("a tables= selection that matches nothing is an error, not an empty database", {
  skip_if_not_installed("duckdb")
  mock_release_fetches("catalog_bad_object.json")
  local_mocked_bindings(.cc_load_httpfs = function(con) invisible(con))
  expect_error(
    cc_get_db(version = "v2026.09.01", local_cache = FALSE,
              cache_dir = withr::local_tempdir(), tables = "typo"),
    "none of the requested tables")
})

test_that("cc_list_tables(), cc_describe_table(), cc_list_measurement_types() reuse a supplied connection", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "CREATE TABLE obs (sample_key VARCHAR, measurement_value DOUBLE)")
  DBI::dbExecute(con, "CREATE TABLE measurement_type AS
    SELECT 'temperature' AS measurement_type, 'in situ' AS description, 'degC' AS units")
  local_mocked_bindings(
    cc_get_db            = function(...) stop("cc_get_db() must not be called when con is supplied"),
    .cc_release_metadata = function(version = "latest") stop("offline"))

  expect_setequal(cc_list_tables(con = con), c("obs", "measurement_type"))

  mt <- cc_list_measurement_types(con = con)
  expect_s3_class(mt, "tbl_df")
  expect_equal(mt$measurement_type, "temperature")

  d <- cc_describe_table("obs", con = con)
  expect_equal(d$column_name, c("sample_key", "measurement_value"))
  expect_equal(d$data_type,   c("VARCHAR", "DOUBLE"))
  expect_true(all(c("name_long", "units", "description_md") %in% names(d)))
  expect_error(cc_describe_table("nope", con = con), "Table 'nope' not found")
})

test_that("the latest release connects with obs present and httpfs loaded (network)", {
  skip_if_offline()
  skip_if_not_installed("duckdb")
  cache <- withr::local_tempdir()
  con <- cc_get_db(cache_dir = cache)
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  tbls <- DBI::dbListTables(con)
  expect_true("obs" %in% tbls)
  expect_true(all(c("sample", "dataset", "measurement_type") %in% tbls))
  loaded <- DBI::dbGetQuery(con,
    "SELECT loaded FROM duckdb_extensions() WHERE extension_name = 'httpfs'")$loaded
  expect_true(isTRUE(loaded))
  # a view is only a promise until it is read
  expect_gt(DBI::dbGetQuery(con, "SELECT count(*) AS n FROM dataset")$n, 0)

  # the second connection to the same cache is the cached path — httpfs must be
  # loaded there too, or the persisted views cannot be read
  con2 <- cc_get_db(cache_dir = cache)
  withr::defer(DBI::dbDisconnect(con2))
  expect_true("obs" %in% DBI::dbListTables(con2))
  expect_gt(DBI::dbGetQuery(con2, "SELECT count(*) AS n FROM dataset")$n, 0)
})

test_that("a catalog object that 404s on the bucket is an error, never a warning (network)", {
  skip_if_offline()
  skip_if_not_installed("duckdb")
  mock_release_fetches("catalog_bad_object.json")
  expect_no_warning(
    expect_error(
      cc_get_db(version = "v2026.09.01", local_cache = FALSE, cache_dir = withr::local_tempdir()),
      "failed to load table 'cruise' of v2026.09.01 as a remote view"))
})
