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

# the obs_bio / obs_env pair as tiny local parquet at the fixture catalog's object
# paths under `root`, so cc_get_db() runs offline against a catalog with views.
# The pair's columns are the released ones (calcofi4db build_obs_slim(), 3.31.0).
write_pair_fixture <- function(root, version = "v2026.09.01") {
  mk <- function(rel) { p <- file.path(root, rel); dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE); p }
  con0 <- DBI::dbConnect(duckdb::duckdb())
  withr::defer(DBI::dbDisconnect(con0, shutdown = TRUE))
  pair_cols <- "obs_id, dataset_key, root_id, sample_key, grid_key, cruise_key, latitude, longitude, datetime, year, quarter,
    depth_min_m, depth_max_m, depth_bin, taxon_key, life_stage, measurement_type, units, value, measurement_qual,
    measurement_prec, qual_ok, tow_type, std_haul_factor, prop_sorted, volume_sampled_m3, density_per_10m2,
    density_per_1000m3, effort_class, hex_id, hex7"
  DBI::dbExecute(con0, glue::glue("CREATE TABLE obs_bio AS SELECT * FROM (VALUES
    (1::BIGINT, 'swfsc_ichthyo', 1, 'ich:net:1', 'st90-ln90', '2019-04-33UD', 32.9, -117.3, TIMESTAMP '2019-04-02 22:10', 2019::SMALLINT, 2::TINYINT,
     0.0, 210.0, 0, 'worms:217452', 'larva', 'abundance', 'count', 10.0, NULL::VARCHAR, NULL::DOUBLE, TRUE, 'CB', 2.0, 0.5, 100.0, 40.0, 200.0, 'count_with_effort',
     623333527607443455::UBIGINT, 608870215845019647::UBIGINT)) t({pair_cols})"))
  DBI::dbExecute(con0, glue::glue("CREATE TABLE obs_env AS SELECT * FROM (VALUES
    (2::BIGINT, 'calcofi_bottle', 2, 'btl:b:1', 'st90-ln90', '2019-04-33UD', 32.9, -117.3, TIMESTAMP '2019-04-02 23:00', 2019::SMALLINT, 2::TINYINT,
     10.0, 10.0, 10, NULL::VARCHAR, NULL::VARCHAR, 'temperature', 'degC', 15.5, '6', NULL::DOUBLE, TRUE, NULL::VARCHAR, NULL::DOUBLE, NULL::DOUBLE, NULL::DOUBLE, NULL::DOUBLE, NULL::DOUBLE, 'other_unit',
     623333527607443455::UBIGINT, 608870215845019647::UBIGINT),
    (3::BIGINT, 'calcofi_bottle', 2, 'btl:b:1', 'st90-ln90', '2019-04-33UD', 32.9, -117.3, TIMESTAMP '2019-04-02 23:00', 2019::SMALLINT, 2::TINYINT,
     10.0, 10.0, 10, NULL::VARCHAR, NULL::VARCHAR, 'salinity', 'psu', 33.4, '6', NULL::DOUBLE, TRUE, NULL::VARCHAR, NULL::DOUBLE, NULL::DOUBLE, NULL::DOUBLE, NULL::DOUBLE, NULL::DOUBLE, 'other_unit',
     623333527607443455::UBIGINT, 608870215845019647::UBIGINT)) t({pair_cols})"))
  DBI::dbExecute(con0, sprintf("COPY obs_bio TO '%s' (FORMAT parquet)",
    mk("ducklake/tables/obs_bio/b19def67a5bcfe2713624ebb/obs_bio.parquet")))
  # the env partitions are written WITHOUT the partition column, as the release writer does
  DBI::dbExecute(con0, sprintf("COPY (SELECT * EXCLUDE (measurement_type) FROM obs_env WHERE measurement_type = 'salinity') TO '%s' (FORMAT parquet)",
    mk("ducklake/tables/obs_env/measurement_type=salinity/4444444444444444444444dd/data_0.parquet")))
  DBI::dbExecute(con0, sprintf("COPY (SELECT * EXCLUDE (measurement_type) FROM obs_env WHERE measurement_type = 'temperature') TO '%s' (FORMAT parquet)",
    mk("ducklake/tables/obs_env/measurement_type=temperature/5555555555555555555555ee/data_0.parquet")))
  # cruise, and the deprecated obs's own objects (a different shape on purpose: they must not be read)
  DBI::dbExecute(con0, sprintf("COPY (SELECT 'x' AS cruise_key UNION ALL SELECT 'y') TO '%s' (FORMAT parquet)",
    mk("ducklake/tables/cruise/a1b2c3d4e5f60718293a4b5c/cruise.parquet")))
  for (rel in c("ducklake/tables/obs/year=2019/1111111111111111111111aa/data_0.parquet",
                "ducklake/tables/obs/year=2020/2222222222222222222222bb/data_0.parquet",
                "ducklake/tables/obs/9999999999999999999999ff/obs.parquet"))
    DBI::dbExecute(con0, sprintf("COPY (SELECT 1 AS legacy_obs) TO '%s' (FORMAT parquet)", mk(rel)))
  invisible(root)
}

test_that("cc_get_db() serves obs as the catalog view over obs_bio + obs_env, offline (D-S1)", {
  skip_if_not_installed("duckdb")
  root  <- withr::local_tempdir(); write_pair_fixture(root)
  cache <- withr::local_tempdir()
  mock_release_fetches("catalog_canonical.json")
  orig_sources <- cc_release_sources
  local_mocked_bindings(
    cc_release_sources = function(catalog, table, base_https = NULL) orig_sources(catalog, table, base_https = root),
    .cc_load_httpfs = function(con) invisible(con))
  expect_message(con <- cc_get_db(version = "v2026.09.01", cache_dir = cache),
                 "obs: served as the catalog view over obs_bio \\+ obs_env — the obs table is deprecated in v2026.09.01")
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  # the default set: obs_bio and obs_env are core, obs_ctd_full is supplemental, obs is the view
  expect_setequal(DBI::dbListTables(con), c("cruise", "obs", "obs_bio", "obs_env"))
  v <- DBI::dbGetQuery(con, "SELECT view_name FROM duckdb_views() WHERE NOT internal")$view_name
  expect_true("obs" %in% v)
  o <- DBI::dbGetQuery(con, "SELECT * FROM obs ORDER BY obs_id")
  expect_equal(nrow(o), 3)
  expect_identical(names(o), c("obs_id", "realm", "dataset_key", "sample_key", "grid_key", "cruise_key",
                               "latitude", "longitude", "datetime", "depth_min_m", "depth_max_m", "taxon_key",
                               "life_stage", "measurement_type", "measurement_value", "measurement_qual",
                               "measurement_prec", "hex_id"))
  expect_equal(o$realm, c("bio", "env", "env")); expect_equal(o$measurement_value, c(10, 15.5, 33.4))
  expect_equal(o$measurement_type, c("abundance", "temperature", "salinity"))   # env's from the hive path
  expect_equal(o$sample_key, c("ich:net:1", "btl:b:1", "btl:b:1"))
  # the deprecated table's own objects were never bound (they hold a `legacy_obs` column)
  expect_false("legacy_obs" %in% names(o))
  # cached path: the persisted view is what the next connection finds
  con2 <- cc_get_db(version = "v2026.09.01", cache_dir = cache)
  withr::defer(DBI::dbDisconnect(con2, shutdown = TRUE))
  expect_equal(DBI::dbGetQuery(con2, "SELECT count(*) AS n FROM obs")$n, 3)
})

test_that("cc_get_db(tables = 'obs') pulls in the view's tables; without them, the deprecated objects", {
  skip_if_not_installed("duckdb")
  root <- withr::local_tempdir(); write_pair_fixture(root)
  mock_release_fetches("catalog_canonical.json")
  orig_sources <- cc_release_sources
  local_mocked_bindings(
    cc_release_sources = function(catalog, table, base_https = NULL) orig_sources(catalog, table, base_https = root),
    .cc_load_httpfs = function(con) invisible(con))
  con <- cc_get_db(version = "v2026.09.01", local_cache = FALSE, cache_dir = withr::local_tempdir(), tables = "obs")
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  expect_setequal(DBI::dbListTables(con), c("obs", "obs_bio", "obs_env"))
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) AS n FROM obs")$n, 3)
  # naming obs beside other tables pulls the pair in too — a requested view is never served from
  # the deprecated objects (those are read only when the catalog cannot build the view)
  con3 <- cc_get_db(version = "v2026.09.01", local_cache = FALSE, cache_dir = withr::local_tempdir(),
                    tables = c("obs", "cruise"))
  withr::defer(DBI::dbDisconnect(con3, shutdown = TRUE))
  expect_setequal(DBI::dbListTables(con3), c("obs", "obs_bio", "obs_env", "cruise"))
  expect_equal(DBI::dbGetQuery(con3, "SELECT count(*) AS n FROM cruise")$n, 2)
  # the release after the window: no obs table at all, the view still answers
  mock_release_fetches("catalog_view_only.json")
  con4 <- cc_get_db(version = "v2026.10.01", local_cache = FALSE, cache_dir = withr::local_tempdir())
  withr::defer(DBI::dbDisconnect(con4, shutdown = TRUE))
  expect_setequal(DBI::dbListTables(con4), c("cruise", "obs", "obs_bio", "obs_env"))
  expect_equal(DBI::dbGetQuery(con4, "SELECT count(*) AS n FROM obs WHERE realm = 'env'")$n, 2)
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
