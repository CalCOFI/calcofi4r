# The fixture catalogs are shared byte-for-byte with calcofi4py/tests/fixtures/;
# both packages must resolve the same URLs from the same catalog.
fx <- function(f) jsonlite::fromJSON(test_path("fixtures", f), simplifyVector = FALSE)

test_that("canonical catalog resolves to content-addressed https objects", {
  cat_ <- fx("catalog_canonical.json")
  s <- cc_release_sources(cat_, "cruise")
  expect_equal(s$urls,
    "https://storage.googleapis.com/calcofi-db/ducklake/tables/cruise/a1b2c3d4e5f60718293a4b5c/cruise.parquet")
  expect_false(s$hive); expect_true(s$canonical)
  expect_equal(s$hashes, "a1b2c3d4e5f60718293a4b5c6d7e8f90")
  expect_equal(s$local_paths, "tables/cruise/a1b2c3d4e5f60718293a4b5c/cruise.parquet")

  s <- cc_release_sources(cat_, "obs")
  expect_equal(s$urls, c(
    "https://storage.googleapis.com/calcofi-db/ducklake/tables/obs/year=2019/1111111111111111111111aa/data_0.parquet",
    "https://storage.googleapis.com/calcofi-db/ducklake/tables/obs/year=2020/2222222222222222222222bb/data_0.parquet"))
  expect_true(s$hive)
  # the single-file twin is exposed separately, never mixed into the partition list
  expect_equal(s$single_file,
    "https://storage.googleapis.com/calcofi-db/ducklake/tables/obs/9999999999999999999999ff/obs.parquet")
  expect_true(is.na(cc_release_sources(cat_, "cruise")$single_file))
  expect_match(cc_read_parquet_sql(s), "^read_parquet\\(\\['https.*', 'https.*'\\], hive_partitioning = true\\)$")
  # list-form sources are recovered for provenance
  expect_equal(calcofi4r:::.cc_extract_source_urls(paste("SELECT * FROM", cc_read_parquet_sql(s))), sort(s$urls))
  expect_equal(cc_read_parquet_sql(cc_release_sources(cat_, "cruise")),
    "read_parquet('https://storage.googleapis.com/calcofi-db/ducklake/tables/cruise/a1b2c3d4e5f60718293a4b5c/cruise.parquet')")
  expect_error(cc_release_sources(cat_, "nope"), "not in the catalog")
})

test_that("the simplified (data frame) catalog form resolves identically", {
  cat_s <- jsonlite::fromJSON(test_path("fixtures", "catalog_canonical.json"))
  expect_true(is.data.frame(cat_s$tables))
  cat_l <- fx("catalog_canonical.json")
  for (tb in c("cruise", "obs", "obs_ctd_full"))
    expect_equal(cc_release_sources(cat_s, tb), cc_release_sources(cat_l, tb))
})

test_that("a catalog's views: listed, their tables named, their SQL resolved through any reader (D-S1)", {
  cat_ <- fx("catalog_canonical.json")
  views <- cc_catalog_views(cat_)
  expect_equal(names(views), "obs")
  expect_equal(cc_view_tables(views$obs), c("obs_bio", "obs_env"))
  expect_match(views$obs, "\\{\\{obs_bio\\}\\}"); expect_match(views$obs, "value AS measurement_value")
  # default: quoted identifiers, as cc_get_db() binds them
  sql <- cc_view_sql(cat_, "obs")
  expect_false(grepl("{{", sql, fixed = TRUE))
  expect_match(sql, 'FROM "obs_bio"\nUNION ALL\n'); expect_match(sql, 'FROM "obs_env"$')
  # any reader: the catalog's own objects
  rp <- function(t) cc_read_parquet_sql(cc_release_sources(cat_, t))
  sql <- cc_view_sql(cat_, "obs", rp)
  expect_match(sql, "FROM read_parquet\\('https://storage.googleapis.com/calcofi-db/ducklake/tables/obs_bio/b19def67a5bcfe2713624ebb/obs_bio.parquet'\\)")
  expect_match(sql, "FROM read_parquet\\(\\['https.*measurement_type=salinity.*', 'https.*measurement_type=temperature.*'\\], hive_partitioning = true\\)")
  expect_equal(calcofi4r:::.cc_extract_source_urls(sql), sort(c(cc_release_sources(cat_, "obs_bio")$urls, cc_release_sources(cat_, "obs_env")$urls)))
  expect_error(cc_view_sql(cat_, "nope"), "not a view.*views: obs")
  # the same in the data-frame form, and nothing for a catalog without views
  expect_equal(cc_catalog_views(jsonlite::fromJSON(test_path("fixtures", "catalog_canonical.json"))), views)
  expect_equal(cc_catalog_views(fx("catalog_legacy.json")), list())
  expect_error(cc_view_sql(fx("catalog_legacy.json"), "obs"), "not a view")
})

test_that("a deprecated table still resolves and says so; a view-only name errors clearly", {
  cat_ <- fx("catalog_canonical.json")
  s <- cc_release_sources(cat_, "obs")
  expect_true(s$deprecated); expect_equal(s$replaced_by, c("obs_bio", "obs_env")); expect_equal(s$removed_in, "next")
  expect_length(s$urls, 2)                      # its objects ship through the window
  c_ <- cc_release_sources(cat_, "cruise")
  expect_false(c_$deprecated); expect_equal(c_$replaced_by, character(0)); expect_true(is.na(c_$removed_in))
  b <- cc_release_sources(cat_, "obs_bio")
  expect_equal(b$urls, "https://storage.googleapis.com/calcofi-db/ducklake/tables/obs_bio/b19def67a5bcfe2713624ebb/obs_bio.parquet")
  expect_false(b$hive); expect_false(b$deprecated)
  e <- cc_release_sources(cat_, "obs_env")
  expect_true(e$hive); expect_length(e$urls, 2); expect_true(is.na(e$single_file))
  # the release after the window: obs is a view alone
  nxt <- fx("catalog_view_only.json")
  expect_false("obs" %in% vapply(nxt$tables, `[[`, "", "name"))
  expect_error(cc_release_sources(nxt, "obs"), "'obs' is a view in the catalog for v2026.10.01 \\(over obs_bio, obs_env\\).*cc_get_db\\(\\).*cc_view_sql")
  expect_error(cc_release_sources(nxt, "casts"), "not in the catalog")
  # data-frame form agrees, deprecation fields included
  cat_s <- jsonlite::fromJSON(test_path("fixtures", "catalog_canonical.json"))
  for (tb in c("obs", "obs_bio", "obs_env", "cruise"))
    expect_equal(cc_release_sources(cat_s, tb), cc_release_sources(cat_, tb))
  # a legacy catalog has no deprecation fields: FALSE / none / NA
  l <- cc_release_sources(fx("catalog_legacy.json"), "obs")
  expect_false(l$deprecated); expect_equal(l$replaced_by, character(0)); expect_true(is.na(l$removed_in))
})

test_that("a legacy catalog (no objects[]) resolves to per-release paths", {
  cat_ <- fx("catalog_legacy.json")
  s <- cc_release_sources(cat_, "cruise")
  expect_equal(s$urls, "https://storage.googleapis.com/calcofi-db/ducklake/releases/v2026.08.14/parquet/cruise.parquet")
  expect_false(s$canonical); expect_true(is.na(s$hashes))
  s <- cc_release_sources(cat_, "obs")
  expect_equal(s$urls, "s3://calcofi-db/ducklake/releases/v2026.08.14/parquet/obs/**/*.parquet")
  expect_true(s$hive)
  expect_equal(s$single_file, "https://storage.googleapis.com/calcofi-db/ducklake/releases/v2026.08.14/parquet/obs.parquet")
})

test_that(".cc_match_con() applies the anonymous-S3 settings a legacy partitioned glob needs", {
  skip_if_not_installed("duckdb")
  m <- calcofi4r:::.cc_match_con(); on.exit(DBI::dbDisconnect(m$con, shutdown = TRUE))
  expect_equal(DBI::dbGetQuery(m$con, "SELECT current_setting('s3_endpoint') e")$e, "storage.googleapis.com")
})

test_that("DuckDB recovers the partition column from a canonical-style file list", {
  skip_if_not_installed("duckdb")
  root <- withr::local_tempdir()
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  for (y in c(2019, 2020)) {
    d <- file.path(root, "obs", paste0("year=", y), paste0("hash", y)); dir.create(d, recursive = TRUE)
    DBI::dbExecute(con, sprintf("COPY (SELECT %d + i AS id, 1.5 AS v FROM range(2) t(i)) TO '%s' (FORMAT parquet)",
                                y, file.path(d, "data_0.parquet")))
  }
  files <- list.files(file.path(root, "obs"), recursive = TRUE, full.names = TRUE)
  src <- list(urls = files, hive = TRUE)
  got <- DBI::dbGetQuery(con, paste("SELECT year, count(*) AS n FROM", cc_read_parquet_sql(src),
                                    "GROUP BY year ORDER BY year"))
  # the {hash} directory between key=value and the file is not a hive segment and is ignored
  expect_equal(got$year, c(2019L, 2020L)); expect_equal(got$n, c(2, 2))
})

test_that("a retired version errors naming its replacement", {
  skip_if_offline()
  # nothing is retired yet in the live register; exercise the message with a stubbed fetch
  local_mocked_bindings(
    fromJSON = function(...) list(versions = list(list(
      version = "v2026.05.15", retired = list(retired_utc = "2026-09-01T00:00:00Z", to = "v2026.06.26")))),
    .package = "jsonlite")
  expect_error(calcofi4r:::.cc_stop_if_retired("v2026.05.15"), "retired on 2026-09-01.*v2026.06.26")
  expect_true(calcofi4r:::.cc_stop_if_retired("v2026.06.26"))
})
