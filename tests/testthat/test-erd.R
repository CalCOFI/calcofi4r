test_that("cc_erd() marks every column of a composite primary key as PK", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "CREATE TABLE sample_spatial (root_sample_key VARCHAR, spatial_key VARCHAR, n INTEGER)")
  DBI::dbExecute(con, "CREATE TABLE spatial (spatial_key VARCHAR, name VARCHAR)")
  rels <- list(
    primary_keys = list(sample_spatial = c("root_sample_key", "spatial_key"), spatial = "spatial_key"),
    foreign_keys = list(list(table = "sample_spatial", column = "spatial_key",
                             ref_table = "spatial", ref_column = "spatial_key")))
  erd <- cc_erd(con, tables = c("sample_spatial", "spatial"), rels = rels)
  txt <- unclass(erd)
  expect_match(txt, "root_sample_key PK")
  expect_match(txt, "spatial_key PK,FK")
  expect_match(txt, "int n\\b|n$", perl = TRUE) # the plain column carries no tag
})
