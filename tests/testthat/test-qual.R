test_that("cc_qual_ok_sql() keeps unflagged rows and drops each dataset's bad codes", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "obs", data.frame(
    id = 1:9,
    dataset_key = c("calcofi_bottle", "calcofi_bottle", "calcofi_bottle", "calcofi_bottle",
                    "calcofi_ctd-cast", "calcofi_ctd-cast", "calcofi_dic", "calcofi_dic",
                    "swfsc_ichthyo"),
    measurement_qual = c(NA, "6.0", "8.0", "9", "2", "9", "2", "3", "8"),
    stringsAsFactors = FALSE))
  kept <- DBI::dbGetQuery(con, paste("SELECT id FROM obs o WHERE", cc_qual_ok_sql("o"), "ORDER BY id"))$id
  # bottle: NULL and 6 kept, 8.0 and 9 dropped; ctd: 2 kept, 9 dropped;
  # dic: 2 kept, 3 dropped; ichthyo carries no vocabulary so its "8" is kept
  expect_equal(kept, c(1L, 2L, 5L, 7L, 9L))
  kept2 <- DBI::dbGetQuery(con, paste("SELECT id FROM obs WHERE", cc_qual_ok_sql(), "ORDER BY id"))$id
  expect_equal(kept2, kept)
  expect_equal(cc_qual_ok(
    c("calcofi_bottle", "calcofi_bottle", "calcofi_dic", "swfsc_ichthyo"),
    c(NA, "8.0", "4", "8")), c(TRUE, FALSE, FALSE, TRUE))
})

test_that("cc_climatology() leaves flagged CTD values out of the mean", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "obs", data.frame(
    dataset_key = "calcofi_ctd-cast", measurement_type = "temperature_ave",
    grid_key = "st50-ln90", depth_min_m = c(10, 10, 10, 10),
    datetime = as.POSIXct(c("2000-04-01", "2001-04-01", "2002-04-01", "2003-04-01"), tz = "UTC"),
    measurement_value = c(10, 10, 10, 100),
    measurement_qual = c(NA, "", "2", "8"),
    stringsAsFactors = FALSE))
  cl <- cc_climatology(con, variables = "temperature_ave", years = c(2000, 2003),
                       depth_max = 20, min_n = 1)
  expect_equal(nrow(cl), 1)
  expect_equal(cl$clim_n, 3)          # the 100 flagged "8" is not averaged in
  expect_equal(cl$clim_mean, 10)
})
