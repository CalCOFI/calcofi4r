# cc_bin_anomaly() / cc_layer_mean(): station x 10 m bin anomalies for any dataset, each against its
# own climatology (Rasmus Swalethorp, email "Missing data" 2026-11-13: bottle back to 1949 + CTD)

bin_con <- function() {
  con <- DBI::dbConnect(duckdb::duckdb())
  obs <- data.frame(
    dataset_key      = c(rep("calcofi_bottle", 6), rep("calcofi_ctd-cast", 3)),
    measurement_type = c(rep("temperature", 6), rep("temperature_ave", 3)),
    cruise_key       = c(rep("1958-11-31XX", 6), rep("2026-11-33P4", 3)),
    site_key         = "093.3 030.0", grid_key = "g",
    depth_bin        = c(0L, 0L, 10L, 20L, 30L, 50L, 0L, 10L, 20L),
    cruise_month     = 11L, year = c(rep(1958L, 6), rep(2026L, 3)),
    datetime         = as.POSIXct(c(rep("1958-11-03", 6), rep("2026-11-01", 3)), tz = "UTC"),
    value            = c(17, 18, 16, 15, 14, 12, 20, 19, 18),
    qual_ok          = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE))
  DBI::dbWriteTable(con, "obs_env", obs)
  # each dataset its own baseline; the bottle 50 m bin has none; the CTD 0 m bin a monthly NULL
  DBI::dbWriteTable(con, "climatology", data.frame(
    dataset_key = c(rep("calcofi_bottle", 4), rep("calcofi_ctd-cast", 3)),
    measurement_type = c(rep("temperature", 4), rep("temperature_ave", 3)),
    site_key = "093.3 030.0", grid_key = "g", month = 11L,
    depth_bin = c(0L, 10L, 20L, 30L, 0L, 10L, 20L),
    clim_mean = c(16, 15, 14, 13, NA, 15, 14), clim_sd = c(1, 1, 1, 0, NA, 2, 2),
    clim_n = 10L, n_cruises = c(rep(8L, 4), NA, 6L, 6L),
    clim_yr_min = 1993L, clim_yr_max = 2013L, quarter = 4L,
    season_mean = c(15.5, 14.5, 13.5, 12.5, 16, 14, 13), season_sd = 1,
    season_n = 30L, season_n_cruises = 20L))
  DBI::dbWriteTable(con, "cruise", data.frame(
    cruise_key = c("1958-11-31XX", "2026-11-33P4"), date_median = as.Date(c("1958-11-05", "2026-11-02"))))
  con
}

test_that("each dataset is differenced against its own baseline, bin by bin", {
  con <- bin_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  b <- cc_bin_anomaly(con)
  btl <- b[b$dataset_key == "calcofi_bottle", ]
  expect_equal(btl$depth_bin, c(0L, 10L, 20L, 30L, 50L))
  expect_equal(btl$value[1], 17.5)                          # two bottles in the 0 m bin: their mean
  expect_equal(btl$n_obs[1], 2L)
  expect_equal(btl$anomaly, c(1.5, 1, 1, 1, NA))            # no 50 m cell: NA, never 0
  expect_equal(btl$anomaly_sd[4], NA_real_)                 # a zero sd gives no standardized anomaly
  ctd <- b[b$dataset_key == "calcofi_ctd-cast", ]
  expect_equal(ctd$depth_bin, c(0L, 10L))                   # the 20 m value is not quality-ok
  expect_equal(ctd$anomaly, c(NA, 4))                       # a NULL monthly mean: no monthly baseline
  expect_equal(unique(b$year), c(1958L, 2026L))
  expect_identical(attr(b, "baseline"), c(1993L, 2013L))
  expect_identical(attr(b, "period"), "month")
})

test_that("season = TRUE reads the seasonal columns, which also cover a thin month", {
  con <- bin_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  s <- cc_bin_anomaly(con, season = TRUE)
  expect_equal(s$anomaly[s$dataset_key == "calcofi_ctd-cast"], c(4, 5))
  expect_equal(unique(s$n_cruises[!is.na(s$n_cruises)]), 20L)
  expect_identical(attr(s, "period"), "season")
})

test_that("filters and the variable pairing select what they say", {
  con <- bin_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  expect_equal(unique(cc_bin_anomaly(con, c(calcofi_bottle = "temperature"))$dataset_key), "calcofi_bottle")
  expect_equal(nrow(cc_bin_anomaly(con, site_key = "nope")), 0)
  expect_equal(max(cc_bin_anomaly(con, depth_max = 20)$depth_bin), 20L)
  expect_equal(unique(cc_bin_anomaly(con, cruise_key = "2026-11-33P4")$cruise_key), "2026-11-33P4")
  expect_error(cc_bin_anomaly(con, variables = "temperature"))   # unnamed: which dataset?
})

test_that("cc_layer_mean() interpolates each cast in depth and needs every bin of the layer", {
  x <- data.frame(dataset_key = "calcofi_bottle", cruise_key = "c1", site_key = "s", variable = "t",
                  depth_bin = c(0, 10, 20, 30, 50), anomaly = c(1, 1, 2, 2, 4))
  m <- cc_layer_mean(x, top = 0, bottom = 40)
  expect_equal(m$anomaly, mean(c(1, 1, 2, 2, 3)))           # 40 m from 30 and 50 m
  expect_equal(m$n_bins_sampled, 4L)
  # a gap wider than max_gap, or a layer deeper than the cast, gives no row
  expect_equal(nrow(cc_layer_mean(x, 0, 40, max_gap = 10)), 0)
  expect_equal(nrow(cc_layer_mean(x, 0, 60)), 0)
  # several casts and another column
  y <- rbind(x, transform(x, cruise_key = "c2", anomaly = anomaly + 1))
  expect_equal(cc_layer_mean(y, 0, 20)$anomaly, c(4 / 3, 7 / 3))
  expect_equal(cc_layer_mean(transform(x, value = 10), 0, 20, value = "value")$value, 10)
})
