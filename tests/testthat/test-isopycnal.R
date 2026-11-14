# cc_isopycnal() / cc_isopycnal_anomaly(): conditions on density surfaces from the 1 m casts
# (Rasmus Swalethorp, email "Missing data" 2026-11-12). Each rule gets its own small fixture.

# one cast's rows in obs_ctd_full: sigma-theta on both sensors (equal unless given) plus properties
iso_rows <- function(sample_key, z, sig, sig2 = sig, q1 = NA, q2 = NA, temp = 20 - z, q_temp = NA,
                     cruise_key = "2026-11-33P4") {
  n <- length(z)
  rbind(
    data.frame(sample_key, cruise_key, dataset_key = "calcofi_ctd-cast", depth_min_m = z,
               measurement_type = "sigma_theta_1", measurement_value = sig,
               measurement_qual = rep_len(as.character(q1), n)),
    data.frame(sample_key, cruise_key, dataset_key = "calcofi_ctd-cast", depth_min_m = z,
               measurement_type = "sigma_theta_2", measurement_value = sig2,
               measurement_qual = rep_len(as.character(q2), n)),
    data.frame(sample_key, cruise_key, dataset_key = "calcofi_ctd-cast", depth_min_m = z,
               measurement_type = "temperature_ave", measurement_value = temp,
               measurement_qual = rep_len(as.character(q_temp), n)))
}

iso_con <- function(obs, sample, cruise = NULL) {
  con <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbWriteTable(con, "obs_ctd_full", obs)
  DBI::dbWriteTable(con, "sample", sample)
  if (!is.null(cruise)) DBI::dbWriteTable(con, "cruise", cruise)
  con
}

iso_sample <- function(sample_key, cruise_key = "2026-11-33P4", site_key = "093.3 030.0",
                       datetime = "2026-11-01") data.frame(
  sample_key, sample_type = "cast", dataset_key = "calcofi_ctd-cast", cruise_key, site_key,
  grid_key = "g", longitude = -118, latitude = 33, datetime = as.POSIXct(datetime, tz = "UTC"),
  data_stage = "final")

val <- function(iso, sk, sg, v = "depth_m") {
  r <- iso[iso$sample_key == sk & abs(iso$sigma_theta - sg) < 1e-9 & iso$variable == v, ]
  if (!nrow(r)) return(NA_real_)
  r$value
}
st <- function(iso, sk, sg) unique(iso$status[iso$sample_key == sk & abs(iso$sigma_theta - sg) < 1e-9])

K <- "calcofi_ctd-cast:cast:2611_001d"
Z <- 0:5
S <- c(24.0, 24.2, 24.6, 25.0, 25.4, 25.8)

test_that("a surface's depth is the first bin reaching it, interpolated with the bin above", {
  con <- iso_con(iso_rows(K, Z, S), iso_sample(K)); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  iso <- cc_isopycnal(con, sigma = c(24.8, 25.4))
  expect_equal(val(iso, K, 24.8), 2.5)                  # between 24.6 at 2 m and 25.0 at 3 m
  expect_equal(val(iso, K, 25.4), 4)                    # exactly a bin
  expect_equal(st(iso, K, 24.8), "ok")
  # a property on the surface: its own profile at that depth (temperature 20 - z)
  expect_equal(val(iso, K, 24.8, "temperature_ave"), 17.5)
  expect_equal(val(iso, K, 25.4, "temperature_ave"), 16)
  expect_setequal(names(iso), c("sample_key", "cruise_key", "site_key", "grid_key", "longitude",
                                "latitude", "datetime", "data_stage", "year", "month", "sigma_theta",
                                "status", "variable", "value"))
})

test_that("an outcropping, unreached or empty surface has a status and no depth", {
  k1 <- "calcofi_ctd-cast:cast:2611_002d"; k2 <- "calcofi_ctd-cast:cast:2611_003d"
  obs <- rbind(iso_rows(K, Z, S), iso_rows(k1, 30:35, S),   # k1 starts at 30 m, below surface_max_m
               iso_rows(k2, 0, 24))                          # k2: a single bin
  con <- iso_con(obs, iso_sample(c(K, k1, k2), site_key = c("a", "b", "c")))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  iso <- cc_isopycnal(con, sigma = c(23.9, 26.5))
  expect_equal(st(iso, K, 23.9), "at_surface")          # the first bin is already denser: it outcrops
  expect_true(is.na(val(iso, K, 23.9)))
  expect_equal(st(iso, k1, 23.9), "no_bracket")         # ... but the cast starts too deep to say so
  expect_equal(st(iso, K, 26.5), "not_reached")         # denser than anything in the cast
  expect_equal(st(iso, k2, 26.5), "no_data")
  # a surface without a depth carries no property rows, but its depth row is always there
  expect_equal(nrow(iso[iso$variable == "depth_m", ]), 3 * 2)
  expect_false(any(iso$variable == "temperature_ave" & iso$status != "ok"))
})

test_that("sigma-theta is the sensor pair by the provider's flags", {
  # 3 m: sensor 1 flagged 9 (and wrong): sensor 2 alone; 2 m: a 2 selects sensor 2 (24.4, not 24.6)
  s1 <- S; s1[4] <- 30; s2 <- S; s2[3] <- 24.4
  q1 <- c(NA, NA, "2", "9.0", NA, NA)
  con <- iso_con(iso_rows(K, Z, s1, s2, q1 = q1), iso_sample(K))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  iso <- cc_isopycnal(con, sigma = 24.8)
  expect_equal(val(iso, K, 24.8), 2 + (24.8 - 24.4) / (25.0 - 24.4))
  # unflagged and different: the mean
  con2 <- iso_con(iso_rows(K, Z, S, S + 0.2), iso_sample(K))
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE), add = TRUE)
  expect_equal(val(cc_isopycnal(con2, sigma = 24.8), K, 24.8), 2.25)   # pair = S + 0.1
})

test_that("only the shallowest crossing counts: an inversion below it does not move the surface", {
  sig <- c(24.0, 25.0, 24.5, 25.5)
  con <- iso_con(iso_rows(K, 0:3, sig), iso_sample(K)); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  iso <- cc_isopycnal(con, sigma = c(24.8, 25.2))
  expect_equal(val(iso, K, 24.8), 0.8)
  expect_equal(val(iso, K, 25.2), 2.7)                  # first bin >= 25.2 is 3 m; the bin above is 24.5
})

test_that("a flagged property value is dropped before interpolating; repeated rows are averaged", {
  qt <- c(NA, NA, NA, "8", NA, NA); tt <- 20 - Z; tt[4] <- 99
  obs <- iso_rows(K, Z, S, temp = tt, q_temp = qt)
  # a repeated row at 2 m: sigma-theta 24.6 and 24.8 average to 24.7, temperature 18 and 17 to 17.5
  obs <- rbind(obs, iso_rows(K, 2, 24.8, temp = 17))
  con <- iso_con(obs, iso_sample(K)); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  iso <- cc_isopycnal(con, sigma = 24.8)
  z <- 2 + (24.8 - 24.7) / (25.0 - 24.7)
  expect_equal(val(iso, K, 24.8), z)
  # temperature at z from 2 m (17.5) and 4 m (16): the flagged 99 at 3 m is out
  expect_equal(val(iso, K, 24.8, "temperature_ave"), 17.5 + (16 - 17.5) * (z - 2) / 2)
})

test_that("direction = 'down' keeps one cast per occupation, the up cast only where there is no down", {
  kd <- "calcofi_ctd-cast:cast:2611_001d"; ku <- "calcofi_ctd-cast:cast:2611_001u"
  k2 <- "calcofi_ctd-cast:cast:2611_002u"
  obs <- rbind(iso_rows(kd, Z, S), iso_rows(ku, Z, S + 0.1), iso_rows(k2, Z, S))
  con <- iso_con(obs, iso_sample(c(kd, ku, k2))); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  expect_setequal(unique(cc_isopycnal(con, sigma = 25)$sample_key), c(kd, k2))
  expect_setequal(unique(cc_isopycnal(con, sigma = 25, direction = "both")$sample_key), c(kd, ku, k2))
  expect_equal(nrow(cc_isopycnal(con, sigma = 25, cruise_key = "nope")), 0)
  expect_setequal(unique(cc_isopycnal(con, sigma = 25, site_key = "093.3 030.0")$sample_key), c(kd, k2))
})

test_that("without obs_ctd_full the error says to open the supplemental tables", {
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "sample", iso_sample(K))
  expect_error(cc_isopycnal(con), "supplemental = TRUE")
  expect_error(cc_isopycnal(con, variables = "depth_m"))
})

test_that("the anomaly is the departure from the station's baseline on the same surface", {
  # five November baseline cruises at one station, the surface 24.8 at 2.5 + k/10 m; one at another
  # station; a target cruise in 2026 with the surface at 3.5 m
  yrs <- 2001:2005
  keys <- sprintf("calcofi_ctd-cast:cast:%02d11_001d", yrs %% 100)
  crs  <- sprintf("%d-11-32NM", yrs)
  obs  <- do.call(rbind, lapply(seq_along(yrs), function(k)
    iso_rows(keys[k], Z + (k - 1) / 10, S, temp = 20 - Z + k, cruise_key = crs[k])))
  obs  <- rbind(obs, iso_rows(K, Z + 1, S))
  smp  <- rbind(iso_sample(keys, cruise_key = crs, datetime = sprintf("%d-11-05", yrs)), iso_sample(K))
  cruise <- data.frame(cruise_key = c(crs, "2026-11-33P4"), month_median = 11L,
                       date_median = as.Date(c(sprintf("%d-11-05", yrs), "2026-11-01")))
  con <- iso_con(obs, smp, cruise); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  an <- cc_isopycnal_anomaly(con, sigma = 24.8, cruise_key = "2026-11-33P4", years = c(1993, 2013))
  d  <- an[an$variable == "depth_m", ]
  expect_equal(d$value, 3.5)
  expect_equal(d$clim_mean, mean(2.5 + (0:4) / 10))
  expect_equal(d$anomaly, 3.5 - 2.7)                    # positive: the surface sits deeper
  expect_equal(d$n_cruises, 5L)
  expect_equal(d$clim_n, 5L)
  # temperature on the surface: baseline cast k reads 17.5 + k there, the target 20 - 3.5
  t_ <- an[an$variable == "temperature_ave", ]
  expect_equal(t_$value, 16.5)
  expect_equal(t_$clim_mean, mean(17.5 + (1:5)))
  expect_identical(attr(an, "baseline"), c(1993, 2013))
  expect_identical(attr(an, "period"), "month")
  expect_true(all(c("site_key", "month", "sigma_theta", "variable", "clim_mean") %in%
                    names(attr(an, "climatology"))))

  # a floor of six cruises leaves no baseline: NA, never 0
  an6 <- cc_isopycnal_anomaly(con, sigma = 24.8, cruise_key = "2026-11-33P4", min_cruises = 6)
  expect_true(all(is.na(an6$anomaly)))
  # the window decides which cruises count
  an2 <- cc_isopycnal_anomaly(con, sigma = 24.8, cruise_key = "2026-11-33P4", years = c(2003, 2005),
                              min_cruises = 3)
  expect_equal(an2$clim_mean[an2$variable == "depth_m"], mean(2.5 + (2:4) / 10))
})

test_that("season = TRUE pools the season's months (SON) into one baseline", {
  yrs <- 2001:2005; mon <- c(9L, 10L, 11L, 10L, 9L)
  keys <- sprintf("calcofi_ctd-cast:cast:%02d%02d_001d", yrs %% 100, mon)
  crs  <- sprintf("%d-%02d-32NM", yrs, mon)
  obs  <- do.call(rbind, lapply(seq_along(yrs), function(k) iso_rows(keys[k], Z, S, cruise_key = crs[k])))
  obs  <- rbind(obs, iso_rows(K, Z + 1, S))
  smp  <- rbind(iso_sample(keys, cruise_key = crs, datetime = sprintf("%d-%02d-05", yrs, mon)), iso_sample(K))
  cruise <- data.frame(cruise_key = c(crs, "2026-11-33P4"), month_median = c(mon, 11L),
                       date_median = as.Date(c(sprintf("%d-%02d-05", yrs, mon), "2026-11-01")))
  con <- iso_con(obs, smp, cruise); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  m <- cc_isopycnal_anomaly(con, sigma = 24.8, cruise_key = "2026-11-33P4", min_cruises = 1)
  expect_equal(m$n_cruises[m$variable == "depth_m"], 1L)         # November alone
  s <- cc_isopycnal_anomaly(con, sigma = 24.8, cruise_key = "2026-11-33P4", season = TRUE)
  expect_equal(s$n_cruises[s$variable == "depth_m"], 5L)
  expect_equal(s$anomaly[s$variable == "depth_m"], 1)
  expect_identical(attr(s, "period"), "season")
  expect_setequal(unique(attr(s, "climatology")$month), 9:11)
})

test_that("the depth rule is calcofi4db::profile_threshold_depth(direction = 'above')", {
  skip_if_not_installed("calcofi4db")
  set.seed(20261113)
  casts <- sprintf("calcofi_ctd-cast:cast:2611_%03dd", 1:40)
  profs <- lapply(casts, function(k) {
    z <- sort(sample(0:300, sample(3:60, 1)))
    s <- 23.5 + cumsum(stats::rnorm(length(z), 0.06, 0.08))        # mostly stable, with inversions
    list(k = k, z = z, s = s)
  })
  obs <- do.call(rbind, lapply(profs, function(p) iso_rows(p$k, p$z, p$s)))
  con <- iso_con(obs, iso_sample(casts)); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  sg  <- seq(23.6, 26, by = 0.2)
  iso <- cc_isopycnal(con, sigma = sg)
  for (p in profs) for (g in sg) {
    r <- calcofi4db::profile_threshold_depth(p$z, p$s, threshold = g, direction = "above")
    expect_equal(st(iso, p$k, g), r$status, info = paste(p$k, g))
    if (r$status == "ok") expect_equal(val(iso, p$k, g), r$depth_m, tolerance = 1e-9, info = paste(p$k, g))
  }
})

test_that("the default surfaces are the provider's: 24.0-27.0 by 0.2 plus 25.5 and 26.5", {
  # Rasmus Swalethorp, 2026-11-13: 25.5 and 26.5 sit near the California Current and Undercurrent cores
  con <- iso_con(iso_rows(K, Z, S), iso_sample(K)); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  sg <- sort(unique(cc_isopycnal(con)$sigma_theta))
  expect_equal(sg, sort(c(seq(24, 27, by = 0.2), 25.5, 26.5)), tolerance = 1e-9)
  expect_length(sg, 18)
  expect_true(all(c(25.5, 26.5) %in% round(sg, 6)))
})

test_that("spice on a surface is TEOS-10 spiciness0 of the temperature and salinity interpolated there", {
  sal <- data.frame(sample_key = K, cruise_key = "2026-11-33P4", dataset_key = "calcofi_ctd-cast",
                    depth_min_m = Z, measurement_type = "salinity_ave_corr",
                    measurement_value = 33.2 + Z / 10, measurement_qual = NA_character_)
  con <- iso_con(rbind(iso_rows(K, Z, S), sal), iso_sample(K)); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  iso <- cc_isopycnal(con, sigma = c(24.8, 25.4))
  # at 24.8 the surface is 2.5 m deep: temperature 17.5, salinity 33.45; the cast is at -118, 33
  p   <- gsw::gsw_p_from_z(-2.5, 33)
  sa  <- gsw::gsw_SA_from_SP(33.45, p, -118, 33)
  expect_equal(val(iso, K, 24.8, "salinity_ave_corr"), 33.45)
  expect_equal(val(iso, K, 24.8, "spiciness0"), gsw::gsw_spiciness0(sa, gsw::gsw_CT_from_t(sa, 17.5, p)))
  expect_equal(unique(iso$variable), c("depth_m", "temperature_ave", "salinity_ave_corr", "spiciness0"))
  # spice alone: temperature and salinity are read for it but not returned
  only <- cc_isopycnal(con, sigma = 24.8, variables = "spiciness0")
  expect_equal(unique(only$variable), c("depth_m", "spiciness0"))
  expect_equal(val(only, K, 24.8, "spiciness0"), val(iso, K, 24.8, "spiciness0"))
  # a cast without salinity has no spice, and no error
  con2 <- iso_con(iso_rows(K, Z, S), iso_sample(K)); on.exit(DBI::dbDisconnect(con2, shutdown = TRUE), add = TRUE)
  expect_false("spiciness0" %in% cc_isopycnal(con2, sigma = 24.8)$variable)
  # the same recipe as calcofi4db::ctd_spice(), which builds the release's spiciness0
  skip_if_not_installed("calcofi4db")
  expect_equal(val(iso, K, 25.4, "spiciness0"),
               calcofi4db::ctd_spice(16, 33.6, gsw::gsw_p_from_z(-4, 33), -118, 33))
})

test_that("spice gets a baseline and an anomaly like any other variable", {
  mk <- function(k, ck, shift) {
    o <- iso_rows(k, Z, S, temp = 20 - Z + shift, cruise_key = ck)
    rbind(o, data.frame(sample_key = k, cruise_key = ck, dataset_key = "calcofi_ctd-cast", depth_min_m = Z,
                        measurement_type = "salinity_ave_corr", measurement_value = 33.2 + Z / 10 + shift / 10,
                        measurement_qual = NA_character_))
  }
  ks  <- sprintf("calcofi_ctd-cast:cast:%02d11_001d", 0:5)
  cks <- c(sprintf("20%02d-11-33XX", 0:4), "2026-11-33P4")
  obs <- do.call(rbind, Map(mk, ks, cks, c(0, 0, 0, 0, 0, 1)))
  smp <- do.call(rbind, Map(iso_sample, ks, cks, datetime = sprintf("%s-11-01", substr(cks, 1, 4))))
  con <- iso_con(obs, smp); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  an  <- cc_isopycnal_anomaly(con, sigma = 24.8, cruise_key = "2026-11-33P4", years = c(2000, 2004))
  sp  <- an[an$variable == "spiciness0", ]
  expect_equal(nrow(sp), 1)
  expect_equal(sp$n_cruises, 5L)
  expect_gt(sp$anomaly, 0)                              # warmer and saltier on the same surface: spicier
})

test_that("cc_isopycnal_type() names a variable as the release does, and cc_isopycnal_variable() inverts it", {
  expect_identical(cc_isopycnal_type(c("depth_m", "temperature_ave")), c("iso_depth_m", "iso_temperature_ave"))
  expect_identical(cc_isopycnal_variable(cc_isopycnal_type("spiciness0")), "spiciness0")
})

# a release's isopycnal baseline, built here the way calcofi4db::build_climatology(isopycnal_tbl =)
# files it: one cell per station x cruise month x surface x type (and the season's columns on every
# month of a passing season), the mean over casts, rounded to 1e-6, cells with >= 5 cruises
iso_release_climatology <- function(con, years = c(1993, 2013), floor = 5L) {
  iso <- cc_isopycnal(con)
  DBI::dbWriteTable(con, "isopycnal", data.frame(
    sample_key = iso$sample_key, sigma_theta = iso$sigma_theta,
    measurement_type = cc_isopycnal_type(iso$variable), measurement_value = iso$value), overwrite = TRUE)
  iso <- iso[!is.na(iso$value) & iso$year >= years[1] & iso$year <= years[2], ]
  iso$measurement_type <- cc_isopycnal_type(iso$variable)
  iso$quarter <- cc_season(iso$month)
  agg <- function(d, by) dplyr::summarize(dplyr::group_by(d, dplyr::across(dplyr::all_of(by))),
    m = round(mean(value), 6), sd = round(stats::sd(value), 6), n = dplyr::n(),
    nc = dplyr::n_distinct(cruise_key), .groups = "drop") |> dplyr::filter(nc >= floor)
  mon <- agg(iso, c("site_key", "month", "sigma_theta", "measurement_type"))
  sea <- agg(iso, c("site_key", "quarter", "sigma_theta", "measurement_type")) |>
    dplyr::inner_join(data.frame(quarter = cc_season(1:12), month = 1:12), by = "quarter",
                      relationship = "many-to-many")
  cl <- dplyr::full_join(
    dplyr::rename(mon, clim_mean = m, clim_sd = sd, clim_n = n, n_cruises = nc),
    dplyr::rename(sea, season_mean = m, season_sd = sd, season_n = n, season_n_cruises = nc),
    by = c("site_key", "month", "sigma_theta", "measurement_type"))
  cl$dataset_key <- "calcofi_ctd-cast"; cl$depth_bin <- NA_integer_
  cl$clim_yr_min <- years[1]; cl$clim_yr_max <- years[2]
  DBI::dbWriteTable(con, "climatology", as.data.frame(cl), overwrite = TRUE)
  invisible(cl)
}

test_that("the release's isopycnal baseline and the one computed here give the same anomalies", {
  # six baseline cruises at one station across Sep-Nov (a monthly and a seasonal baseline), a target
  # cast in 2026; temperature and salinity, so spice gets a baseline too
  yrs <- 2000:2005; mon <- c(11L, 11L, 11L, 11L, 11L, 10L)
  ks  <- sprintf("calcofi_ctd-cast:cast:%02d%02d_001d", yrs %% 100, mon)
  crs <- sprintf("%d-%02d-32NM", yrs, mon)
  mk <- function(k, ck, shift) {
    o <- iso_rows(k, Z + shift / 10, S, temp = 20 - Z + shift, cruise_key = ck)
    rbind(o, data.frame(sample_key = k, cruise_key = ck, dataset_key = "calcofi_ctd-cast",
                        depth_min_m = Z + shift / 10, measurement_type = "salinity_ave_corr",
                        measurement_value = 33.2 + Z / 10 + shift / 10, measurement_qual = NA_character_))
  }
  obs <- do.call(rbind, Map(mk, c(ks, K), c(crs, "2026-11-33P4"), c(seq_along(yrs), 9)))
  smp <- rbind(iso_sample(ks, cruise_key = crs, datetime = sprintf("%d-%02d-05", yrs, mon)), iso_sample(K))
  cruise <- data.frame(cruise_key = c(crs, "2026-11-33P4"), month_median = c(mon, 11L),
                       date_median = as.Date(c(sprintf("%d-%02d-05", yrs, mon), "2026-11-01")))
  con <- iso_con(obs, smp, cruise); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  sig <- formals(cc_isopycnal)$sigma |> eval()
  vars <- c("temperature_ave", "salinity_ave_corr", "spiciness0")

  for (ssn in c(FALSE, TRUE)) {
    DBI::dbExecute(con, "DROP TABLE IF EXISTS climatology"); DBI::dbExecute(con, "DROP TABLE IF EXISTS isopycnal")
    cmp <- cc_isopycnal_anomaly(con, cruise_key = "2026-11-33P4", variables = vars, season = ssn)
    expect_identical(attr(cmp, "source"), "computed")
    iso_release_climatology(con)
    rel <- cc_isopycnal_anomaly(con, cruise_key = "2026-11-33P4", variables = vars, season = ssn)
    expect_identical(attr(rel, "source"), "release")
    expect_identical(attr(rel, "period"), attr(cmp, "period"))
    expect_equal(nrow(rel), nrow(cmp))
    key <- c("sample_key", "sigma_theta", "variable")
    rel <- dplyr::arrange(rel, sample_key, sigma_theta, variable)
    cmp <- dplyr::arrange(cmp, sample_key, sigma_theta, variable)
    for (k in key) expect_identical(rel[[k]], cmp[[k]])
    expect_gt(sum(!is.na(cmp$anomaly)), 0)
    expect_identical(is.na(rel$anomaly), is.na(cmp$anomaly))
    expect_equal(rel$anomaly, cmp$anomaly, tolerance = 1e-6)
    expect_equal(rel$clim_n, cmp$clim_n)
    expect_equal(rel$n_cruises, cmp$n_cruises)
    # forcing the computed path on a connection that has the release's gives the same again
    frc <- cc_isopycnal_anomaly(con, cruise_key = "2026-11-33P4", variables = vars, season = ssn,
                                baseline = "compute")
    expect_identical(attr(frc, "source"), "computed")
    expect_equal(dplyr::arrange(frc, sample_key, sigma_theta, variable)$anomaly, cmp$anomaly)
  }
  # the monthly baseline: November only (5 cruises); the seasonal one adds October's (6)
  expect_equal(unique(stats::na.omit(
    cc_isopycnal_anomaly(con, cruise_key = "2026-11-33P4", variables = vars)$n_cruises)), 5L)
  expect_equal(unique(stats::na.omit(
    cc_isopycnal_anomaly(con, cruise_key = "2026-11-33P4", variables = vars, season = TRUE)$n_cruises)), 6L)
})

test_that("the release's baseline is used only when it is the one asked for", {
  yrs <- 2000:2004
  ks  <- sprintf("calcofi_ctd-cast:cast:%02d11_001d", yrs %% 100)
  crs <- sprintf("%d-11-32NM", yrs)
  obs <- do.call(rbind, lapply(seq_along(yrs), function(k) iso_rows(ks[k], Z + k / 10, S, cruise_key = crs[k])))
  obs <- rbind(obs, iso_rows(K, Z + 1, S))
  smp <- rbind(iso_sample(ks, cruise_key = crs, datetime = sprintf("%d-11-05", yrs)), iso_sample(K))
  con <- iso_con(obs, smp); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  iso_release_climatology(con)
  v <- "temperature_ave"
  expect_identical(attr(cc_isopycnal_anomaly(con, cruise_key = "2026-11-33P4", variables = v), "source"), "release")
  # another window, both directions, a surface or a variable the release lacks: computed
  expect_identical(attr(cc_isopycnal_anomaly(con, cruise_key = "2026-11-33P4", variables = v,
                                             years = c(2000, 2013)), "source"), "computed")
  expect_identical(attr(cc_isopycnal_anomaly(con, cruise_key = "2026-11-33P4", variables = v,
                                             direction = "both"), "source"), "computed")
  expect_identical(attr(cc_isopycnal_anomaly(con, cruise_key = "2026-11-33P4", variables = v,
                                             sigma = 24.85), "source"), "computed")
  expect_identical(attr(cc_isopycnal_anomaly(con, cruise_key = "2026-11-33P4",
                                             variables = "est_nitrate_sta_corr"), "source"), "computed")
  expect_error(cc_isopycnal_anomaly(con, cruise_key = "2026-11-33P4", variables = v, sigma = 24.85,
                                    baseline = "release"), "no isopycnal baseline")
})
