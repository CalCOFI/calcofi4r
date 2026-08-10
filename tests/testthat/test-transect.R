# Transect / climatology / anomaly rules.
#
# The DB-backed functions run against a synthetic in-memory DuckDB rather than a
# release: the rules under test (station ordering, the two distance rulers, depth
# binning, the min_n floor) are rules about SHAPE, and a fixture small enough to
# assert exact numbers on catches a regression that "it ran on the release and
# looked plausible" never would.

fixture_con <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  # line 93.3, stations 30..90 at 0.5 deg lon apart on one latitude, so
  # great-circle spacing between neighbours is constant and hand-checkable
  grid <- data.frame(
    grid_key = sprintf("st%03d-ln93.3", seq(30, 90, by = 10)),
    line     = 93.3,
    station  = seq(30, 90, by = 10))
  lon <- -117.5 - (seq_len(nrow(grid)) - 1) * 0.5

  # cruise A occupies every station; cruise B stops at station 50 (the real
  # pattern: line 93.3 has not been sampled past station 90 since 2025-01)
  smp <- rbind(
    data.frame(sample_key = paste0("A", grid$station, "d"), cruise_key = "2401XX",
               grid_key = grid$grid_key, longitude = lon, latitude = 32.9,
               datetime = as.POSIXct("2024-01-15", tz = "UTC"),
               data_stage = "final"),
    data.frame(sample_key = paste0("B", grid$station[1:3], "d"), cruise_key = "2407XX",
               grid_key = grid$grid_key[1:3], longitude = lon[1:3], latitude = 32.9,
               datetime = as.POSIXct("2024-07-15", tz = "UTC"),
               data_stage = "preliminary_without_bottle"))
  smp$dataset_key <- "calcofi_ctd-cast"
  smp$sample_type <- "cast"

  # temperature declining with depth; station 30 of cruise B is 3 degC warm
  obs <- do.call(rbind, lapply(seq_len(nrow(smp)), function(i) {
    d <- c(1.2, 4.8, 9.9)  # -> bins 0, 5, 10 at depth_bin = 5
    data.frame(
      cruise_key        = smp$cruise_key[i],
      grid_key          = smp$grid_key[i],
      depth_min_m       = d,
      measurement_type  = "temperature_ave",
      measurement_value = 18 - d * 0.1 +
        (smp$cruise_key[i] == "2407XX" && smp$grid_key[i] == grid$grid_key[1]) * 3,
      datetime          = smp$datetime[i],
      dataset_key       = "calcofi_ctd-cast")
  }))

  DBI::dbWriteTable(con, "grid", grid)
  DBI::dbWriteTable(con, "sample", smp)
  DBI::dbWriteTable(con, "obs", obs)
  con
}

test_that(".cumdist is great-circle, starts at zero, monotone", {
  # 1 degree of latitude is ~111.19 km on the sphere calcofi4r uses
  expect_equal(.cumdist(c(0, 0), c(0, 1)), c(0, 111.195), tolerance = 1e-3)
  expect_equal(.cumdist(1, 1), 0)                        # single point
  expect_equal(.cumdist(numeric(0), numeric(0)), numeric(0))
  d <- .cumdist(c(-117, -118, -119), c(33, 33, 33))
  expect_true(all(diff(d) > 0))
  expect_identical(d[1], 0)
})

test_that("stations run nearshore to offshore, never in track order", {
  con <- fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  s <- cc_transect_stations(con, 93.3, cruise_key = "2401XX")
  expect_identical(s$sta, seq(30, 90, by = 10))
  expect_true(all(diff(s$dist_km) > 0))
  expect_identical(s$dist_km[1], 0)
  expect_true(all(diff(s$lon) < 0))          # offshore = westward here
})

test_that("x='occupied' fills the axis; x='line' shares one ruler", {
  con <- fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # cruise B stopped at station 50, so under BOTH rulers it spans stations 30-50
  occ <- cc_transect_stations(con, 93.3, "2407XX", x = "occupied")
  lin <- cc_transect_stations(con, 93.3, "2407XX", x = "line")
  expect_identical(occ$sta, lin$sta)
  expect_equal(max(occ$dist_km), max(lin$dist_km), tolerance = 1e-6)

  # the difference is the ORIGIN: "line" measures from station 30 of the full
  # line, which B also occupies, so both start at 0 here...
  expect_identical(occ$dist_km[1], 0)
  expect_identical(lin$dist_km[1], 0)

  # ...and the full-line ruler is identical for a cruise that occupied all of it
  full_occ <- cc_transect_stations(con, 93.3, "2401XX", x = "occupied")
  full_lin <- cc_transect_stations(con, 93.3, "2401XX", x = "line")
  expect_equal(full_occ$dist_km, full_lin$dist_km, tolerance = 1e-6)

  # the feature that matters: B's span is SHORTER than A's on the shared ruler,
  # which is exactly what makes the two comparable rather than both full-width
  expect_lt(max(lin$dist_km), max(full_lin$dist_km))
})

test_that("a station missing from a cruise does not shift its neighbours' x", {
  # regression: computing "line" distance per cruise would renumber the ruler
  con <- fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  a <- cc_transect_stations(con, 93.3, "2401XX", x = "line")
  b <- cc_transect_stations(con, 93.3, "2407XX", x = "line")
  shared <- intersect(a$sta, b$sta)
  expect_equal(a$dist_km[match(shared, a$sta)],
               b$dist_km[match(shared, b$sta)], tolerance = 1e-9)
})

test_that("section bins depth and respects depth_max", {
  con <- fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  s <- cc_transect_section(con, 93.3, "2401XX", depth_bin = 5, depth_max = 500)
  expect_setequal(unique(s$depth_m), c(0, 5, 10))   # 1.2 -> 0, 4.8 -> 5, 9.9 -> 10
  expect_equal(nrow(cc_transect_section(con, 93.3, "2401XX", depth_max = 4)),
               sum(s$depth_m <= 4))
  expect_true(all(c("cruise_key", "sta", "dist_km", "depth_m", "variable",
                    "value") %in% names(s)))
})

test_that("climatology honours the year window and the min_n floor", {
  con <- fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  # every fixture cell has exactly one observation
  expect_equal(nrow(cc_climatology(con, years = c(2024, 2024), min_n = 3)), 0)
  cl <- cc_climatology(con, years = c(2024, 2024), min_n = 1)
  expect_true(all(cl$clim_n == 1))
  expect_setequal(unique(cl$month), c(1L, 7L))
  expect_identical(attr(cl, "baseline"), c(2024, 2024))
  expect_equal(nrow(cc_climatology(con, years = c(1990, 1995), min_n = 1)), 0)
  expect_error(cc_climatology(con, years = c(2013, 1993)))
})

test_that("an unsampled baseline yields NA, never a zero anomaly", {
  con <- fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  # baseline from January only, then difference the JULY cruise against it:
  # month never matches, so every anomaly must be NA
  cl  <- cc_climatology(con, years = c(2024, 2024), min_n = 1)
  cl  <- cl[cl$month == 1L, ]
  sta <- cc_transect_stations(con, 93.3, "2407XX")
  sec <- cc_transect_section(con, 93.3, "2407XX")
  an  <- cc_anomaly(sec, cl, sta)
  expect_true(all(is.na(an$anomaly)))
  expect_false(any(an$anomaly %in% 0, na.rm = TRUE))
  expect_identical(attr(an, "baseline"), c(2024, 2024))
})

test_that("anomaly differences against the matching month and station", {
  con <- fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  cl  <- cc_climatology(con, years = c(2024, 2024), min_n = 1)
  sta <- cc_transect_stations(con, 93.3, "2407XX")
  sec <- cc_transect_section(con, 93.3, "2407XX")
  an  <- cc_anomaly(sec, cl, sta)
  # July climatology IS the July cruise here, so anomaly is identically zero
  expect_true(all(abs(an$anomaly) < 1e-9))
  # a single-observation baseline has sd 0 -> anomaly_sd must be NA, not Inf/NaN
  expect_true(all(is.na(an$anomaly_sd)))
})

test_that("matrix is z[[depth]][[station]] with station-ordered columns", {
  con <- fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  sec <- cc_transect_section(con, 93.3, "2401XX")
  m   <- cc_transect_matrix(sec)
  expect_identical(m$sta, seq(30, 90, by = 10))
  expect_identical(m$y, c(0, 5, 10))
  expect_length(m$z, length(m$y))
  expect_length(m$z[[1]], length(m$sta))
  expect_length(m$x, length(m$sta))
  # surface warmer than 10 m, in every column
  expect_true(all(m$z[[1]] > m$z[[3]]))

  # forcing depths keeps matrices aligned across cruises, padding with NA
  m2 <- cc_transect_matrix(sec, depths = c(0, 5, 10, 15))
  expect_length(m2$z, 4)
  expect_true(all(is.na(m2$z[[4]])))

  expect_error(cc_transect_matrix(sec, value = "nope"))
})

test_that("an empty selection returns zero rows rather than erroring", {
  con <- fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  expect_equal(nrow(cc_transect_stations(con, 93.3, "9999XX")), 0)
  expect_equal(nrow(cc_transect_section(con, 93.3, "9999XX")), 0)
  expect_error(cc_transect_stations(con, 66.7), "no grid stations")
})

# --- bathymetry ---------------------------------------------------------------
# A synthetic grid rather than the real GEBCO crop: the rule under test is that
# a feature BETWEEN two positions is found, and a fixture whose seafloor is a
# formula lets that be asserted as a number. Deep everywhere, with one narrow
# ridge placed off any round fraction of the test track (so no sampling interval
# lands on its crest by luck) and a land block in the east.

RIDGE_LON <- -118.43   # crest
RIDGE_SD  <-    0.03   # ~2.8 km, so the ridge is ~10 km wide

bathy_fixture <- function() {
  r <- terra::rast(
    xmin = -120, xmax = -117, ymin = 32, ymax = 34,
    resolution = 0.01, crs = "EPSG:4326")
  lon <- terra::xyFromCell(r, seq_len(terra::ncell(r)))[, 1]
  d <- 1000 - 800 * exp(-((lon - RIDGE_LON)^2) / (2 * RIDGE_SD^2))
  d[lon > -117.2] <- 0                      # land
  terra::values(r) <- d
  names(r) <- "depth_m"
  r
}

test_that(".gc_interp walks the great circle, and survives coincident points", {
  m <- .gc_interp(0, 0, 90, 0, c(0, 0.5, 1))          # along the equator
  expect_equal(m[, 1], c(0, 45, 90), tolerance = 1e-8)
  expect_equal(m[, 2], c(0, 0, 0),   tolerance = 1e-8)

  # a great circle between two points at one latitude bows poleward — a lon/lat
  # blend would put the midpoint at lat 33 exactly, which is the shortcut this
  # replaces
  mid <- .gc_interp(-125, 33, -117, 33, 0.5)
  expect_gt(mid[, 2], 33)
  expect_equal(mid[, 1], -121, tolerance = 1e-6)

  same <- .gc_interp(-119, 33, -119, 33, c(0, 0.5, 1))
  expect_true(all(same[, 1] == -119))       # no division by sin(0)
  expect_true(all(same[, 2] == 33))
})

test_that("cc_bathy_depth is NA off-grid and never negative on land", {
  b <- bathy_fixture()
  expect_equal(cc_bathy_depth(-119, 33, b), 1000, tolerance = 1e-6)
  expect_equal(cc_bathy_depth(RIDGE_LON, 33, b), 200, tolerance = 1)
  expect_identical(cc_bathy_depth(-117.1, 33, b), 0)      # land clamp
  expect_true(is.na(cc_bathy_depth(-130, 33, b)))         # outside the extent
  expect_identical(cc_bathy_depth(numeric(0), numeric(0), b), numeric(0))
  expect_error(cc_bathy_depth(c(-119, -118), 33, b))
})

test_that("a ridge BETWEEN two stations is found, and station-only misses it", {
  # the regression this whole function exists for: line 86.7 drew a bank as a
  # 74 km triangle because nothing was sampled between the stations
  b <- bathy_fixture()
  lon <- c(-119, -118); lat <- c(33, 33)

  expect_equal(cc_bathy_depth(lon, lat, b), c(1000, 1000), tolerance = 1)

  p <- cc_transect_bathy(lon, lat, interval_m = 500, bathy = b)
  expect_lt(min(p$depth_m), 250)                # the crest is ~200 m
  expect_gt(nrow(p), 150)                       # ~93 km leg at 500 m

  # and a coarse interval straightens the flanks: the same ridge reads far
  # deeper, which is exactly the 2 km-vs-500 m failure on line 93.3
  coarse <- cc_transect_bathy(lon, lat, interval_m = 10000, bathy = b)
  expect_gt(min(coarse$depth_m), min(p$depth_m) + 100)
  expect_lt(nrow(coarse), nrow(p))
})

test_that("samples sit interval_m apart and land exactly on every station", {
  b <- bathy_fixture()
  lon <- c(-119, -118.5, -117.5); lat <- c(33, 33, 33)
  p <- cc_transect_bathy(lon, lat, interval_m = 500, bathy = b)

  expect_true(all(diff(p$dist_km) > 0))                      # ordered, unique
  expect_lte(max(diff(p$dist_km)), 0.5 + 1e-6)               # never coarser
  expect_identical(p$dist_km[1], 0)

  # the leg must END on the next station, or the fill leaks past the last sounding
  knot_km <- .cumdist(lon, lat)
  for (k in knot_km)
    expect_true(any(abs(p$dist_km - k) < 1e-6))
  expect_equal(max(p$dist_km), max(knot_km), tolerance = 1e-9)
})

test_that("dist_km puts the profile on the caller's ruler, anchored at stations", {
  # the section's x-axis (station-to-station hops) is a different ruler from
  # distance along the track; the profile has to be stretched onto it
  b <- bathy_fixture()
  lon <- c(-119, -118.5, -117.5); lat <- c(33, 33, 33)
  ruler <- c(0, 10, 200)                # deliberately unlike the true spacing
  p <- cc_transect_bathy(lon, lat, dist_km = ruler, interval_m = 500, bathy = b)

  expect_identical(range(p$dist_km), c(0, 200))
  for (k in ruler)
    expect_true(any(abs(p$dist_km - k) < 1e-9))
  expect_true(all(diff(p$dist_km) > 0))

  # anchored: the depth reported AT a station equals the station's own sounding
  at_knot <- p$depth_m[match(TRUE, abs(p$dist_km - 10) < 1e-9)]
  expect_equal(at_knot, cc_bathy_depth(lon[2], lat[2], b), tolerance = 1e-6)

  expect_error(cc_transect_bathy(lon, lat, dist_km = c(0, 1), bathy = b))
})

test_that("crossing land is flagged, not smoothed away or dropped", {
  b <- bathy_fixture()
  # a leg that runs from deep water into the land block in the east
  p <- cc_transect_bathy(c(-119, -117.05), c(33, 33), interval_m = 500, bathy = b)

  expect_true(any(p$on_land))
  expect_true(all(p$depth_m[p$on_land] == 0))
  expect_false(any(p$depth_m < 0, na.rm = TRUE))
  expect_false(any(p$on_land[p$depth_m > 0]))
  # land samples are RETURNED — a consumer needs them to break its polygon
  expect_identical(nrow(p), nrow(dplyr::filter(p, !is.na(depth_m))))
})

test_that("a profile off the grid is NA, and one point is zero rows", {
  b <- bathy_fixture()
  p <- cc_transect_bathy(c(-130, -129), c(33, 33), interval_m = 500, bathy = b)
  expect_true(all(is.na(p$depth_m)))
  expect_true(all(!p$on_land))          # unknown is not land

  expect_identical(nrow(cc_transect_bathy(-119, 33, bathy = b)), 0L)
  expect_named(cc_transect_bathy(-119, 33, bathy = b),
               c("dist_km", "lon", "lat", "depth_m", "on_land"))
  expect_error(cc_transect_bathy(c(-119, -118), c(33, 33), interval_m = 0,
                                 bathy = b))
})

test_that("cc_bathy prefers an explicit local raster over the download", {
  b   <- bathy_fixture()
  tif <- withr::local_tempfile(fileext = ".tif")
  terra::writeRaster(b, tif, overwrite = TRUE)

  expect_equal(terra::ncell(cc_bathy(tif)), terra::ncell(b))
  withr::with_options(list(calcofi4r.bathy = tif), {
    expect_equal(terra::ncell(cc_bathy()), terra::ncell(b))
  })
  expect_error(cc_bathy("/nope/missing.tif"), "not found")
})
