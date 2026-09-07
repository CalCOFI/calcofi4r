# one algorithm in three runtimes: the fixture is written by the browser's own interpolator
# (explore/scripts/parity/contour_fixture.mjs from src/contour.worker.ts) and shared byte-for-byte with
# calcofi4py/tests/fixtures/contour_fixture.json; every cell of every surface must agree
fx  <- jsonlite::fromJSON(test_path("fixtures", "contour_fixture.json"), simplifyVector = TRUE)
pts <- fx$points
as_mat <- function(v, g) matrix(as.numeric(v), g$ny, g$nx, byrow = TRUE)   # the fixture is row-major, row 0 = north

test_that("cc_interpolate() lays the grid out exactly as the browser does", {
  s <- cc_interpolate(pts, "idw", cell_deg = fx$params$cellDeg, mask_km = fx$params$maskKm)
  expect_equal(s$grid$nx, fx$grid$nx); expect_equal(s$grid$ny, fx$grid$ny)
  expect_equal(s$grid$lon0, fx$grid$lon0, tolerance = 1e-9); expect_equal(s$grid$lon1, fx$grid$lon1, tolerance = 1e-9)
  expect_equal(s$grid$lat_s, fx$grid$latS, tolerance = 1e-9); expect_equal(s$grid$lat_n, fx$grid$latN, tolerance = 1e-9)
  expect_equal(s$fit$n_cells, fx$methods$idw$fit$nCells)
  # the mask: the same cells blank
  expect_identical(is.na(s$values), is.na(as_mat(fx$methods$idw$values, fx$grid)))
})

for (key in names(fx$methods)) test_that(sprintf("cc_interpolate() case '%s' reproduces the browser's surface cell for cell", key), {
  f <- fx$methods[[key]]; m <- f$method
  s <- cc_interpolate(pts, m, cell_deg = fx$params$cellDeg, mask_km = fx$params$maskKm, se = TRUE, nmax = f$nmax)
  expect_equal(s$fit$nmax, f$nmax)
  expect_equal(s$fit$n, f$fit$n); expect_equal(s$fit$n_cells, f$fit$nCells)
  expect_equal(s$fit$loo, f$fit$loo, tolerance = 1e-5)
  expect_equal(s$values, as_mat(f$values, fx$grid), tolerance = 1e-5)        # the fixture is rounded to 6 dp
  if (m == "idw") expect_null(s$se) else expect_equal(s$se, as_mat(f$se, fx$grid), tolerance = 1e-5)
  if (m == "ok") { expect_equal(s$fit$vg$nugget, f$fit$vg$nugget, tolerance = 1e-5); expect_equal(s$fit$vg$psill, f$fit$vg$psill, tolerance = 1e-5); expect_equal(s$fit$vg$range, f$fit$vg$range, tolerance = 1e-5) }
  if (m == "tps") expect_equal(s$fit$edf, f$fit$edf, tolerance = 1e-5)
})

test_that("cc_interpolate() never extrapolates past the mask, and idw has no error surface", {
  s <- cc_interpolate(pts, "ok", cell_deg = 0.25, mask_km = 20, se = FALSE)
  expect_true(anyNA(s$values)); expect_null(s$se)
  expect_lt(s$fit$n_cells, fx$methods$ok$fit$nCells)   # a tighter mask keeps fewer cells
  expect_error(cc_interpolate(pts[1:3, ], "ok"), "n >= 4")
  expect_error(cc_interpolate(pts, "tps", nmax = 8), "every point in one system")
  # the seeded draw is the browser's: the first five of a 30-of-100 partial Fisher-Yates at seed 2
  expect_identical(calcofi4r:::.cc_lcg_sample(100, 30, 2)[1:5], c(24L, 47L, 21L, 75L, 73L))
})

test_that("cc_interpolate_rast() is a Web-Mercator raster with the grid's shape", {
  skip_if_not_installed("terra")
  s <- cc_interpolate(pts, "ok", cell_deg = fx$params$cellDeg, se = TRUE)
  r <- cc_interpolate_rast(s)
  expect_equal(dim(r)[1:2], c(s$grid$ny, s$grid$nx)); expect_equal(names(r), c("value", "se"))
  expect_equal(as.numeric(terra::values(r[["value"]]))[1:s$grid$nx], s$values[1, ])
})
