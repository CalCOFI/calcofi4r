# The Explorer's Contours lens, as a function (plan 2026-09-07 § D31–D33, D39). One algorithm in three runtimes:
# the browser's src/contour.worker.ts, this file, and calcofi4py/src/calcofi4py/interpolate.py reproduce the same
# numbers from the same point set — pinned by tests/testthat/fixtures/contour_fixture.json, which the browser's own
# code writes (explore/scripts/parity/contour_fixture.mjs) and both packages' tests read. Deliberately NOT gstat /
# mgcv: those would give a *different* surface, and the point is that a figure made in R matches the map.
#   idw  inverse-distance weighting, power 1.3, radius 200 km, 5 km smoothing (parity with terra::interpIDW as the
#        superseded Contour Explorer used it); no error surface
#   ok   ordinary kriging: an exponential variogram fitted by weighted least squares over a small grid, the augmented
#        system inverted once; the kriging standard deviation is the error; leave-one-out by Dubrule (1983)
#   tps  a thin-plate spline (r^2 log r + a linear trend, mgcv's s(lon, lat) basis) with the ridge picked by GCV over
#        nine values; its standard error from the smoother rows

.cc_merc  <- function(lat) log(tan(pi / 4 + lat * pi / 360))
.cc_imerc <- function(y) (2 * atan(exp(y)) - pi / 2) * 180 / pi

# the empirical semivariogram (15 bins to half the maximum distance) and an exponential model by weighted least squares
.cc_variogram <- function(X, Y, Z) {
  n <- length(X); nb <- 15L
  D <- as.matrix(stats::dist(cbind(X, Y)))
  iu <- which(upper.tri(D), arr.ind = TRUE)
  d  <- D[iu]; g <- 0.5 * (Z[iu[, 1]] - Z[iu[, 2]])^2
  dmax <- max(d) / 2
  keep <- d < dmax
  b  <- floor(d[keep] / dmax * nb)
  bs <- tapply(g[keep], b, sum); bn <- tapply(g[keep], b, length)
  emp <- data.frame(h = (as.numeric(names(bs)) + 0.5) * dmax / nb, g = as.numeric(bs) / as.numeric(bn), n = as.numeric(bn))
  emp <- emp[order(emp$h), ]
  svar <- max(1e-9, emp$g[nrow(emp)])
  best <- list(ss = Inf, nugget = 0, psill = svar, range = dmax / 3)
  for (nug in c(0, 0.05, 0.1, 0.2, 0.3)) for (rg in c(0.1, 0.2, 0.35, 0.5, 0.75, 1, 1.5)) for (sill in c(0.6, 0.8, 1, 1.2, 1.5)) {
    a <- rg * dmax; c0 <- nug * svar; c1 <- max(1e-9, sill * svar - c0)
    m  <- c0 + c1 * (1 - exp(-emp$h / a))
    ss <- sum(emp$n * (emp$g - m)^2 / m^2)
    if (ss < best$ss) best <- list(ss = ss, nugget = c0, psill = c1, range = a)
  }
  best[c("nugget", "psill", "range")]
}

#' Interpolate point values to a surface, exactly as the Explorer's Contours lens does
#'
#' The same algorithm as `calcofi.io/explore` (`lens=contour`) and `calcofi4py.interpolate()`, so a surface
#' drawn in R matches the map cell for cell: a grid of `cell_deg` degrees of longitude whose rows are evenly
#' spaced in Web-Mercator y (what the map stretches a bitmap over), a local equirectangular km frame for the
#' distances, and **no value farther than `mask_km` from every point** — the surface never extrapolates.
#' Typically the points are the Explorer's station table (`grid_key` centres with a summary), i.e. what
#' *Share → Download data* writes as `summary/station.csv`.
#'
#' @param pts a data frame with `lon`, `lat` and `z` columns (`NA` rows are dropped).
#' @param method `"ok"` ordinary kriging (default; the kriging SD is the error surface), `"idw"`
#'   inverse-distance weighting (power 1.3, radius 200 km, 5 km smoothing — no error surface), or `"tps"`
#'   a thin-plate spline with the smoothing chosen by GCV (its standard error is the error surface).
#' @param cell_deg cell size in degrees of longitude (the Explorer uses 0.06).
#' @param mask_km cells farther than this from every point are `NA` (the Explorer uses 60).
#' @param se compute the error surface (`ok`, `tps`; always `NULL` for `idw`). It is the slow part.
#' @return A list: `grid` (`lon0`, `lon1`, `lat_s`, `lat_n`, `nx`, `ny`, `cell_deg`), `values` (an `ny x nx`
#'   matrix, **row 1 = north**), `se` (the same shape, or `NULL`), `fit` (`n`, `n_cells`, `loo` the leave-one-out
#'   RMSE, `vg` the fitted variogram for `ok`, `edf` the effective degrees of freedom for `tps`), and `method`.
#'   Turn it into a raster with [cc_interpolate_rast()].
#' @examples
#' set.seed(1)
#' pts <- data.frame(lon = runif(40, -122, -118), lat = runif(40, 31, 34))
#' pts$z <- 12 + 3 * sin(pts$lon + 122) + rnorm(40, 0, 0.3)
#' s <- cc_interpolate(pts, "ok", cell_deg = 0.25)
#' s$fit$loo; dim(s$values)
#' @seealso [cc_interpolate_rast()], [pts_to_rast_idw()] (the superseded server-side IDW)
#' @export
#' @concept analyze
cc_interpolate <- function(pts, method = c("ok", "idw", "tps"), cell_deg = 0.06, mask_km = 60, se = TRUE) {
  method <- match.arg(method)
  stopifnot(is.data.frame(pts), all(c("lon", "lat", "z") %in% names(pts)), cell_deg > 0, mask_km > 0)
  pts <- pts[stats::complete.cases(pts[, c("lon", "lat", "z")]), c("lon", "lat", "z")]
  n <- nrow(pts); stopifnot(n >= 4)
  lon <- as.numeric(pts$lon); lat <- as.numeric(pts$lat); z <- as.numeric(pts$z)
  R <- pi / 180
  # the grid: rows evenly spaced in Web-Mercator y, so the bitmap the map stretches between the bounds is exact
  lo0 <- min(lon); lo1 <- max(lon); la0 <- min(lat); la1 <- max(lat)
  pad <- 0.7; s <- cell_deg * R
  lon0 <- lo0 - pad; yN <- .cc_merc(la1 + pad); yS <- .cc_merc(la0 - pad)
  nx <- ceiling((lo1 + pad - lon0) * R / s); ny <- ceiling((yN - yS) / s)
  grid <- list(lon0 = lon0, lon1 = lon0 + nx * s / R, lat_s = .cc_imerc(yN - ny * s), lat_n = .cc_imerc(yN), nx = nx, ny = ny, cell_deg = cell_deg)
  # a local equirectangular km frame about the points' centre
  lonc <- (lo0 + lo1) / 2; latc <- (la0 + la1) / 2; kx <- 111.32 * cos(latc * R); ky <- 110.57
  X <- (lon - lonc) * kx; Y <- (lat - latc) * ky
  cx <- (lon0 + (seq_len(nx) - 0.5) * s / R - lonc) * kx            # cell centres, x per column
  cy <- (.cc_imerc(yN - (seq_len(ny) - 0.5) * s) - latc) * ky       # y per row, north first
  values <- matrix(NA_real_, ny, nx); se_m <- if (se && method != "idw") matrix(NA_real_, ny, nx) else NULL
  fit <- list(n = n, n_cells = 0L, loo = NA_real_)
  r2 <- mask_km^2
  # per row: the cells' squared distances to every point (nx x n), the mask, then the method
  d2_row <- function(j) outer(cx, X, "-")^2 + outer(rep(cy[j], nx), Y, "-")^2
  if (method == "idw") {
    power <- 1.3; rad2 <- 200^2; sm2 <- 5^2
    for (j in seq_len(ny)) {
      d2 <- d2_row(j); m <- rowSums(d2 <= r2) > 0; if (!any(m)) next
      w <- (d2 + sm2)^(-power / 2); w[d2 > rad2] <- 0
      sw <- rowSums(w); v <- (w %*% z) / sw; v[sw == 0] <- NA
      values[j, m] <- v[m]; fit$n_cells <- fit$n_cells + sum(m)
    }
    Dp <- outer(X, X, "-")^2 + outer(Y, Y, "-")^2
    w <- (Dp + sm2)^(-power / 2); diag(w) <- 0
    fit$loo <- sqrt(mean(((w %*% z) / rowSums(w) - z)^2))
  } else if (method == "ok") {
    vg <- .cc_variogram(X, Y, z); fit$vg <- vg
    m <- n + 1; cov <- function(d) vg$psill * exp(-d / vg$range)
    Dp <- sqrt(outer(X, X, "-")^2 + outer(Y, Y, "-")^2)
    K <- matrix(1, m, m); K[1:n, 1:n] <- cov(Dp); diag(K)[1:n] <- vg$psill + vg$nugget + 1e-6 * vg$psill; K[m, m] <- 0
    Ki <- solve(K)
    wz <- as.numeric(Ki[, 1:n] %*% z)
    fit$loo <- sqrt(mean((wz[1:n] / diag(Ki)[1:n])^2))
    for (j in seq_len(ny)) {
      d2 <- d2_row(j); msk <- rowSums(d2 <= r2) > 0; if (!any(msk)) next
      kv <- cov(sqrt(d2))
      values[j, msk] <- (kv %*% wz[1:n] + wz[m])[msk]
      fit$n_cells <- fit$n_cells + sum(msk)
      if (!is.null(se_m)) {
        kva <- cbind(kv, 1)[msk, , drop = FALSE]
        lam <- kva %*% Ki
        v <- vg$psill + vg$nugget - rowSums(lam * kva)
        se_m[j, msk] <- sqrt(pmax(0, v))
      }
    }
  } else {
    m <- n + 3; ker <- function(r) ifelse(r > 0, r^2 * log(r), 0)
    Dp <- sqrt(outer(X, X, "-")^2 + outer(Y, Y, "-")^2)
    K0 <- matrix(0, m, m); K0[1:n, 1:n] <- ker(Dp)
    K0[1:n, n + 1] <- K0[n + 1, 1:n] <- 1; K0[1:n, n + 2] <- K0[n + 2, 1:n] <- X; K0[1:n, n + 3] <- K0[n + 3, 1:n] <- Y
    scale <- max(abs(K0[1:n, ]))
    best <- NULL
    for (lam in c(1e-4, 1e-3, 1e-2, 3e-2, 1e-1, 3e-1, 1, 3, 10) * scale) {
      K <- K0; diag(K)[1:n] <- diag(K)[1:n] + lam
      Ki <- solve(K); cc <- as.numeric(Ki[, 1:n] %*% z)
      e <- cc[1:n] * lam; kd <- diag(Ki)[1:n]
      rss <- sum(e^2); sse <- sum((e / (lam * kd))^2); tr <- sum(1 - lam * kd)
      gcv <- (sse / n) / (1 - tr / n)^2
      if (is.null(best) || gcv < best$gcv) best <- list(gcv = gcv, lam = lam, c = cc, Ki = Ki, loo = sqrt(sse / n), edf = tr, rss = rss)
    }
    fit$loo <- best$loo; fit$edf <- best$edf
    sigma2 <- best$rss / max(1, n - best$edf)
    for (j in seq_len(ny)) {
      d2 <- d2_row(j); msk <- rowSums(d2 <= r2) > 0; if (!any(msk)) next
      B <- cbind(ker(sqrt(d2)), 1, cx, cy[j])
      values[j, msk] <- (B %*% best$c)[msk]
      fit$n_cells <- fit$n_cells + sum(msk)
      if (!is.null(se_m)) {
        W <- B[msk, , drop = FALSE] %*% t(best$Ki[1:n, , drop = FALSE])   # (cells x n): each column a smoother weight
        se_m[j, msk] <- sqrt(sigma2 * rowSums(W^2))
      }
    }
  }
  list(grid = grid, values = values, se = se_m, fit = fit, method = method)
}

#' The surface from [cc_interpolate()] as a `terra` raster in Web Mercator
#'
#' The grid's rows are evenly spaced in Web-Mercator y, so the honest raster is EPSG:3857 (regular there;
#' `terra::project()` it to 4326 if a lon/lat raster is wanted). Two layers: `value`, and `se` when present.
#' @param s the list [cc_interpolate()] returns.
#' @return A `SpatRaster`.
#' @export
#' @concept analyze
cc_interpolate_rast <- function(s) {
  g <- s$grid; Rm <- 6378137
  ext <- c(g$lon0 * pi / 180 * Rm, g$lon1 * pi / 180 * Rm, .cc_merc(g$lat_s) * Rm, .cc_merc(g$lat_n) * Rm)
  r <- terra::rast(nrows = g$ny, ncols = g$nx, xmin = ext[1], xmax = ext[2], ymin = ext[3], ymax = ext[4], crs = "EPSG:3857", vals = as.numeric(t(s$values)))
  names(r) <- "value"
  if (!is.null(s$se)) { r2 <- terra::rast(r); terra::values(r2) <- as.numeric(t(s$se)); names(r2) <- "se"; r <- c(r, r2) }
  r
}
