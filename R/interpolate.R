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

# a seeded LCG (Numerical Recipes) and a partial Fisher-Yates on it — the SAME draws as the browser's worker and
# calcofi4py, so a subsample (the variogram's 2,000 points, the LOO's 500) is the same subsample in every runtime
.cc_lcg_sample <- function(n, k, seed) {
  idx <- seq_len(n) - 1L
  if (k >= n) return(idx + 1L)
  s <- seed
  for (i in seq_len(k)) {
    s <- (s * 1664525 + 1013904223) %% 4294967296
    j <- i + floor(s / 4294967296 * (n - i + 1))
    t <- idx[i]; idx[i] <- idx[j]; idx[j] <- t
  }
  idx[seq_len(k)] + 1L
}
# the k nearest points to a cell (squared distances given), ties by index — what the worker's bucket search returns
.cc_nearest <- function(d2, k, lim2 = Inf) { o <- order(d2, seq_along(d2)); o <- o[seq_len(min(k, length(d2)))]; o[d2[o] <= lim2] }

.cc_merc  <- function(lat) log(tan(pi / 4 + lat * pi / 360))
.cc_imerc <- function(y) (2 * atan(exp(y)) - pi / 2) * 180 / pi

# the empirical semivariogram (15 bins to half the maximum distance) and an exponential model by weighted least squares
.cc_variogram <- function(X, Y, Z) {
  if (length(X) > 2000) { pick <- .cc_lcg_sample(length(X), 2000, 1); X <- X[pick]; Y <- Y[pick]; Z <- Z[pick] }
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
  c(best[c("nugget", "psill", "range")], n_fit = n)
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
#' @param se compute the error surface (`ok`, `tps`; always `NULL` for `idw`). It is the slow part in the global mode.
#' @param nmax `0` (the station grid): every point in one system. `> 0` (the cast grain; the Explorer uses 32):
#'   the `nmax` nearest points per cell — one small solve each, which gives the value and its error together;
#'   the variogram then fits on at most 2,000 points and the leave-one-out error runs on at most 500, both drawn by
#'   a seeded generator shared with the browser; a neighbour is never farther than `3 * mask_km`. Not for `"tps"`.
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
cc_interpolate <- function(pts, method = c("ok", "idw", "tps"), cell_deg = 0.06, mask_km = 60, se = TRUE, nmax = 0) {
  method <- match.arg(method)
  stopifnot(is.data.frame(pts), all(c("lon", "lat", "z") %in% names(pts)), cell_deg > 0, mask_km > 0)
  pts <- pts[stats::complete.cases(pts[, c("lon", "lat", "z")]), c("lon", "lat", "z")]
  n <- nrow(pts); stopifnot(n >= 4)
  local <- nmax > 0 && nmax < n
  if (local && method == "tps") stop("the spline needs every point in one system: use nmax = 0 (the station grid), or kriging / IDW at the cast grain")
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
  fit <- list(n = n, n_cells = 0L, loo = NA_real_, nmax = if (local) as.integer(nmax) else 0L)
  r2 <- mask_km^2; lim2 <- (3 * mask_km)^2
  # per row: the cells' squared distances to every point (nx x n), the mask, then the method
  d2_row <- function(j) outer(cx, X, "-")^2 + outer(rep(cy[j], nx), Y, "-")^2
  if (method == "idw") {
    power <- 1.3; rad2 <- 200^2; sm2 <- 5^2
    idw1 <- function(d2, skip = 0L) {   # one cell: every point (global) or the nmax nearest (local), within the radius
      if (skip > 0) d2[skip] <- Inf
      if (local) { k <- .cc_nearest(d2, nmax, lim2); d2 <- d2[k]; zz <- z[k] } else zz <- z
      w <- (d2 + sm2)^(-power / 2); w[d2 > rad2] <- 0; sw <- sum(w)
      if (sw > 0) sum(w * zz) / sw else NA_real_
    }
    for (j in seq_len(ny)) {
      d2 <- d2_row(j); m <- rowSums(d2 <= r2) > 0; if (!any(m)) next
      if (local) { for (i in which(m)) values[j, i] <- idw1(d2[i, ]) }
      else { w <- (d2 + sm2)^(-power / 2); w[d2 > rad2] <- 0; sw <- rowSums(w); v <- (w %*% z) / sw; v[sw == 0] <- NA; values[j, m] <- v[m] }
      fit$n_cells <- fit$n_cells + sum(m)
    }
    pick <- .cc_lcg_sample(n, 500, 2)
    e <- vapply(pick, function(i) idw1((X - X[i])^2 + (Y - Y[i])^2, i) - z[i], 0)
    fit$loo <- sqrt(mean(e[is.finite(e)]^2)); fit$n_loo <- length(pick)
  } else if (method == "ok") {
    vg <- .cc_variogram(X, Y, z); fit$vg <- vg[c("nugget", "psill", "range")]; fit$n_fit <- vg$n_fit
    cov <- function(d) vg$psill * exp(-d / vg$range); dg <- vg$psill + vg$nugget + 1e-6 * vg$psill
    if (local) {
      # one (c+1)-system per cell: the weights, the value and the variance together
      krige1 <- function(d2, skip = 0L) {
        if (skip > 0) d2[skip] <- Inf
        k <- .cc_nearest(d2, nmax, lim2); k <- k[is.finite(d2[k])]; c <- length(k); if (c < 2) return(c(NA_real_, NA_real_))
        K <- matrix(1, c + 1, c + 1); K[1:c, 1:c] <- cov(sqrt(outer(X[k], X[k], "-")^2 + outer(Y[k], Y[k], "-")^2)); diag(K)[1:c] <- dg; K[c + 1, c + 1] <- 0
        kv <- c(cov(sqrt(d2[k])), 1); lam <- solve(K, kv)
        c(sum(lam[1:c] * z[k]), sqrt(max(0, vg$psill + vg$nugget - sum(lam * kv))))
      }
      if (is.null(se_m)) se_m <- matrix(NA_real_, ny, nx)   # the local solve gives it anyway
      for (j in seq_len(ny)) {
        d2 <- d2_row(j); msk <- rowSums(d2 <= r2) > 0; if (!any(msk)) next
        for (i in which(msk)) { r <- krige1(d2[i, ]); values[j, i] <- r[1]; se_m[j, i] <- r[2] }
        fit$n_cells <- fit$n_cells + sum(msk)
      }
      if (!se) se_m <- NULL
      pick <- .cc_lcg_sample(n, 500, 2)
      e <- vapply(pick, function(i) krige1((X - X[i])^2 + (Y - Y[i])^2, i)[1] - z[i], 0)
      fit$loo <- sqrt(mean(e[is.finite(e)]^2)); fit$n_loo <- length(pick)
      return(list(grid = grid, values = values, se = se_m, fit = fit, method = method))
    }
    m <- n + 1
    Dp <- sqrt(outer(X, X, "-")^2 + outer(Y, Y, "-")^2)
    K <- matrix(1, m, m); K[1:n, 1:n] <- cov(Dp); diag(K)[1:n] <- dg; K[m, m] <- 0
    Ki <- solve(K)
    wz <- as.numeric(Ki[, 1:n] %*% z)
    fit$loo <- sqrt(mean((wz[1:n] / diag(Ki)[1:n])^2)); fit$n_loo <- n
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
    fit$loo <- best$loo; fit$edf <- best$edf; fit$n_loo <- n
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
