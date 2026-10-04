# the CalCOFI grid: cc_grid_key(), cc_grid_land_prep(), cc_grid_build() on a small synthetic sea
# (one fixture per rule), and the bundled cc_grid* datasets

CRS_M <- 3310

# a rectangle in km, relative to a point in open ocean off California, as EPSG:3310 metres
rect_m <- function(x0, x1, y0, y1) {
  o <- c(-300e3, -500e3)
  sf::st_polygon(list(1e3 * rbind(c(x0, y0), c(x1, y0), c(x1, y1), c(x0, y1), c(x0, y0)) +
                        matrix(o, 5, 2, byrow = TRUE)))
}
pt_m <- function(x, y) sf::st_point(c(-300e3, -500e3) + 1e3 * c(x, y))
to_ll <- function(g) sf::st_transform(sf::st_segmentize(sf::st_sfc(g, crs = CRS_M), 1000), 4326)
ll_of <- function(x, y) sf::st_coordinates(sf::st_transform(sf::st_sfc(pt_m(x, y), crs = CRS_M), 4326))

# the synthetic sea, 500 x 100 km:
#   previous cells A [0,100], B [100,300], C [300,400], D [400,500] (x km), each 100 km tall
#   stations S1 (30,50) and S2 (70,50); A's label (50,50) is inside their 20 nmi zone
#   land: a wall x 10-12 across the whole sea (cuts off the water west of it),
#         a wall y 70-72 from x 12 to 55 (cuts the north of S1's Voronoi cell off from S1),
#         an island on C's label, and an island on both D's label and D's centre
synthetic_sea <- function() {
  lab <- function(x, y) {
    p <- sf::st_coordinates(sf::st_transform(sf::st_sfc(pt_m(x, y), crs = CRS_M), sf::st_crs("+proj=calcofi")))
    paste0(format(p[1], digits = 15), ",", format(p[2], digits = 15))
  }
  prev <- sf::st_sf(
    sta_key     = c(lab(50, 50), lab(250, 50), lab(350, 50), lab(450, 50)),
    sta_pattern = c("standard", "historical", "historical", "historical"),
    sta_shore   = c("nearshore", "offshore", "offshore", "offshore"),
    sta_dpos    = c(5L, 20L, 20L, 20L),
    lon_ctr     = c(ll_of(50, 50)[1], ll_of(200, 50)[1], ll_of(320, 50)[1], ll_of(450, 50)[1]),
    lat_ctr     = c(ll_of(50, 50)[2], ll_of(200, 50)[2], ll_of(320, 50)[2], ll_of(450, 50)[2]),
    geom        = c(to_ll(rect_m(0, 100, 0, 100)), to_ll(rect_m(100, 300, 0, 100)),
                    to_ll(rect_m(300, 400, 0, 100)), to_ll(rect_m(400, 500, 0, 100))))
  land <- sf::st_sfc(
    rect_m(10, 12, -5, 105), rect_m(12, 55, 70, 72), rect_m(340, 360, 40, 60), rect_m(440, 460, 40, 60),
    crs = CRS_M)
  stations <- data.frame(
    line = c(90, 70), station = c(55, 80),
    longitude = c(ll_of(30, 50)[1], ll_of(70, 50)[1]),
    latitude  = c(ll_of(30, 50)[2], ll_of(70, 50)[2]),
    sta_type  = c("ROS", "SCCOOS"))
  list(prev = prev, land = land, stations = stations)
}
area_km2 <- function(x) as.numeric(sf::st_area(sf::st_transform(x, CRS_M))) / 1e6

# cc_grid_key ----

two_squares <- function() sf::st_sf(
  grid_key = c("st20-ln90", "st100-ln90"),   # "st100-ln90" sorts before "st20-ln90" in byte order
  geom = sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
    sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0)))), crs = 4326))

test_that("cc_grid_key: a point inside a cell takes that cell", {
  g <- two_squares()
  expect_identical(cc_grid_key(c(0.5, 1.5), c(0.5, 0.5), g), c("st20-ln90", "st100-ln90"))
})

test_that("cc_grid_key: a point on a shared edge takes the key that sorts first in byte order", {
  g <- two_squares()
  expect_identical(cc_grid_key(1, 0.5, g), "st100-ln90")
  expect_identical(cc_grid_key(1, 0.5, g[2:1, ]), "st100-ln90")   # whatever the row order
  expect_identical(cc_grid_key(1, 1, g), "st100-ln90")            # a shared vertex too
})

test_that("cc_grid_key: outside every cell, and a missing or non-finite coordinate, is NA", {
  g <- two_squares()
  expect_identical(cc_grid_key(c(5, NA, 0.5, NaN, Inf), c(5, 0.5, NA, 0.5, 0.5), g), rep(NA_character_, 5))
  expect_identical(cc_grid_key(numeric(0), numeric(0), g), character(0))
})

test_that("cc_grid_key: edges are straight in lon/lat whatever sf_use_s2() says", {
  # a 40-degree-wide cell at 40-50N: the great circle between its northern corners bulges north of
  # 50N, so s2 would put (20, 50.5) inside; the planar rule does not
  g <- sf::st_sf(grid_key = "a", geom = sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 40), c(40, 40), c(40, 50), c(0, 50), c(0, 40)))), crs = 4326))
  for (s2 in c(TRUE, FALSE)) {
    old <- suppressMessages(sf::sf_use_s2(s2))
    expect_identical(cc_grid_key(c(20, 20), c(50.5, 49.5), g), c(NA, "a"))
    suppressMessages(sf::sf_use_s2(old))
  }
})

# cc_grid_land_prep ----

test_that("cc_grid_land_prep: islets are dropped and the coastline is pulled back", {
  land <- sf::st_sfc(rect_m(0, 10, 0, 10), rect_m(20, 20.5, 0, 0.5), crs = CRS_M)
  m <- cc_grid_land_prep(land, islet_min_km2 = 1, shrink_m = 300, simplify_m = 0)
  expect_length(m, 1)                                        # the 0.25 km2 islet is gone
  expect_equal(area_km2(m), (10 - 0.6)^2, tolerance = 1e-3)  # 300 m off every side
  expect_equal(sf::st_crs(m), sf::st_crs(4326))
  # a point 200 m inland of the original coast is water under the mask
  p <- sf::st_transform(sf::st_sfc(pt_m(0.2, 5), crs = CRS_M), 4326)
  expect_false(any(sf::st_intersects(p, m, sparse = FALSE)))
  # `near` keeps only land close to the grid
  far <- sf::st_sfc(rect_m(0, 10, 0, 10), rect_m(500, 510, 0, 10), crs = CRS_M)
  expect_length(cc_grid_land_prep(far, near = sf::st_sfc(rect_m(-20, -5, 0, 10), crs = CRS_M), near_m = 1e4), 1)
})

# cc_grid_build: one rule per test, on the synthetic sea ----

test_that("cc_grid_build: every station is the generator of its own, single-polygon cell", {
  w <- synthetic_sea()
  b <- cc_grid_build(w$stations, w$prev, w$land)
  g <- b$grid
  expect_true(all(sf::st_geometry_type(g) == "POLYGON"))
  expect_true(all(sf::st_is_valid(g)))
  expect_false(anyDuplicated(g$grid_key) > 0)
  xy <- sf::st_coordinates(b$ctrs)
  expect_identical(cc_grid_key(xy[, 1], xy[, 2], g), b$ctrs$grid_key)
  expect_identical(cc_grid_key(w$stations$longitude, w$stations$latitude, g), c("st55-ln90", "st80-ln70"))
  expect_equal(unname(b$report[c("stations", "cells", "multipart_cells")]), c(2, 5, 0))
})

test_that("cc_grid_build: station cells take shore, pattern and spacing from their station and line", {
  w <- synthetic_sea()
  g <- sf::st_drop_geometry(cc_grid_build(w$stations, w$prev, w$land)$grid)
  s <- g[match(c("st55-ln90", "st80-ln70"), g$grid_key), ]
  expect_identical(s$sta_shore,   c("nearshore", "offshore"))   # station <= 60 is nearshore
  expect_identical(s$sta_pattern, c("standard", "extended"))    # line >= 76.7 is standard
  expect_identical(s$sta_dpos,    c(5L, 10L))
  expect_identical(s$zone_key,    c("nearshore-standard", "offshore-extended"))
  expect_identical(s$sta_type,    c("ROS", "SCCOOS"))
  expect_identical(s$sta_key,     c("90,55", "70,80"))
  expect_identical(s$sta_source,  c("official", "official"))
})

test_that("cc_grid_build: a previous cell within 20 nmi of the stations is retired, one beyond is kept with its key", {
  w <- synthetic_sea()
  b <- cc_grid_build(w$stations, w$prev, w$land)
  expect_identical(b$seeds$seed_at, c("retired", "label", "centre", "water"))
  expect_identical(b$seeds$in_zone, c(TRUE, FALSE, FALSE, FALSE))
  kept <- b$grid[b$grid$sta_source == "previous", ]
  expect_setequal(kept$grid_key, b$seeds$grid_key[-1])
  expect_true(all(grepl("_hist$", kept$grid_key)))
  expect_true(all(kept$sta_pattern == "historical" & kept$sta_dpos == 20L & is.na(kept$sta_type)))
  expect_false(b$seeds$grid_key[1] %in% b$grid$grid_key)
  # the threshold is the argument: with a 300 km zone nothing beyond the stations is a seed
  b2 <- cc_grid_build(w$stations, w$prev[1:2, ], w$land[1:2], seed_dist_m = 3e5)
  expect_identical(b2$seeds$seed_at, c("retired", "retired"))
  expect_identical(nrow(b2$grid), 2L)
})

test_that("cc_grid_build: a kept cell's site is its label, else the previous centre, else a point on the cell", {
  w <- synthetic_sea()
  b <- cc_grid_build(w$stations, w$prev, w$land)
  at <- function(i) sf::st_coordinates(sf::st_transform(
    b$ctrs[b$ctrs$grid_key == b$seeds$grid_key[i], ], CRS_M)) / 1e3 - c(-300, -500)
  expect_equal(unname(at(2)[1, ]), c(250, 50), tolerance = 1e-6)   # label, in water
  expect_equal(unname(at(3)[1, ]), c(320, 50), tolerance = 1e-6)   # label on land: the previous centre
  d <- unname(at(4)[1, ])                                          # label and centre on land: a point on its water
  expect_true(d[1] > 400 && d[1] < 500 && !(d[1] > 440 && d[1] < 460 && d[2] > 40 && d[2] < 60))
})

test_that("cc_grid_build: a seed on land still generates from its label, so its edges stay where the lattice put them", {
  w <- synthetic_sea()
  b <- cc_grid_build(w$stations, w$prev, w$land)
  a <- setNames(area_km2(b$grid), b$grid$grid_key)[b$seeds$grid_key[2:4]]
  # B (label 250, in water), C (label 350, on an island) and D (label 450, on an island): the
  # edges are the bisectors of the LABELS, x = 300 and x = 400, wherever the sites sit
  expect_equal(unname(a[1]), (300 - 160) * 100,      tolerance = 1e-4)   # B: x 160-300
  expect_equal(unname(a[2]), 100 * 100 - 20 * 20,    tolerance = 1e-4)   # C: x 300-400 less its island
  expect_equal(unname(a[3]), 100 * 100 - 20 * 20,    tolerance = 1e-4)   # D: x 400-500 less its island
  # regression: with the seed moved to the previous centre (x = 320) the B|C edge sat at x = 285
  # and C took 15 km of B's water; a position at x = 295 must stay in B
  p <- ll_of(295, 50)
  expect_identical(cc_grid_key(p[1], p[2], b$grid), b$seeds$grid_key[2])
  # and a seed whose Voronoi cell holds no water at all gives no cell, and says so
  inland <- w$prev[4, ]
  sf::st_geometry(inland) <- to_ll(rect_m(440, 460, 40, 60))            # the whole previous cell is the island
  prev2 <- rbind(w$prev[1:3, ], inland)
  land2 <- c(w$land[1:3], sf::st_sfc(rect_m(400, 500, -5, 105), crs = CRS_M))   # and its Voronoi cell is all land
  b2 <- cc_grid_build(w$stations, prev2, land2)
  expect_identical(b2$seeds$seed_at[4], "no water")
  expect_identical(nrow(b2$grid), 4L)
  expect_equal(unname(b2$report["prev_no_water"]), 1)
})

test_that("cc_grid_build: a pocket land cuts off from its station joins the neighbour it shares water with", {
  w <- synthetic_sea()
  b <- cc_grid_build(w$stations, w$prev, w$land)
  # the north of S1's Voronoi cell (x 12-50, y 72-100) is behind the wall: it goes to S2
  expect_identical(nrow(b$pockets), 1L)
  expect_identical(b$pockets$key_from, "st55-ln90")
  expect_identical(b$pockets$key_to,   "st80-ln70")
  expect_equal(b$pockets$area_km2, 38 * 28, tolerance = 1e-4)
  inside <- ll_of(30, 85)
  expect_identical(cc_grid_key(inside[1], inside[2], b$grid), "st80-ln70")
  a <- area_km2(b$grid)[match(c("st55-ln90", "st80-ln70"), b$grid$grid_key)]
  expect_equal(a[1], 38 * 70, tolerance = 1e-4)                         # S1 keeps x 12-50, y 0-70
  expect_equal(a[2], 110 * 100 - 5 * 2 + 38 * 28, tolerance = 1e-4)     # S2: x 50-160 less the wall, plus the pocket
})

test_that("cc_grid_build: water that land cuts off from every generator is not in the grid", {
  w <- synthetic_sea()
  b <- cc_grid_build(w$stations, w$prev, w$land)
  expect_identical(nrow(b$dropped), 1L)
  expect_equal(b$dropped$area_km2, 10 * 100, tolerance = 1e-4)          # x 0-10, west of the wall
  cut <- ll_of(5, 50); on_land <- ll_of(11, 50); outside <- ll_of(-50, 50)
  expect_identical(cc_grid_key(c(cut[1], on_land[1], outside[1]), c(cut[2], on_land[2], outside[2]), b$grid),
                   rep(NA_character_, 3))
})

test_that("cc_grid_build: the cells tile the extent: no overlap, and their area is the water that holds a generator", {
  w <- synthetic_sea()
  b <- cc_grid_build(w$stations, w$prev, w$land)
  water <- 500 * 100 - (2 * 100 + 43 * 2 + 20 * 20 + 20 * 20) - 10 * 100   # sea, less land, less the cut-off west
  expect_equal(sum(area_km2(b$grid)), water, tolerance = 1e-5)
  gm <- sf::st_transform(b$grid, CRS_M)
  ov <- suppressWarnings(sf::st_intersection(gm, gm))
  ov <- ov[ov$grid_key != ov$grid_key.1, ]
  expect_lt(sum(as.numeric(sf::st_area(ov))), 1)                         # < 1 m2 of overlap in all
  z <- b$zones
  expect_setequal(z$zone_key, c("nearshore-standard", "offshore-extended", "offshore-historical"))
  expect_equal(sum(area_km2(z)), water, tolerance = 1e-5)
})

test_that("cc_grid_build: refuses a station on land and duplicated stations", {
  w <- synthetic_sea()
  s <- w$stations; s$longitude[1] <- ll_of(11, 50)[1]; s$latitude[1] <- ll_of(11, 50)[2]
  expect_error(cc_grid_build(s, w$prev, w$land), "stations on land")
  expect_error(cc_grid_build(rbind(w$stations, w$stations[1, ]), w$prev, w$land))
})

# the bundled grid ----

test_that("cc_grid: 225 single-polygon cells, unique keys, the attributes consumers read", {
  g <- calcofi4r::cc_grid
  expect_identical(nrow(g), 225L)
  expect_true(all(sf::st_geometry_type(g) == "POLYGON"))
  expect_true(all(sf::st_is_valid(g)))
  expect_false(anyDuplicated(g$grid_key) > 0)
  expect_false(anyDuplicated(g$sta_key) > 0)
  expect_true(all(grepl("^st-?[0-9.]+-ln[0-9.]+(_hist)?$", g$grid_key)))
  expect_setequal(unique(g$zone_key), c(
    "nearshore-standard", "offshore-standard", "nearshore-extended", "offshore-extended",
    "nearshore-historical", "offshore-historical"))
  expect_identical(g$zone_key, paste0(g$sta_shore, "-", g$sta_pattern))
  expect_true(is.double(g$sta_lin) && is.double(g$sta_pos) && is.integer(g$sta_dpos))
  expect_identical(sum(g$sta_source == "official"), 113L)
  expect_identical(as.integer(table(g$sta_type)[c("ROS", "SCCOOS")]), c(104L, 9L))
  expect_equal(sf::st_crs(g), sf::st_crs(4326))
})

test_that("cc_grid: every official station, and every site, lies inside its own cell", {
  s <- calcofi4r::cc_station_positions
  expect_identical(nrow(s), 113L)
  expect_identical(cc_grid_key(s$longitude, s$latitude), s$grid_key)
  xy <- sf::st_coordinates(calcofi4r::cc_grid_ctrs)
  expect_identical(cc_grid_key(xy[, 1], xy[, 2]), calcofi4r::cc_grid_ctrs$grid_key)
  expect_identical(calcofi4r::cc_grid_ctrs$grid_key, calcofi4r::cc_grid$grid_key)
})

test_that("cc_grid: named positions key as the rules say", {
  k <- function(lon, lat) cc_grid_key(lon, lat)
  expect_identical(k(-118.38708, 33.18462), "st37-ln90")          # official 90.0 37.0, its own cell ...
  expect_identical(cc_grid_key(-118.38708, 33.18462, calcofi4r::cc_grid_v1), "st35-ln90")  # ... not the lattice's
  expect_identical(k(-117.27357, 32.94905), "st26.4-ln93.4")      # the SCCOOS station off Del Mar
  # water south of Point Loma is nearest the SCCOOS station 93.4 26.4, which land cuts it off from:
  # the pocket joined the cell it shares water with
  expect_identical(k(-117.1601, 32.57779), "st28-ln93.3")
  expect_identical(k(-118.25, 34.05), NA_character_)              # Los Angeles: on land
  expect_identical(k(-157.8, 21.3), NA_character_)                # Honolulu: outside the hull
  expect_identical(k(-117.2368, 32.70737), "st28-ln93.3")         # a ship at its berth in San Diego Bay
  # 90.0 32.0 (occupied 1959-84, not on the official list, within 20 nmi): the nearest station's cell
  p <- sf::st_coordinates(sf::st_transform(
    sf::st_sfc(sf::st_point(c(90, 32)), crs = sf::st_crs("+proj=calcofi")), 4326))
  expect_identical(k(p[1], p[2]), "st30-ln90")
  # 110.0 100.0 (beyond 20 nmi of the official pattern): its historical cell, key unchanged
  p <- sf::st_coordinates(sf::st_transform(
    sf::st_sfc(sf::st_point(c(110, 100)), crs = sf::st_crs("+proj=calcofi")), 4326))
  expect_identical(k(p[1], p[2]), "st100-ln110_hist")
  expect_identical(cc_grid_key(p[1], p[2], calcofi4r::cc_grid_v1), "st100-ln110_hist")
})

test_that("cc_grid is cc_grid_build() of the bundled inputs", {
  b <- cc_grid_build()
  g <- calcofi4r::cc_grid
  expect_identical(b$grid$grid_key, g$grid_key)
  expect_identical(sf::st_drop_geometry(b$grid), sf::st_drop_geometry(g))
  expect_equal(area_km2(b$grid), area_km2(g), tolerance = 1e-9)
  expect_equal(sf::st_coordinates(b$ctrs), sf::st_coordinates(calcofi4r::cc_grid_ctrs), tolerance = 1e-9)
  expect_equal(unname(b$report["multipart_cells"]), 0)
})

test_that("cc_grid_zones is cc_grid dissolved, and cc_places carries the same zones", {
  z <- calcofi4r::cc_grid_zones
  expect_identical(nrow(z), 6L)
  expect_equal(sum(area_km2(z)), sum(area_km2(calcofi4r::cc_grid)), tolerance = 1e-9)
  expect_identical(sf::st_drop_geometry(cc_grid_zones_build(calcofi4r::cc_grid)), sf::st_drop_geometry(z))
  p <- calcofi4r::cc_places
  i <- match(paste0("cc_", z$zone_key), p$key)
  expect_false(anyNA(i))
  expect_equal(area_km2(p[i, ]), area_km2(z), tolerance = 1e-9)
})

test_that("cc_grid_v1: the previous grid, with the keys releases through v2026.10.01 carry", {
  v <- calcofi4r::cc_grid_v1
  expect_identical(nrow(v), 218L)
  expect_false(anyDuplicated(v$grid_key) > 0)
  expect_identical(sum(grepl("_hist$", v$grid_key)), 114L)
  expect_true(all(c("st30-ln90", "st120-ln90", "st120-ln90_hist") %in% v$grid_key))
  # 84 of the 113 station cells reuse a previous name: never map by name
  expect_identical(sum(calcofi4r::cc_station_positions$grid_key %in% v$grid_key), 84L)
})
