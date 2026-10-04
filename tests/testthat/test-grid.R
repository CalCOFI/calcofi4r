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

# the synthetic sea, 500 x 100 km (x, y in km):
#   previous cells A [0,100], B [100,300], C [300,400], D [400,500], each 100 km tall
#   stations S1 (30,50) and S2 (70,50): A's label (50,50) is inside their 20 nmi zone, so A is
#     replaced; the labels of B (250,50), C (350,50) and D (450,50) are beyond it, so they are kept
#   the previous grid's coarse land: C has a hole x 330-370, y 30-70 and D a hole x 440-460,
#     y 40-60 (islands), and C and D each have a 10 x 10 notch either side of x = 400 at y 80-90
#     (a gap the previous grid left between them)
#   the new land: a wall x 10-12 across the whole sea (cuts the west of A off from the stations),
#     a wall y 70-72 from x 12 to 55 (cuts the north of S1's Voronoi cell off from S1),
#     C's island, smaller than before (x 340-360, y 40-60, on C's label), and D's island
#     (on D's label and centre) with a lagoon x 448-452, y 48-52
with_hole <- function(outer, ...) {
  holes <- list(...)
  sf::st_polygon(c(list(outer[[1]]), lapply(holes, function(h) h[[1]][5:1, ])))
}
synthetic_sea <- function() {
  lab <- function(x, y) {
    p <- sf::st_coordinates(sf::st_transform(sf::st_sfc(pt_m(x, y), crs = CRS_M), sf::st_crs("+proj=calcofi")))
    paste0(format(p[1], digits = 15), ",", format(p[2], digits = 15))
  }
  c_prev <- sf::st_difference(
    sf::st_sfc(with_hole(rect_m(300, 400, 0, 100), rect_m(330, 370, 30, 70)), crs = CRS_M),
    sf::st_sfc(rect_m(390, 400, 80, 90), crs = CRS_M))[[1]]
  d_prev <- sf::st_difference(
    sf::st_sfc(with_hole(rect_m(400, 500, 0, 100), rect_m(440, 460, 40, 60)), crs = CRS_M),
    sf::st_sfc(rect_m(400, 410, 80, 90), crs = CRS_M))[[1]]
  prev <- sf::st_sf(
    sta_key     = c(lab(50, 50), lab(250, 50), lab(350, 50), lab(450, 50)),
    sta_pattern = c("standard", "historical", "historical", "historical"),
    sta_shore   = c("nearshore", "offshore", "offshore", "offshore"),
    sta_dpos    = c(5L, 20L, 20L, 20L),
    lon_ctr     = c(ll_of(50, 50)[1], ll_of(200, 50)[1], ll_of(320, 50)[1], ll_of(450, 50)[1]),
    lat_ctr     = c(ll_of(50, 50)[2], ll_of(200, 50)[2], ll_of(320, 50)[2], ll_of(450, 50)[2]),
    geom        = c(to_ll(rect_m(0, 100, 0, 100)), to_ll(rect_m(100, 300, 0, 100)), to_ll(c_prev), to_ll(d_prev)))
  land <- sf::st_sfc(
    rect_m(10, 12, -5, 105), rect_m(12, 55, 70, 72), rect_m(340, 360, 40, 60),
    with_hole(rect_m(440, 460, 40, 60), rect_m(448, 452, 48, 52)),
    crs = CRS_M)
  stations <- data.frame(
    line = c(90, 70), station = c(55, 80),
    longitude = c(ll_of(30, 50)[1], ll_of(70, 50)[1]),
    latitude  = c(ll_of(30, 50)[2], ll_of(70, 50)[2]),
    sta_type  = c("ROS", "SCCOOS"))
  list(prev = prev, land = land, stations = stations)
}
area_km2 <- function(x) as.numeric(sf::st_area(sf::st_transform(x, CRS_M))) / 1e6
key_at   <- function(x, y, grid) { p <- ll_of(x, y); cc_grid_key(p[1], p[2], grid) }

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

test_that("cc_grid_build: every station is the generator of its own cell", {
  w <- synthetic_sea()
  b <- cc_grid_build(w$stations, w$prev, w$land)
  g <- b$grid
  expect_true(all(sf::st_is_valid(g)))
  expect_false(anyDuplicated(g$grid_key) > 0)
  xy <- sf::st_coordinates(b$ctrs)
  expect_identical(cc_grid_key(xy[, 1], xy[, 2], g), b$ctrs$grid_key)       # every site in its own cell
  expect_identical(cc_grid_key(w$stations$longitude, w$stations$latitude, g), c("st55-ln90", "st80-ln70"))
  expect_equal(unname(b$report[c("stations", "kept", "replaced", "cells")]), c(2, 3, 1, 5))
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

test_that("cc_grid_build: a previous cell within 20 nmi of the stations is replaced, one beyond is kept with its key and attributes", {
  w <- synthetic_sea()
  b <- cc_grid_build(w$stations, w$prev, w$land)
  expect_identical(b$prev$fate,    c("replaced", "kept", "kept", "kept"))
  expect_identical(b$prev$in_zone, c(TRUE, FALSE, FALSE, FALSE))
  kept <- b$grid[b$grid$sta_source == "previous", ]
  expect_setequal(kept$grid_key, b$prev$grid_key[-1])
  expect_true(all(grepl("_hist$", kept$grid_key)))
  expect_true(all(kept$sta_pattern == "historical" & kept$sta_dpos == 20L & is.na(kept$sta_type)))
  expect_false(b$prev$grid_key[1] %in% b$grid$grid_key)
  # the threshold is the argument: within 300 km of the stations B and C are replaced too
  b2 <- cc_grid_build(w$stations, w$prev, w$land, keep_dist_m = 3e5)
  expect_identical(b2$prev$fate, c("replaced", "replaced", "replaced", "kept"))
  expect_identical(nrow(b2$grid), 3L)
})

test_that("cc_grid_build: a kept cell keeps the previous cell's own boundaries; the station cells stop at them", {
  w <- synthetic_sea()
  b <- cc_grid_build(w$stations, w$prev, w$land)
  a <- setNames(area_km2(b$grid), b$grid$grid_key)
  k <- b$prev$grid_key
  # B is the previous B exactly, x 100-300: station S2 at x 70 would reach x 160 as a free Voronoi cell
  expect_equal(unname(a[k[2]]), 200 * 100, tolerance = 1e-6)
  expect_identical(key_at(130, 50, b$grid), k[2])               # 30 km beyond A's edge: still B, not S2's cell
  expect_identical(key_at(99, 50, b$grid), "st80-ln70")         # 1 km inside A: S2's cell
  # the station tessellation is confined to the replaced region: together the station cells are A's water
  expect_equal(sum(a[c("st55-ln90", "st80-ln70")]), 100 * 100 - (2 * 100 + 43 * 2), tolerance = 1e-6)
  # with B replaced as well (keep_dist_m = 3e5, C replaced too) S2's cell runs on to the next kept cell
  b2 <- cc_grid_build(w$stations, w$prev, w$land, keep_dist_m = 3e5)
  expect_identical(key_at(130, 50, b2$grid), "st80-ln70")
  expect_identical(key_at(390, 50, b2$grid), "st80-ln70")
  expect_identical(key_at(410, 50, b2$grid), k[4])
})

test_that("cc_grid_build: a kept cell's coast side ends at the new land, and gap water joins the cell it is nearest to", {
  w <- synthetic_sea()
  b <- cc_grid_build(w$stations, w$prev, w$land)
  a <- setNames(area_km2(b$grid), b$grid$grid_key)
  k <- b$prev$grid_key
  # C: its 100 x 100 less the new island (20 x 20). It gained the ring between the previous
  # island (40 x 40) and the new one, and its half of the 20 x 10 gap on the C|D edge
  expect_equal(unname(a[k[3]]), 100 * 100 - 20 * 20, tolerance = 2e-4)
  expect_equal(b$prev$gap_km2[3], (40 * 40 - 20 * 20) + 10 * 10, tolerance = 2e-3)
  expect_identical(key_at(335, 50, b$grid), k[3])               # in the ring: was no cell, now C
  expect_identical(key_at(395, 85, b$grid), k[3])               # the gap's west half
  expect_identical(key_at(405, 85, b$grid), k[4])               # the gap's east half
  expect_identical(cc_grid_key(ll_of(395, 85)[1], ll_of(395, 85)[2],
                               sf::st_sf(grid_key = k, geom = sf::st_geometry(w$prev))), NA_character_)
  # D: its 100 x 100 less its island; the island's lagoon touches no cell and is left out
  expect_equal(unname(a[k[4]]), 100 * 100 - 20 * 20, tolerance = 2e-4)
  expect_identical(nrow(b$dropped), 1L)
  expect_equal(b$dropped$area_km2, 4 * 4, tolerance = 1e-4)
  expect_identical(key_at(450, 50, b$grid), NA_character_)      # in the lagoon
  expect_identical(key_at(350, 50, b$grid), NA_character_)      # on C's island
  expect_identical(key_at(-50, 50, b$grid), NA_character_)      # outside the hull
})

test_that("cc_grid_build: a kept cell's site is its label, else the previous centre, else a point on the cell", {
  w <- synthetic_sea()
  b <- cc_grid_build(w$stations, w$prev, w$land)
  expect_identical(b$prev$site_at, c(NA, "label", "centre", "water"))
  at <- function(i) unname(sf::st_coordinates(sf::st_transform(
    b$ctrs[b$ctrs$grid_key == b$prev$grid_key[i], ], CRS_M))[1, ] / 1e3 - c(-300, -500))
  expect_equal(at(2), c(250, 50), tolerance = 1e-6)             # label, in the cell
  expect_equal(at(3), c(320, 50), tolerance = 1e-6)             # label on the island: the previous centre
  d <- at(4)                                                    # label and centre in the lagoon: a point on the cell
  expect_true(d[1] > 400 && d[1] < 500 && !(d[1] > 440 && d[1] < 460 && d[2] > 40 && d[2] < 60))
})

test_that("cc_grid_build: a pocket land cuts off from its station joins the station cell it shares water with", {
  w <- synthetic_sea()
  b <- cc_grid_build(w$stations, w$prev, w$land)
  # the north of S1's Voronoi cell (x 12-50, y 72-100) is behind the wall: it goes to S2
  expect_identical(nrow(b$pockets), 1L)
  expect_identical(b$pockets$key_from, "st55-ln90")
  expect_identical(b$pockets$key_to,   "st80-ln70")
  expect_equal(b$pockets$area_km2, 38 * 28, tolerance = 1e-4)
  expect_identical(key_at(30, 85, b$grid), "st80-ln70")
  a <- setNames(area_km2(b$grid), b$grid$grid_key)
  expect_equal(unname(a["st80-ln70"]), 50 * 100 - 5 * 2 + 38 * 28, tolerance = 1e-4)   # x 50-100 less the wall, plus the pocket
  expect_true(sf::st_geometry_type(b$grid[b$grid$grid_key == "st80-ln70", ]) == "POLYGON")
})

test_that("cc_grid_build: water of a replaced cell that no station cell reaches stays with its nearest station, and is reported", {
  w <- synthetic_sea()
  b <- cc_grid_build(w$stations, w$prev, w$land)
  # x 0-10, west of the wall, was in the previous cell A: a position there had a cell and keeps one
  expect_identical(key_at(5, 50, b$grid), "st55-ln90")
  s1 <- b$grid[b$grid$grid_key == "st55-ln90", ]
  expect_true(sf::st_geometry_type(s1) == "MULTIPOLYGON")
  expect_equal(area_km2(s1), 38 * 70 + 10 * 100, tolerance = 1e-4)     # its home x 12-50, y 0-70, and the detached strip
  expect_equal(unname(b$report[c("multipart_cells", "multipart_official")]), c(1, 1))
  expect_setequal(b$cases$case, c("replaced water no station cell reaches", "replaced region piece apart from the main one"))
  expect_true(all(b$cases$grid_key == "st55-ln90"))
  expect_equal(b$cases$area_km2, c(1000, 1000), tolerance = 1e-4)
  # it never joins a kept cell, and it is not a pocket
  expect_false("st55-ln90" %in% b$pockets$key_to)
})

test_that("cc_grid_build: the cells tile the extent: no overlap, and their area is the water a cell reaches", {
  w <- synthetic_sea()
  b <- cc_grid_build(w$stations, w$prev, w$land)
  water <- 500 * 100 - (2 * 100 + 43 * 2 + 20 * 20 + 20 * 20)        # sea less land; the lagoon is inside D's island
  expect_equal(sum(area_km2(b$grid)), water, tolerance = 1e-5)
  gm <- sf::st_transform(b$grid, CRS_M)
  ov <- suppressWarnings(sf::st_intersection(gm, gm))
  ov <- ov[ov$grid_key != ov$grid_key.1, ]
  expect_lt(sum(as.numeric(sf::st_area(ov))), 1)                     # < 1 m2 of overlap in all
  z <- b$zones
  expect_setequal(z$zone_key, c("nearshore-standard", "offshore-extended", "offshore-historical"))
  expect_equal(sum(area_km2(z)), water, tolerance = 1e-5)
})

test_that("cc_grid_build: where previous cells overlap, the overlap is the cell whose key sorts first", {
  w <- synthetic_sea()
  k <- cc_grid_build(w$stations, w$prev, w$land)$prev$grid_key
  # C reaches 10 km into D (x 400-410, y 0-30); both are kept
  lap <- w$prev
  sf::st_geometry(lap)[3] <- to_ll(sf::st_union(sf::st_sfc(
    sf::st_geometry(sf::st_transform(w$prev[3, ], CRS_M))[[1]], rect_m(400, 410, 0, 30), crs = CRS_M))[[1]])
  b <- cc_grid_build(w$stations, lap, w$land)
  first <- sort(k[3:4], method = "radix")[1]
  expect_identical(key_at(405, 15, b$grid), first)
  # the same cell cc_grid_key() gives the position in the previous grid
  expect_identical(cc_grid_key(ll_of(405, 15)[1], ll_of(405, 15)[2], sf::st_sf(grid_key = k, geom = sf::st_geometry(lap))), first)
  expect_equal(sum(area_km2(b$grid)), 500 * 100 - (2 * 100 + 43 * 2 + 20 * 20 + 20 * 20), tolerance = 1e-5)
})

test_that("cc_grid_build: refuses a station on land, a station inside a kept cell, and duplicated stations", {
  w <- synthetic_sea()
  s <- w$stations; s$longitude[1] <- ll_of(11, 50)[1]; s$latitude[1] <- ll_of(11, 50)[2]
  expect_error(cc_grid_build(s, w$prev, w$land), "stations on land")
  # a third station at (110, 5): inside B, whose label (250, 50) is still beyond 20 nmi of the
  # stations' hull, so B is kept and the station has no cell to be the station of
  s <- rbind(w$stations, data.frame(line = 60, station = 90, longitude = ll_of(110, 5)[1],
                                    latitude = ll_of(110, 5)[2], sta_type = "ROS"))
  expect_error(cc_grid_build(s, w$prev, w$land), "stations outside the replaced region.*st90-ln60")
  expect_error(cc_grid_build(rbind(w$stations, w$stations[1, ]), w$prev, w$land))
})

# the bundled grid ----

n_parts <- function(g) vapply(sf::st_geometry(g), function(x) if (inherits(x, "MULTIPOLYGON")) length(x) else 1L, 1L)

test_that("cc_grid: 225 cells, unique keys, the attributes consumers read", {
  g <- calcofi4r::cc_grid
  expect_identical(nrow(g), 225L)
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

test_that("cc_grid: a station cell is one polygon; a kept cell is the previous cell, pieces and all", {
  g  <- calcofi4r::cc_grid
  np <- n_parts(g)
  # the one station cell in two pieces: 21 km2 of the previous st50-ln60 in Tomales Bay, which
  # opens to the sea through a kept cell, so no station cell reaches it by water (measured 2026-10-04)
  expect_identical(g$grid_key[np > 1 & g$sta_source == "official"], "st53-ln60")
  expect_identical(sum(np > 1 & g$sta_source == "previous"), 13L)
  # the kept cells: same keys, same attributes, and the same water but for the coast
  kept <- g[g$sta_source == "previous", ]
  v1   <- calcofi4r::cc_grid_v1[match(kept$grid_key, calcofi4r::cc_grid_v1$grid_key), ]
  expect_identical(nrow(kept), 112L)
  expect_false(anyNA(v1$grid_key))
  expect_identical(sf::st_drop_geometry(kept)[c("sta_key", "sta_dpos", "sta_shore", "sta_pattern", "zone_key")],
                   sf::st_drop_geometry(v1)[c("sta_key", "sta_dpos", "sta_shore", "sta_pattern", "zone_key")])
  # the previous cell's area as positions were keyed to it: its edges straight in lon/lat
  a_v1  <- as.numeric(sf::st_area(sf::st_make_valid(sf::st_transform(sf::st_set_crs(sf::st_segmentize(
    sf::st_set_crs(sf::st_geometry(v1), NA), 0.05), 4326), CRS_M)))) / 1e6
  ratio <- area_km2(kept) / a_v1
  expect_gt(min(ratio), 0.98); expect_lt(max(ratio), 1.02)        # measured 0.9828 to 1.0124, at the coast
  # away from the coast a kept cell is the previous polygon: 82 of the 112 match to a part in a million
  expect_gte(sum(abs(ratio - 1) < 1e-6), 82)
  off <- kept$sta_shore == "offshore" & kept$sta_pos >= 100
  expect_gt(sum(off), 30)
  expect_equal(area_km2(kept)[off], a_v1[off], tolerance = 1e-6)
})

test_that("cc_grid: named positions key as the rules say", {
  k <- function(lon, lat) cc_grid_key(lon, lat)
  from_calcofi <- function(line, station) sf::st_coordinates(sf::st_transform(
    sf::st_sfc(sf::st_point(c(line, station)), crs = sf::st_crs("+proj=calcofi")), 4326))
  expect_identical(k(-118.38708, 33.18462), "st37-ln90")          # official 90.0 37.0, its own cell ...
  expect_identical(cc_grid_key(-118.38708, 33.18462, calcofi4r::cc_grid_v1), "st35-ln90")  # ... not the lattice's
  expect_identical(k(-117.27357, 32.94905), "st26.4-ln93.4")      # the SCCOOS station off Del Mar
  # the mouth of San Diego Bay is nearest the SCCOOS station 93.4 26.4, which Point Loma cuts it
  # off from: the pocket joined the station cell it shares water with
  expect_identical(k(-117.2067, 32.66724), "st28-ln93.3")
  # the station cells stop where the previous grid stopped them, 20 nmi (line 95.0) south of line
  # 93.3: 15 nmi south is 93.3 60's cell, 25 nmi south is the kept line-100 cell, as before
  p <- from_calcofi(93.3 + 15 / 12, 60); expect_identical(k(p[1], p[2]), "st60-ln93.3")
  p <- from_calcofi(93.3 + 25 / 12, 60); expect_identical(k(p[1], p[2]), "st60-ln100_hist")
  expect_identical(cc_grid_key(p[1], p[2], calcofi4r::cc_grid_v1), "st60-ln100_hist")
  # historical line 96.7 keys to the line-100 cells, as in every release through v2026.10.01
  for (sta in c(40, 60, 80, 100)) {
    p <- from_calcofi(96.7, sta)
    expect_identical(k(p[1], p[2]), cc_grid_key(p[1], p[2], calcofi4r::cc_grid_v1))
    expect_match(k(p[1], p[2]), "-ln100_hist$")
  }
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
  expect_identical(n_parts(b$grid), n_parts(g))
  expect_equal(unname(b$report[c("kept", "replaced", "multipart_official", "dropped_water")]), c(112, 106, 1, 0))
  # what the rules could not attach is reported, not hidden: one piece, and it is why the
  # replaced region is not one piece of water
  expect_setequal(b$cases$case, c("replaced water no station cell reaches", "replaced region piece apart from the main one"))
  expect_true(all(b$cases$grid_key == "st53-ln60"))
  expect_equal(b$cases$area_km2, c(21.07, 21.07), tolerance = 1e-3)
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
