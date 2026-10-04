# the CalCOFI grid: one cell per official station (a Voronoi tessellation of the station positions,
# confined to the cells of the previous grid it replaces) beside the previous cells it keeps
# (cc_grid_build(), with the land mask of cc_grid_land_prep()), and the one rule that puts a
# position in a cell (cc_grid_key()). data-raw/cc_grid.R calls cc_grid_build() to make the bundled cc_grid /
# cc_grid_ctrs / cc_grid_zones; calcofi4db::build_grid_reference() ships them as the release's
# `grid`, and calcofi4db::build_grid_crosswalk() as `grid_crosswalk` against cc_grid_v1.

# constants ----

#' nautical mile, in metres
#' @noRd
CC_NMI_M <- 1852

# helpers ----

# the release key of a cell: st{station}-ln{line}, `_hist` for the historical pattern
.cc_grid_key_label <- function(line, station, pattern) {
  paste0("st", .format_num(station), "-ln", .format_num(line), ifelse(pattern == "historical", "_hist", ""))
}

# a number as R prints it at 15 significant digits (30 -> "30", 26.7 -> "26.7", -40 -> "-40")
.format_num <- function(x) vapply(x, function(v) format(v, digits = 15, trim = TRUE, scientific = FALSE), "")

# remove the interior rings of every polygon of a (multi)polygon sfc and dissolve
.fill_holes <- function(x) {
  p <- sf::st_cast(sf::st_cast(sf::st_sfc(x, crs = sf::st_crs(x)), "MULTIPOLYGON"), "POLYGON")
  sf::st_union(sf::st_sfc(lapply(p, function(g) sf::st_polygon(list(g[[1]]))), crs = sf::st_crs(x)))
}

# add vertices along each lon/lat edge so that no edge is longer than `deg` degrees; the chord is
# the planar lon/lat one (never a great circle), which is the edge DuckDB and GEOS test against
.densify_lonlat <- function(x, deg) {
  crs <- sf::st_crs(x)
  sf::st_set_crs(sf::st_segmentize(sf::st_set_crs(x, NA), deg), crs)
}

# the polygons of a geometry set, one per element (lines and points an overlay leaves are dropped)
.polys <- function(x) {
  x <- x[!sf::st_is_empty(x)]
  if (!length(x)) return(x)
  suppressWarnings(sf::st_cast(sf::st_collection_extract(x, "POLYGON"), "POLYGON"))
}

# round every vertex of a (multi)polygon sfc to `digits` decimals and drop a vertex that repeats
# its predecessor: two vertices a nanometre apart are valid for GEOS and a degenerate edge for s2.
# the same vertex rounds the same way in every cell that shares it, so the cells still tile. A
# cell in one piece comes back a POLYGON, one in several a MULTIPOLYGON
.clean_rings <- function(x, digits = 9) {
  ring <- function(r) {
    r <- round(r[, 1:2, drop = FALSE], digits)
    r[c(TRUE, rowSums(abs(diff(r))) > 0), , drop = FALSE]
  }
  poly <- function(p) {            # a list of rings, or NULL when the outer ring collapses
    rings <- lapply(p, ring)
    if (nrow(rings[[1]]) < 4) return(NULL)
    rings[vapply(rings, nrow, 1L) >= 4]
  }
  sf::st_sfc(lapply(x, function(g) {
    ps <- if (inherits(g, "MULTIPOLYGON")) lapply(unclass(g), poly) else list(poly(unclass(g)))
    ps <- ps[!vapply(ps, is.null, logical(1))]
    stopifnot("a cell collapsed" = length(ps) >= 1)
    if (length(ps) == 1) sf::st_polygon(ps[[1]]) else sf::st_multipolygon(ps)
  }), crs = sf::st_crs(x))
}

# the lines that cut gap water where the nearest previous cell changes: the Voronoi edges between
# sample points (`gp`: x, y, cell) of different cells. Each edge's two generators are the sample
# nearest its midpoint and that sample's mirror image across the edge, so no polygon is built
.gap_cuts <- function(gp, env, crs, clip) {
  u <- gp[!duplicated(gp[c("x", "y")]), ]
  if (nrow(u) < 2 || length(unique(u$cell)) < 2) return(NULL)
  as_pts <- function(m) sf::st_geometry(sf::st_as_sf(as.data.frame(m), coords = 1:2, crs = crs))
  pts <- as_pts(u[c("x", "y")])
  e   <- sf::st_voronoi(sf::st_union(pts), envelope = env, bOnlyEdges = TRUE)
  co  <- sf::st_coordinates(suppressWarnings(sf::st_cast(sf::st_collection_extract(e, "LINESTRING"), "LINESTRING")))
  n   <- nrow(co)
  i   <- which(co[-n, "L1"] == co[-1, "L1"])
  a   <- co[i, 1:2, drop = FALSE]; b <- co[i + 1, 1:2, drop = FALSE]
  len <- sqrt(rowSums((b - a)^2))
  ok  <- len > 0
  a   <- a[ok, , drop = FALSE]; b <- b[ok, , drop = FALSE]; len <- len[ok]
  n1  <- sf::st_nearest_feature(as_pts((a + b) / 2), pts)
  p1  <- as.matrix(u[n1, c("x", "y")])
  d   <- (b - a) / len
  p2  <- 2 * (a + d * rowSums((p1 - a) * d)) - p1
  n2  <- sf::st_nearest_feature(as_pts(p2), pts)
  # an edge too short to have a direction (four samples on one circle meet in two vertices a
  # hair apart) is kept: dropping it would leave the cut it joins in two unconnected halves
  keep <- which(u$cell[n1] != u$cell[n2] | len < 1e-3)
  if (!length(keep)) return(NULL)
  lines <- sf::st_sfc(sf::st_multilinestring(lapply(keep, function(k) rbind(a[k, ], b[k, ]))), crs = crs)
  suppressWarnings(sf::st_intersection(lines, clip))
}

# byte-order minimum of a character vector (DuckDB's min() over VARCHAR)
.min_c <- function(x) sort(x, method = "radix")[1]

# cc_grid_key ----

#' The grid cell a position falls in
#'
#' The one rule that keys a position to a cell of the CalCOFI grid: the cell whose polygon
#' intersects the point, tested on longitude/latitude as planar coordinates (no great-circle edges),
#' and, for a point on an edge shared by two or more cells, the cell whose key sorts first in byte
#' order. `calcofi4db::assign_grid_key()` applies the same rule in DuckDB
#' (`min(grid_key) ... WHERE ST_Intersects(point, geom)`), so R and the database agree on every
#' position, including one on an edge.
#'
#' @param lon,lat longitude and latitude in decimal degrees (WGS 84); a missing or non-finite
#'   coordinate returns `NA`
#' @param grid an `sf` of cells in EPSG:4326 with a key column; default the bundled [cc_grid]
#' @param key name of the key column in `grid` (default `"grid_key"`)
#' @return character vector of keys, `NA` where the position is in no cell (on land, or outside
#'   the grid)
#' @export
#' @concept grid
#' @examples
#' # official station 90.0 37.0, and a point in Los Angeles (on land: NA)
#' cc_grid_key(lon = c(-118.38708, -118.25), lat = c(33.18462, 34.05))
#' # the same station in the previous grid: another cell, under a name the new grid also uses
#' cc_grid_key(-118.38708, 33.18462, grid = cc_grid_v1)
cc_grid_key <- function(lon, lat, grid = calcofi4r::cc_grid, key = "grid_key") {
  stopifnot(
    length(lon) == length(lat),
    inherits(grid, "sf"),
    key %in% names(grid))
  out <- rep(NA_character_, length(lon))
  ok  <- !is.na(lon) & !is.na(lat) & is.finite(lon) & is.finite(lat)
  if (!any(ok)) return(out)
  # planar on lon/lat whatever sf_use_s2() says: drop the CRS on both sides
  pts   <- sf::st_as_sf(data.frame(x = lon[ok], y = lat[ok]), coords = c("x", "y"))
  cells <- sf::st_set_crs(sf::st_geometry(grid), NA)
  keys  <- as.character(grid[[key]])
  hit   <- sf::st_intersects(pts, cells)
  out[ok] <- vapply(hit, function(i) if (length(i) == 0L) NA_character_ else
    if (length(i) == 1L) keys[i] else .min_c(keys[i]), "")
  out
}

# cc_grid_land_prep ----

#' Prepare a land mask for the grid's coastline
#'
#' Turns detailed land polygons (OpenStreetMap's, for the bundled [cc_grid_land]) into the mask
#' [cc_grid_build()] clips cells with: islets smaller than `islet_min_km2` are dropped (a rock is
#' not a hole in a cell), the coastline is pulled back `shrink_m` so that a position at the
#' water's edge (a ship at its berth, a nearshore position rounded to the minute) still falls in a
#' cell, and the result is simplified to `simplify_m` so the cells stay light enough to draw in a
#' browser. A position less than `shrink_m - simplify_m` inland is therefore inside the grid.
#'
#' @param land land polygons (`sf` or `sfc`, any CRS)
#' @param near optional `sf`/`sfc`: keep only land within `near_m` of it (e.g. the previous grid)
#' @param near_m distance for `near`, in metres (default 10 km)
#' @param islet_min_km2 drop land polygons smaller than this (default 1 km2)
#' @param shrink_m pull the coastline back by this many metres (default 300)
#' @param simplify_m Douglas-Peucker tolerance, in metres (default 100)
#' @param crs_m projected CRS, in metres (default 3310)
#' @return `sfc` of polygons in EPSG:4326
#' @export
#' @concept grid
cc_grid_land_prep <- function(
    land, near = NULL, near_m = 1e4, islet_min_km2 = 1, shrink_m = 300, simplify_m = 100, crs_m = 3310) {
  stopifnot(inherits(land, c("sf", "sfc")), shrink_m >= 0, simplify_m >= 0)
  x <- sf::st_union(sf::st_make_valid(sf::st_transform(sf::st_geometry(land), crs_m)))
  x <- sf::st_cast(sf::st_cast(x, "MULTIPOLYGON"), "POLYGON")
  x <- x[as.numeric(sf::st_area(x)) / 1e6 >= islet_min_km2]
  if (!is.null(near)) {
    nr <- sf::st_union(sf::st_make_valid(sf::st_transform(sf::st_geometry(near), crs_m)))
    x  <- x[lengths(sf::st_is_within_distance(x, nr, near_m)) > 0]
  }
  if (shrink_m > 0)
    x <- sf::st_buffer(x, -shrink_m, nQuadSegs = 2)
  if (simplify_m > 0)
    x <- sf::st_simplify(x, preserveTopology = TRUE, dTolerance = simplify_m)
  x <- sf::st_make_valid(x)
  sf::st_transform(.polys(x), 4326)
}

# cc_grid_build ----

#' Build the CalCOFI grid: one cell per official station, inside the previous grid
#'
#' The cells of the CalCOFI grid, built from the official station positions
#' (<https://calcofi.org/sampling-info/station-positions/>) so that every station is the generator
#' of its own cell and sits on its line by construction (CalCOFI/workflows#130). The bundled
#' [cc_grid], [cc_grid_ctrs] and [cc_grid_zones] are this function's output
#' (`data-raw/cc_grid.R`); it is exported so the build can be tested and a variant (another set of
#' stations) measured beside it.
#'
#' The rules, in order:
#'
#' 1. **Extent.** The grid covers the outer hull of the previous grid (the union of its cells with
#'    interior holes filled) minus `land`.
#' 2. **Kept or replaced.** A previous cell whose labelled (line, station) lies farther than
#'    `keep_dist_m` (20 nautical miles) from the convex hull of `stations` is **kept**; the others
#'    are **replaced**. Where two previous cells overlap, the overlap belongs to the key that sorts
#'    first, which is the cell [cc_grid_key()] gave a position there.
#' 3. **Kept cells are the previous cells.** A kept cell keeps its key, its attributes and its own
#'    boundaries, toward the other kept cells and toward the replaced region, so a position that
#'    was in a kept cell is still in it. Only its coast side changes: it ends at `land`.
#' 4. **Station cells.** The Voronoi tessellation of `stations` in metres (`crs_m`), confined to
#'    the replaced region `R` (the union of the replaced cells): an outer station's cell stops
#'    where the previous grid stopped. Voronoi edges get a vertex every `edge_m` so the lon/lat
#'    polygon follows the metric edge (to well under a metre at 5 km).
#' 5. **Pockets.** A piece of a station's cell that land separates from the piece holding the
#'    station joins the neighbouring station cell it shares the longest water edge with (ties:
#'    the key that sorts first), repeated until every piece is attached. A pocket never joins a
#'    kept cell. A piece of `R` that no station's cell reaches by water stays with the station it
#'    is nearest to, as a detached part, and is reported in `cases`.
#' 6. **Gap water.** Water of the extent that no previous cell covered (the previous grid was
#'    clipped by a coarser coastline) joins the cell whose edge it is nearest to, kept or
#'    replaced; inside `R` it is then part of the station tessellation. Gap water that touches no
#'    cell is left out of the grid.
#' 7. **Sites.** The site of a station cell (`ctrs`, the release's `grid.geom_ctr`) is the
#'    station. The site of a kept cell is its label under `+proj=calcofi` when the cell holds it,
#'    else the previous cell's centre, else a point on the cell.
#' 8. **Attributes.** A station's cell is `nearshore` at station 60 or less and `offshore` beyond;
#'    `standard` on lines 76.7 and south (the 75-station pattern) and `extended` north of it;
#'    spacing class 5 nearshore and 10 offshore.
#'
#' It stops when a station lies on land, outside the previous grid's hull, or outside the
#' replaced region `R`: a station inside a kept cell has no cell to be the station of.
#'
#' @param stations data frame of generators: `line`, `station`, `longitude`, `latitude` and
#'   `sta_type` (e.g. `"ROS"`, `"SCCOOS"`); default the bundled [cc_station_positions]
#' @param grid_prev the previous grid: `sf` polygons in EPSG:4326 with `sta_key` (`"line,station"`),
#'   `sta_pattern`, `sta_shore`, `sta_dpos` and, optionally, each cell's centre as `lon_ctr` and
#'   `lat_ctr` (else the centroid of its largest polygon); default the bundled [cc_grid_v1]
#' @param land land polygons (`sf` or `sfc`, any CRS), used as given; default the bundled
#'   [cc_grid_land], which is [cc_grid_land_prep()] of OpenStreetMap's land polygons
#' @param keep_dist_m a previous cell is kept when its label is farther than this from the convex
#'   hull of `stations`, in metres (default 20 nautical miles)
#' @param edge_m longest Voronoi edge segment, in metres (default 5000)
#' @param gap_m spacing of the points along a gap's edges that decide which cell gap water is
#'   nearest to, in metres (default 250)
#' @param crs_m projected CRS, in metres, for the tessellation and the areas (default 3310,
#'   California Albers)
#' @param verbose print what the build did
#' @return a list: `grid` (`sf`, EPSG:4326: `grid_key`, `sta_key`, `sta_lin`, `sta_pos`,
#'   `sta_dpos`, `sta_shore`, `sta_pattern`, `zone_key`, `sta_type`, `sta_source` and `geom`),
#'   `ctrs` (the same rows with each cell's site as `geom`), `zones` (cells dissolved by
#'   pattern and shore), `pockets` (`sf`, EPSG:4326: every piece that changed cell under rule 5,
#'   with `key_from`, `key_to`, `area_km2`), `dropped` (`sf`: the gap water left out under rule
#'   6), `prev` (one row per previous cell: `grid_key`, `sta_key`, `sta_pattern`, `in_zone`,
#'   `fate` (`"kept"`, `"replaced"`, or `"no water"` for a kept cell that is all land),
#'   `site_at` (`"label"`, `"centre"` or `"water"`) and `gap_km2`, the gap water it gained),
#'   `cases` (a data frame of what rule 5 could not attach and of a replaced region in several
#'   pieces: `case`, `grid_key`, `area_km2`, `note`) and `report` (named counts)
#' @export
#' @concept grid
cc_grid_build <- function(
    stations    = calcofi4r::cc_station_positions,
    grid_prev   = calcofi4r::cc_grid_v1,
    land        = calcofi4r::cc_grid_land,
    keep_dist_m = 20 * CC_NMI_M,
    edge_m      = 5000,
    gap_m       = 250,
    crs_m       = 3310,
    verbose     = FALSE) {

  stopifnot(
    is.data.frame(stations),
    all(c("line", "station", "longitude", "latitude", "sta_type") %in% names(stations)),
    !anyDuplicated(stations[c("line", "station")]),
    inherits(grid_prev, "sf"),
    all(c("sta_key", "sta_pattern", "sta_shore", "sta_dpos") %in% names(grid_prev)),
    inherits(land, c("sf", "sfc")))
  say  <- function(...) if (verbose) cat(..., "\n")
  km2  <- function(x) as.numeric(sf::st_area(x)) / 1e6
  one  <- function(i) vapply(i, function(k) if (length(k)) k[1] else NA_integer_, 1L)
  n_sta <- nrow(stations)

  # 1. the previous cells, in metres ----
  prev <- sf::st_drop_geometry(grid_prev)[c("sta_key", "sta_pattern", "sta_shore", "sta_dpos")]
  prev$sta_lin  <- as.numeric(sub(",.*$", "", prev$sta_key))
  prev$sta_pos  <- as.numeric(sub("^.*,", "", prev$sta_key))
  prev$grid_key <- .cc_grid_key_label(prev$sta_lin, prev$sta_pos, prev$sta_pattern)
  stopifnot("two previous cells share a key" = !anyDuplicated(prev$grid_key))
  first_key <- function(i) i[order(prev$grid_key[i], method = "radix")[1]]
  prev_m  <- sf::st_make_valid(sf::st_transform(.densify_lonlat(sf::st_geometry(grid_prev), 0.05), crs_m))
  outer_m <- .fill_holes(sf::st_union(prev_m))
  # one claim per previous cell: an overlap goes to the key that sorts first (rule 2)
  claim <- prev_m
  ord   <- order(prev$grid_key, method = "radix")
  touch <- sf::st_intersects(prev_m, prev_m)
  for (k in seq_along(ord)[-1]) {
    i <- ord[k]
    j <- intersect(touch[[i]], ord[seq_len(k - 1)])
    if (!length(j)) next
    d <- .polys(sf::st_difference(prev_m[i], sf::st_union(prev_m[j])))
    claim[i] <- if (length(d)) sf::st_union(d) else sf::st_sfc(sf::st_polygon(), crs = crs_m)
  }

  # 2. extent: the previous grid's outer hull, minus land (rule 1) ----
  land_m <- sf::st_union(sf::st_make_valid(sf::st_transform(sf::st_geometry(land), crs_m)))
  domain <- sf::st_union(.polys(sf::st_difference(outer_m, land_m)))
  env    <- sf::st_as_sfc(sf::st_bbox(sf::st_buffer(outer_m, 5e5)))

  # 3. kept or replaced (rule 2) ----
  sta <- sf::st_transform(sf::st_as_sf(
    as.data.frame(stations), coords = c("longitude", "latitude"), crs = 4326, remove = FALSE), crs_m)
  zone    <- sf::st_buffer(sf::st_convex_hull(sf::st_union(sf::st_geometry(sta))), keep_dist_m)
  label_m <- sf::st_geometry(sf::st_transform(sf::st_as_sf(
    prev[c("sta_lin", "sta_pos")], coords = c("sta_lin", "sta_pos"), crs = sf::st_crs("+proj=calcofi")), crs_m))
  ctr_m <- if (all(c("lon_ctr", "lat_ctr") %in% names(grid_prev))) {
    sf::st_transform(sf::st_geometry(sf::st_as_sf(
      sf::st_drop_geometry(grid_prev)[c("lon_ctr", "lat_ctr")], coords = c("lon_ctr", "lat_ctr"), crs = 4326)), crs_m)
  } else {
    suppressWarnings(sf::st_centroid(prev_m, of_largest_polygon = TRUE))
  }
  prev$in_zone <- lengths(sf::st_intersects(label_m, zone)) > 0
  kept         <- !prev$in_zone
  wet          <- lengths(sf::st_intersects(sta, domain)) > 0
  sta_key_lab  <- .cc_grid_key_label(stations$line, stations$station, "official")
  if (!all(wet))
    stop("stations on land or outside the previous grid's hull: ",
         paste(sta_key_lab[!wet], collapse = ", "), call. = FALSE)

  # 4. gap water, and the cell each piece of it is nearest to (rule 6) ----
  has   <- !sf::st_is_empty(claim)
  gap   <- .polys(sf::st_difference(domain, sf::st_union(claim[has])))
  alloc <- NULL
  gp    <- data.frame(x = numeric(), y = numeric(), piece = integer(), cell = integer())
  if (length(gap)) {
    # points every `gap_m` along each gap's edge; one on a previous cell's edge belongs to that cell
    co <- sf::st_coordinates(sf::st_segmentize(sf::st_cast(sf::st_boundary(gap), "MULTILINESTRING"), gap_m))
    gp <- unique(data.frame(x = co[, "X"], y = co[, "Y"], piece = as.integer(co[, ncol(co)])))
    gp$cell <- vapply(
      sf::st_intersects(sf::st_geometry(sf::st_as_sf(gp, coords = c("x", "y"), crs = crs_m)), sf::st_buffer(claim, 0.01)),
      function(i) if (length(i)) first_key(i) else NA_integer_, 1L)
    gp <- gp[!is.na(gp$cell), ]
    # a gap that touches one cell is that cell's; one that touches several is cut where the nearest changes
    multi <- as.integer(names(which(tapply(gp$cell, gp$piece, function(v) length(unique(v))) > 1)))
    if (length(multi))
      alloc <- .gap_cuts(gp[gp$piece %in% multi, ], env, crs_m, sf::st_buffer(sf::st_union(gap[multi]), 10))
  }

  # 5. the station tessellation, confined to the replaced region (rule 4) ----
  vor <- if (n_sta > 1) {
    sf::st_collection_extract(sf::st_voronoi(sf::st_union(sf::st_geometry(sta)), envelope = env), "POLYGON")
  } else env
  sta_v <- one(sf::st_intersects(sta, vor))
  stopifnot(!anyNA(sta_v), !anyDuplicated(sta_v))
  edges <- sf::st_segmentize(sf::st_union(sf::st_boundary(vor)), edge_m)   # unique edges, densified once
  if (any(!kept & has))
    edges <- suppressWarnings(sf::st_intersection(edges, sf::st_buffer(sf::st_union(claim[!kept & has]), 1e4)))

  # 6. one planar partition: every line above, noded together ----
  parts <- list(sf::st_boundary(domain), sf::st_union(sf::st_boundary(claim[has])), edges, alloc)
  parts <- parts[!vapply(parts, function(x) is.null(x) || !length(x) || all(sf::st_is_empty(x)), logical(1))]
  faces <- .polys(sf::st_polygonize(sf::st_union(do.call(c, parts))))
  pt    <- sf::st_point_on_surface(faces)
  keep  <- lengths(sf::st_intersects(pt, domain)) > 0
  faces <- faces[keep]; pt <- pt[keep]
  nf    <- length(faces)
  f_km2 <- km2(faces)
  # the previous cell each face is in, or for gap water the cell it is nearest to
  f_claim <- vapply(sf::st_intersects(pt, claim), function(i) if (length(i)) first_key(i) else NA_integer_, 1L)
  is_gap  <- is.na(f_claim)
  f_cell  <- f_claim
  if (any(is_gap) && nrow(gp)) {
    gi    <- which(is_gap)
    piece <- one(sf::st_intersects(pt[gi], gap))
    for (pc in unique(stats::na.omit(piece))) {
      q <- gp[gp$piece == pc, ]
      if (!nrow(q)) next
      f <- gi[which(piece == pc)]
      f_cell[f] <- if (length(unique(q$cell)) == 1) q$cell[1] else
        q$cell[sf::st_nearest_feature(pt[f], sf::st_geometry(sf::st_as_sf(q, coords = c("x", "y"), crs = crs_m)))]
    }
  }
  in_r    <- !is.na(f_cell) & !kept[f_cell]
  in_kept <- !is.na(f_cell) & kept[f_cell]

  nb     <- sf::st_relate(faces, faces, pattern = "F***1****")
  bnd    <- sf::st_boundary(faces)
  shared <- function(i, j) as.numeric(sum(sf::st_length(sf::st_intersection(bnd[i], bnd[j]))))
  # connected sets of faces (by a shared edge) among `idx` whose `lab` is equal
  components <- function(idx, lab) {
    comp <- rep(NA_integer_, nf); n <- 0L
    inx  <- rep(FALSE, nf); inx[idx] <- TRUE
    for (s in idx) {
      if (!is.na(comp[s])) next
      n <- n + 1L; comp[s] <- n; queue <- s
      while (length(queue)) {
        q <- queue[1]; queue <- queue[-1]
        for (j in nb[[q]]) if (inx[j] && is.na(comp[j]) && identical(lab[j], lab[s])) {
          comp[j] <- n; queue <- c(queue, j)
        }
      }
    }
    comp
  }
  # the cell a set of faces joins: the anchored neighbour owner it shares the longest edge with
  join_to <- function(fi, ok) {
    tot <- numeric(0)
    for (i in fi) for (j in nb[[i]]) if (ok[j]) {
      k <- as.character(owner[j]); tot[k] <- sum(tot[k], shared(i, j), na.rm = TRUE)
    }
    if (!length(tot)) return(NA_integer_)
    top <- as.integer(names(tot)[tot >= max(tot) * (1 - 1e-9)])
    top[order(gen_key[top], method = "radix")[1]]
  }

  # generators: the stations, then the kept cells
  kept_i   <- which(kept)
  gen_key  <- c(sta_key_lab, prev$grid_key[kept_i])
  owner    <- rep(NA_integer_, nf)     # index into gen_key
  anchored <- rep(FALSE, nf)

  # kept cells are their own claims (rule 3)
  k <- which(!is_gap & in_kept)
  owner[k] <- n_sta + match(f_claim[k], kept_i); anchored[k] <- TRUE

  # station cells inside the replaced region (rules 4 and 5)
  vo <- rep(NA_integer_, nf)
  vo[in_r] <- match(one(sf::st_intersects(pt[in_r], vor)), sta_v)
  comp <- components(which(in_r), vo)
  hf   <- sf::st_intersects(sta, faces)
  home <- vapply(seq_len(n_sta), function(g) {
    i <- hf[[g]]; i <- i[in_r[i] & vo[i] %in% g]
    if (length(i)) i[1] else NA_integer_
  }, 1L)
  if (anyNA(home))
    stop("stations outside the replaced region (inside a kept cell of the previous grid): ",
         paste(sta_key_lab[is.na(home)], collapse = ", "), call. = FALSE)
  for (g in seq_len(n_sta)) { i <- which(comp == comp[home[g]]); owner[i] <- g; anchored[i] <- TRUE }
  n_round <- 0L
  repeat {
    todo <- unique(comp[in_r & !anchored])
    if (!length(todo)) break
    new <- vapply(todo, function(cc) join_to(which(comp == cc), anchored & in_r), 1L)
    if (all(is.na(new))) break
    for (q in which(!is.na(new))) { i <- which(comp == todo[q]); owner[i] <- new[q]; anchored[i] <- TRUE }
    n_round <- n_round + 1L
  }
  cases <- data.frame(case = character(), grid_key = character(), area_km2 = numeric(), note = character())
  # water of a replaced cell that no station's cell reaches: it stays with its nearest station
  for (cc in unique(comp[in_r & !anchored])) {
    i <- which(comp == cc)
    if (all(is_gap[i])) next
    owner[i] <- vo[i]; anchored[i] <- TRUE
    cases <- rbind(cases, data.frame(
      case = "replaced water no station cell reaches", grid_key = gen_key[vo[i][1]], area_km2 = sum(f_km2[i]),
      note = paste0("in previous ", paste(unique(prev$grid_key[stats::na.omit(f_claim[i])]), collapse = ", "),
                    "; kept with its nearest station as a detached part")))
  }
  # is the replaced region one piece of water?
  rc <- components(which(in_r), rep(1L, nf))
  if (length(unique(stats::na.omit(rc))) > 1) {
    main <- as.integer(names(which.max(tapply(f_km2[in_r], rc[in_r], sum))))
    for (cc in setdiff(unique(stats::na.omit(rc)), main)) {
      i <- which(rc == cc)
      if (all(is_gap[i])) next
      cases <- rbind(cases, data.frame(
        case = "replaced region piece apart from the main one", grid_key = paste(unique(gen_key[vo[i]]), collapse = ", "),
        area_km2 = sum(f_km2[i]),
        note = paste0(sum(home %in% i), " station(s) inside; in previous ",
                      paste(unique(prev$grid_key[stats::na.omit(f_claim[i])]), collapse = ", "))))
    }
  }

  # gap water nearest a kept cell joins it when it touches it (rule 6)
  repeat {
    i <- which(is_gap & in_kept & !anchored)
    if (!length(i)) break
    want <- n_sta + match(f_cell[i], kept_i)
    ok   <- vapply(seq_along(i), function(q) any(anchored[nb[[i[q]]]] & owner[nb[[i[q]]]] %in% want[q]), logical(1))
    if (!any(ok)) break
    owner[i[ok]] <- want[ok]; anchored[i[ok]] <- TRUE
  }
  # any other gap water joins the cell it shares the longest edge with; what touches no cell is left out
  repeat {
    i <- which(is_gap & !anchored)
    if (!length(i)) break
    new <- vapply(i, function(q) join_to(q, anchored), 1L)
    if (all(is.na(new))) break
    owner[i[!is.na(new)]] <- new[!is.na(new)]; anchored[i[!is.na(new)]] <- TRUE
  }

  moved   <- which(anchored & in_r & !is.na(vo) & owner != vo)
  pockets <- sf::st_transform(sf::st_sf(
    key_from = gen_key[vo[moved]], key_to = gen_key[owner[moved]],
    area_km2 = f_km2[moved], geom = faces[moved]), 4326)
  cut     <- if (any(!anchored)) .polys(sf::st_union(faces[!anchored])) else faces[0]
  dropped <- sf::st_sf(area_km2 = km2(cut), geom = cut)

  # 7. dissolve, sites, attributes, lon/lat ----
  gen <- rbind(
    data.frame(
      grid_key    = sta_key_lab,
      sta_key     = paste0(.format_num(stations$line), ",", .format_num(stations$station)),
      sta_lin     = stations$line,
      sta_pos     = stations$station,
      sta_dpos    = ifelse(stations$station <= 60, 5L, 10L),
      sta_shore   = ifelse(stations$station <= 60, "nearshore", "offshore"),
      sta_pattern = ifelse(stations$line >= 76.7, "standard", "extended"),
      sta_type    = as.character(stations$sta_type),
      sta_source  = rep("official", n_sta)),
    data.frame(
      grid_key    = prev$grid_key[kept_i],
      sta_key     = prev$sta_key[kept_i],
      sta_lin     = prev$sta_lin[kept_i],
      sta_pos     = prev$sta_pos[kept_i],
      sta_dpos    = as.integer(prev$sta_dpos[kept_i]),
      sta_shore   = prev$sta_shore[kept_i],
      sta_pattern = prev$sta_pattern[kept_i],
      sta_type    = rep(NA_character_, length(kept_i)),
      sta_source  = rep("previous", length(kept_i))))
  gen$zone_key <- paste0(gen$sta_shore, "-", gen$sta_pattern)
  stopifnot(
    "a station and a kept cell share a key" = !anyDuplicated(gen$grid_key),
    "two cells share a station key"         = !anyDuplicated(gen$sta_key))
  has_cell <- vapply(seq_len(nrow(gen)), function(g) any(anchored & owner %in% g), logical(1))
  g_id     <- which(has_cell)
  cell_m   <- do.call(c, lapply(g_id, function(g) sf::st_union(faces[anchored & owner %in% g])))
  parts_of <- function(g) if (inherits(g, "MULTIPOLYGON")) length(g) else 1L
  n_part   <- vapply(cell_m, parts_of, 1L)

  # sites (rule 7)
  site_m  <- c(sf::st_geometry(sta), label_m[kept_i])[g_id]
  site_at <- rep("station", length(g_id))
  for (q in which(g_id > n_sta)) {
    p <- kept_i[g_id[q] - n_sta]
    if (lengths(sf::st_intersects(label_m[p], cell_m[q])) > 0) {
      site_at[q] <- "label"
    } else if (lengths(sf::st_intersects(ctr_m[p], cell_m[q])) > 0) {
      site_m[q] <- ctr_m[p]; site_at[q] <- "centre"
    } else {
      big <- .polys(cell_m[q]); big <- big[which.max(sf::st_area(big))]
      site_m[q] <- sf::st_point_on_surface(big); site_at[q] <- "water"
    }
  }

  cols  <- c("grid_key", "sta_key", "sta_lin", "sta_pos", "sta_dpos", "sta_shore", "sta_pattern",
             "zone_key", "sta_type", "sta_source")
  att   <- gen[g_id, cols]
  ord   <- order(att$sta_lin, att$sta_pos, att$grid_key, method = "radix")
  cell  <- .clean_rings(sf::st_transform(cell_m, 4326))   # lon/lat to 1e-9 degrees (0.1 mm)
  grid  <- sf::st_sf(tibble::as_tibble(att), geom = cell)[ord, ]
  ctrs  <- sf::st_sf(tibble::as_tibble(att), geom = sf::st_transform(site_m, 4326))[ord, ]
  zones <- cc_grid_zones_build(grid)

  prev$fate <- ifelse(kept, "kept", "replaced")
  prev$fate[kept_i[!has_cell[n_sta + seq_along(kept_i)]]] <- "no water"
  prev$site_at <- NA_character_
  prev$site_at[kept_i[g_id[g_id > n_sta] - n_sta]] <- site_at[g_id > n_sta]
  prev$gap_km2 <- 0
  gk <- which(is_gap & anchored & owner > n_sta)
  if (length(gk)) {
    a <- tapply(f_km2[gk], kept_i[owner[gk] - n_sta], sum)
    prev$gap_km2[as.integer(names(a))] <- as.numeric(a)
  }
  for (p in kept_i[!has_cell[n_sta + seq_along(kept_i)]])
    cases <- rbind(cases, data.frame(case = "kept cell with no water", grid_key = prev$grid_key[p],
                                     area_km2 = 0, note = "all of it is land under the new coastline; no cell"))
  prev_out <- tibble::as_tibble(prev[c("grid_key", "sta_key", "sta_pattern", "in_zone", "fate", "site_at", "gap_km2")])
  rownames(cases) <- NULL

  report <- c(
    stations           = n_sta,
    kept               = sum(prev$fate == "kept"),
    replaced           = sum(prev$fate == "replaced"),
    cells              = nrow(grid),
    faces              = nf,
    pockets_moved      = length(moved),
    pocket_rounds      = n_round,
    multipart_cells    = sum(n_part > 1),
    multipart_official = sum(n_part[g_id <= n_sta] > 1),
    gap_faces          = sum(is_gap & anchored),
    dropped_water      = nrow(dropped),
    cases              = nrow(cases))
  say(paste(names(report), report, sep = " = ", collapse = "; "))
  list(grid = grid, ctrs = ctrs, zones = zones, pockets = pockets,
       dropped = sf::st_transform(dropped, 4326), prev = prev_out, cases = cases, report = report)
}

#' Dissolve grid cells into zones by station pattern and shore
#'
#' The six zones of [cc_grid_zones]: the cells of a grid dissolved by `sta_pattern` and
#' `sta_shore`, with the range of lines and stations each holds.
#'
#' @param grid an `sf` of cells with `sta_pattern`, `sta_shore`, `sta_dpos`, `sta_lin`, `sta_pos`
#'   and `zone_key`, as [cc_grid_build()] returns
#' @return `sf` with one row per zone: `zone_key`, `sta_pattern`, `sta_shore`, `sta_dpos`,
#'   `sta_lin_min`, `sta_lin_max`, `sta_pos_min`, `sta_pos_max`, `geom`
#' @export
#' @concept grid
cc_grid_zones_build <- function(grid) {
  stopifnot(inherits(grid, "sf"),
            all(c("zone_key", "sta_pattern", "sta_shore", "sta_dpos", "sta_lin", "sta_pos") %in% names(grid)))
  crs <- sf::st_crs(grid)
  z   <- split(seq_len(nrow(grid)), grid$zone_key)
  z   <- z[order(names(z), method = "radix")]
  out <- do.call(rbind, lapply(names(z), function(k) {
    g <- grid[z[[k]], ]
    stopifnot(length(unique(g$sta_dpos)) == 1)
    data.frame(
      zone_key = k, sta_pattern = g$sta_pattern[1], sta_shore = g$sta_shore[1], sta_dpos = g$sta_dpos[1],
      sta_lin_min = min(g$sta_lin), sta_lin_max = max(g$sta_lin),
      sta_pos_min = min(g$sta_pos), sta_pos_max = max(g$sta_pos))
  }))
  geom <- do.call(c, lapply(z, function(i) sf::st_union(sf::st_set_crs(sf::st_geometry(grid)[i], NA))))
  sf::st_sf(tibble::as_tibble(out), geom = sf::st_set_crs(geom, crs))
}
