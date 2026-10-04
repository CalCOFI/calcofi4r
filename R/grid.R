# the CalCOFI grid: cells built as a Voronoi tessellation of station positions (cc_grid_build(),
# with the land mask of cc_grid_land_prep()) and the one rule that puts a position in a cell
# (cc_grid_key()). data-raw/cc_grid.R calls cc_grid_build() to make the bundled cc_grid /
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

# round every vertex of a POLYGON sfc to `digits` decimals and drop a vertex that repeats its
# predecessor: two vertices a nanometre apart are valid for GEOS and a degenerate edge for s2.
# the same vertex rounds the same way in every cell that shares it, so the cells still tile
.clean_rings <- function(x, digits = 9) {
  sf::st_sfc(lapply(x, function(p) {
    rings <- lapply(unclass(p), function(r) {
      r <- round(r[, 1:2, drop = FALSE], digits)
      r[c(TRUE, rowSums(abs(diff(r))) > 0), , drop = FALSE]
    })
    stopifnot("a cell's outer ring collapsed" = nrow(rings[[1]]) >= 4)
    sf::st_polygon(rings[vapply(rings, nrow, 1L) >= 4])
  }), crs = sf::st_crs(x))
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

#' Build the CalCOFI grid as a Voronoi tessellation of station positions
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
#'    interior holes filled) minus `land`. A piece of water that land cuts off from every cell
#'    (a lagoon behind the previous grid's coarser coastline) is not part of the grid.
#' 2. **Generators.** Every row of `stations` at its listed position, plus a **seed** for each
#'    previous cell whose labelled (line, station) lies farther than `seed_dist_m` (20 nautical
#'    miles) from the convex hull of the stations, so the extended historical grid keeps its
#'    cells and keys. A seed is the cell's label under `+proj=calcofi`, **on land or not**: the
#'    edge it shares with its neighbours then stays where the previous lattice put it, and the
#'    samples of a coastal cell stay in it. (Moving a seed off land onto the cell's water pulls
#'    that edge seaward and hands the cell its neighbour's nearshore stations.) A previous cell
#'    inside the 20 nautical mile zone that is not a station is retired: its water goes to the
#'    nearest generator.
#' 3. **Cells.** The Voronoi tessellation of the generators in metres (`crs_m`), clipped to the
#'    extent. Voronoi edges get a vertex every `edge_m` so that the lon/lat polygon follows the
#'    metric edge (to well under a metre at 5 km).
#' 4. **Pockets.** A piece of a cell that land separates from the cell's home piece joins the
#'    neighbouring cell it shares the longest water edge with (ties: the key that sorts first),
#'    repeated until every piece is attached. A cell is then one polygon. The home piece is the
#'    one holding the generator; for a seed on land, the one holding the previous cell's centre,
#'    else the largest.
#' 5. **Sites.** The site of a cell (`ctrs`, the release's `grid.geom_ctr`) is its generator; for
#'    a seed on land it is the previous cell's centre when that lies in the new cell, else a
#'    point on the cell.
#' 6. **Attributes.** A station's cell is `nearshore` at station 60 or less and `offshore` beyond;
#'    `standard` on lines 76.7 and south (the 75-station pattern) and `extended` north of it;
#'    spacing class 5 nearshore and 10 offshore. A seed's cell keeps the previous cell's
#'    attributes and key.
#'
#' @param stations data frame of generators: `line`, `station`, `longitude`, `latitude` and
#'   `sta_type` (e.g. `"ROS"`, `"SCCOOS"`); default the bundled [cc_station_positions]
#' @param grid_prev the previous grid: `sf` polygons in EPSG:4326 with `sta_key` (`"line,station"`),
#'   `sta_pattern`, `sta_shore`, `sta_dpos` and, optionally, each cell's centre as `lon_ctr` and
#'   `lat_ctr` (else the centroid of its largest polygon); default the bundled [cc_grid_v1]
#' @param land land polygons (`sf` or `sfc`, any CRS), used as given; default the bundled
#'   [cc_grid_land], which is [cc_grid_land_prep()] of OpenStreetMap's land polygons
#' @param seed_dist_m a previous cell is kept as a seed when its label is farther than this from
#'   the convex hull of `stations`, in metres (default 20 nautical miles)
#' @param edge_m longest Voronoi edge segment, in metres (default 5000)
#' @param crs_m projected CRS, in metres, for the tessellation and the areas (default 3310,
#'   California Albers)
#' @param verbose print what the build did
#' @return a list: `grid` (`sf` polygons, EPSG:4326: `grid_key`, `sta_key`, `sta_lin`, `sta_pos`,
#'   `sta_dpos`, `sta_shore`, `sta_pattern`, `zone_key`, `sta_type`, `sta_source` and `geom`),
#'   `ctrs` (the same rows with each cell's site as `geom`), `zones` (cells dissolved by
#'   pattern and shore), `pockets` (`sf`, EPSG:4326: every piece that changed cell under rule 4,
#'   with `key_from`, `key_to`, `area_km2`), `dropped` (`sf`: the cut-off water of rule 1),
#'   `seeds` (one row per previous cell: `in_zone`, whether its label and its centre are in
#'   water, and `seed_at`: `"retired"`, or where the kept cell's site is, `"label"`, `"centre"`
#'   or `"water"`, or `"no water"` for a seed whose cell came out empty) and `report` (named
#'   counts)
#' @export
#' @concept grid
cc_grid_build <- function(
    stations    = calcofi4r::cc_station_positions,
    grid_prev   = calcofi4r::cc_grid_v1,
    land        = calcofi4r::cc_grid_land,
    seed_dist_m = 20 * CC_NMI_M,
    edge_m      = 5000,
    crs_m       = 3310,
    verbose     = FALSE) {

  stopifnot(
    is.data.frame(stations),
    all(c("line", "station", "longitude", "latitude", "sta_type") %in% names(stations)),
    !anyDuplicated(stations[c("line", "station")]),
    inherits(grid_prev, "sf"),
    all(c("sta_key", "sta_pattern", "sta_shore", "sta_dpos") %in% names(grid_prev)),
    inherits(land, c("sf", "sfc")))
  say <- function(...) if (verbose) cat(..., "\n")
  km2 <- function(x) as.numeric(sf::st_area(x)) / 1e6

  # 1. extent: the previous grid's outer hull, minus land ----
  prev_m  <- sf::st_make_valid(sf::st_transform(.densify_lonlat(sf::st_geometry(grid_prev), 0.05), crs_m))
  outer_m <- .fill_holes(sf::st_union(prev_m))
  land_m  <- sf::st_union(sf::st_make_valid(sf::st_transform(sf::st_geometry(land), crs_m)))
  water   <- sf::st_cast(sf::st_cast(sf::st_difference(outer_m, land_m), "MULTIPOLYGON"), "POLYGON")

  # 2. generators ----
  sta <- sf::st_transform(sf::st_as_sf(
    as.data.frame(stations), coords = c("longitude", "latitude"), crs = 4326, remove = FALSE), crs_m)
  zone <- sf::st_buffer(sf::st_convex_hull(sf::st_union(sf::st_geometry(sta))), seed_dist_m)

  prev <- sf::st_drop_geometry(grid_prev)[c("sta_key", "sta_pattern", "sta_shore", "sta_dpos")]
  prev$sta_lin <- as.numeric(sub(",.*$", "", prev$sta_key))
  prev$sta_pos <- as.numeric(sub("^.*,", "", prev$sta_key))
  label_m <- sf::st_geometry(sf::st_transform(sf::st_as_sf(
    prev[c("sta_lin", "sta_pos")], coords = c("sta_lin", "sta_pos"), crs = sf::st_crs("+proj=calcofi")), crs_m))
  in_water      <- function(p) lengths(sf::st_intersects(p, water)) > 0
  prev$in_zone  <- lengths(sf::st_intersects(label_m, zone)) > 0
  prev$label_ok <- in_water(label_m)
  ctr_m <- if (all(c("lon_ctr", "lat_ctr") %in% names(grid_prev))) {
    sf::st_transform(sf::st_geometry(sf::st_as_sf(
      sf::st_drop_geometry(grid_prev)[c("lon_ctr", "lat_ctr")], coords = c("lon_ctr", "lat_ctr"), crs = 4326)), crs_m)
  } else {
    suppressWarnings(sf::st_centroid(prev_m, of_largest_polygon = TRUE))
  }
  prev$ctr_ok <- in_water(ctr_m)
  is_seed     <- !prev$in_zone
  n_sta       <- nrow(stations)

  gen <- rbind(
    data.frame(
      sta_key     = paste0(.format_num(stations$line), ",", .format_num(stations$station)),
      sta_lin     = stations$line,
      sta_pos     = stations$station,
      sta_dpos    = ifelse(stations$station <= 60, 5L, 10L),
      sta_shore   = ifelse(stations$station <= 60, "nearshore", "offshore"),
      sta_pattern = ifelse(stations$line >= 76.7, "standard", "extended"),
      sta_type    = as.character(stations$sta_type),
      sta_source  = "official"),
    data.frame(
      sta_key     = prev$sta_key[is_seed],
      sta_lin     = prev$sta_lin[is_seed],
      sta_pos     = prev$sta_pos[is_seed],
      sta_dpos    = as.integer(prev$sta_dpos[is_seed]),
      sta_shore   = prev$sta_shore[is_seed],
      sta_pattern = prev$sta_pattern[is_seed],
      sta_type    = rep(NA_character_, sum(is_seed)),
      sta_source  = rep("previous", sum(is_seed))))
  gen$grid_key <- .cc_grid_key_label(gen$sta_lin, gen$sta_pos, gen$sta_pattern)
  gen$zone_key <- paste0(gen$sta_shore, "-", gen$sta_pattern)
  # a seed's generator is its label, on land or not (rule 2); `prev_i` is its previous cell
  gen <- sf::st_sf(gen, geom = c(sf::st_geometry(sta), label_m[is_seed]))
  gen$prev_i <- c(rep(NA_integer_, n_sta), which(is_seed))
  stopifnot(
    "two generators share a key"      = !anyDuplicated(gen$grid_key),
    "two generators share a position" = !anyDuplicated(sf::st_as_text(sf::st_geometry(gen))))
  gen$wet <- in_water(gen)
  if (!all(gen$wet[seq_len(n_sta)]))
    stop("stations on land or outside the previous grid's hull: ",
         paste(gen$grid_key[seq_len(n_sta)][!gen$wet[seq_len(n_sta)]], collapse = ", "), call. = FALSE)

  # 3. one planar partition: Voronoi edges and the extent's boundary, noded together ----
  domain <- sf::st_union(water)
  env   <- sf::st_as_sfc(sf::st_bbox(sf::st_buffer(outer_m, 5e5)))
  vor   <- sf::st_collection_extract(sf::st_voronoi(sf::st_union(sf::st_geometry(gen)), envelope = env), "POLYGON")
  edges <- sf::st_segmentize(sf::st_union(sf::st_boundary(vor)), edge_m)   # unique edges, densified once
  lines <- sf::st_union(c(edges, sf::st_boundary(domain)))
  faces <- .polys(sf::st_polygonize(lines))
  pt    <- sf::st_point_on_surface(faces)
  keep  <- lengths(sf::st_intersects(pt, domain)) > 0
  faces <- faces[keep]; pt <- pt[keep]
  vor_i <- vapply(sf::st_intersects(pt, vor), function(i) i[1], 1L)
  gen_v <- vapply(sf::st_intersects(gen, vor), function(i) i[1], 1L)
  stopifnot(!anyNA(vor_i), !anyNA(gen_v), !anyDuplicated(gen_v))
  owner <- match(vor_i, gen_v)                      # the generator whose Voronoi cell the face is in
  f_km2 <- km2(faces)

  # the home piece of each cell: the one holding its generator; for a seed on land the one
  # holding the previous cell's centre, else the largest piece of its Voronoi cell
  home <- rep(FALSE, length(faces))
  hf   <- sf::st_intersects(gen, faces)
  for (g in seq_len(nrow(gen))) {
    own <- which(owner == g)
    if (!length(own)) next                          # a seed whose Voronoi cell holds no water
    i <- intersect(hf[[g]], own)
    if (!length(i) && !is.na(gen$prev_i[g]))
      i <- intersect(sf::st_intersects(ctr_m[gen$prev_i[g]], faces)[[1]], own)
    if (!length(i)) i <- own[which.max(f_km2[own])]
    home[i[1]] <- TRUE
  }
  has_cell <- vapply(seq_len(nrow(gen)), function(g) any(home & owner == g), logical(1))
  stopifnot("a station has no cell" = all(has_cell[seq_len(n_sta)]))

  # 4. pockets join the neighbour they share the longest water edge with ----
  owner0   <- owner
  anchored <- home
  bnd      <- sf::st_boundary(faces)
  nb       <- sf::st_relate(faces, faces, pattern = "F***1****")
  shared   <- function(i, j) as.numeric(sum(sf::st_length(sf::st_intersection(bnd[i], bnd[j]))))
  n_round  <- 0L
  repeat {
    todo <- which(!anchored)
    if (!length(todo)) break
    new_owner <- rep(NA_integer_, length(faces))
    for (i in todo) {
      j <- nb[[i]][anchored[nb[[i]]]]
      if (!length(j)) next
      len <- vapply(j, function(jj) shared(i, jj), numeric(1))
      tot <- tapply(len, owner[j], sum)
      top <- as.integer(names(tot)[tot >= max(tot) * (1 - 1e-9)])
      new_owner[i] <- top[order(gen$grid_key[top], method = "radix")[1]]
    }
    done <- which(!is.na(new_owner))
    if (!length(done)) break
    owner[done]    <- new_owner[done]
    anchored[done] <- TRUE
    n_round <- n_round + 1L
  }
  moved   <- which(anchored & owner != owner0)
  pockets <- sf::st_transform(sf::st_sf(
    key_from = gen$grid_key[owner0[moved]], key_to = gen$grid_key[owner[moved]],
    area_km2 = f_km2[moved], geom = faces[moved]), 4326)
  # water no cell reaches is cut off from the grid (rule 1)
  cut     <- if (any(!anchored)) .polys(sf::st_union(faces[!anchored])) else faces[0]
  dropped <- sf::st_sf(area_km2 = km2(cut), geom = cut)

  # 5. dissolve, sites, attributes, lon/lat ----
  gen    <- gen[has_cell, ]
  g_id   <- which(has_cell)
  cell_m <- do.call(c, lapply(g_id, function(g) sf::st_union(faces[anchored & owner == g])))
  n_part <- lengths(lapply(cell_m, function(g) if (inherits(g, "MULTIPOLYGON")) seq_along(g) else 1L))
  # the site: the generator, or for a seed on land the previous centre when the cell holds it
  site_m  <- sf::st_geometry(gen)
  site_at <- ifelse(gen$sta_source == "official", "station", "label")
  for (k in which(!gen$wet)) {
    ctr <- ctr_m[gen$prev_i[k]]
    if (lengths(sf::st_intersects(ctr, cell_m[k])) > 0) {
      site_m[k] <- ctr; site_at[k] <- "centre"
    } else {
      site_m[k] <- sf::st_point_on_surface(cell_m[k]); site_at[k] <- "water"
    }
  }
  cols   <- c("grid_key", "sta_key", "sta_lin", "sta_pos", "sta_dpos", "sta_shore", "sta_pattern",
              "zone_key", "sta_type", "sta_source")
  att    <- sf::st_drop_geometry(gen)[cols]
  ord    <- order(att$sta_lin, att$sta_pos, att$grid_key, method = "radix")
  cell   <- sf::st_transform(cell_m, 4326)
  if (all(n_part == 1)) cell <- .clean_rings(cell)   # lon/lat to 1e-9 degrees (0.1 mm)
  grid   <- sf::st_sf(tibble::as_tibble(att), geom = cell)[ord, ]
  ctrs   <- sf::st_sf(tibble::as_tibble(att), geom = sf::st_transform(site_m, 4326))[ord, ]
  zones  <- cc_grid_zones_build(grid)

  prev$seed_at <- "retired"
  prev$seed_at[is_seed] <- "no water"
  prev$seed_at[gen$prev_i[!is.na(gen$prev_i)]] <- site_at[!is.na(gen$prev_i)]
  seeds <- tibble::as_tibble(cbind(
    grid_key = .cc_grid_key_label(prev$sta_lin, prev$sta_pos, prev$sta_pattern),
    prev[c("sta_key", "sta_pattern", "in_zone", "label_ok", "ctr_ok", "seed_at")]))
  report <- c(
    stations        = n_sta,
    seeds           = sum(gen$sta_source == "previous"),
    seeds_at_label  = sum(prev$seed_at == "label"),
    seeds_at_centre = sum(prev$seed_at == "centre"),
    seeds_at_water  = sum(prev$seed_at == "water"),
    prev_retired    = sum(prev$seed_at == "retired"),
    prev_no_water   = sum(prev$seed_at == "no water"),
    cells           = nrow(grid),
    faces           = length(faces),
    pockets_moved   = length(moved),
    pocket_rounds   = n_round,
    multipart_cells = sum(n_part > 1),
    dropped_water   = nrow(dropped))
  say(paste(names(report), report, sep = " = ", collapse = "; "))
  list(grid = grid, ctrs = ctrs, zones = zones, pockets = pockets,
       dropped = sf::st_transform(dropped, 4326), seeds = seeds, report = report)
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
