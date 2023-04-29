#' Get oceanographic variable for area of interest
#'
#' @param var variable of interest (TODO: see keys)
#' @param aoi area of interest
#' @param date_step eg month, quarter, year
#' @param depth_min depth minimum
#' @param depth_max depth maximum
#'
#' @concept analyze
#' @importFrom glue glue
#' @return data frame of values
#' @export
#' @examples \dontrun{
#' get_oceano_var_aoi("Bottle O2(ml_L)", cinms_ply, "year", 0, 4000)
#' }
#'
get_oceano_var_aoi <- function(
    var, aoi,
    date_step = c("year", "day", "week", "month", "quarter", "decade"),
    depth_min = 0, depth_max = 10){

  # test values
  # var = "Bottle O2(ml_L)"; aoi = cinms_ply; date_step = "year"; depth_min = 0; depth_max = 4000
  # var = "Salnty"; aoi = cinms_ply; date_step = "year"; depth_min = 0; depth_max = 1000

  d <- eval(parse(text = glue::glue("var_lookup$`{var}`$data_source_name"))) %>%
    as.name() %>% eval()

  pts <- get_pts(d)

  # find stations in aoi
  pts_aoi <- pts %>%
    mutate(
      x = st_intersects(pts, aoi) %>% as.logical()) %>%
    filter(x)

  # d_var <- d %>% filter(!is.na(eval(parse(text = glue("d$`{var}`")))))

  d_summ     <- d %>% filter(!is.na(.data[[var]]))
  d_aoi_summ <- d_summ %>% filter(Sta_ID %in% pts_aoi$Sta_ID)

  # d_test <- d %>% filter(!is.na(`Bottle O2(ml_L)`))
  # d_test_aoi <- d_test %>% filter(Sta_ID %in% pts_aoi$Sta_ID)

  # d_aoi_summ <- d_aoi %>%
  #   filter(!is.na(.data[[var]]))

  empty_data_for_var <- ifelse(nrow(d_aoi_summ) == 0, TRUE, FALSE)

  if (empty_data_for_var) {
    d_aoi_summ <- d_summ
  }
  if (any(!is.na(.data[[Depthm]]))) {
    d_aoi_summ <- d_aoi_summ %>%
      filter(Depthm >= depth_min, Depthm < depth_max)
  }
  d_aoi_summ <- d_aoi_summ %>%
    mutate(Date_Step = update_date(Date, unit = date_step)) %>%
    group_by(Date_Step) %>%
    summarize(
      var_n   = n(),
      var_min = min(.data[[var]], na.rm = T),
      var_q10 = quantile(.data[[var]], probs = 0.10, na.rm = T),
      var_avg = mean(.data[[var]], na.rm = T),
      var_q90 = quantile(.data[[var]], 0.90, na.rm = T),
      var_max = max(.data[[var]], na.rm = T),
      var_sd  = sd(.data[[var]], na.rm = T)) %>%
    rename(Date = Date_Step)

  attr(d_aoi_summ, "labels")    <- eval(parse(text = glue("var_lookup$`{var}`")))
  attr(d_aoi_summ, "date_step") <- date_step
  attr(d_aoi_summ, "date_msg")  <- glue::glue("This dataset was summarized by {date_step}.")
  attr(d_aoi_summ, "aoi") <- ifelse(
    empty_data_for_var,
    glue::glue("No data were found for {var} in this area of interest. Summaries were conducted across all existing data points."),
    glue::glue("Data for {var} in selected area of interest")
  )

  d_aoi_summ
}

#' Interpolate points to contours using a Generalized Additive Model (GAM)
#'
#' Fit generalized additive model to a variable in space using a smoother on
#' latitude & longitude. Further clip by bounding polygon.
#'
#' @param df data frame containing the fields: `lon` (longitude), `lat`
#'   (latitude) and `v` (value)
#' @param ply polygon describing the study area mask
#' @param gam_k number of knots to use on the spatial smoother `s(lon, lat)`;
#'   default: `100`
#' @param grid_width cell width in decimal degrees to run interpolation; default: `0.1`
#' @param n_breaks number of breaks in values for creating contours; default: `7`
#'
#' @return polygon simple features (`sf`) of contour `isoband::isobands()`
#' @import dplyr sf
#' @importFrom glue glue
#' @importFrom mapview mapView
#' @importFrom isoband isobands iso_to_sfg
#' @importFrom mgcv gam
#' @importFrom scales extended_breaks
#' @importFrom tidyr pivot_wider
#' @importFrom tibble column_to_rownames
#' @export
#'
#' @concept analyze
#' @examples
#' v_ply <- pts_to_contours_gam(bottle_temp_lonlat, area_calcofi_extended)
#' mapview::mapView(v_ply, zcol="v", layer.name="temp(ºC)")
pts_to_contours_gam <- function(df, ply, gam_k=60, grid_width=0.1, n_breaks=7){

  # df=bottle_temp_lonlat; ply=area_calcofi_extended; k=60; cw=0.1
  # df = cc_bottle_temp %>% select(lon, lat, v = t_degc); ply = cc_grid_area; gam_k=60; grid_width=0.1; n_breaks=5

  # check column names in data frame
  stopifnot(all(c("lon", "lat", "v") %in% names(df)))

  # check geographic projection of input polygon boundary
  if(is.na(sf::st_crs(ply))){
    warning(glue::glue(
      "The coordinate reference system for input `ply` is not set, so assuming
      geographic (EPSG:4326)."))
    ply <- sf::st_set_crs(ply, 4326)
  }
  if(!is.na(sf::st_crs(ply)) & sf::st_crs(ply) != sf::st_crs(4326)){
    ply <- sf::st_transform(ply, 4326)
  }

  # filter NAs
  df <- df %>%
    dplyr::filter(!is.na(v))

  # get points
  pts <- sf::st_as_sf(
    df,
    coords = c("lon", "lat"), crs = 4326, remove = T)

  # fit generalized additive model using a smoother on latitude & longitude
  f <- mgcv::gam(
    v ~ s(lon, lat, k = gam_k),
    data = df, method = "REML")

  # make regularized grid of points
  g <- sf::st_make_grid(
    pts, cellsize = c(grid_width, grid_width), what = "centers") %>%
    sf::st_as_sf() %>%
    dplyr::rename(geom = x) %>%
    cbind(., sf::st_coordinates(.)) %>%
    dplyr::rename(lon = X, lat = Y) # %>%
  # dplyr::mutate(
  #   in_ply = sf::st_intersects(ply, geom, sparse = F)[1,])
  g$in_ply <- sf::st_intersects(st_union(ply), g, sparse = F)[1,]
  # table(g$in_ply)

  # predict values using fitted model
  g$v <- predict(
    f, newdata = g, type = "response")

  # matrix from grid for input to isobands()
  m <- g %>%
    sf::st_drop_geometry() %>%
    dplyr::select(-in_ply) %>%
    tidyr::pivot_wider(
      names_from = lon, values_from = v) %>%
    tibble::column_to_rownames("lat") %>%
    as.matrix()

  # pull values within the polygon for determining breaks
  v <- g %>%
    dplyr::filter(in_ply) %>%
    dplyr::pull(v)

  # determine breaks
  brks <- scales::extended_breaks(n_breaks)(v)
  # pad breaks if not inclusive of full range of values
  db <- diff(brks)[1]
  if (min(v) < min(brks))
    brks <- c(brks[1] - db, brks)
  if (max(v) > max(brks))
    brks <- c(brks, brks[length(brks)] + db)

  # get isobands
  b <- isoband::isobands(
    dimnames(m)[[2]], dimnames(m)[[1]], m,
    brks[-1], brks[-length(brks)]) # , levels_low, levels_high)

  # convert to sf polygon
  b_sf <- tibble::tibble(
    geom = isoband::iso_to_sfg(b),
    name = names(geom),
    v_lo = stringr::str_split(name, ':', simplify=T)[,1] %>% as.numeric(),
    v_hi = stringr::str_split(name, ':', simplify=T)[,2] %>% as.numeric()) %>%
    dplyr::rowwise() %>%
    mutate(
      v = mean(c(v_lo, v_hi))) %>%
    sf::st_as_sf(crs=4326)

  # crop to ply
  sf::st_agr(b_sf) = "constant"
  b_sf <- sf::st_intersection(b_sf, st_union(ply))

  b_sf
}

#' Interpolate points to raster using Inverse-Distance Weighting (IDW)
#'
#' Interpolate from points to raster using inverse-distance weighting
#' (`terra::interpIDW()`). The further the input point from the destination pixel
#' in the output raster, the less its value is applied. The dimensions of the
#' output raster are automatically calculated based on approaching 100 pixels in
#' the maximum dimension (of longitude or latitude) based on the extent of the
#' area of interest.
#'
#' @param pts input points as simple features `sf::sf`
#' @param fld quoted string of field from `pts` with z-value to interpolate
#' @param aoi area of interest as simple features `sf::sf`
#' @param radius argument for `terra::interpIDW()`; default = `1.5`
#' @param power argument for `terra::interpIDW()`; default = `1.3`
#' @param smooth argument for `terra::interpIDW()`; default = `0.2`
#' @param maxPoints argument for `terra::interpIDW()`; default = `Inf`
#' @param minPoints argument for `terra::interpIDW()`; default = `1`
#' @param near argument for `terra::interpIDW()`; default = `FALSE`
#' @param fill argument for `terra::interpIDW()`; default = `NA`
#' @param out_tif output path to write raster tif using `terra::writeRaster()`; default = `NULL`
#' @param verbose show messages of all variables used; default = FALSE
#'
#' @return `terra::rast()` object
#' @import dplyr
#' @importFrom sf st_bbox st_buffer st_drop_geometry
#' @importFrom terra interpIDW mask rast trim writeRaster
#' @importFrom glue glue
#' @export
#' @concept analyze
pts_to_rast_idw <- function(
    pts, fld, aoi,
    radius = 1.5, power = 1.3, smooth = 0.2, maxPoints = Inf, minPoints = 1, near = FALSE, fill= NA,
    out_tif = NULL,
    verbose = F){
  # pts = d_t; fld = "v"; aoi = cc_grid_zones

  # * check inputs ----
  stopifnot("sf" %in% class(pts))
  stopifnot(fld %in% colnames(pts))
  stopifnot("sf" %in% class(aoi))

  # * create template raster of extent ----

  # get bounding box dimensions for area of interest
  b <- sf::st_bbox(sf::st_union(aoi))
  dx <- b$xmax - b$xmin
  dy <- b$ymax - b$ymin

  # calculate pixel size approaching 100 pixels for max dim (x or y of aoi)
  d100 <- max(c(dx, dy)) / 100
  brks <- c(-Inf, 0.01, 0.1, 1, 10, Inf)
  d <- cut(d100, brks, include.lowest=T)
  pxl_size <- brks[which(d == levels(d))]
  pxl_size <- dplyr::case_match(
    pxl_size,
    -Inf ~  0.01,  # min
    Inf ~ 10.00,  # max
    .default = pxl_size)
  nx <- ceiling( dx / pxl_size )
  ny <- ceiling( dy / pxl_size )

  # create extent template raster
  r_ext <- terra::rast(
    nrows = nx,
    ncols = ny,
    xmin  = b$xmin,
    xmax  = b$xmax,
    ymin  = b$ymin,
    ymax  = b$ymax,
    crs = "EPSG:4326") |>
    # extend by 2 pixels in x & y
    terra::extend(c(2, 2))

  # get matrix of points ---
  # TODO: get points as sf and subselect field
  geom <- sf::st_geometry(pts)
  m_pts <- pts |>
    rename(z = all_of(fld)) |>
    dplyr::mutate(
      x = sf::st_coordinates(geom)[,"X"],
      y = sf::st_coordinates(geom)[,"Y"]) |>
    sf::st_drop_geometry() |>
    dplyr::select(x, y, z) |>
    as.matrix()

  # * interpolate points to raster ----
  # alg <- "invdistnn:power=1.3:smoothing=0.2:radius=2.0:max_points=12:nodata=-9999.0"
  if (verbose)
    message(glue::glue(
      "pxl_size: {pxl_size}\n
       nx: {nx}\n
       ny: {ny}"))

  r_i <- terra::interpIDW(
    x = r_ext, y = m_pts,
    # TODO: make these input arguments to fxn
    radius = radius, power = power, smooth = smooth,
    maxPoints = maxPoints, minPoints = minPoints, near = near, fill= fill)

  # * clip raster ----
  aoi_b <- aoi |>
    sf::st_buffer(pxl_size * 2)
  r <- r_i |>
    terra::mask(aoi_b) |>
    terra::trim()
  names(r) <- fld

  # * write raster (if out_tif) ----
  if (!is.null(out_tif))
    terra::writeRaster(r, out_tif, overwrite = T)

  r
}

#' Raster to contour polygons
#'
#' Contour the input raster (using `stars::st_contour`). Use "pretty" breaks
#' (using `classInt::classIntervals()`). Clip by area of interest.
#'
#' @param r input raster as `terra::rast`
#' @param aoi area of interest as simple features `sf::sf`
#' @param n_brks number of breaks
#'
#' @return contour polygon simple features (`sf::sf`)
#' @importFrom stars st_as_stars st_contour
#' @importFrom sf st_intersection
#' @importFrom classInt classIntervals
#' @import dplyr
#' @export
#' @concept analyze
rast_to_contours <- function(r, aoi, n_brks = 8){
  # r = r_k_all; aoi = cc_grid_zones; n_brks = 8

  brks = classInt::classIntervals(
    terra::values(r, na.rm=T), n=n_brks,
    style = "pretty")$brks

  ply <- stars::st_contour(
    stars::st_as_stars(r), breaks = brks) |>
    sf::st_make_valid() |>
    sf::st_set_agr("constant") |>
    sf::st_intersection(
      sf::st_union(aoi) |>
        sf::st_make_valid()) |>
    sf::st_make_valid() |>
    dplyr::mutate(
      z_avg = (Min + Max) / 2 ) |>
    select(
      z_avg,
      z_min   = Min,
      z_max   = Max,
      z_label = 1,
      geom    = geometry)

  ply
}


