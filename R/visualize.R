#' Plot interactive time series of an oceanographic variable
#'
#' @param d data frame with data
#' @param fld_avg unquoted name of field containing the average value, e.g. `t_deg_c_avg`
#' @param fld_date unquoted name of field containing the date, e.g. `year`
#' @param fld_sd unquoted name of field containing the standard deviation, e.g. `sd`
#' @param title quoted title, Default is `"Temperature"`.
#' @param y_label quoted label for y-axis. Default is `"Temperature (ºC)"`.
#' @param color quoted color. Should be one of `grDevices::colors()`. Default is `"red"`.
#'
#' @return interactive plot of `dygraphs::dygraph()`
#' @concept visualize
#' @import dplyr dygraphs stringr
#' @export
#'
#' @examples
#' # get variables
#' (v <- get_variables())
#'
#' # get data for the first variable
#' (d <- get_timeseries(v$table_field[1]))
#'
#' # plot time series with the first variable
#' with(v[1,],
#'   plot_timeseries(
#'     # data and columns (from d)
#'     d, year, t_deg_c_avg, t_deg_c_sd,
#'     # plot attributes (from v)
#'     plot_title, plot_label, plot_color))
plot_timeseries <- function(
    d, fld_date, fld_avg, fld_sd,
    title = "Temperature", y_label = "Temperature (ºC)", color = "red") {
  select <- dplyr::select

  x_label <- select(d, {{ fld_date }}) %>% names() %>% stringr::str_to_sentence()
  x <- d %>%
    dplyr::mutate(
      `-sd` = {{ fld_avg }} - {{ fld_sd }},
      `+sd` = {{ fld_avg }} + {{ fld_sd }}) %>%
    select(
      {{ fld_date }}, `-sd`, `avg` = {{ fld_avg }}, `+sd`)
  # var_attrs <- tibble(
  #   title = attributes(d)$labels$var_title,
  #   var   = attributes(d)$labels$var_label)
  # xts::xts(
  #   x        = select(x, -{{ fld_date }}),
  #   order.by = pull(x, {{ fld_date }})) %>%
  # browser()
  x %>%
    dygraphs::dygraph(
      # main = var_attrs$title,
      main = title,
      xlab = x_label, ylab = y_label) %>% # ...) %>%
    dygraphs::dySeries(
      c("-sd", "avg", "+sd"),
      label = y_label, color = color) # %>%
    # dygraphs::dyRangeSelector(fillColor = "#FFFFFF", strokeColor = "#FFFFFF")
}

#' Map raster
#'
#' @param r raster of type `raster::raster()`
#' @param legend_title title for legend of variable mapped
#'
#' @return interactive plot of `leaflet::leaflet()`
#' @concept visualize
#' @import dplyr leaflet
#' @importFrom sf st_bbox
#' @importFrom raster extent projectExtent values
#' @export
#'
#' @examples
#' # get variables
#' (v <- get_variables())
#'
#' # get data for the first variable
#' (d <- get_timeseries(v$table_field[1]))
#'
#' # plot time series with the first variable
#' with(v[1,],
#'   plot_timeseries(
#'     # data and columns (from d)
#'     d, year, t_deg_c_avg, t_deg_c_sd,
#'     # plot attributes (from v)
#'     plot_title, plot_label, plot_color))
map_raster <- function(
  r, legend_title = "Temperature (ºC)"){ # }, color = "red") {

  # legend_title = v$plot_label[2]

  b <- sf::st_bbox(raster::extent(raster::projectExtent(r, crs = 4326)))
  r_v <- raster::values(r)
  pal <- colorNumeric("Spectral", r_v, na.color = NA)

  leaflet() %>%
    addProviderTiles(
      providers$Stamen.TonerLite,
      options = providerTileOptions(noWrap = TRUE)) %>%
      addRasterImage(
        r, project = F,
        colors = pal, opacity=0.7) %>%
      # TODO: add log/log10 option
      addLegend(
        pal = pal, values = r_v,
        title = legend_title) %>%
      flyToBounds(b[['xmin']], b[['ymin']], b[['xmax']], b[['ymax']])
}

#' Table to Contour Map
#'
#' Fit generalized additive model to a variable in space using a smoother on
#' latitude & longitude. Further clip by bouding polygon.
#'
#' @param df data frame containing the fields: `lon` (longitude), `lat`
#'   (latitude) and `v` (value)
#' @param ply polygon describing the study area mask
#' @param k number of knots to use on the spatial smoother `s(lon, lat)`;
#'   default: `60`
#' @param k cell width in decimal degrees to run interpolation; default: `0.1`
#'
#' @return
#' @export
#'
#' @examples
#' v_ply <- tbl_to_contour_ply(stations_t_degc, area_calcofi_extended)
#' mapview::mapView(v_ply, zcol="v", layer.name="temp(ºC)")
tbl_to_contour_ply <- function(df, ply, k=60, cw=0.1){

  # df=stations_t_degc; ply=area_calcofi_extended; k=60; cw=0.1

  # check column names in data frame
  stopifnot(all(c("lon", "lat", "v") %in% names(df)))

  # check geographic projection of input polygon boundary
  if(sf::st_crs(ply) != sf::st_crs(4326)){
    warning(glue::glue("The input parameter `ply` to function `tbl_to_contour_ply()` is not exactly geographic coordinate ref system (4326), so setting."))
    sf::st_crs(ply) = 4326
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
    v ~ s(lon, lat, k = k),
    data = df, method = "REML")

  # make regularized grid of points
  g <- sf::st_make_grid(
    pts, cellsize = c(cw, cw), what = "centers") %>%
    sf::st_as_sf() %>%
    rename(geom = x) %>%
    cbind(., sf::st_coordinates(.)) %>%
    dplyr::rename(lon = X, lat = Y) # %>%
    # dplyr::mutate(
    #   in_ply = sf::st_intersects(ply, geom, sparse = F)[1,])
  g$in_ply <- sf::st_intersects(ply, g, sparse = F)[1,]
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

  # determine breaks, pad lower value
  brks <- scales::extended_breaks(5)(v)
  brks <- c(brks[1]-diff(brks)[1], brks)

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

  # clip land
  sf::st_agr(b_sf) = "constant"
  b_sf <- sf::st_intersection(b_sf, ply)
  # mapview::mapView(b_sf, zcol = "v")

  b_sf
}


