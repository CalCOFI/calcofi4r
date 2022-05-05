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
