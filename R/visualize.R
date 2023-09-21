#' Map raster interactively
#'
#' Map raster of interpolated oceanographic variable for a cruise.
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
#' (r_tif <- tempfile(fileext=".tif"))
#'
#' # use second variable from previously fetched v
#' c(v$table_field[2], v$plot_label[2])
#'
#' # fetch interpolated raster from CalCOFI API
#' get_raster(
#'   variable = v$table_field[2],
#'   cruise_id = "2020-01-05-C-33RL",
#'   depth_m_min = 0, depth_m_max = 200,
#'   out_tif = r_tif)
#'
# read raster
#' r <- raster::raster(r_tif)
#'
#' # plot raster
#' map_raster(r, v$plot_label[2])
map_raster <- function(
    r, legend_title = "Temperature (C)"){ # }, color = "red") {

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

#' Plot interactive depth of an oceanographic variable
#'
#' The plot initially has descending depth on y-axis and ascending variable
#' (e.g., Temperature) on x-axis with each line representing a unique CTD cast
#' across each depth recorded. If the plot is interactive, then on hover the CTD
#' cast (uniquely identified by `cast_count`) is highlighted in red and others
#' dimmed in gray.
#'
#' @param df data frame with columns: `cast_count`, `depth_m`, `v`
#' @param interactive whether to render interactive plot; default: `TRUE`
#' @param variable variable (character), should be one of `plot_title` from
#'   `get_variables()`; default: `"Temperature"`
#'
#' @return interactive plot of type `plotly::ggplotly()` or static plot of type
#'   `ggplot2::ggplot()`.
#' @concept visualize
#' @import dplyr ggplot2 stringr
#' @importFrom plotly ggplotly highlight highlight_key
#'
#' @export
#'
#' @examples
#' # plot depth with the station data
#' plot_depth(df = bottle_temp_depth, variable = "Temperature")
plot_depth <- function(
    df,
    variable = "Temperature",
    interactive = TRUE){
  # df = bottle_temp_depth; interactive = TRUE; variable = "Temperature"

  # get variable attributes
  d_var <- get_variables() %>%
    dplyr::filter(plot_title == !!variable)

  # check for single set of variable attributes and required names in df
  stopifnot(nrow(d_var) == 1)
  stopifnot(all(names(df) %in% c("cast_count", "depth_m", "v")))

  # filter for NAs
  df <- df %>%
    dplyr::select(cast_count, depth_m, v) %>%
    dplyr::filter(!is.na(v))

  # get plot parameters from variable
  clr   <- d_var$plot_color
  v_lbl <- d_var$plot_title
  x_lbl <- d_var$plot_label
  y_lbl <- "Depth (m)"

  # if interactive, setup data frame for highlighting
  if (interactive){
    d <- plotly::highlight_key(df, ~cast_count)
  } else {
    d <- df
  }

  # create ggplot of variable with depth
  g <- ggplot2::ggplot(
    data = d,
    ggplot2::aes(
      x     = v,
      y     = depth_m,
      group = cast_count)) +
    ggplot2::geom_line(size = 0.5, alpha = 0.5) +
    ggplot2::scale_y_reverse() +
    ggplot2::labs(
      x     = x_lbl,
      y     = y_lbl,
      title = glue::glue("{v_lbl} with Depth"))

  # if interactive, return plotly, otherwise ggplot
  if (interactive){
    p <- plotly::ggplotly(
      g, tooltip = "cast_count") %>%
      plotly::highlight(
        on      = "plotly_hover",
        off     = "plotly_doubleclick",
        dynamic = T,
        color   = c(NULL, RColorBrewer::brewer.pal(4, "Set1")))

    # Setting the `off` event (i.e., 'plotly_doubleclick')
    # to match the `on` event (i.e., 'plotly_hover'). You can
    # change this default via the `highlight()` function.
    return(p)
  } else {
    return(g)
  }
}

#' Plot interactive time series of an oceanographic variable
#'
#' @param d data frame with data
#' @param fld_avg unquoted name of field containing the average value, e.g. `t_deg_c_avg`
#' @param fld_date unquoted name of field containing the date, e.g. `year`
#' @param fld_sd unquoted name of field containing the standard deviation, e.g. `sd`
#' @param title quoted title, Default is `"Temperature"`.
#' @param y_label quoted label for y-axis. Default is `"Temperature (C)"`.
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
    title = "Temperature", y_label = "Temperature (C)", color = "red") {
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

