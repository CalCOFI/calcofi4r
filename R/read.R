#' Get variables from CalCOFI API
#'
#' @return data frame with columns:
#' - `category`: category, e.g. "Oceanographic"
#' - `table_field`: unique identifier of the variable given by the form of table.field
#' - `plot_title`: title used in a typical time series plot output
#' - `plot_label`: y-axis label used in a typical time series plot output
#' @export
#' @import httr2 readr
#' @concept read
#' @examples
get_variables <- function(){
  req_vars <- req_perform(request("https://api.calcofi.io/variables"))
  stopifnot(!req_vars %>% httr2::resp_is_error())
  req_vars %>% resp_body_raw() %>% read_csv()
}

#' Get timeseries summary from CalCOFI API
#'
#' @param variable Variable to fetch from the CalCOFI API. One of `table_field` values from `get_variables()`.
#'   Default is `"ctdcast_bottle.t_deg_c"`.
#' @param aoi_wkt Area of interest (AOI), spatially described as
#'   [well-known text (WKT)](https://en.wikipedia.org/wiki/Well-known_text_representation_of_geometry).
#' @param depth_m_min Minimum depth range in meters, e.g. 0. Default is `NULL`, as in not filtered.
#' @param depth_m_max Maximum depth range in meters, e.g. 5351. Default is `NULL`, as in not filtered.
#' @param date_beg Beginning of date range, e.g."1949-02-28". Default is `NULL`, as in not filtered.
#' @param date_end End of date range, e.g. "2020-01-26". Default is `NULL`, as in not filtered.
#' @param time_step Time step over which to summarize. One of: a sequential
#'   increment ("decade","year","year.quarter","year.month","year.week","date"),
#'   or a climatology ("quarter","month","week","julianday","hour").
#'   Default is `"year"`.
#' @param stats Statistics to show per `date_step`. Acceptable values include any combination of:
#'   "mean", "median", "min", "max", "sd" or "p#"
#'   where "sd" is the standard deviation and
#'   "p#" represents the percentile value 0 to 100 within available range of values.
#'   Default is `c("mean", "sd")`.
#' @concept read
#' @return
#' @export
#'
#' @examples
get_timeseries <- function(
  variable = "ctdcast_bottle.t_deg_c",
  aoi_wkt = NULL,
  depth_m_min = NULL, depth_m_max = NULL,
  date_beg = NULL, date_end = NULL,
  time_step = "year",
  stats = c("mean", "sd")){

  req <- request("https://api.calcofi.io")
  resp <- try(req %>%
    req_url_path_append("timeseries") %>%
    req_url_query(
      variable    = variable,
      aoi_wkt     = aoi_wkt,
      depth_m_min = depth_m_min,
      depth_m_max = depth_m_max,
      date_beg    = date_beg,
      date_end    = date_end,
      time_step   = time_step,
      stats       = paste(stats, collapse=", ")) %>%
    req_perform())

  if ("try-error" %in% class(resp)){
    last_response() %>%
      resp_body_json() %>%
      .$message %>%
      stop()
  }
  resp %>%
    resp_body_raw() %>%
    read_csv(show_col_types = F)
}
