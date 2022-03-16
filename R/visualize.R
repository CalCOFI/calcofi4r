#' Plot timeseries
#'
#' @param d data frame with expected fields (`Date`, `var_q10`, `var_avg`, `var_q90`) from [get_oceano_var_aoi()]
#'
#' @return [dygraphs::dygraph]
#' @concept visualize
#' @export
#'
#' @examples
plot_timeseries <- function(d) {
  x <- d %>% select(
    Date,
    `10% quantile` = var_q10,
    `Average`      = var_avg,
    `90% quantile` = var_q90)
  var_attrs <- tibble(
    title = attributes(d)$labels$var_title,
    var   = attributes(d)$labels$var_label)
  xts::xts(x = x %>% select(-Date), order.by = x %>% pull(Date)) %>%
    dygraph(
      main = var_attrs$title,
      xlab = "Date", ylab = var_attrs$var) %>% # ...) %>%
    dySeries(
      c("10% quantile", "Average", "90% quantile"),
      label = var_attrs$var, color = "Red") %>%
    dyRangeSelector(fillColor = "#FFFFFF", strokeColor = "#FFFFFF")
}
