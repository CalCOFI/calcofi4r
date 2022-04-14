#' Get oceanographic variable for area of interest
#'
#' @param var variable of interest (TODO: see keys)
#' @param aoi area of interest
#' @param date_step eg month, quarter, year
#' @param depth_min depth minimum
#' @param depth_max depth maximum
#'
#' @concept analyze
#' @import glue
#' @return
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

  d <- eval(parse(text = glue("var_lookup$`{var}`$data_source_name"))) %>%
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
  attr(d_aoi_summ, "date_msg")  <- glue("This dataset was summarized by {date_step}.")
  attr(d_aoi_summ, "aoi") <- ifelse(
    empty_data_for_var,
    glue("No data were found for {var} in this area of interest. Summaries were conducted across all existing data points."),
    glue("Data for {var} in selected area of interest")
  )

  d_aoi_summ
}
