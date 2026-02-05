#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecate_warn deprecated
## usethis namespace: end
NULL

# lifecycle badge documentation ----
# the following deprecated functions use lifecycle:
# - get_variables() -> cc_list_measurement_types()
# - get_cruises() -> cc_read_cruise()
# - get_raster() -> cc_get_db() + pts_to_rast_idw()
# - get_timeseries() -> cc_get_db() + dplyr aggregation
# - get_oceano_var_aoi() -> cc_get_db() + cc_read_measurements()
# - cc_db_catalog() -> cc_list_tables() + cc_describe_table()
# - cc_db_connect() -> cc_get_db()
