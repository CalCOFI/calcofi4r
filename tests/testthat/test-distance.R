# cc_distance_km_sql(): the haversine that replaced ST_Distance_Sphere(ST_Point(lon, lat))

.km <- function(con, lon1, lat1, lon2, lat2)
  DBI::dbGetQuery(con, paste("SELECT", cc_distance_km_sql(lon1, lat1, lon2, lat2), "AS km"))$km

test_that("one degree of latitude is 111.2 km, not the 55.6 km lon/lat-swapped ST_Distance_Sphere gave", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb()); withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  expect_equal(.km(con, -120, 34, -120, 35), 2 * pi * 6371.0088 / 360, tolerance = 1e-9)
})

test_that("one degree of longitude shrinks by cos(latitude)", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb()); withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  # haversine along a parallel: 2R asin(cos(lat) sin(dlon/2))
  expect_equal(.km(con, -121, 34, -120, 34),
               2 * 6371.0088 * asin(cos(34 * pi / 180) * sin(0.5 * pi / 180)), tolerance = 1e-9)
  expect_equal(.km(con, -120, 34, -120, 34), 0)
  expect_equal(.km(con, -120, 34, -120, 35), .km(con, -120, 35, -120, 34))
})

test_that("the bio-env match keeps an env cast inside max_dist_km and drops one outside it", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb()); withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  bio <- "SELECT 'b1' AS bio_id, TIMESTAMP '2020-01-15 12:00' AS bio_datetime, -120.0 AS bio_lon, 34.0 AS bio_lat"
  # env 1: 0.1° north (11.1 km); env 2: 0.3° north (33.4 km), which the swapped
  # ST_Distance_Sphere put at 16.7 km and so let through a 20 km window
  env <- "SELECT * FROM (VALUES
    ('e1', 10.0, TIMESTAMP '2020-01-15 13:00', 5.0, -120.0, 34.1),
    ('e2', 20.0, TIMESTAMP '2020-01-15 13:00', 5.0, -120.0, 34.3))
    t(env_id, env_value, env_datetime, env_depth_m, env_lon, env_lat)"
  d <- DBI::dbGetQuery(con, calcofi4r:::.cc_build_match_sql(bio, env, 20, 6, "average"))
  expect_equal(d$n_env, 1)
  expect_equal(d$env_value, 10)
  expect_equal(d$dist_km, 0.1 * 2 * pi * 6371.0088 / 360, tolerance = 1e-9)
  d50 <- DBI::dbGetQuery(con, calcofi4r:::.cc_build_match_sql(bio, env, 50, 6, "average"))
  expect_equal(d50$n_env, 2)
})
