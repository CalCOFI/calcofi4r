# the density expression is one set of bytes in three runtimes: this fixture is shared byte-for-byte
# with calcofi4py/tests/fixtures/density_sql.txt and CalCOFI/explore/sql/density.sql
test_that("cc_density_sql() matches the shared fixture byte for byte", {
  fx <- readChar(test_path("fixtures", "density_sql.txt"), 1e5, useBytes = TRUE)
  expect_identical(cc_density_sql(), sub("\n$", "", fx))
  expect_identical(cc_density_sql(as = FALSE) |> names(), c("density_per_10m2", "density_per_1000m3", "effort_class"))
  expect_match(cc_density_sql("o"), "o\\.measurement_value \\* o\\.std_haul_factor")
})

test_that("cc_density_sql() derives the D8 rule-2 densities and classes", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "obs", data.frame(
    id = 1:9,
    measurement_value = c(10, 10, 10, 10, 5, 7, 3, 4, 8),
    units = c("count", "count", "count", "count", "count/m2", "count/1000m3", "mgC/m2", "numberPerMeterSquared", "count"),
    tow_type = c("C1", "MT", NA, "CB", NA, NA, NA, NA, "PV"),
    std_haul_factor = c(2, 2, NA, 3, NA, NA, NA, NA, 1.5),
    prop_sorted = c(0.5, 0.5, NA, 0, NA, NA, NA, NA, NA),
    volume_sampled_m3 = c(100, 100, NA, NA, NA, NA, NA, NA, 0)))
  r <- DBI::dbGetQuery(con, paste("SELECT id,", cc_density_sql(), "FROM obs ORDER BY id"))
  # 1 oblique C1: count 10, shf 2, 50% sorted -> 40 per 10 m2; volume 100 m3 -> 10/0.5/100*1000 = 200 per 1000 m3
  expect_equal(r$density_per_10m2[1], 40); expect_equal(r$density_per_1000m3[1], 200); expect_equal(r$effort_class[1], "count_with_effort")
  # 2 manta MT: surface tow, never areal; volumetric fine
  expect_true(is.na(r$density_per_10m2[2])); expect_equal(r$density_per_1000m3[2], 200)
  # 3 no effort at all -> raw count, no density
  expect_true(is.na(r$density_per_10m2[3])); expect_true(is.na(r$density_per_1000m3[3])); expect_equal(r$effort_class[3], "raw_count_no_effort")
  # 4 prop_sorted 0 means all sorted (never divide by zero); no volume -> no volumetric
  expect_equal(r$density_per_10m2[4], 30); expect_true(is.na(r$density_per_1000m3[4]))
  # 5 published per m2 -> x10; 6 published per 1000 m3 as is; both density_as_published
  expect_equal(r$density_per_10m2[5], 50); expect_equal(r$density_per_1000m3[6], 7)
  expect_equal(r$effort_class[5:6], c("density_as_published", "density_as_published"))
  # 7 biomass per m2 is not a count density
  expect_true(is.na(r$density_per_10m2[7])); expect_equal(r$effort_class[7], "other_unit")
  # 8 the euphausiid spelling
  expect_equal(r$density_per_10m2[8], 40)
  # 9 volume 0 is not a volume; NULL prop_sorted is all sorted
  expect_equal(r$density_per_10m2[9], 12); expect_true(is.na(r$density_per_1000m3[9]))
})

test_that("the picker defaults follow rule 4 (sardine opens larva · per 10 m2; never largest-n)", {
  p <- data.frame(dataset_key = c("swfsc_ichthyo", "swfsc_ichthyo", "swfsc_cufes"),
                  life_stage = c("larva", "egg", "egg"), n = c(7420, 5906, 49572),
                  n_10m2 = c(6158, 4907, 0), n_1000m3 = c(7420, 5906, 0))
  expect_equal(cc_default_stage(p), "larva")           # most rows WITH effort, not most rows (egg has 55,478)
  expect_equal(cc_default_denominator(p, "larva"), "per_10m2")   # tie on datasets-with-effort -> areal
  expect_equal(cc_default_denominator(p, "egg"), "per_10m2")
  # a manta-only taxon: only the volumetric denominator has effort
  m <- data.frame(dataset_key = "swfsc_ichthyo", life_stage = "larva", n = 100, n_10m2 = 0, n_1000m3 = 100)
  expect_equal(cc_default_denominator(m, "larva"), "per_1000m3")
  # nothing with effort -> raw, labelled as not comparable by the caller
  r <- data.frame(dataset_key = "swfsc_cufes", life_stage = "egg", n = 10, n_10m2 = 0, n_1000m3 = 0)
  expect_equal(cc_default_denominator(r, "egg"), "raw")
  # stage-less datasets (NA) are a stage of their own
  z <- data.frame(dataset_key = "cce-lter_zoodb", life_stage = NA, n = 5, n_10m2 = 5, n_1000m3 = 5)
  expect_true(is.na(cc_default_stage(z))); expect_equal(cc_default_denominator(z, NA), "per_10m2")
})
