# cc_datasets() / cc_dataset_page_url() — the dataset-catalog record's read side (plan
# 2026-09-05, WS-P2). Fixture shared byte-for-byte with calcofi4py
# (tests/fixtures/datasets_sample.json).

test_that("cc_dataset_page_url() builds the page URL from the key, never a lookup table", {
  expect_identical(cc_dataset_page_url("calcofi_bottle"), "https://calcofi.io/datasets/calcofi_bottle/")
  expect_identical(
    cc_dataset_page_url(c("calcofi_dic", "cce-lter_zoodb")),
    c("https://calcofi.io/datasets/calcofi_dic/", "https://calcofi.io/datasets/cce-lter_zoodb/"))
})

test_that("cc_datasets() reads datasets.json into one row per dataset, with list-columns", {
  fx <- test_path("fixtures", "datasets_sample.json")

  ds <- calcofi4r:::.cc_datasets_read(fx, what = "datasets")
  expect_s3_class(ds, "tbl_df")
  expect_equal(nrow(ds), 2)
  expect_identical(ds$dataset_key, c("calcofi_dic", "cce-lter_zoodb"))
  expect_true(is.list(ds$distributions))
  expect_true(is.list(ds$registrations))
  expect_identical(ds$links$page, cc_dataset_page_url(ds$dataset_key))

  holdings <- calcofi4r:::.cc_datasets_read(fx, what = "holdings")
  expect_equal(nrow(holdings), 1)
  expect_identical(holdings$key, "calcofi_prodo")

  ref <- calcofi4r:::.cc_datasets_read(fx, what = "reference")
  expect_equal(nrow(ref), 0)
})

test_that("cc_datasets() builds the URL from version and base_https, then delegates", {
  local_mocked_bindings(
    .cc_resolve_version = function(version = "latest") "v2026.09.05",
    .cc_datasets_read = function(url, what = "datasets", version_hint = url) {
      expect_identical(url, "https://example.org/ducklake/releases/v2026.09.05/datasets.json")
      expect_identical(what, "holdings")
      tibble::tibble(key = "ok")
    })
  out <- cc_datasets(version = "latest", what = "holdings", base_https = "https://example.org")
  expect_identical(out$key, "ok")
})

test_that("cc_datasets() errors naming the version when a release has no datasets.json", {
  expect_error(
    calcofi4r:::.cc_datasets_read("https://storage.googleapis.com/calcofi-db-does-not-exist/x.json",
                                  what = "datasets", version_hint = "v2026.01.01"),
    "v2026.01.01")
})
