test_that("cc_catalog() reads the release under CALCOFI_RELEASE_PREFIX and falls back to the promoted prefix", {
  skip_if_offline()
  withr::local_envvar(CALCOFI_RELEASE_PREFIX = "ducklake-nowhere/releases",
                      CALCOFI_RELEASE_VERSION = "")
  # a pinned historical version is not under the override prefix: read from ducklake/releases
  cat_ <- cc_catalog("v2026.09.06")
  expect_equal(cat_$version, "v2026.09.06")
  withr::local_envvar(CALCOFI_RELEASE_PREFIX = "", CALCOFI_RELEASE_VERSION = "v2026.09.06")
  expect_equal(cc_catalog("latest")$version, "v2026.09.06")
})

test_that("cc_list_versions() carries doi and consolidated even when the manifest does not", {
  skip_if_offline()
  withr::local_envvar(CALCOFI_RELEASE_PREFIX = "ducklake-staging/releases", CALCOFI_RELEASE_VERSION = "")
  v <- cc_list_versions()
  expect_true(all(c("version", "doi", "consolidated", "is_latest") %in% names(v)))
})
