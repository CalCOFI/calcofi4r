# The README's examples run. On 2026-09-09 a researcher copied the first example of the
# Python sibling's README and it failed on a column (`datetime_utc`) the release never
# had; this README had drifted further (retired readers, tables gone since the core
# consolidation) because its chunks were `eval = FALSE` and nothing ever executed them.
#
# Now README.Rmd is knit for real, here and in the database release pipeline:
# CalCOFI/workflows test_release.qmd runs this file against a freshly uploaded,
# not-yet-promoted release by setting CALCOFI_RELEASE_VERSION (+ CALCOFI_RELEASE_PREFIX
# on a staging run), so a schema change fails the release before it reaches a reader.
# README.Rmd sets `error = FALSE`, so any failing chunk fails the knit.

readme_rmd <- function() {
  # the source tree when run via devtools::test(); the installed package has no README.Rmd
  for (p in c(testthat::test_path("../../README.Rmd"), file.path(getwd(), "README.Rmd")))
    if (file.exists(p)) return(normalizePath(p))
  NA_character_
}

test_that("README.Rmd knits against the release (every chunk executes)", {
  skip_if_offline("storage.googleapis.com")
  skip_if_not_installed("rmarkdown")
  rmd <- readme_rmd()
  skip_if(is.na(rmd), "README.Rmd not found (installed package, not the source tree)")
  out_dir <- withr::local_tempdir()
  ver <- Sys.getenv("CALCOFI_RELEASE_VERSION", "")
  if (nzchar(ver)) message("knitting README against CALCOFI_RELEASE_VERSION=", ver,
                           " under ", Sys.getenv("CALCOFI_RELEASE_PREFIX", "ducklake/releases"))
  md <- rmarkdown::render(
    rmd, output_dir = out_dir, intermediates_dir = out_dir, knit_root_dir = dirname(rmd),
    quiet = TRUE, envir = new.env(parent = globalenv()))
  expect_true(file.exists(md))
  txt <- readLines(md, warn = FALSE)
  # the knit ran the chunks: outputs are present, and no chunk printed an error
  expect_true(any(grepl("^#> ", txt)))
  expect_false(any(grepl("^#> Error", txt)))
  # the example every reader copies first
  expect_true(any(grepl("date_trunc\\('year', s\\.datetime\\)", txt)))
})

test_that("README.Rmd does not name columns or tables the release no longer has", {
  rmd <- readme_rmd()
  skip_if(is.na(rmd))
  txt <- readLines(rmd, warn = FALSE)
  code <- txt[!grepl("^\\s*#", txt)]
  for (gone in c("datetime_utc", "lat_dec", "lon_dec", "species_id", "bottle_measurement",
                 "FROM casts", "FROM ichthyo", "cc_read_ichthyo(", "cc_read_casts(",
                 "cc_read_bottle(", "cc_read_species("))
    expect_false(any(grepl(gone, code, fixed = TRUE)), info = gone)
})
