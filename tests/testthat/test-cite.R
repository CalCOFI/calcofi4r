# cc_cite() — shared byte-for-byte with calcofi4py (tests/fixtures/cite_*),
# generated from this same synthetic three-dataset `dataset` table + catalog:
# calcofi_dic (DOI + CC-BY-4.0), cce-lter_zoodb (custom license), farallon_bird-mammal
# (acknowledgement, no DOI). See CalCOFI/workflows .claude/plans_todo/2026-09-03 WS-A2.

.cite_test_con <- function() {
  con <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbExecute(con, "
    CREATE TABLE dataset AS SELECT * FROM (VALUES
      ('calcofi_dic', 'CalCOFI Dissolved Inorganic Carbon Data',
       'Wang, X.J. et al. (2021). CalCOFI Dissolved Inorganic Carbon Data. NOAA National Centers for Environmental Information.',
       'CC-BY-4.0', NULL, '10.25921/3w9f-jd72', NULL, 'Wang, X.J.; Sutula, M.'),
      ('cce-lter_zoodb', 'Zooplankton biomass and net sampling data (CCE LTER ZooDB)',
       'CCE LTER (2019). Zooplankton biomass and net sampling data. oceaninformatics.ucsd.edu.',
       'custom', 'https://oceaninformatics.ucsd.edu/zoodb/terms', NULL, NULL, 'CCE LTER'),
      ('farallon_bird-mammal', 'Farallon Islands seabird and pinniped census',
       'Point Blue Conservation Science (2020). Farallon Islands seabird and pinniped census.',
       'CC-BY-4.0', NULL, NULL,
       'Data collected under National Marine Sanctuary permit; please acknowledge Point Blue Conservation Science.',
       'Point Blue Conservation Science')
    ) t(dataset_key, dataset_name, citation_main, license, license_url, doi, acknowledgement, pi_names)")
  con
}

.cite_test_catalog <- function() list(
  version = "v2026.09.03", release_date = "2026-09-03", doi = "10.5281/zenodo.99999999",
  citation = paste0(
    "CalCOFI (2026). CalCOFI Integrated Database, release v2026.09.03 [Data set]. ",
    "Scripps Institution of Oceanography, NOAA Fisheries, and California Department of Fish and Wildlife. ",
    "https://doi.org/10.5281/zenodo.99999999"))

test_that("cc_cite() with x = NULL matches the shared text/bibtex/csl fixtures", {
  skip_if_not_installed("duckdb")
  con <- .cite_test_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  local_mocked_bindings(cc_catalog = function(...) .cite_test_catalog())

  txt <- cc_cite(con = con, version = "v2026.09.03", format = "text")
  fx_txt <- readChar(test_path("fixtures", "cite_text.txt"), 1e5, useBytes = TRUE)
  expect_identical(as.character(txt), strsplit(sub("\n$", "", fx_txt), "\n\n", fixed = TRUE)[[1]])
  expect_identical(attr(txt, "source"), "release")

  bib <- cc_cite(con = con, version = "v2026.09.03", format = "bibtex")
  fx_bib <- readChar(test_path("fixtures", "cite_bibtex.txt"), 1e5, useBytes = TRUE)
  expect_identical(as.character(bib), sub("\n$", "", fx_bib))

  csl <- cc_cite(con = con, version = "v2026.09.03", format = "csl")
  fx_csl <- jsonlite::fromJSON(test_path("fixtures", "cite_csl.json"), simplifyVector = FALSE)
  attr(csl, "source") <- NULL
  expect_identical(csl, fx_csl)
})

test_that("cc_cite() defaults to alphabetical dataset_key order and format = 'text'", {
  skip_if_not_installed("duckdb")
  con <- .cite_test_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  local_mocked_bindings(cc_catalog = function(...) .cite_test_catalog())
  out <- cc_cite(con = con, version = "v2026.09.03")
  # release, then calcofi_dic, cce-lter_zoodb, farallon_bird-mammal (alphabetical)
  expect_length(out, 4)
  expect_match(out[2], "^Wang, X\\.J\\.")
  expect_match(out[3], "^CCE LTER")
  expect_match(out[4], "^Point Blue")
})

test_that("cc_cite() with a character vector cites just those keys, in the order given", {
  skip_if_not_installed("duckdb")
  con <- .cite_test_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  local_mocked_bindings(cc_catalog = function(...) .cite_test_catalog())
  out <- cc_cite(c("farallon_bird-mammal", "calcofi_dic"), con = con, version = "v2026.09.03")
  expect_length(out, 3)
  expect_match(out[2], "^Point Blue")
  expect_match(out[3], "^Wang, X\\.J\\.")
})

test_that("cc_cite() with a data frame uses its distinct dataset_key, first-occurrence order", {
  skip_if_not_installed("duckdb")
  con <- .cite_test_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  local_mocked_bindings(cc_catalog = function(...) .cite_test_catalog())
  df <- data.frame(dataset_key = c("cce-lter_zoodb", "cce-lter_zoodb", "calcofi_dic"))
  out <- cc_cite(df, con = con, version = "v2026.09.03")
  expect_length(out, 3)              # release + 2 distinct keys, not 3 rows + release
  expect_match(out[2], "^CCE LTER")
  expect_match(out[3], "^Wang, X\\.J\\.")

  no_key <- data.frame(x = 1)
  expect_error(cc_cite(no_key, con = con, version = "v2026.09.03"), "dataset_key column")
})

test_that("cc_cite() errors naming an unknown dataset_key", {
  skip_if_not_installed("duckdb")
  con <- .cite_test_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  local_mocked_bindings(cc_catalog = function(...) .cite_test_catalog())
  expect_error(cc_cite("nope_dataset", con = con, version = "v2026.09.03"), "nope_dataset")
  expect_error(cc_cite(c("calcofi_dic", "nope"), con = con, version = "v2026.09.03"), "nope")
})

test_that("a pre-A0 catalog with no `citation` falls back to the computed release wording", {
  skip_if_not_installed("duckdb")
  con <- .cite_test_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  # catalog_legacy.json (v2026.08.14) predates the attribution contract: no
  # citation, no doi — read the same fixture cc_release_sources() tests use
  legacy <- jsonlite::fromJSON(test_path("fixtures", "catalog_legacy.json"), simplifyVector = FALSE)
  local_mocked_bindings(cc_catalog = function(...) legacy)
  out <- cc_cite(character(0), con = con, version = "v2026.08.14", format = "text")
  expect_identical(attr(out, "source"), "computed")
  expect_identical(out[1], paste0(
    "CalCOFI (2026). CalCOFI Integrated Database, release v2026.08.14 [Data set]. ",
    "Scripps Institution of Oceanography, NOAA Fisheries, and California Department of Fish and Wildlife. ",
    "https://calcofi.io/db-schema/?v=v2026.08.14"))
  expect_length(out, 1)  # no datasets requested
})

test_that("cc_cite(format = 'bibtex', resolve = TRUE) tries the DOI's own BibTeX first", {
  skip_if_not_installed("duckdb")
  con <- .cite_test_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  local_mocked_bindings(cc_catalog = function(...) .cite_test_catalog())

  local_mocked_bindings(.cc_cite_doi_bibtex = function(doi) sprintf("@misc{resolved_%s}", doi))
  out <- cc_cite("calcofi_dic", con = con, version = "v2026.09.03", format = "bibtex", resolve = TRUE)
  expect_match(out, "@misc\\{resolved_10\\.25921/3w9f-jd72\\}", fixed = FALSE)

  # a failed resolve falls back to the offline entry rather than erroring or dropping the dataset
  local_mocked_bindings(.cc_cite_doi_bibtex = function(doi) stop("offline"))
  out2 <- cc_cite("calcofi_dic", con = con, version = "v2026.09.03", format = "bibtex", resolve = TRUE)
  expect_match(out2, "@misc\\{calcofi_dic,")

  # resolve = FALSE (default) never calls the network helper
  local_mocked_bindings(.cc_cite_doi_bibtex = function(doi) stop("must not be called"))
  expect_no_error(cc_cite("calcofi_dic", con = con, version = "v2026.09.03", format = "bibtex"))
})
