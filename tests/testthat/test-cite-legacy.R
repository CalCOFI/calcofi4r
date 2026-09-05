# cc_cite() against a `dataset` table frozen BEFORE the attribution contract
# (v2026.08.25, calcofi4db < 3.30.0): the 18 columns it had, none of doi /
# license_url / acknowledgement. Found by WS-A4: the fixed SELECT was a DuckDB
# binder error on every call. Mirrors calcofi4py tests/test_cite.py.

# the same catalog test-cite.R uses (helpers are file-scoped in testthat)
.cite_legacy_catalog <- function() list(
  version = "v2026.09.03", release_date = "2026-09-03", doi = "10.5281/zenodo.99999999",
  citation = paste0(
    "CalCOFI (2026). CalCOFI Integrated Database, release v2026.09.03 [Data set]. ",
    "Scripps Institution of Oceanography, NOAA Fisheries, and California Department of Fish and Wildlife. ",
    "https://doi.org/10.5281/zenodo.99999999"))

.cite_legacy_con <- function() {
  con <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbExecute(con, "
    CREATE TABLE dataset (
      dataset_key VARCHAR, provider VARCHAR, dataset VARCHAR, dataset_name VARCHAR,
      dataset_name_short VARCHAR, category VARCHAR, color VARCHAR, description VARCHAR,
      citation_main VARCHAR, citation_others VARCHAR, link_calcofi_org VARCHAR,
      link_data_source VARCHAR, link_others VARCHAR, tables VARCHAR,
      coverage_temporal VARCHAR, coverage_spatial VARCHAR, license VARCHAR, pi_names VARCHAR)")
  DBI::dbExecute(con, "
    INSERT INTO dataset (dataset_key, provider, dataset, dataset_name, citation_main, license, pi_names) VALUES
      ('calcofi_dic', 'calcofi', 'dic', 'CalCOFI Dissolved Inorganic Carbon Data',
       'Wang, X.J. et al. (2021). CalCOFI Dissolved Inorganic Carbon Data. NOAA National Centers for Environmental Information.',
       'CC-BY-4.0', 'Wang, X.J.; Sutula, M.'),
      ('cce-lter_zoodb', 'cce-lter', 'zoodb', 'Zooplankton biomass and net sampling data (CCE LTER ZooDB)',
       'CCE LTER (2019). Zooplankton biomass and net sampling data. oceaninformatics.ucsd.edu.',
       'custom', 'CCE LTER'),
      ('farallon_bird-mammal', 'farallon', 'bird-mammal', 'Farallon Islands seabird and pinniped census',
       'Point Blue Conservation Science (2020). Farallon Islands seabird and pinniped census.',
       'CC-BY-4.0', 'Point Blue Conservation Science')")
  con
}

test_that("a pre-contract `dataset` table (v2026.08.25's 18 columns) cites without doi / license_url / acknowledgement, not an error", {
  skip_if_not_installed("duckdb")
  con <- .cite_legacy_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  local_mocked_bindings(cc_catalog = function(...) .cite_legacy_catalog())
  flds <- DBI::dbListFields(con, "dataset")
  expect_length(flds, 18L)
  expect_false(any(c("doi", "license_url", "acknowledgement") %in% flds))

  # text: the release citation, then citation_main + its License line, always a
  # trailing Page: line (2026-09-05, plan D-4) — and nothing else the table cannot
  # supply (no DOI: / Acknowledgement: line, no URL after `custom`)
  txt <- cc_cite(con = con, version = "v2026.09.03")
  expect_length(txt, 4)
  expect_identical(txt[[1]], paste0(
    .cite_legacy_catalog()$citation, "\nPage: https://calcofi.io/datasets/release/"))
  expect_identical(attr(txt, "source"), "release")
  expect_identical(txt[[2]], paste0(
    "Wang, X.J. et al. (2021). CalCOFI Dissolved Inorganic Carbon Data. ",
    "NOAA National Centers for Environmental Information.\nLicense: CC-BY-4.0",
    "\nPage: https://calcofi.io/datasets/calcofi_dic/"))
  expect_identical(txt[[3]], paste0(
    "CCE LTER (2019). Zooplankton biomass and net sampling data. oceaninformatics.ucsd.edu.",
    "\nLicense: custom",
    "\nPage: https://calcofi.io/datasets/cce-lter_zoodb/"))
  expect_false(any(grepl("DOI: |Acknowledgement: |License: custom \\(", txt)))

  # bibtex: the dataset entries carry no doi / url field (the release entry keeps its DOI)
  bib  <- cc_cite(con = con, version = "v2026.09.03", format = "bibtex")
  ents <- strsplit(as.character(bib), "\n\n", fixed = TRUE)[[1]]
  expect_length(ents, 4)
  expect_match(ents[2], "^@misc\\{calcofi_dic,")
  expect_false(any(grepl("\n  (doi|url) ", ents[2:4])))
  expect_true(all(grepl("\n  note ", ents[2:4])))

  # csl: no DOI / URL on a dataset item; the note is the license alone
  csl <- cc_cite(con = con, version = "v2026.09.03", format = "csl")
  expect_length(csl, 4)
  expect_null(csl[[2]]$DOI)
  expect_null(csl[[2]]$URL)
  expect_identical(csl[[2]]$note, "License: CC-BY-4.0")
  expect_identical(csl[[3]]$note, "License: custom")

  # a subset and a data frame go through the same path
  expect_length(cc_cite("cce-lter_zoodb", con = con, version = "v2026.09.03"), 2)
  expect_length(cc_cite(data.frame(dataset_key = c("calcofi_dic", "calcofi_dic")), con = con,
                        version = "v2026.09.03"), 2)
})

test_that("a `dataset` table without dataset_key is a clear error, not a binder error", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "CREATE TABLE dataset (dataset VARCHAR, citation_main VARCHAR)")
  local_mocked_bindings(cc_catalog = function(...) .cite_legacy_catalog())
  expect_error(cc_cite(con = con, version = "v2026.09.03"), "no dataset_key column")
})
