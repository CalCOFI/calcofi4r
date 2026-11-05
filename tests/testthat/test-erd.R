test_that("cc_erd() marks every column of a composite primary key as PK", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "CREATE TABLE sample_spatial (root_sample_key VARCHAR, spatial_key VARCHAR, n INTEGER)")
  DBI::dbExecute(con, "CREATE TABLE spatial (spatial_key VARCHAR, name VARCHAR)")
  rels <- list(
    primary_keys = list(sample_spatial = c("root_sample_key", "spatial_key"), spatial = "spatial_key"),
    foreign_keys = list(list(table = "sample_spatial", column = "spatial_key",
                             ref_table = "spatial", ref_column = "spatial_key")))
  erd <- cc_erd(con, tables = c("sample_spatial", "spatial"), rels = rels)
  txt <- unclass(erd)
  expect_match(txt, "root_sample_key PK")
  expect_match(txt, "spatial_key PK,FK")
  expect_match(txt, "int n\\b|n$", perl = TRUE) # the plain column carries no tag
})

test_that("in Quarto, knit_print() hands a cc_erd to Quarto's own mermaid, never the DiagrammeR widget", {
  # regression 2026-11-05: rendered where mmdc is absent (the Mac mini), the
  # DiagrammeR fallback loaded mermaid 0.3, which replaced Quarto's global
  # `mermaid` and blanked every {mermaid} block on the ingest page
  erd <- structure("erDiagram\n    a {\n    }", class = c("cc_erd", "character"))
  old <- knitr::opts_knit$get("quarto.version")
  knitr::opts_knit$set(quarto.version = numeric_version("1.8.0"))
  withr::defer(knitr::opts_knit$set(quarto.version = old))
  local_mocked_bindings(Sys.which = function(...) c(mmdc = "/usr/bin/mmdc"), .package = "base")
  out <- knit_print.cc_erd(erd)
  expect_s3_class(out, "knit_asis")
  expect_identical(as.character(out), "\n\n```{mermaid}\nerDiagram\n    a {\n    }\n```\n\n")
  # the viewer rides along as knit_meta, so Quarto puts its scripts in the page
  meta <- attr(out, "knit_meta")
  expect_length(meta, 1)
  expect_s3_class(meta[[1]], "html_dependency")
  expect_identical(meta[[1]]$name, "cc-diagram-viewer")
})

test_that("cc_diagram_viewer() ships the viewer's three files", {
  dep <- cc_diagram_viewer()
  expect_s3_class(dep, "html_dependency")
  expect_identical(dep$script, c("svg-pan-zoom.min.js", "diagram.js"))
  expect_identical(dep$stylesheet, "diagram.css")
  files <- file.path(dep$src$file, c(dep$script, dep$stylesheet))
  expect_true(all(file.exists(files)))
  # the viewer waits for Quarto's renders and wraps each svg.mermaid-js
  js <- paste(readLines(file.path(dep$src$file, "diagram.js")), collapse = "\n")
  expect_match(js, "pre.mermaid-js", fixed = TRUE)
  expect_match(js, "svg.mermaid-js", fixed = TRUE)
})
