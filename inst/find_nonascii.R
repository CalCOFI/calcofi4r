librarian::shelf(
  fs, glue, here, stringr)

files <- fs::dir_ls(
  path    = here(),
  recurse = TRUE,
  type    = "file",
  glob    = "*.R")

f_out <- here("inst/find_nonascii_results.txt")
if (file_exists(f_out)) file_delete(f_out)

fc_out <- file(f_out, "a")

for (f in files){ # f = files[1]
  message(glue("f: {f}"))

  f_tmp <- tempfile(fileext = ".txt")
  if (file_exists(f_tmp)) file_delete(f_tmp)

  fc_tmp <- file(f_tmp, "w")
  sink(file = fc_tmp, type = "message")
  tools::showNonASCIIfile(f)
  sink()
  # close(fc_tmp)
  lns <- readLines(f_tmp)
  if (length(lns) > 0){
    # browser()
    writeLines(
      c(
        glue("{str_replace(f, here(), '')} ----"),
        lns,
        "\n"),
      fc_out)
  }
  unlink(f_tmp)
}

close(fc_out)
readLines(f_out)

