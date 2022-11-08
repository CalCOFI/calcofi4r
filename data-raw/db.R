# packages ----
if (!require("librarian")){
  install.packages("librarian")
  library(librarian)
}
librarian::shelf(
  DBI, dbplyr, dplyr, glue, here, purrr, readr, stringr, RPostgres, sf, tibble, tidyr)
options(readr.show_col_types = F)

# database connect ----
db_pass_txt <- "~/.calcofi_db_pass.txt"
stopifnot(file.exists(db_pass_txt))

# stop existing db before connecting to tunneled db
# brew services stop postgresql@14
# brew services start postgresql@14

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname   = "gis",
  host     = "localhost",
  port     = 5432,
  user     = "admin",
  password = readLines(db_pass_txt))
