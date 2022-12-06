#' Connect to the CalCOFI PostgreSQL database (Admin only)
#'
#' Note that you must either be running this from the CalCOFI server or have a
#' [tunnelled SSH
#' connection](https://github.com/calcofi/server#ssh-tunnel-connection-to-postgis-db)
#' with a user account on the server and the password in a file located at
#' `"~/.calcofi_db_pass.txt"`.
#'
#' @param path_pw path to password file with one line containing the database
#'   password; default: `"~/.calcofi_db_pass.txt"`
#'
#' @return a `DBI::dbConnect()` object
#' @import DBI
#' @importFrom RPostgres Postgres
#' @export
#' @concept database
#'
#' @examples
#' \dontrun{
#' con <- cc_db_connect()
#' DBI::dbListTables(con)
#' }
cc_db_connect <- function(path_pw = "~/.calcofi_db_pass.txt"){

  is_server <- Sys.info()[["sysname"]] == "Linux"
  host <- ifelse(
    is_server,
    "postgis",   # from rstudio to postgis docker container on server
    "localhost") # from laptop to locally tunneled connection to db container on server
  # for localhost db, see: https://github.com/calcofi/server#ssh-tunnel-connection-to-postgis-db

  # database connect ----
  stopifnot(file.exists(path_pw))

  DBI::dbConnect(
    RPostgres::Postgres(),
    dbname   = "gis",
    host     = host,
    port     = 5432,
    user     = "admin",
    password = readLines(path_pw))
}

#' Create index in database
#'
#' @param con database connection object from `DBI::dbConnect()`, e.g. from `cc_db_connect()`
#' @param tbl table name as character
#' @param flds character vector of fields in table used for index
#' @param is_geom logical (default: FALSE) whether geometry field, so create GIST() index
#' @param is_unique logical (default: FALSE) whether to impose a unique constraint, to prevent duplicates; default: FALSE
#' @param overwrite logical (default: FALSE) whether to overwrite existing index
#' @param show logical (default: FALSE) whether to show SQL statement
#' @param exec logical (default: TRUE) whether to execute SQL statement
#'
#' @return nothing
#' @import DBI
#' @importFrom glue glue
#' @export
#' @concept database
#'
#' @examples
#' \dontrun{
#' con <- cc_db_connect()
#' create_index(con, "ctd_casts", "geom", is_geom=T)
#' create_index(con, "ctd_casts", "cast_count", is_unique=T)
#' }
create_index <- function(con, tbl, flds, is_geom=F, is_unique=F, overwrite=F, show=F, exec=T){
  # tbl = "taxa"; flds = c("tbl_orig", "aphia_id"); is_unique = T; is_geom=F
  stopifnot(!(is_geom == T & length(flds) != 1))
  sfx <- ifelse(
    is_geom,
    glue::glue(" USING GIST ({flds})"),
    glue::glue("({paste(flds, collapse=', ')})"))
  idx <- glue("{tbl}_{paste(flds, collapse='_')}_idx")

  if (overwrite)
    dbSendQuery(con, glue("DROP INDEX IF EXISTS {idx}"))

  sql <- glue::glue(
    "CREATE {ifelse(is_unique, 'UNIQUE','')} INDEX IF NOT EXISTS {idx} ON {tbl}{sfx}")
  if (show)
    message(sql)
  if (exec)
    DBI::dbSendQuery(con, sql)
}

