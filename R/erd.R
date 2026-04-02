#'  Update the mermaid library in the package
#' @param version A [character] of the desired version (the latest by default)
#' @return See value returned by [download.file]
updateMermaid <- function(version = "") {
  url <- "https://cdn.jsdelivr.net/npm/mermaid@version/dist/mermaid.min.js"
  if (version != "") {
    stopifnot(grepl("^[[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+$", version))
    version <- paste0("@", version)
  }
  url <- gsub("@version", version, url)
  try(
    download.file(url,
                  system.file("htmlwidgets/lib/mermaid/dist/mermaid.slim.min.js",
                              package = "DiagrammeR"))
  )
}
# updateMermaid()

# ─── entity relationship diagrams ─────────────────────────────────────────────

#' Generate Mermaid ERD from DuckDB Connection
#'
#' Generates a [Mermaid](https://mermaid.js.org/syntax/entityRelationshipDiagram.html)
#' entity relationship diagram from a DuckDB connection by querying
#' `information_schema.columns`. Unlike `dm::dm_draw()`, this handles
#' `GEOMETRY` columns without errors, so spatial tables like `site`, `grid`,
#' `casts`, and `segment` are included in the diagram.
#'
#' @param con DBI connection to a DuckDB database
#' @param tables Character vector of table names to include. If NULL (default),
#'   all tables in the connection are included.
#' @param exclude Character vector of table names to exclude (default: NULL).
#'   Applied after `tables`.
#' @param rels_path Path to a `relationships.json` file for primary key and
#'   foreign key definitions. If NULL (default), the diagram shows table
#'   structures without relationship lines. Ignored when `rels` is provided.
#' @param rels A list with `primary_keys` (named list: table → column) and
#'   `foreign_keys` (list of lists with `table`, `column`, `ref_table`,
#'   `ref_column`). Alternative to `rels_path` for passing relationships
#'   inline. Takes precedence over `rels_path`.
#' @param colors Named list mapping color names or hex codes to character
#'   vectors of table names. Emitted as Mermaid `classDef`/`class`
#'   directives with a darker auto-generated stroke.
#'   Example: `list(lightblue = c("cruise", "ship"))`.
#' @param layout Layout engine: `"elk"` (default) or `"dagre"`.
#' @param view_type What columns to show: `"all"` (default), `"keys_only"`
#'   (PK/FK columns only), or `"title_only"` (table names, no columns).
#'
#' @return An object of class `"cc_erd"` (inherits `"character"`).
#'   Printing outputs the Mermaid code; in Quarto/RMarkdown it auto-renders
#'   as a Mermaid diagram (respects `mermaid-format` in `_quarto.yml`).
#'
#' @export
#' @concept database
#'
#' @examples
#' \dontrun{
#' con <- cc_get_db()
#'
#' # basic ERD with all tables (including geometry tables)
#' cc_erd(con)
#'
#' # with relationships from frozen release
#' cc_erd(con, rels_path = "data/releases/v2026.03.25/relationships.json")
#'
#' # color-coded groups matching release_database.qmd palette
#' cc_erd(con,
#'   rels_path = "relationships.json",
#'   colors = list(
#'     lightblue   = c("cruise", "ship", "site", "tow", "net"),
#'     lightyellow = c("ichthyo", "species", "lookup", "taxon", "taxa_rank"),
#'     lightgreen  = c("grid", "segment"),
#'     pink        = c("casts", "bottle", "bottle_measurement",
#'                      "cast_condition", "measurement_type"),
#'     lavender    = c("ctd_cast", "ctd_measurement", "ctd_summary"),
#'     lightsalmon = c("dic_sample", "dic_measurement",
#'                      "dic_measurement_summary"),
#'     white       = c("dataset")))
#'
#' # inline relationships (alternative to rels_path)
#' cc_erd(con, rels = list(
#'   primary_keys = list(cruise = "cruise_key", ship = "ship_key"),
#'   foreign_keys = list(
#'     list(table = "cruise", column = "ship_key",
#'          ref_table = "ship", ref_column = "ship_key"))))
#'
#' # compact: show only key columns
#' cc_erd(con, rels_path = "relationships.json", view_type = "keys_only")
#' }
#' @importFrom DBI dbGetQuery dbListTables
#' @importFrom glue glue
cc_erd <- function(
  con,
  tables    = NULL,
  exclude   = NULL,
  rels_path = NULL,
  rels      = NULL,
  colors    = NULL,
  layout    = "elk",
  view_type = "all"
) {
  stopifnot(
    inherits(con, "DBIConnection"),
    view_type %in% c("all", "keys_only", "title_only"),
    layout    %in% c("dagre", "elk")
  )

  # resolve table list ----
  all_tbls <- DBI::dbListTables(con)
  if (is.null(tables)) {
    tables <- all_tbls
  }
  if (!is.null(exclude)) {
    tables <- setdiff(tables, exclude)
  }
  tables <- intersect(tables, all_tbls)

  if (length(tables) == 0) {
    stop("No tables to include in the ERD.")
  }

  # column metadata (information_schema handles GEOMETRY fine) ----
  tbl_in <- paste0("'", tables, "'", collapse = ", ")
  cols <- DBI::dbGetQuery(
    con,
    glue::glue(
      "SELECT table_name, column_name, data_type, ordinal_position
     FROM information_schema.columns
     WHERE table_name IN ({tbl_in})
     ORDER BY table_name, ordinal_position"
    )
  )

  # load relationships ----
  pks <- list()
  fks <- list()
  if (!is.null(rels)) {
    # accept pre-parsed list directly (takes precedence over rels_path)
    if (!is.null(rels$primary_keys)) pks <- rels$primary_keys
    if (!is.null(rels$foreign_keys)) fks <- rels$foreign_keys
  } else if (!is.null(rels_path) && file.exists(rels_path)) {
    rels <- jsonlite::fromJSON(rels_path, simplifyVector = FALSE)
    if (!is.null(rels$primary_keys)) pks <- rels$primary_keys
    if (!is.null(rels$foreign_keys)) fks <- rels$foreign_keys
  }

  # FK lookup set: "table.column" ----
  fk_set <- if (length(fks) > 0) {
    vapply(fks, function(f) paste0(f$table, ".", f$column), character(1))
  } else {
    character(0)
  }

  # assemble mermaid code ----
  lines <- c(
    "---",
    "config:",
    paste0("  layout: ", layout),
    "---",
    "erDiagram")

  for (tbl in tables) {
    tbl_cols <- cols[cols$table_name == tbl, , drop = FALSE]

    if (view_type == "title_only" || nrow(tbl_cols) == 0) {
      lines <- c(lines, paste0("    ", tbl, " {"), "    }")
      next
    }

    # optionally filter to key columns
    if (view_type == "keys_only") {
      pk_col <- pks[[tbl]]
      fk_col_names <- if (length(fks) > 0) {
        vapply(
          Filter(function(f) f$table == tbl, fks),
          function(f) f$column,
          character(1)
        )
      } else {
        character(0)
      }
      keep <- unique(c(pk_col, fk_col_names))
      tbl_cols <- tbl_cols[tbl_cols$column_name %in% keep, , drop = FALSE]
      if (nrow(tbl_cols) == 0) {
        lines <- c(lines, paste0("    ", tbl, " {"), "    }")
        next
      }
    }

    # column lines with PK/FK annotations
    col_lines <- vapply(
      seq_len(nrow(tbl_cols)),
      function(i) {
        cname <- tbl_cols$column_name[i]
        ctype <- .erd_shorten_type(tbl_cols$data_type[i])

        is_pk <- !is.null(pks[[tbl]]) && cname == pks[[tbl]]
        is_fk <- paste0(tbl, ".", cname) %in% fk_set

        tag <- if (is_pk && is_fk) {
          " PK,FK"
        } else if (is_pk) {
          " PK"
        } else if (is_fk) {
          " FK"
        } else {
          ""
        }

        paste0("        ", ctype, " ", cname, tag)
      },
      character(1)
    )

    lines <- c(lines, paste0("    ", tbl, " {"), col_lines, "    }")
  }

  # relationship lines ----
  for (fk in fks) {
    if (fk$table %in% tables && fk$ref_table %in% tables) {
      lines <- c(
        lines,
        paste0(
          "    ",
          fk$ref_table,
          " ||--o{ ",
          fk$table,
          " : \"",
          fk$column,
          "\""
        )
      )
    }
  }

  # classDef + class at bottom (required for ER diagrams) ----
  if (!is.null(colors)) {
    for (color_name in names(colors)) {
      hex    <- .erd_color_to_hex(color_name)
      stroke <- .erd_darken(hex)
      cls    <- gsub("[^a-zA-Z0-9]", "_", color_name)
      lines  <- c(lines, paste0(
        '    classDef ', cls,
        ' fill:', hex, ',stroke:', stroke))
      tbls_in_group <- intersect(colors[[color_name]], tables)
      if (length(tbls_in_group) > 0)
        lines <- c(lines, paste0(
          "    class ", paste(tbls_in_group, collapse = ","), " ", cls))
    }
  }

  mermaid_code <- paste(lines, collapse = "\n")

  structure(
    mermaid_code,
    class  = c("cc_erd", "character"),
    tables = tables
  )
}


#' @export
print.cc_erd <- function(x, ...) {
  cat(unclass(x), "\n")
  invisible(x)
}


#' Plot a cc_erd object as an interactive diagram
#'
#' Renders the Mermaid ERD in the Viewer pane via
#' [DiagrammeR::mermaid()]. Colors are embedded as `themeCSS` in the
#' Mermaid `%%{init}%%` directive and render natively.
#'
#' @param x A `cc_erd` object returned by [cc_erd()]
#' @param ... Ignored
#'
#' @return The [DiagrammeR::mermaid()] htmlwidget (invisibly)
#' @export
plot.cc_erd <- function(x, ...) {
  if (!requireNamespace("DiagrammeR", quietly = TRUE))
    stop(
      "Package 'DiagrammeR' is required for rendering. ",
      "Install with: install.packages('DiagrammeR')")

  widget <- DiagrammeR::mermaid(unclass(x))
  print(widget)
  invisible(widget)
}


#' @exportS3Method knitr::knit_print
knit_print.cc_erd <- function(x, ...) {
  # output raw mermaid code block for Quarto native rendering
  # (respects mermaid-format: png and lightbox settings in _quarto.yml)
  knitr::asis_output(
    paste0("\n\n```mermaid\n", unclass(x), "\n```\n\n")
  )
}


# ─── internal helpers ─────────────────────────────────────────────────────────

# shorten DuckDB data types for ERD display
.erd_shorten_type <- function(dtype) {
  dtype <- tolower(trimws(sub("\\(.*\\)", "", dtype)))
  switch(
    dtype,
    "integer" = "int",
    "bigint" = "bigint",
    "smallint" = "smallint",
    "tinyint" = "tinyint",
    "double" = "double",
    "float" = "float",
    "real" = "float",
    "boolean" = "bool",
    "timestamp with time zone" = "timestamptz",
    "timestamp" = "timestamp",
    "date" = "date",
    "varchar" = "varchar",
    "text" = "text",
    "uuid" = "uuid",
    "geometry" = "geometry",
    "blob" = "blob",
    "hugeint" = "hugeint",
    "decimal" = "decimal",
    "interval" = "interval",
    dtype
  )
}


# convert named color list to table_name -> hex lookup
.erd_build_color_map <- function(colors) {
  out <- list()
  for (color in names(colors)) {
    hex <- .erd_color_to_hex(color)
    for (tbl in colors[[color]]) {
      out[[tbl]] <- hex
    }
  }
  out
}


# darken a hex color by 40% for stroke/outline
.erd_darken <- function(hex) {
  v <- round(grDevices::col2rgb(hex)[, 1] * 0.6)
  grDevices::rgb(v[1], v[2], v[3], maxColorValue = 255)
}


# resolve R color name to hex (pass-through if already hex)
.erd_color_to_hex <- function(color) {
  if (grepl("^#", color)) {
    return(color)
  }
  tryCatch(
    {
      v <- grDevices::col2rgb(color)
      grDevices::rgb(v[1], v[2], v[3], maxColorValue = 255)
    },
    error = function(e) color
  )
}


