# Generate Mermaid ERD from DuckDB Connection

Generates a
[Mermaid](https://mermaid.js.org/syntax/entityRelationshipDiagram.html)
entity relationship diagram from a DuckDB connection by querying
`information_schema.columns`. Unlike
[`dm::dm_draw()`](https://dm.cynkra.com/reference/dm_draw.html), this
handles `GEOMETRY` columns without errors, so spatial tables like
`site`, `grid`, `casts`, and `segment` are included in the diagram.

## Usage

``` r
cc_erd(
  con,
  tables = NULL,
  exclude = NULL,
  rels_path = NULL,
  rels = NULL,
  colors = NULL,
  layout = "elk",
  view_type = "all"
)
```

## Arguments

- con:

  DBI connection to a DuckDB database

- tables:

  Character vector of table names to include. If NULL (default), all
  tables in the connection are included.

- exclude:

  Character vector of table names to exclude (default: NULL). Applied
  after `tables`.

- rels_path:

  Path to a `relationships.json` file for primary key and foreign key
  definitions. If NULL (default), the diagram shows table structures
  without relationship lines. Ignored when `rels` is provided.

- rels:

  A list with `primary_keys` (named list: table → column) and
  `foreign_keys` (list of lists with `table`, `column`, `ref_table`,
  `ref_column`). Alternative to `rels_path` for passing relationships
  inline. Takes precedence over `rels_path`.

- colors:

  Named list mapping color names or hex codes to character vectors of
  table names. Emitted as Mermaid `classDef`/`class` directives that
  color the entity outline (`stroke`) rather than the fill, so multi-row
  entities stay legible. Build this programmatically from table →
  dataset metadata with
  [`cc_erd_color_map()`](https://calcofi.io/calcofi4r/reference/cc_erd_color_map.md).
  Example: `list(lightblue = c("cruise", "ship"))`.

- layout:

  Layout engine: `"elk"` (default) or `"dagre"`.

- view_type:

  What columns to show: `"all"` (default), `"keys_only"` (PK/FK columns
  only), or `"title_only"` (table names, no columns).

## Value

An object of class `"cc_erd"` (inherits `"character"`). Printing outputs
the Mermaid code; in Quarto/RMarkdown it auto-renders as a Mermaid
diagram (respects `mermaid-format` in `_quarto.yml`).

## Examples

``` r
if (FALSE) { # \dontrun{
con <- cc_get_db()

# basic ERD with all tables (including geometry tables)
cc_erd(con)

# with relationships from frozen release
cc_erd(con, rels_path = "data/releases/v2026.03.25/relationships.json")

# color-coded groups matching release_database.qmd palette
cc_erd(con,
  rels_path = "relationships.json",
  colors = list(
    lightblue   = c("cruise", "ship", "site", "tow", "net"),
    lightyellow = c("ichthyo", "species", "lookup", "taxon", "taxa_rank"),
    lightgreen  = c("grid", "segment"),
    pink        = c("casts", "bottle", "bottle_measurement",
                     "cast_condition", "measurement_type"),
    lavender    = c("ctd_cast", "ctd_measurement", "ctd_summary"),
    lightsalmon = c("dic_sample", "dic_measurement",
                     "dic_measurement_summary"),
    white       = c("dataset")))

# inline relationships (alternative to rels_path)
cc_erd(con, rels = list(
  primary_keys = list(cruise = "cruise_key", ship = "ship_key"),
  foreign_keys = list(
    list(table = "cruise", column = "ship_key",
         ref_table = "ship", ref_column = "ship_key"))))

# compact: show only key columns
cc_erd(con, rels_path = "relationships.json", view_type = "keys_only")
} # }
```
