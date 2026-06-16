# Create a Derived VIEW in the Database

Creates a SQL VIEW with derived columns on top of base tables. Supports
prebaked templates (e.g., "casts_extra") or custom column definitions
specified as named SQL expressions.

## Usage

``` r
cc_make_view(
  con,
  template = NULL,
  view_name = template,
  base_table = NULL,
  column_definitions = NULL
)
```

## Arguments

- con:

  DBI connection to DuckDB

- template:

  Character. Name of a prebaked view template. Available: "casts_extra".
  If provided, view_name and column_definitions are taken from the
  template (but can be overridden).

- view_name:

  Character. Name for the VIEW. Defaults to the template name if using a
  template.

- base_table:

  Character. Base table name. Required if no template is provided.

- column_definitions:

  Named character vector. Names are new column names, values are DuckDB
  SQL expressions. Appended as `expression AS column_name` to
  `SELECT *, ...`.

## Value

A lazy dbplyr table reference to the created VIEW.
