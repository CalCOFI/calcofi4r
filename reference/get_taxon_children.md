# Retrieve Taxon Children from Database

Queries the taxonomy table to find all child taxa of a given taxonID,
using a recursive CTE. Returns a tibble with taxon details and depth
levels.

## Usage

``` r
get_taxon_children(taxonID, con, authority = "worms")
```

## Arguments

- taxonID:

  Character string of the parent taxonID to query

- con:

  DuckDB database connection object

- authority:

  Character string specifying the taxonomic authority (default: "worms")
