# Changelog

## calcofi4r 1.3.0

*Dataset-driven ERD coloring (stroke-based)*

- **[`cc_erd()`](https://calcofi.io/calcofi4r/reference/cc_erd.md)
  stroke-only coloring** Entity `classDef`s now color the table outline
  (`stroke`) instead of the fill, so multi-row entities read cleanly.
  The `colors` argument is unchanged.
- **[`cc_erd_color_map()`](https://calcofi.io/calcofi4r/reference/cc_erd_color_map.md)**
  New exported helper that builds the `colors` list for
  [`cc_erd()`](https://calcofi.io/calcofi4r/reference/cc_erd.md) from
  authoritative table → dataset metadata: single-owner tables take their
  dataset color, shared/multi-dataset tables take a neutral color, and
  per-table `overrides` win. Drives the dataset-source coloring on the
  schema site (calcofi.io/schema).

## calcofi4r 1.1.10

*Local data download and table filtering in cc_get_db()*

- **`local_data` parameter** `cc_get_db(local_data = TRUE)` now
  downloads parquet files to a local cache directory and creates
  `TABLE`s instead of remote `VIEW`s. Files are only downloaded if
  missing or if `refresh = TRUE`, making repeated calls idempotent.
- **`tables` parameter**
  `cc_get_db(tables = c("species", "ichthyo", ...))` filters which
  tables to load from the catalog. Useful for excluding large tables
  like CTD data when building app-specific databases.

## calcofi4r 1.1.9

*Native GEOMETRY storage in cc_get_db()*

- **`storage_compatibility_version = 'latest'`**
  [`cc_get_db()`](https://calcofi.io/calcofi4r/reference/cc_get_db.md)
  now uses a named DuckDB driver with `autoload_known_extensions` and
  latest storage format, matching calcofi4db’s `get_duckdb_con()`
  pattern. Ensures native GEOMETRY type is used for spatial queries.

## calcofi4r 1.1.8

*Require DuckDB \>= 1.5.1 for native GEOMETRY*

- **Requires `duckdb >= 1.5.1`** Added minimum version constraint to
  ensure the native built-in GEOMETRY type is available for spatial
  queries.

## calcofi4r 1.1.7

*ERD diagrams render as PNG in Quarto via mermaid-cli*

- **`knit_print.cc_erd()`** now renders Mermaid diagrams to PNG via
  `mmdc` (mermaid-cli) at 2x scale with transparent background, saving
  to [`knitr::fig_path()`](https://rdrr.io/pkg/knitr/man/fig_path.html)
  for proper Quarto HTML output. Lightbox applies automatically via
  `_quarto.yml` settings.
- **Fallback chain**: `mmdc` →
  [`DiagrammeR::mermaid()`](https://rich-iannone.github.io/DiagrammeR/reference/mermaid.html)
  htmlwidget → raw mermaid code block.

## calcofi4r 1.1.6

*New [`cc_tbl()`](https://calcofi.io/calcofi4r/reference/cc_tbl.md)
unified table accessor*

- **[`cc_tbl()`](https://calcofi.io/calcofi4r/reference/cc_tbl.md)** New
  function providing unified access to any CalCOFI database table.
  Returns lazy
  [`dplyr::tbl()`](https://dplyr.tidyverse.org/reference/tbl.html) for
  non-spatial tables, `sf` object for spatial tables (via DuckDB
  `EXCLUDE` + `ST_AsWKB`), and pivoted-wide `sf` for `_spatial` table
  (requires `layer` argument). Optional `geom_col` selects alternate
  geometry columns (e.g., `geom_ctr` for grid centroids).

## calcofi4r 1.1.5

- [`cc_erd()`](https://calcofi.io/calcofi4r/reference/cc_erd.md) gains a
  `rels` parameter to accept pre-parsed relationship lists inline (as
  alternative to `rels_path`), matching the `calcofi4db`
  `build_relationships_json()` list format.
- `knit_print.cc_erd()` now outputs raw mermaid code blocks for Quarto
  native rendering (respects `mermaid-format: png` and lightbox settings
  in `_quarto.yml`) instead of rendering via DiagrammeR widget.

## calcofi4r 1.1.4

- Enable reading of partitioned parquet files, eg table
  `ctd_measurement` (15 GB) partitioned by `cruise_key`, in online
  DuckDB for improved performance and scalability.

## calcofi4r 1.1.3

- Fixed database functions:
  [`cc_get_db()`](https://calcofi.io/calcofi4r/reference/cc_get_db.md),
  [`cc_list_versions()`](https://calcofi.io/calcofi4r/reference/cc_list_versions.md)
- Updated vignette “Get started” to using the online DuckDB and latest
  functions.
- Added deprecation warnings to old functions that made API calls to
  Postgres database in favor of new direct data querying abilities with
  the online DuckDB.

## calcofi4r 1.1.2

- Added database functions to make views, starting with `casts_extra`:
  [`cc_make_view()`](https://calcofi.io/calcofi4r/reference/cc_make_view.md),
  [`cc_list_view_templates()`](https://calcofi.io/calcofi4r/reference/cc_list_view_templates.md)

## calcofi4r 1.1.1

- Added
  [`cc_read_sf()`](https://calcofi.io/calcofi4r/reference/cc_read_sf.md)
  to read spatial tables in DuckDB connection as sf objects.

## calcofi4r 1.1.0

- Highlights DuckDB as the primary data access method  
- Quick Start Section - Shows
  [`cc_get_db()`](https://calcofi.io/calcofi4r/reference/cc_get_db.md)
  as the main entry point
- Convenience Functions - Documents the new read functions:
  - [`cc_read_larvae()`](https://calcofi.io/calcofi4r/reference/cc_read_ichthyo.md)  
  - [`cc_read_bottle()`](https://calcofi.io/calcofi4r/reference/cc_read_bottle.md)
  - [`cc_read_cast()`](https://calcofi.io/calcofi4r/reference/cc_read_casts.md)
  - With filter examples using dplyr syntax
- Version Control Section - Shows how to:
  - List versions with
    [`cc_list_versions()`](https://calcofi.io/calcofi4r/reference/cc_list_versions.md)
  - Connect to specific versions
  - View release info and notes
- Custom Queries Section - Documents
  [`cc_query()`](https://calcofi.io/calcofi4r/reference/cc_query.md) and
  [`cc_describe_table()`](https://calcofi.io/calcofi4r/reference/cc_describe_table.md)
- API Functions - Kept the existing API functions as a secondary option

## calcofi4r 0.9.0

- Migrated
  [functions.R](https://github.com/CalCOFI/int-app/blob/0e6cc9bcb236be4073ee21533b59cb74ef496ef3/app/functions.R)
  from [CalCOFI Integrated Assessment Shiny
  App](https://app.calcofi.io/int/) that utilizes a local or remote
  duckdb of CalCOFI data (rather than an API).

- TODO: add `@concept` to functions (to move out of **Other** heading in
  [Reference](https://calcofi.io/calcofi4r/reference/)) and build
  vignette (like [Get
  started](https://calcofi.io/calcofi4r/news/articles/calcofi4r.md))
  demonstrating use of these new functions.

## calcofi4r 0.8.1

- Added
  [`cc_db_catalog()`](https://calcofi.io/calcofi4r/reference/cc_db_catalog.md)
  to list tables and columns in the database with descriptions (possibly
  formatted in markdown) by reading from new CalCOFI API endpoints:
  [api.calcofi.io/db_tables](https://api.calcofi.io/db_tables),
  [api.calcofi.io/db_columns](https://api.calcofi.io/db_columns).

## calcofi4r 0.8.0

- Removed non-ASCII characters to allow install of package on Windows.

- Used
  [`stars::st_as_stars()`](https://r-spatial.github.io/stars/reference/st_as_stars.html)
  to use
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  SpatRaster with
  [`mapview::mapView()`](https://r-spatial.github.io/mapview/reference/mapView.html)
  (vs retired
  [`raster::raster()`](https://rdrr.io/pkg/raster/man/raster.html)).

## calcofi4r 0.7.0

- Added interpolation functions
  [`pts_to_rast_idw()`](https://calcofi.io/calcofi4r/reference/pts_to_rast_idw.md)
  and
  [`rast_to_contours()`](https://calcofi.io/calcofi4r/reference/rast_to_contours.md).
  Renamed `map_contours()` to
  [`pts_to_contours_gam()`](https://calcofi.io/calcofi4r/reference/pts_to_contours_gam.md)
  and moved concept from “visualize” to “analyze”.

- Added to `cc_places`:

  - “NOAA Aquaculture Opportunity Areas”
  - “BOEM Wind Planning Areas”
  - “National Marine Sanctuaries”: “Chumash Proposed Action”

## calcofi4r 0.6.0

- Added database connection functions
  [`cc_db_connect()`](https://calcofi.io/calcofi4r/reference/cc_db_connect.md)
  and
  [`create_index()`](https://calcofi.io/calcofi4r/reference/create_index.md).

## calcofi4r 0.5.5

- Fixed nearshore `cc_grid` to include `sta_pos == 60`. Renamed
  `cc_grid_areas` to `cc_grid_zones` with new categories for `sta_shore`
  (“nearshore” OR “offshore”) and `sta_pattern` (“standard”, “extended”,
  “historical”) per
  [\#4](https://github.com/calcofi/calcofi4r/issues/4). Updated
  `cc_places` to have the 6 combinations of `cc_grid_zones`.

## calcofi4r 0.5.4

- Added `cc_places` with three categories for places: 1) CalCOFI (Core,
  Extended, Nearshore, Offshore); 2) Integrated Ecosystem Assessment
  (California Current); and 3) National Marine Sanctuary (Cordell Bank,
  Channel Islands, Greater Farallones, Monterey Bay, Olympic Coast).
  Augmented `cc_grid*` with missing cells to reduce slivers when
  intersecting.

## calcofi4r 0.5.3

- `cc_grid_area` -\> `cc_grid_areas` data for showing study areas with
  combinations of `sta_dpos` nearshore (5), offshore (10) and outside
  (20); where `area_dpos` can be one of: `"5"`, `"10"`, `"20"`,
  `"5,10"`, `"10,20"` or `"5,10,20"`

## calcofi4r 0.5.2

- Added `cc_grid`, `cc_grid_ctrs` showing the CalCOFI station sampling
  at varying seperation distances of station positions (`sta_pos`) in
  the CalCOFI coordinate system from nearshore (`5`), to offshore (`10`)
  to outside the 113 station extended repeated area (`20`), per [Station
  Positions –
  CalCOFI](https://calcofi.org/sampling-info/station-positions/)}

## calcofi4r 0.5.1

- handle defaults with
  [`plot_depth()`](https://calcofi.io/calcofi4r/reference/plot_depth.md),
  streamline headings in article and descriptions in functions

## calcofi4r 0.5.0

- Added
  [`plot_depth()`](https://calcofi.io/calcofi4r/reference/plot_depth.md)
  and example data `bottle_temp_depth`
- `stations_t_degc` -\> `bottle_temp_lonlat`

## calcofi4r 0.4.1

- rename `tbl_to_contour_ply()` to `map_contours()`

## calcofi4r 0.4.0

- Added `tbl_to_contour_ply()` and example data `area_calcofi_extended`
  `stations_t_degc`

## calcofi4r 0.3.0

- Added
  [`get_cruises()`](https://calcofi.io/calcofi4r/reference/get_cruises.md)
  and
  [`get_raster()`](https://calcofi.io/calcofi4r/reference/get_raster.md)
  for reading from the CalCOFI API.
- Added
  [`map_raster()`](https://calcofi.io/calcofi4r/reference/map_raster.md)
  and
  [`plot_timeseries()`](https://calcofi.io/calcofi4r/reference/plot_timeseries.md)
  for visualizing.
- Added “Get started” vignette and README.Rmd -\> README.md with example
  usage

## calcofi4r 0.2.0

- Added
  [`get_variables()`](https://calcofi.io/calcofi4r/reference/get_variables.md),
  [`get_timeseries()`](https://calcofi.io/calcofi4r/reference/get_timeseries.md)
  for reading from the API at <https://api.calcofi.io>

## calcofi4r 0.1.2

- ∆ order of `stations` columns

## calcofi4r 0.1.1

- Added authors

## calcofi4r 0.1.0

- Bumped version. Have functions for Analyze
  ([`get_oceano_var_aoi()`](https://calcofi.io/calcofi4r/reference/get_oceano_var_aoi.md))
  and Visualize
  ([`plot_timeseries()`](https://calcofi.io/calcofi4r/reference/plot_timeseries.md)),
  plus Data (`bottle`, `dic`, `stations`).

## calcofi4r 0.0.0.9000

- Added a `NEWS.md` file to track changes to the package.
