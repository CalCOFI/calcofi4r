# calcofi4r 1.4.2

*One GA4 snippet for apps that do not depend on calcofi4r*

- **`cc_ga_html()`** New: writes [cc_ga_js()]'s snippet to a standalone HTML file, so an app can keep `includeHTML("google-analytics.html")` and gain nothing at runtime. Loading the calcofi4r namespace costs ~4 s of cold start (sf/terra/stars come with it), which a small app should not pay just to be counted. The file carries a banner spelling out the exact regeneration command — these files live in several repos and must not be hand-patched apart. Static by nature, so the Sheet leg and the per-request `ip` are off; GA4 is unaffected, since gtag resolves the client IP in the browser.

# calcofi4r 1.4.1

*Log the real client IP, not the proxy*

- **`cc_ga_js()` / `cc_ga_head()` gain `ip`** Stamps a client IP on every logged row from the **page** request. Behind shiny-server this is the only place a real one exists: shiny-server does not proxy the websocket upgrade — it opens a fresh localhost connection to the R worker — so `session$request` has no `X-Forwarded-For` and `REMOTE_ADDR` is always `127.0.0.1`, no matter how correctly the reverse proxy is configured. Make the app's `ui` a `function(req)` and pass `ip = cc_client_ip(req)`.
- **`cc_client_ip()` accepts a `ui(req)`** as well as a `session` — the argument is now `x`, and the request fields are read directly when there is no `$request`.
- **`cc_track_session()`'s IP is now a fallback**, not an override: the client keeps the page-supplied address rather than letting the session's `127.0.0.1` clobber it.

# calcofi4r 1.4.0

*Non-blocking usage analytics for the Shiny apps*

- **`cc_track()`** New: send a usage event from the Shiny server to the browser over the session's existing websocket. Makes **no HTTP request**, so instrumenting a hot control can never stall a reactive — replacing the synchronous `httr2::req_perform()` per query that `db-viz-hex` used to run on every filter submit and download.
- **`cc_track_query()`** New: wrap a query expression to record its row count, duration, and any error (`n_rows` / `ms` / `status` / `error` get their own Sheet columns, so they stay numeric and chartable). The result — including a lazy `dbplyr` table — passes through untouched, and an error is re-raised after being logged.
- **`cc_ga_head()` / `cc_ga_js()`** New: the one `<head>` snippet every CalCOFI app installs. Emits GA4 events for aggregate behavior and beacons full-cardinality detail to a Google Sheet, batched (10 events / 15 s / page-hide) via `navigator.sendBeacon()`. Also defines `window.ccTrack()` for pure UI events that never need to reach R.
- **`cc_track_session()`, `cc_client_ip()`** New: hand the browser the client IP and Shiny session token, which JavaScript cannot read, so the log's `ip`/`session` columns survive the move to a browser-sent beacon.
- **`cc_log_header()`, `cc_apps_script()`** New: the Sheet's column order and the generated `Code.gs` that appends a whole batch in one `setValues()` call — kept in one place so the Sheet, the Apps Script, and the client payload cannot drift.
- **testthat suite** The package now has tests (`devtools::test()`), starting with 18 for the analytics module.

# calcofi4r 1.3.0

*Dataset-driven ERD coloring (stroke-based)*

- **`cc_erd()` stroke-only coloring** Entity `classDef`s now color the table outline (`stroke`) instead of the fill, so multi-row entities read cleanly. The `colors` argument is unchanged.
- **`cc_erd_color_map()`** New exported helper that builds the `colors` list for `cc_erd()` from authoritative table → dataset metadata: single-owner tables take their dataset color, shared/multi-dataset tables take a neutral color, and per-table `overrides` win. Drives the dataset-source coloring on the schema site (calcofi.io/schema).

# calcofi4r 1.1.10

*Local data download and table filtering in cc_get_db()*

- **`local_data` parameter** `cc_get_db(local_data = TRUE)` now downloads parquet files to a local cache directory and creates `TABLE`s instead of remote `VIEW`s. Files are only downloaded if missing or if `refresh = TRUE`, making repeated calls idempotent.
- **`tables` parameter** `cc_get_db(tables = c("species", "ichthyo", ...))` filters which tables to load from the catalog. Useful for excluding large tables like CTD data when building app-specific databases.

# calcofi4r 1.1.9

*Native GEOMETRY storage in cc_get_db()*

- **`storage_compatibility_version = 'latest'`** `cc_get_db()` now uses a named DuckDB driver with `autoload_known_extensions` and latest storage format, matching calcofi4db's `get_duckdb_con()` pattern. Ensures native GEOMETRY type is used for spatial queries.

# calcofi4r 1.1.8

*Require DuckDB >= 1.5.1 for native GEOMETRY*

- **Requires `duckdb >= 1.5.1`** Added minimum version constraint to ensure the native built-in GEOMETRY type is available for spatial queries.

# calcofi4r 1.1.7

*ERD diagrams render as PNG in Quarto via mermaid-cli*

- **`knit_print.cc_erd()`** now renders Mermaid diagrams to PNG via `mmdc` (mermaid-cli) at 2x scale with transparent background, saving to `knitr::fig_path()` for proper Quarto HTML output. Lightbox applies automatically via `_quarto.yml` settings.
- **Fallback chain**: `mmdc` → `DiagrammeR::mermaid()` htmlwidget → raw mermaid code block.

# calcofi4r 1.1.6

*New `cc_tbl()` unified table accessor*

- **`cc_tbl()`** New function providing unified access to any CalCOFI database table. Returns lazy `dplyr::tbl()` for non-spatial tables, `sf` object for spatial tables (via DuckDB `EXCLUDE` + `ST_AsWKB`), and pivoted-wide `sf` for `_spatial` table (requires `layer` argument). Optional `geom_col` selects alternate geometry columns (e.g., `geom_ctr` for grid centroids).

# calcofi4r 1.1.5

* `cc_erd()` gains a `rels` parameter to accept pre-parsed relationship lists
  inline (as alternative to `rels_path`), matching the `calcofi4db` 
  `build_relationships_json()` list format.
* `knit_print.cc_erd()` now outputs raw mermaid code blocks for Quarto native
  rendering (respects `mermaid-format: png` and lightbox settings in 
  `_quarto.yml`) instead of rendering via DiagrammeR widget.

# calcofi4r 1.1.4

* Enable reading of partitioned parquet files, eg table `ctd_measurement` (15 GB) 
  partitioned by `cruise_key`, in online DuckDB for improved performance and 
  scalability.

# calcofi4r 1.1.3

* Fixed database functions: `cc_get_db()`, `cc_list_versions()`
* Updated vignette "Get started" to using the online DuckDB and latest functions. 
* Added deprecation warnings to old functions that made API calls to Postgres database in favor of new direct data querying abilities with the online DuckDB.

# calcofi4r 1.1.2

* Added database functions to make views, starting with `casts_extra`: `cc_make_view()`, `cc_list_view_templates()`

# calcofi4r 1.1.1

* Added `cc_read_sf()` to read spatial tables in DuckDB connection as sf objects.

# calcofi4r 1.1.0

* Highlights DuckDB as the primary data access method                                                                                                                                 
* Quick Start Section - Shows `cc_get_db()` as the main entry point
* Convenience Functions - Documents the new read functions:                                                                                                                                                  
  - `cc_read_larvae()`                                                                                                                                                                                        
  - `cc_read_bottle()`
  - `cc_read_cast()`
  - With filter examples using dplyr syntax
* Version Control Section - Shows how to:
  - List versions with `cc_list_versions()`
  - Connect to specific versions
  - View release info and notes
* Custom Queries Section - Documents `cc_query()` and `cc_describe_table()`
* API Functions - Kept the existing API functions as a secondary option

# calcofi4r 0.9.0

* Migrated [functions.R](https://github.com/CalCOFI/int-app/blob/0e6cc9bcb236be4073ee21533b59cb74ef496ef3/app/functions.R) from [CalCOFI Integrated Assessment Shiny App](https://app.calcofi.io/int/) that utilizes a local or remote duckdb of CalCOFI data (rather than an API).

* TODO: add `@concept` to functions (to move out of **Other** heading in [Reference](../reference/)) and build vignette (like [Get started](articles/calcofi4r.html)) demonstrating use of these new functions.

# calcofi4r 0.8.1

* Added `cc_db_catalog()` to list tables and columns in the database with descriptions (possibly formatted in markdown) by reading from new CalCOFI API endpoints: [api.calcofi.io/db_tables](https://api.calcofi.io/db_tables), [api.calcofi.io/db_columns](https://api.calcofi.io/db_columns).

# calcofi4r 0.8.0

* Removed non-ASCII characters to allow install of package on Windows.

* Used `stars::st_as_stars()` to use `terra::rast()` SpatRaster with `mapview::mapView()` (vs retired `raster::raster()`).

# calcofi4r 0.7.0

* Added interpolation functions `pts_to_rast_idw()` and `rast_to_contours()`. Renamed `map_contours()` to `pts_to_contours_gam()` and moved concept from "visualize" to "analyze".

* Added to `cc_places`: 
  - "NOAA Aquaculture Opportunity Areas"
  - "BOEM Wind Planning Areas"
  - "National Marine Sanctuaries": "Chumash Proposed Action"

# calcofi4r 0.6.0

* Added database connection functions `cc_db_connect()` and `create_index()`.

# calcofi4r 0.5.5

* Fixed nearshore `cc_grid` to include `sta_pos == 60`. Renamed `cc_grid_areas` to `cc_grid_zones` with new categories for `sta_shore` ("nearshore" OR "offshore") and `sta_pattern` ("standard", "extended", "historical") per #4. Updated `cc_places` to have the 6 combinations of `cc_grid_zones`.

# calcofi4r 0.5.4

* Added `cc_places` with three categories for places: 1) CalCOFI (Core, Extended, Nearshore, Offshore); 2) Integrated Ecosystem Assessment (California Current); and 3) National Marine Sanctuary (Cordell Bank, Channel Islands, Greater Farallones, Monterey Bay, Olympic Coast). Augmented `cc_grid*` with missing cells to reduce slivers when intersecting.

# calcofi4r 0.5.3

* `cc_grid_area` -> `cc_grid_areas` data for showing study areas with combinations of `sta_dpos` nearshore (5), offshore (10) and outside (20); where `area_dpos` can be one of: `"5"`, `"10"`, `"20"`, `"5,10"`, `"10,20"` or `"5,10,20"`

# calcofi4r 0.5.2

* Added `cc_grid`, `cc_grid_ctrs` showing the CalCOFI station sampling at varying seperation distances of station positions (`sta_pos`) in the CalCOFI coordinate system from nearshore (`5`), to offshore (`10`) to outside the 113 station extended repeated area (`20`), per [Station Positions – CalCOFI](https://calcofi.org/sampling-info/station-positions/)}

# calcofi4r 0.5.1

* handle defaults with `plot_depth()`, streamline headings in article and descriptions in functions

# calcofi4r 0.5.0

* Added `plot_depth()` and example data `bottle_temp_depth`
* `stations_t_degc` -> `bottle_temp_lonlat`

# calcofi4r 0.4.1

* rename `tbl_to_contour_ply()` to `map_contours()`

# calcofi4r 0.4.0

* Added `tbl_to_contour_ply()` and example data `area_calcofi_extended` `stations_t_degc`

# calcofi4r 0.3.0

* Added `get_cruises()` and `get_raster()` for reading from the CalCOFI API.
* Added `map_raster()` and `plot_timeseries()` for visualizing.
* Added "Get started" vignette and README.Rmd -> README.md with example usage

# calcofi4r 0.2.0

* Added `get_variables()`, `get_timeseries()` for reading from the API at https://api.calcofi.io

# calcofi4r 0.1.2

* ∆ order of `stations` columns

# calcofi4r 0.1.1

* Added authors

# calcofi4r 0.1.0

* Bumped version. Have functions for Analyze (`get_oceano_var_aoi()`) and Visualize (`plot_timeseries()`), plus Data (`bottle`, `dic`, `stations`).

# calcofi4r 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
