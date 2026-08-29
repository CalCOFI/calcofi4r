# Package index

## Read

functions reading data, particularly from the CalCOFI API

- [`cc_describe_table()`](https://calcofi.io/calcofi4r/reference/cc_describe_table.md)
  : Describe a CalCOFI database table

- [`cc_list_measurement_types()`](https://calcofi.io/calcofi4r/reference/cc_list_measurement_types.md)
  : List available measurement types

- [`cc_list_tables()`](https://calcofi.io/calcofi4r/reference/cc_list_tables.md)
  : List tables in CalCOFI database

- [`cc_query()`](https://calcofi.io/calcofi4r/reference/cc_query.md) :
  Execute SQL query on CalCOFI database

- [`cc_read_casts()`](https://calcofi.io/calcofi4r/reference/cc_read_casts.md)
  [`cc_read_cast()`](https://calcofi.io/calcofi4r/reference/cc_read_casts.md)
  : Read CalCOFI cast data

- [`cc_read_cruise()`](https://calcofi.io/calcofi4r/reference/cc_read_cruise.md)
  : Read CalCOFI cruise data

- [`cc_read_ctd_full()`](https://calcofi.io/calcofi4r/reference/cc_read_ctd_full.md)
  :

  Read full-resolution CTD scans (`obs_ctd_full`, supplemental)

- [`cc_read_ichthyo()`](https://calcofi.io/calcofi4r/reference/cc_read_ichthyo.md)
  [`cc_read_larvae()`](https://calcofi.io/calcofi4r/reference/cc_read_ichthyo.md)
  : Read CalCOFI ichthyoplankton (larvae) data

- [`cc_read_measurements()`](https://calcofi.io/calcofi4r/reference/cc_read_measurements.md)
  : Read CalCOFI bottle measurements

- [`cc_read_obs()`](https://calcofi.io/calcofi4r/reference/cc_read_obs.md)
  :

  Read consolidated observations (`obs`)

- [`cc_read_obs_freq()`](https://calcofi.io/calcofi4r/reference/cc_read_obs_freq.md)
  :

  Read sub-occurrence frequency distributions (`obs_freq`)

- [`cc_read_sample()`](https://calcofi.io/calcofi4r/reference/cc_read_sample.md)
  :

  Read the sampling-event dimension (`sample`)

- [`cc_read_sample_measurement()`](https://calcofi.io/calcofi4r/reference/cc_read_sample_measurement.md)
  :

  Read event-level effort measurements (`sample_measurement`)

- [`cc_read_site()`](https://calcofi.io/calcofi4r/reference/cc_read_site.md)
  : Read CalCOFI site data

- [`cc_read_taxon()`](https://calcofi.io/calcofi4r/reference/cc_read_taxon.md)
  : Read CalCOFI taxonomy

- [`cc_read_tow()`](https://calcofi.io/calcofi4r/reference/cc_read_tow.md)
  : Read CalCOFI tow data

- [`.cc_reader_retired()`](https://calcofi.io/calcofi4r/reference/dot-cc_reader_retired.md)
  : Read CalCOFI bottle data

- [`get_cruises()`](https://calcofi.io/calcofi4r/reference/get_cruises.md)
  **\[deprecated\]** : Get cruises from CalCOFI API

- [`get_env()`](https://calcofi.io/calcofi4r/reference/get_env.md) :
  Retrieve Environmental Data from Database

- [`get_raster()`](https://calcofi.io/calcofi4r/reference/get_raster.md)
  **\[deprecated\]** : Get raster of interpolated values from CalCOFI
  API

- [`get_sp()`](https://calcofi.io/calcofi4r/reference/get_sp.md) :
  Retrieve Species Larval Abundance Data from Database

- [`get_timeseries()`](https://calcofi.io/calcofi4r/reference/get_timeseries.md)
  **\[deprecated\]** : Get timeseries summary from CalCOFI API

- [`get_variables()`](https://calcofi.io/calcofi4r/reference/get_variables.md)
  **\[deprecated\]** : Get variables from CalCOFI API

## Analyze

functions for analyzing data

- [`buffer_transect()`](https://calcofi.io/calcofi4r/reference/buffer_transect.md)
  : Create Buffer Around Line Segment with Dateline Handling
- [`cc_ts_gaps()`](https://calcofi.io/calcofi4r/reference/cc_ts_gaps.md)
  : Insert NA rows at time steps with no observations
- [`get_oceano_var_aoi()`](https://calcofi.io/calcofi4r/reference/get_oceano_var_aoi.md)
  **\[deprecated\]** : Get oceanographic variable for area of interest
- [`prep_env_hex()`](https://calcofi.io/calcofi4r/reference/prep_env_hex.md)
  : Aggregate Environmental Data into H3 Hexagons
- [`prep_filter_summary()`](https://calcofi.io/calcofi4r/reference/prep_filter_summary.md)
  : Build Filter Summary for Display
- [`prep_sp_hex()`](https://calcofi.io/calcofi4r/reference/prep_sp_hex.md)
  : Aggregate Species Data into H3 Hexagons
- [`prep_splot()`](https://calcofi.io/calcofi4r/reference/prep_splot.md)
  : Prepare Data for Species-Environment Scatterplot
- [`prep_ts_env()`](https://calcofi.io/calcofi4r/reference/prep_ts_env.md)
  : Build Environmental Time Series Data
- [`prep_ts_sp()`](https://calcofi.io/calcofi4r/reference/prep_ts_sp.md)
  : Build Species Time Series Data
- [`pts_to_contours_gam()`](https://calcofi.io/calcofi4r/reference/pts_to_contours_gam.md)
  : Interpolate points to contours using a Generalized Additive Model
  (GAM)
- [`pts_to_rast_idw()`](https://calcofi.io/calcofi4r/reference/pts_to_rast_idw.md)
  : Interpolate points to raster using Inverse-Distance Weighting (IDW)
- [`rast_to_contours()`](https://calcofi.io/calcofi4r/reference/rast_to_contours.md)
  : Raster to contour polygons

## Visualize

functions for visualizing data

- [`map_env()`](https://calcofi.io/calcofi4r/reference/map_env.md) :
  Create Interactive Environmental Map with Hexagonal Binning
- [`map_raster()`](https://calcofi.io/calcofi4r/reference/map_raster.md)
  : Map raster interactively
- [`map_sp()`](https://calcofi.io/calcofi4r/reference/map_sp.md) :
  Create Interactive Species Distribution Map with Hexagonal Binning
- [`modal_data()`](https://calcofi.io/calcofi4r/reference/modal_data.md)
  : Data Selection Modal Dialog
- [`modal_depth_profile()`](https://calcofi.io/calcofi4r/reference/modal_depth_profile.md)
  : Depth Profile Modal Dialog
- [`plot_depth()`](https://calcofi.io/calcofi4r/reference/plot_depth.md)
  : Plot interactive depth of an oceanographic variable
- [`plot_timeseries()`](https://calcofi.io/calcofi4r/reference/plot_timeseries.md)
  : Plot interactive time series of an oceanographic variable
- [`plot_ts()`](https://calcofi.io/calcofi4r/reference/plot_ts.md) :
  Create Dual-Panel Time Series Plot
- [`ui_placeholder()`](https://calcofi.io/calcofi4r/reference/ui_placeholder.md)
  : Create Placeholder Message UI

## Data

small lookup and example datasets not requiring API access

- [`bottle_temp_depth`](https://calcofi.io/calcofi4r/reference/bottle_temp_depth.md)
  : Bottle data of temperature with depth (m)
- [`bottle_temp_lonlat`](https://calcofi.io/calcofi4r/reference/bottle_temp_lonlat.md)
  : Bottle data of temperature in space (latitude, longitude)
- [`cc_bottle`](https://calcofi.io/calcofi4r/reference/cc_bottle.md) :
  Bottle data in space and time
- [`cc_grid`](https://calcofi.io/calcofi4r/reference/cc_grid.md) :
  CalCOFI Grid for Extracting Effort
- [`cc_grid_ctrs`](https://calcofi.io/calcofi4r/reference/cc_grid_ctrs.md)
  : CalCOFI Grid Centroids for Extracting Effort
- [`cc_grid_zones`](https://calcofi.io/calcofi4r/reference/cc_grid_zones.md)
  : CalCOFI Grid Zones
- [`cc_places`](https://calcofi.io/calcofi4r/reference/cc_places.md) :
  CalCOFI Places
- [`stations`](https://calcofi.io/calcofi4r/reference/stations.md) :
  Oceanographic stations

## Database

functions specific to database (Admin only)

- [`cc_catalog()`](https://calcofi.io/calcofi4r/reference/cc_catalog.md)
  : Read a release catalog

- [`cc_db_catalog()`](https://calcofi.io/calcofi4r/reference/cc_db_catalog.md)
  : Show CalCOFI database catalog as interactive table

- [`cc_db_connect()`](https://calcofi.io/calcofi4r/reference/cc_db_connect.md)
  : Connect to the CalCOFI PostgreSQL database (Admin only) - DEPRECATED

- [`cc_db_info()`](https://calcofi.io/calcofi4r/reference/cc_db_info.md)
  : Get CalCOFI database information

- [`cc_erd()`](https://calcofi.io/calcofi4r/reference/cc_erd.md) :
  Generate Mermaid ERD from DuckDB Connection

- [`cc_erd_color_map()`](https://calcofi.io/calcofi4r/reference/cc_erd_color_map.md)
  : Build an ERD color map from table → dataset metadata

- [`cc_get_db()`](https://calcofi.io/calcofi4r/reference/cc_get_db.md) :
  Connect to CalCOFI Database

- [`cc_get_dm()`](https://calcofi.io/calcofi4r/reference/cc_get_dm.md) :
  Get CalCOFI Database as dm Object with Relationships

- [`cc_latest_version()`](https://calcofi.io/calcofi4r/reference/cc_latest_version.md)
  :

  Which release is currently `latest`

- [`cc_list_versions()`](https://calcofi.io/calcofi4r/reference/cc_list_versions.md)
  : List available CalCOFI database versions

- [`cc_list_view_templates()`](https://calcofi.io/calcofi4r/reference/cc_list_view_templates.md)
  : List Available View Templates

- [`cc_make_view()`](https://calcofi.io/calcofi4r/reference/cc_make_view.md)
  : Create a Derived VIEW in the Database

- [`cc_pg_attach()`](https://calcofi.io/calcofi4r/reference/cc_pg_attach.md)
  : Attach the CalCOFI PostgreSQL database inside a DuckDB connection

- [`cc_pg_connect()`](https://calcofi.io/calcofi4r/reference/cc_pg_connect.md)
  : Connect to the CalCOFI PostgreSQL database

- [`cc_pg_tunnel()`](https://calcofi.io/calcofi4r/reference/cc_pg_tunnel.md)
  [`cc_pg_tunnel_close()`](https://calcofi.io/calcofi4r/reference/cc_pg_tunnel.md)
  : Open an SSH tunnel to the CalCOFI server's PostgreSQL

- [`cc_read_parquet_sql()`](https://calcofi.io/calcofi4r/reference/cc_read_parquet_sql.md)
  :

  The `read_parquet(...)` SQL for a resolved source

- [`cc_read_sf()`](https://calcofi.io/calcofi4r/reference/cc_read_sf.md)
  : Read Spatial Table from DuckDB as sf Object

- [`cc_release_notes()`](https://calcofi.io/calcofi4r/reference/cc_release_notes.md)
  : View CalCOFI database release notes

- [`cc_release_sources()`](https://calcofi.io/calcofi4r/reference/cc_release_sources.md)
  : Resolve where a release table's parquet bytes live

- [`cc_tbl()`](https://calcofi.io/calcofi4r/reference/cc_tbl.md) :
  Access a CalCOFI Database Table

- [`create_index()`](https://calcofi.io/calcofi4r/reference/create_index.md)
  : Create index in database

## Analytics

non-blocking usage tracking for the Shiny apps (App maintainers only)

- [`cc_apps_script()`](https://calcofi.io/calcofi4r/reference/cc_apps_script.md)
  : Apps Script source for the usage-log Sheet

- [`cc_client_ip()`](https://calcofi.io/calcofi4r/reference/cc_client_ip.md)
  : Best-effort client IP from a Shiny request

- [`cc_event()`](https://calcofi.io/calcofi4r/reference/cc_event.md) :
  Build a tracking-event payload

- [`cc_ga_head()`](https://calcofi.io/calcofi4r/reference/cc_ga_head.md)
  :

  Analytics `<head>` snippet as a Shiny tag

- [`cc_ga_html()`](https://calcofi.io/calcofi4r/reference/cc_ga_html.md)
  : Write the analytics snippet to a standalone HTML file

- [`cc_ga_js()`](https://calcofi.io/calcofi4r/reference/cc_ga_js.md) :

  Analytics `<head>` snippet (GA4 + batched Sheet beacon)

- [`cc_log_header()`](https://calcofi.io/calcofi4r/reference/cc_log_header.md)
  : Column header for the usage-log Sheet

- [`cc_track()`](https://calcofi.io/calcofi4r/reference/cc_track.md) :
  Send a tracking event from the Shiny server to the browser

- [`cc_track_query()`](https://calcofi.io/calcofi4r/reference/cc_track_query.md)
  : Time a query, log it, and return its result

- [`cc_track_session()`](https://calcofi.io/calcofi4r/reference/cc_track_session.md)
  : Hand the browser the session facts only the server knows

## Brand

the calcofi.io theme / header / favicon contract for the Shiny apps (App
maintainers only)

- [`cc_brand_head()`](https://calcofi.io/calcofi4r/reference/cc_brand_head.md)
  :

  Brand `<head>` tags for a Shiny app

- [`cc_brand_header()`](https://calcofi.io/calcofi4r/reference/cc_brand_header.md)
  : The brand header bar for a Shiny app

- [`cc_ggplot_theme()`](https://calcofi.io/calcofi4r/reference/cc_ggplot_theme.md)
  : A ggplot2 theme for the current theme

- [`cc_is_dark()`](https://calcofi.io/calcofi4r/reference/cc_is_dark.md)
  : Is the app currently in dark mode?

- [`cc_plot_colors()`](https://calcofi.io/calcofi4r/reference/cc_plot_colors.md)
  : Plot colours for the current theme

- [`cc_plotly_theme()`](https://calcofi.io/calcofi4r/reference/cc_plotly_theme.md)
  : Theme a plotly figure for the current theme

- [`cc_release_chip()`](https://calcofi.io/calcofi4r/reference/cc_release_chip.md)
  : The integrated-database release chip

- [`cc_release_url()`](https://calcofi.io/calcofi4r/reference/cc_release_url.md)
  : Where a database release is documented

- [`cc_theme()`](https://calcofi.io/calcofi4r/reference/cc_theme.md) :
  Resolve the theme a Shiny request asks for

- [`cc_tour_enabled()`](https://calcofi.io/calcofi4r/reference/cc_tour_enabled.md)
  :

  Should the guided tour run? (`?tour=off`)

## Other

check for other functions or datasets not captured by above categories

- [`CC_AREAL_GEARS`](https://calcofi.io/calcofi4r/reference/CC_AREAL_GEARS.md)
  :

  Gears whose `std_haul_factor` standardizes to a depth-integrated 10 m2
  (oblique / vertical tows)

- [`CC_DENSITY_UNITS`](https://calcofi.io/calcofi4r/reference/CC_DENSITY_UNITS.md)
  : Units the release publishes as densities (per unit area, per unit
  volume)

- [`CC_QUAL_EXCLUDE`](https://calcofi.io/calcofi4r/reference/CC_QUAL_EXCLUDE.md)
  : Quality codes a consumer should exclude, per dataset

- [`cc_anomaly()`](https://calcofi.io/calcofi4r/reference/cc_anomaly.md)
  : Join a section to a climatology and difference it

- [`cc_bathy()`](https://calcofi.io/calcofi4r/reference/cc_bathy.md) :
  GEBCO seafloor bathymetry over the CalCOFI area

- [`cc_bathy_depth()`](https://calcofi.io/calcofi4r/reference/cc_bathy_depth.md)
  : Seafloor depth at points

- [`cc_climatology()`](https://calcofi.io/calcofi4r/reference/cc_climatology.md)
  : Seasonal climatology for one or more measurement types

- [`cc_default_stage()`](https://calcofi.io/calcofi4r/reference/cc_default_stage.md)
  [`cc_default_denominator()`](https://calcofi.io/calcofi4r/reference/cc_default_stage.md)
  : The picker's defaults: which life stage and which denominator open a
  taxon (D8 rule 4)

- [`cc_density_sql()`](https://calcofi.io/calcofi4r/reference/cc_density_sql.md)
  : SQL deriving the two canonical densities and the effort class of a
  bio observation

- [`cc_match_bio_env()`](https://calcofi.io/calcofi4r/reference/cc_match_bio_env.md)
  : Match biological to environmental observations

- [`cc_match_ichthyo_by_name()`](https://calcofi.io/calcofi4r/reference/cc_match_ichthyo_by_name.md)
  : Match ichthyoplankton to environmental data by scientific name

- [`cc_match_ichthyo_by_taxon()`](https://calcofi.io/calcofi4r/reference/cc_match_ichthyo_by_taxon.md)
  : Match ichthyoplankton to environmental data by WoRMS taxon subtree

- [`cc_match_zooplankton_biomass()`](https://calcofi.io/calcofi4r/reference/cc_match_zooplankton_biomass.md)
  : Match zooplankton biomass to environmental data

- [`cc_qual_ok()`](https://calcofi.io/calcofi4r/reference/cc_qual_ok.md)
  : Is each observation's quality flag acceptable? (vectorised, in R)

- [`cc_qual_ok_sql()`](https://calcofi.io/calcofi4r/reference/cc_qual_ok_sql.md)
  : SQL predicate keeping only observations whose quality flag is not
  bad

- [`cc_transect_bathy()`](https://calcofi.io/calcofi4r/reference/cc_transect_bathy.md)
  : Seafloor profile along a transect, sampled at a regular interval

- [`cc_transect_matrix()`](https://calcofi.io/calcofi4r/reference/cc_transect_matrix.md)
  : Pivot a section (or anomaly) to a station x depth matrix

- [`cc_transect_section()`](https://calcofi.io/calcofi4r/reference/cc_transect_section.md)
  : Observations along a transect, binned by depth

- [`cc_transect_stations()`](https://calcofi.io/calcofi4r/reference/cc_transect_stations.md)
  : Stations along a CalCOFI line, ordered nearshore to offshore

- [`get_taxon_children()`](https://calcofi.io/calcofi4r/reference/get_taxon_children.md)
  : Retrieve Taxon Children from Database

- [`plot(`*`<cc_erd>`*`)`](https://calcofi.io/calcofi4r/reference/plot.cc_erd.md)
  : Plot a cc_erd object as an interactive diagram

- [`updateMermaid()`](https://calcofi.io/calcofi4r/reference/updateMermaid.md)
  : Update the mermaid library in the package
