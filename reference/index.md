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
- [`cc_read_bottle()`](https://calcofi.io/calcofi4r/reference/cc_read_bottle.md)
  : Read CalCOFI bottle data
- [`cc_read_casts()`](https://calcofi.io/calcofi4r/reference/cc_read_casts.md)
  [`cc_read_cast()`](https://calcofi.io/calcofi4r/reference/cc_read_casts.md)
  : Read CalCOFI cast data
- [`cc_read_cruise()`](https://calcofi.io/calcofi4r/reference/cc_read_cruise.md)
  : Read CalCOFI cruise data
- [`cc_read_ichthyo()`](https://calcofi.io/calcofi4r/reference/cc_read_ichthyo.md)
  [`cc_read_larvae()`](https://calcofi.io/calcofi4r/reference/cc_read_ichthyo.md)
  : Read CalCOFI ichthyoplankton (larvae) data
- [`cc_read_measurements()`](https://calcofi.io/calcofi4r/reference/cc_read_measurements.md)
  : Read CalCOFI bottle measurements
- [`cc_read_site()`](https://calcofi.io/calcofi4r/reference/cc_read_site.md)
  : Read CalCOFI site data
- [`cc_read_species()`](https://calcofi.io/calcofi4r/reference/cc_read_species.md)
  : Read CalCOFI species data
- [`cc_read_tow()`](https://calcofi.io/calcofi4r/reference/cc_read_tow.md)
  : Read CalCOFI tow data
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
- [`cc_list_versions()`](https://calcofi.io/calcofi4r/reference/cc_list_versions.md)
  : List available CalCOFI database versions
- [`cc_list_view_templates()`](https://calcofi.io/calcofi4r/reference/cc_list_view_templates.md)
  : List Available View Templates
- [`cc_make_view()`](https://calcofi.io/calcofi4r/reference/cc_make_view.md)
  : Create a Derived VIEW in the Database
- [`cc_read_sf()`](https://calcofi.io/calcofi4r/reference/cc_read_sf.md)
  : Read Spatial Table from DuckDB as sf Object
- [`cc_release_notes()`](https://calcofi.io/calcofi4r/reference/cc_release_notes.md)
  : View CalCOFI database release notes
- [`cc_tbl()`](https://calcofi.io/calcofi4r/reference/cc_tbl.md) :
  Access a CalCOFI Database Table
- [`create_index()`](https://calcofi.io/calcofi4r/reference/create_index.md)
  : Create index in database

## Other

check for other functions or datasets not captured by above categories

- [`cc_match_bio_env()`](https://calcofi.io/calcofi4r/reference/cc_match_bio_env.md)
  : Match biological to environmental observations
- [`cc_match_ichthyo_by_name()`](https://calcofi.io/calcofi4r/reference/cc_match_ichthyo_by_name.md)
  : Match ichthyoplankton to environmental data by scientific name
- [`cc_match_ichthyo_by_taxon()`](https://calcofi.io/calcofi4r/reference/cc_match_ichthyo_by_taxon.md)
  : Match ichthyoplankton to environmental data by WoRMS taxon subtree
- [`cc_match_zooplankton_biomass()`](https://calcofi.io/calcofi4r/reference/cc_match_zooplankton_biomass.md)
  : Match zooplankton biomass to environmental data
- [`get_taxon_children()`](https://calcofi.io/calcofi4r/reference/get_taxon_children.md)
  : Retrieve Taxon Children from Database
- [`plot(`*`<cc_erd>`*`)`](https://calcofi.io/calcofi4r/reference/plot.cc_erd.md)
  : Plot a cc_erd object as an interactive diagram
- [`updateMermaid()`](https://calcofi.io/calcofi4r/reference/updateMermaid.md)
  : Update the mermaid library in the package
