# calcofi4r 0.5.4

* \+ `cc_places` with three categories for places: 1) CalCOFI (Core, Extended, Nearshore, Offshore); 2) Integrated Ecosystem Assessment (California Current); and 3) National Marine Sanctuary (Cordell Bank, Channel Islands, Greater Farallones, Monterey Bay, Olympic Coast). Augmented `cc_grid*` with missing cells to reduce slivers when intersecting.

# calcofi4r 0.5.3

* `cc_grid_area` -> `cc_grid_areas` data for showing study areas with combinations of `sta_dpos` nearshore (5), offshore (10) and outside (20); where `area_dpos` can be one of: `"5"`, `"10"`, `"20"`, `"5,10"`, `"10,20"` or `"5,10,20"`

# calcofi4r 0.5.2

* \+ `cc_grid`, `cc_grid_ctrs` showing the CalCOFI station sampling at varying seperation distances of station positions (`sta_pos`) in the CalCOFI coordinate system from nearshore (`5`), to offshore (`10`) to outside the 113 station extended repeated area (`20`), per [Station Positions – CalCOFI](https://calcofi.org/sampling-info/station-positions/)}

# calcofi4r 0.5.1

* handle defaults with `plot_depth()`, streamline headings in article and descriptions in functions

# calcofi4r 0.5.0

* \+ `plot_depth()` and example data `bottle_temp_depth`
* `stations_t_degc` -> `bottle_temp_lonlat`

# calcofi4r 0.4.1

* rename `tbl_to_contour_ply()` to `map_contours()`

# calcofi4r 0.4.0

* \+ `tbl_to_contour_ply()` and example data `area_calcofi_extended` `stations_t_degc`

# calcofi4r 0.3.0

* \+ `get_cruises()` and `get_raster()` for reading from the CalCOFI API.
* \+ `map_raster()` and `plot_timeseries()` for visualizing.
* \+ "Get started" vignette and README.Rmd -> README.md with example usage

# calcofi4r 0.2.0

* \+ `get_variables()`, `get_timeseries()` for reading from the API at https://api.calcofi.io

# calcofi4r 0.1.2

* ∆ order of `stations` columns

# calcofi4r 0.1.1

* \+ authors

# calcofi4r 0.1.0

* Bumped version. Have functions for Analyze (`get_oceano_var_aoi()`) and Visualize (`plot_timeseries()`), plus Data (`bottle`, `dic`, `stations`).

# calcofi4r 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
