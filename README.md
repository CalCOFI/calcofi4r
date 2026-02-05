
# calcofi4r <img src="man/figures/logo.svg" style="float:right; height:150px">

R package for accessing and visualizing CalCOFI data. Connect directly to the CalCOFI database via DuckDB or use the CalCOFI API.

## Install

This package lives on Github, not yet CRAN, so you'll need to run the following to install or update the package:

```r
remotes::install_github("calcofi/calcofi4r")
```

Then load the package:

```r
library(calcofi4r)
```

## Quick Start

### Connect to CalCOFI Database

Access the CalCOFI integrated database directly via DuckDB:

```r
# connect to latest frozen release
con <- cc_get_db()

# list available tables
cc_list_tables()
#> [1] "bottle"             "bottle_measurement" "cast_condition"
#> [4] "casts"              "cruise"             "grid"
#> [7] "ichthyo"            "lookup"             "measurement_type"
#> ...

# query with SQL
DBI::dbGetQuery(con, "SELECT COUNT(*) FROM ichthyo")
```

### Read Data with Convenience Functions

```r
# read ichthyoplankton (larvae) data
ichthyo <- cc_read_ichthyo()

# read bottle samples
bottles <- cc_read_bottle()

# read cast data
casts <- cc_read_casts()

# read species taxonomy
species <- cc_read_species()

# get measurement types
cc_list_measurement_types()

# filter while reading (uses dplyr syntax, returns lazy table)
anchovy <- cc_read_ichthyo(species_id == 19, collect = FALSE)
```

### Version Control

Access specific database versions for reproducibility:

```r
# list available versions
cc_list_versions()
#>    version release_date tables total_rows size_mb is_latest
#> 1 v2026.02   2026-02-05     17   13410422    80.9      TRUE

# connect to specific version
con <- cc_get_db(version = "v2026.02")

# get release information
cc_db_info("v2026.02")

# view release notes
cc_release_notes("v2026.02")
```

### Execute Custom Queries

```r
# run SQL queries
results <- cc_query("
  SELECT species_id, COUNT(*) as n
  FROM ichthyo
  GROUP BY species_id
  ORDER BY n DESC
  LIMIT 10")

# describe table schema
cc_describe_table("ichthyo")
cc_describe_table("casts")
```

## CalCOFI API Functions

The package also provides functions for the CalCOFI API at [api.calcofi.io](https://api.calcofi.io):

```r
# get available variables
get_variables()

# get cruise information
get_cruises()

# get interpolated raster
get_raster(
  variable  = "ctdcast_bottle.t_deg_c",
  cruise_id = "2020-01-05-C-33RL",
  out_tif   = "temperature.tif")

# get time series summary
get_timeseries(
  variable    = "ctdcast_bottle.t_deg_c",
  aoi_wkt     = "POLYGON((-121 33, -119 33, -119 35, -121 35, -121 33))",
  depth_m_min = 0,
  depth_m_max = 100,
  time_step   = "year")
```

## Package Data

The package includes small lookup and example datasets:

```r
# CalCOFI sampling grid
cc_grid
cc_grid_ctrs
cc_grid_zones

# example bottle data
cc_bottle

# station locations
stations

# geographic places
cc_places
```

## Documentation

- [Get Started Vignette](https://calcofi.io/calcofi4r/articles/calcofi4r.html)
- [Function Reference](https://calcofi.io/calcofi4r/reference/)
- [CalCOFI Data Workflow Plan](https://calcofi.io/workflows/README_PLAN.html)

## Data Architecture

CalCOFI data is stored in frozen DuckLake releases on Google Cloud Storage:

```
gs://calcofi-db/ducklake/releases/
├── v2026.02/
│   ├── catalog.json
│   ├── RELEASE_NOTES.md
│   └── parquet/
│       ├── bottle.parquet
│       ├── casts.parquet
│       ├── ichthyo.parquet
│       ├── species.parquet
│       └── ...
├── versions.json
└── latest.txt → v2026.02
```

Data is accessed directly via DuckDB's httpfs extension - no download required for queries.

## Code of Conduct

This is an open-source project so your input is greatly welcomed! Please
note that the `calcofi4r` project is released with a [Contributor Code
of Conduct](https://calcofi.github.io/calcofi4r/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
