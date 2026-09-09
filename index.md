# calcofi4r

R package for accessing and visualizing CalCOFI data: the public,
versioned database releases (DuckDB over Parquet on Google Cloud
Storage, no account needed) and the CTD team’s PostgreSQL working
database (account required). The Python sibling is
[calcofi4py](https://calcofi.io/calcofi4py/) — same verbs, same
defaults.

**These examples are tests.** `tests/testthat/test-readme.R` knits this
file against the promoted release, and the CalCOFI database release
pipeline
([`test_release.qmd`](https://github.com/CalCOFI/workflows/blob/main/test_release.qmd))
knits it against every new release *before* promoting it — so a column
the release renames fails the release, not you. The outputs below are
from the release named in [Versions](#versions), with calcofi4r 1.24.0.

## Install

This package lives on GitHub, not CRAN:

``` r

remotes::install_github("calcofi/calcofi4r")
```

### System requirements

`calcofi4r` pulls in `sf`, and `sf` pulls in
[`s2`](https://r-spatial.github.io/s2/), which is C++ and must compile.
CRAN publishes no macOS arm64 binaries for R 4.6 yet, so on Apple
Silicon that compile is not optional and it needs **`cmake`**:

``` sh
brew install cmake
```

`brew install abseil` does *not* substitute for this, despite what s2’s
own error message suggests. Its `configure` defaults
`S2_FORCE_BUNDLED_ABSEIL=true` and so always builds its vendored copy of
Abseil with cmake, never consulting `pkg-config` for a system one.

To skip the compile altogether, point R at Posit Package Manager, which
serves prebuilt macOS arm64 and Linux binaries for R 4.6. Put this in
your `~/.Rprofile`:

``` r

options(repos = c(P3M = "https://packagemanager.posit.co/cran/latest"))
```

## The public database releases (no account needed)

Immutable, versioned Parquet on a public bucket; DuckDB reads only what
a query touches, straight over HTTPS.
[`cc_get_db()`](https://calcofi.io/calcofi4r/reference/cc_get_db.md)
registers every release table as a view, so you query by bare table
name:

``` r

library(calcofi4r)
library(dplyr)

con <- cc_get_db()                       # latest release, every table as a view
DBI::dbListTables(con)
#>  [1] "climatology"        "cruise"             "dataset"            "dataset_taxon"     
#>  [5] "grid"               "lookup"             "measurement_type"   "obs"               
#>  [9] "obs_attribute"      "obs_bio"            "obs_env"            "region"            
#> [13] "sample"             "sample_measurement" "sample_spatial"     "ship"              
#> [17] "spatial"            "spatial_attribute"  "taxon"              "taxon_group"

# CTD casts per year: sample is one row per sampling event; datetime is UTC
DBI::dbGetQuery(con, "
  SELECT date_trunc('year', s.datetime) AS year, count(*) AS casts
  FROM sample s WHERE s.dataset_key = 'calcofi_ctd-cast'
  GROUP BY 1 ORDER BY 1 DESC LIMIT 5")
#>         year casts
#> 1 2026-01-01   534
#> 2 2025-01-01   638
#> 3 2024-01-01   539
#> 4 2023-01-01   646
#> 5 2022-01-01   454

# one-shot, without keeping a connection
cc_query("SELECT dataset_key, count(*) AS n FROM obs_env GROUP BY 1 ORDER BY 2 DESC")
#> # A tibble: 5 × 2
#>   dataset_key                           n
#>   <chr>                             <dbl>
#> 1 calcofi_ctd-cast               13295014
#> 2 calcofi_bottle                 11135600
#> 3 calcofi_mets                     511459
#> 4 cce-lter_picoplankton-bacteria    60802
#> 5 calcofi_dic                        3708

# lazy dbplyr over a view
tbl(con, "taxon") |>
  filter(scientific_name %in% c("Sardinops sagax", "Engraulis mordax")) |>
  select(taxon_key, scientific_name, common_name) |>
  collect()
#> # A tibble: 2 × 3
#>   taxon_key    scientific_name  common_name               
#>   <chr>        <chr>            <chr>                     
#> 1 worms:217452 Sardinops sagax  Pacific sardine (pilchard)
#> 2 worms:272286 Engraulis mordax Northern anchovy
```

Every dataset projects into one small **core** family — `sample` (the
event: cast, tow, net, bottle) and the observation pair `obs_bio` /
`obs_env` (one scalar per row; `obs` is a view over both) — so a query
written against ichthyoplankton works unchanged against CTD, zooplankton
or seabirds. To see what’s in each table first: the [Schema
explorer](https://calcofi.io/db-schema/), or from R:

``` r

cc_describe_table("sample", con = con)   # columns, units, descriptions from metadata.json
#> # A tibble: 21 × 6
#>    column_name       data_type is_nullable name_long           units           description_md       
#>    <chr>             <chr>     <chr>       <chr>               <chr>           <chr>                
#>  1 sample_key        VARCHAR   YES         Sample Key          <NA>            Globally unique even…
#>  2 sample_type       VARCHAR   YES         Sample Type         <NA>            Event grain: `site`,…
#>  3 parent_sample_key VARCHAR   YES         Parent Sample Key   <NA>            The containing event…
#>  4 root_sample_key   VARCHAR   YES         Root Sample Key     <NA>            Top of this event's …
#>  5 dataset_key       VARCHAR   YES         Dataset Key         <NA>            Provenance stamp: `p…
#>  6 grid_key          VARCHAR   YES         Grid Key            <NA>            CalCOFI station grid…
#>  7 site_key          VARCHAR   YES         Site Key            <NA>            CalCOFI station natu…
#>  8 cruise_key        VARCHAR   YES         Cruise Key          <NA>            CalCOFI cruise natur…
#>  9 order_occ         INTEGER   YES         Order of Occupation <NA>            Order in which the s…
#> 10 latitude          DOUBLE    YES         Latitude            decimal degrees Event latitude (WGS8…
#> # ℹ 11 more rows
cc_list_measurement_types(con = con) |> head(8)
#> # A tibble: 8 × 3
#>   measurement_type description                                                                 units
#>   <chr>            <chr>                                                                       <chr>
#> 1 abundance        Specimen count per net tow (headline occurrence; standardize via std_haul_… count
#> 2 air_temp_c       Air temperature                                                             deg_C
#> 3 alkalinity       Total alkalinity                                                            umol…
#> 4 alkalinity_rep1  Total alkalinity replicate 1                                                umol…
#> 5 alkalinity_rep2  Total alkalinity replicate 2                                                umol…
#> 6 ammonia          Ammonia concentration (QC'd)                                                umol…
#> 7 anchovy_eggs     Northern anchovy egg count                                                  count
#> 8 atm_pressure_mb  Atmospheric pressure (ship level)                                           mb
```

### Read with convenience functions

``` r

# taxonomy: one row per taxon, keyed worms:<id> or itis:<id>
cc_read_taxon(scientific_name == "Engraulis mordax")
#> # A tibble: 1 × 19
#>   taxon_key    worms_id itis_id gbif_id ncbi_id inat_id scientific_name  common_n…¹ rank  rank_order
#>   <chr>           <int>   <int>   <int>   <int>   <int> <chr>            <chr>      <chr>      <int>
#> 1 worms:272286   272286  161828 2414024      NA      NA Engraulis mordax Northern … Spec…         43
#> # ℹ abbreviated name: ¹​common_name
#> # ℹ 9 more variables: taxonomic_status <chr>, status_checked <chr>, parent_taxon_key <chr>,
#> #   kingdom <chr>, phylum <chr>, class <chr>, order_taxon <chr>, family <chr>, notes <chr>

# occurrences of one taxon in one dataset, lazily (collect = FALSE)
anchovy <- cc_read_obs(
  taxon_key == "worms:272286", realm = "bio", datasets = "swfsc_ichthyo", collect = FALSE)
anchovy |> count(life_stage, measurement_type) |> collect()
#> # A tibble: 2 × 3
#>   life_stage measurement_type     n
#>   <chr>      <chr>            <dbl>
#> 1 egg        abundance        15551
#> 2 larva      abundance        26493

# sampling events, filtered by core columns
cc_read_sample(datasets = "calcofi_bottle", sample_types = "cast") |>
  select(sample_key, cruise_key, site_key, datetime, latitude, longitude) |>
  head(3)
#> # A tibble: 3 × 6
#>   sample_key              cruise_key   site_key    datetime            latitude longitude
#>   <chr>                   <chr>        <chr>       <dttm>                 <dbl>     <dbl>
#> 1 calcofi_bottle:cast:1   1949-03-31CR 054.0 056.0 1949-03-01 09:30:00     38.8     -124.
#> 2 calcofi_bottle:cast:10  1949-03-31CR 059.0 117.0 1949-03-04 23:54:00     36.1     -128.
#> 3 calcofi_bottle:cast:100 1949-04-31HO 082.0 077.0 1949-04-02 17:30:00     33.2     -122.
```

### Quality flags

`measurement_qual` is each dataset’s *own* code set (bottle/CTD `8` =
suspect, `9` = missing/bad; DIC WOCE 3/4/9). A flagged value is still a
row — filter it:

``` r

cc_qual_ok_sql("o")                      # the predicate, for obs / obs_env / sample_measurement
#> [1] "COALESCE(NOT ((o.dataset_key = 'calcofi_bottle' AND regexp_replace(o.measurement_qual, '\\.0+$', '') IN ('8', '9')) OR (o.dataset_key = 'calcofi_ctd-cast' AND regexp_replace(o.measurement_qual, '\\.0+$', '') IN ('8', '9')) OR (o.dataset_key = 'calcofi_dic' AND regexp_replace(o.measurement_qual, '\\.0+$', '') IN ('3', '4', '9'))), TRUE)"

DBI::dbGetQuery(con, glue::glue("
  SELECT count(*) AS n_ok
  FROM obs_env o
  WHERE o.dataset_key = 'calcofi_bottle' AND o.measurement_type = 'oxygen_ml_l'
    AND {cc_qual_ok_sql('o')}"))
#>     n_ok
#> 1 724175
```

### Match biology to environment

The question CalCOFI exists to answer: what water were the larvae in?
One call joins ichthyoplankton tows to the nearest CTD-bottle
measurement, and `return_sql = TRUE` hands back the exact query so
anyone can re-run it in DuckDB (R, Python, the CLI):

``` r

d <- cc_match_ichthyo_by_name(
  scientific_name = "Sardinops sagax",
  env_var         = "temperature",
  date_min        = "2018-01-01",
  date_max        = "2018-12-31",
  relax_matching  = TRUE)
dim(d)
#> [1] 51 19
d |> select(bio_datetime, bio_lat, bio_lon, bio_value, env_value, dist_km, time_diff_hr) |> head(3)
#> # A tibble: 3 × 7
#>   bio_datetime        bio_lat bio_lon bio_value env_value dist_km time_diff_hr
#>   <dttm>                <dbl>   <dbl>     <dbl>     <dbl>   <dbl>        <dbl>
#> 1 2018-04-19 08:15:00    32.8   -124.     24.6       11.1   0.431        1.37 
#> 2 2018-04-15 04:33:00    31.7   -123.      4.91      11.4   0.737        0.909
#> 3 2018-04-08 03:38:00    30.5   -122.     42.2       11.5   0.240        0.858
```

See the [bio–env matching
article](https://calcofi.io/calcofi4r/articles/bio-env-matching.html)
for the column contract and
[`cc_match_bio_env()`](https://calcofi.io/calcofi4r/reference/cc_match_bio_env.md)
for your own subqueries.

### Versions

Releases are immutable, so pin one for a reproducible analysis:

``` r

cc_list_versions() |> select(version, release_date, tables, total_rows, doi, is_latest) |> head(3)
#> # A tibble: 3 × 6
#>   version     release_date tables total_rows doi                     is_latest
#>   <chr>       <chr>         <int>      <int> <chr>                   <lgl>    
#> 1 v2026.09.06 2026-09-06       23  348657010 10.5281/zenodo.22514953 TRUE     
#> 2 v2026.09.04 2026-09-04       23  348657010 10.5281/zenodo.22310858 FALSE    
#> 3 v2026.08.25 2026-08-25       18  320260205 <NA>                    FALSE

cc_latest_version()                      # what "latest" resolves to right now — record it
#> [1] "v2026.09.06"
con_pinned <- cc_get_db(version = "v2026.08.25")   # a consolidated release stays readable
DBI::dbGetQuery(con_pinned, "SELECT count(*) AS n FROM cruise")
#>     n
#> 1 691

cc_db_info()$version                     # catalog.json for a version
#> [1] "v2026.09.06"
cat(substr(cc_release_notes(), 1, 400))  # its RELEASE_NOTES.md
#> # CalCOFI integrated database release v2026.09.06
#> 
#> **Release date:** 2026-09-06 · **promoted** (`latest.txt`)
#> 
#> ## The dataset catalog record says what a page needs to say (schema 1.1)
#> 
#> `datasets.json` grew five fields, all additive, all read from a registry the team already edits
#> (calcofi4db 4.5.0; UI plan 2026-09-05 § D-9). Each of them retires a hand-typed map in
#> calcofi.io's own generator — a f
```

### Cite

``` r

cites <- cc_cite()                       # the release, then every dataset it carries
length(cites)
#> [1] 17
cat(cites[1:2], sep = "\n\n")
#> CalCOFI (2026). CalCOFI Integrated Database, release v2026.09.06 [Data set]. Scripps Institution of Oceanography, NOAA Fisheries, and California Department of Fish and Wildlife. https://doi.org/10.5281/zenodo.22514953
#> Page: https://calcofi.io/datasets/release/
#> 
#> CalCOFI. (2023). CalCOFI Bottle Database 194903-202105. CalCOFI.org.
#> Page: https://calcofi.io/datasets/calcofi_bottle/
```

## The CTD team’s PostgreSQL working database (account required)

Private, multi-user, reached over SSH — see [Server
Access](https://calcofi.io/docs/server-access.html) for the account, the
`~/.ssh/config` alias `calcofi`, and the `~/.pgpass` file (your password
lives there and in no script, ever). `cc_pg_connect(tunnel = TRUE)`
opens the tunnel for you; `cc_pg_attach(con)` ATTACHes PostgreSQL to a
[`cc_get_db()`](https://calcofi.io/calcofi4r/reference/cc_get_db.md)
DuckDB connection as the catalog `pg`, so one query can join the public
release to the team’s working state. See
[`?cc_pg_connect`](https://calcofi.io/calcofi4r/reference/cc_pg_connect.md).

## Package data

Small lookup and example datasets ship with the package:

``` r

cc_grid          # CalCOFI sampling grid (sf polygons)
#> Simple feature collection with 218 features and 7 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -135.2301 ymin: 18.42757 xmax: -105.7769 ymax: 49.23891
#> Geodetic CRS:  WGS 84
#> # A tibble: 218 × 8
#>    sta_key sta_lin sta_pos sta_dpos sta_shore sta_pattern                              geom zone_key
#>  * <chr>     <int>   <int>    <int> <chr>     <chr>                           <POLYGON [°]> <glue>  
#>  1 10,0         10       0       20 nearshore historical  ((-124.2721 48.47964, -124.3891 … nearsho…
#>  2 10,20        10      20       20 nearshore historical  ((-126.7498 48.80705, -128.4865 … nearsho…
#>  3 10,40        10      40       20 nearshore historical  ((-128.4865 48.14038, -130.2005 … nearsho…
#>  4 10,60        10      60       20 nearshore historical  ((-130.2005 47.47371, -131.8929 … nearsho…
#>  5 10,80        10      80       20 offshore  historical  ((-131.8929 46.80705, -133.5642 … offshor…
#>  6 10,100       10     100       20 offshore  historical  ((-133.5642 46.14038, -135.2153 … offshor…
#>  7 20,0         20       0       20 nearshore historical  ((-125.4461 47.075, -124.1741 45… nearsho…
#>  8 20,20        20      20       20 nearshore historical  ((-125.4461 47.075, -127.1258 46… nearsho…
#>  9 20,40        20      40       20 nearshore historical  ((-127.1258 46.40833, -128.785 4… nearsho…
#> 10 20,60        20      60       20 nearshore historical  ((-128.785 45.74166, -130.4243 4… nearsho…
#> # ℹ 208 more rows
cc_grid_zones    # zones aggregated by station pattern
#> Simple feature collection with 6 features and 8 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -135.2301 ymin: 18.42757 xmax: -105.7769 ymax: 49.23891
#> Geodetic CRS:  WGS 84
#> # A tibble: 6 × 9
#>   zone_key             sta_pattern sta_shore sta_dpos sta_lin_min sta_lin_max sta_pos_min sta_pos_…¹
#>   <chr>                <chr>       <chr>        <int>       <int>       <int>       <int>      <int>
#> 1 nearshore-extended   extended    nearshore        5          60          73          45         60
#> 2 offshore-extended    extended    offshore        10          60          73          70        100
#> 3 nearshore-historical historical  nearshore       20          10         160         -40         60
#> 4 offshore-historical  historical  offshore        20          10         160          80        200
#> 5 nearshore-standard   standard    nearshore        5          76          93          25         60
#> 6 offshore-standard    standard    offshore        10          76          93          70        120
#> # ℹ abbreviated name: ¹​sta_pos_max
#> # ℹ 1 more variable: geom <GEOMETRY [°]>
head(cc_places)  # geographic places
#> Simple feature collection with 6 features and 3 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -127.0621 ymin: 33.51603 xmax: -120.8844 ymax: 43.95971
#> Geodetic CRS:  WGS 84
#> # A tibble: 6 × 4
#>   category                 key                   name                                           geom
#>   <chr>                    <glue>                <glue>                               <GEOMETRY [°]>
#> 1 BOEM Wind Planning Areas boem-wpa_NI10-03      California Call Area - D… MULTIPOLYGON (((-121.146…
#> 2 BOEM Wind Planning Areas boem-wpa_NI10-01      California Call Area - M… MULTIPOLYGON (((-121.804…
#> 3 BOEM Wind Planning Areas boem-wpa_NK10-04      Oregon Call Area - Brook… MULTIPOLYGON (((-124.707…
#> 4 BOEM Wind Planning Areas boem-wpa_NK10-01      Oregon Call Area - Coos … MULTIPOLYGON (((-124.434…
#> 5 CalCOFI Zones            cc_nearshore-extended Extended Nearshore        MULTIPOLYGON (((-122.936…
#> 6 CalCOFI Zones            cc_offshore-extended  Extended Offshore         POLYGON ((-122.804 34.51…
```

## Data architecture

Since the v2026.09 releases each table’s bytes are **content-addressed**
objects that the release catalog points at, so a table unchanged between
releases is stored, and fetched, once:

``` R
gs://calcofi-db/ducklake/
├── releases/
│   ├── latest.txt            → v2026.09.06 (promoted only after test_release.qmd passes)
│   ├── versions.json
│   ├── RELEASES.md           # the database's NEWS file
│   └── v2026.09.06/
│       ├── catalog.json      # tables, row counts, objects[] (path + sha256), views
│       ├── metadata.json     # table/column descriptions, units, datasets
│       ├── relationships.json
│       ├── datasets.json, taxa.json, integrity.json
│       └── RELEASE_NOTES.md
└── tables/{table}/{hash}/…   # the parquet, one immutable object per table (or partition)
```

Never build a `releases/{v}/parquet/` path by hand:
[`cc_catalog()`](https://calcofi.io/calcofi4r/reference/cc_catalog.md) →
[`cc_release_sources()`](https://calcofi.io/calcofi4r/reference/cc_release_sources.md)
→
[`cc_read_parquet_sql()`](https://calcofi.io/calcofi4r/reference/cc_read_parquet_sql.md)
is how every table here is resolved, and you can hand that SQL fragment
to any DuckDB. Details: [Data
Access](https://calcofi.io/docs/data-access.html).

``` r

cat_ <- cc_catalog("latest")
src  <- cc_release_sources(cat_, "obs_env")
length(src$urls)                         # one https URL per partition
#> [1] 84
substr(cc_read_parquet_sql(src), 1, 120) # read_parquet([...], hive_partitioning = true)
#> read_parquet(['https://storage.googleapis.com/calcofi-db/ducklake/tables/obs_env/measurement_type=air_temp_c/a002495adbb
```

## Documentation

- [Get Started](https://calcofi.io/calcofi4r/articles/calcofi4r.html)
  and the other articles
- [Function Reference](https://calcofi.io/calcofi4r/reference/)
- [**CalCOFI Docs**](https://calcofi.io/docs/) — data access, helpers,
  portals, API

## See also

- [**CalCOFI Explorer**](https://calcofi.io/explore/) — the browser app
  over the same release
- [**CalCOFI Schema**](https://calcofi.io/db-schema/) — per-release ERD,
  tables, columns (units + descriptions), datasets, and measurement-type
  registry
- [**CalCOFI Query**](https://calcofi.io/db-query/) — browser-only
  DuckDB-WASM playground against the public release Parquet

## Code of Conduct

This is an open-source project so your input is greatly welcomed! Please
note that the `calcofi4r` project is released with a [Contributor Code
of Conduct](https://calcofi.github.io/calcofi4r/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
