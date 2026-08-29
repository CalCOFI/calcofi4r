# Changelog

## calcofi4r 1.13.1

### `cc_get_db()` never hands back an empty database

- **`httpfs` is installed (once) and loaded explicitly** on every
  connection, before any view is created and on the cached path too. It
  used to rely on DuckDB autoloading, which only loads an extension that
  is *already installed* — true on a laptop that has ever read an https
  parquet, false on a fresh CI runner or a first install. The legacy
  `s3://` glob of a partitioned table happened to route through the
  anonymous-S3 setup that installs it; the first content-addressed
  release (v2026.08.25) had no glob left, and from that day every remote
  view on the pkgdown runner failed to bind.
- **A table that fails to load is an error** naming the table and
  DuckDB’s message — never a per-table
  [`warning()`](https://rdrr.io/r/base/warning.html), which a vignette’s
  `warning = FALSE` hid until `cc_describe_table("obs")` reported
  `Table 'obs' not found` three chunks later. The views are created in
  one transaction, so a failed call leaves no partial
  `calcofi_{version}.duckdb` for the next call to return as “cached”. A
  `tables =` selection that matches nothing is an error too.
- [`cc_list_tables()`](https://calcofi.io/calcofi4r/reference/cc_list_tables.md),
  [`cc_describe_table()`](https://calcofi.io/calcofi4r/reference/cc_describe_table.md)
  and
  [`cc_list_measurement_types()`](https://calcofi.io/calcofi4r/reference/cc_list_measurement_types.md)
  accept `con =` to reuse an open connection (as
  [`cc_get_dm()`](https://calcofi.io/calcofi4r/reference/cc_get_dm.md)
  already did); the vignette passes its own.
- pkgdown CI smoke-tests
  [`cc_get_db()`](https://calcofi.io/calcofi4r/reference/cc_get_db.md)
  with warnings visible before building the site.

## calcofi4r 1.13.0

### Effort and denominators (plan D8)

- [`cc_density_sql()`](https://calcofi.io/calcofi4r/reference/cc_density_sql.md)
  — the one SQL expression deriving `density_per_10m2` (areal:
  `count × std_haul_factor / prop_sorted` for C1/CB/CV/PV tows,
  published per-m² × 10), `density_per_1000m3` (volumetric:
  `count / prop_sorted / volume_sampled_m3 × 1000`, published per-1000
  m³ as is) and `effort_class`
  (`count_with_effort | raw_count_no_effort | density_as_published | other_unit`).
  Byte-identical to `calcofi4py.density_sql()` and the explorer’s
  `sql/density.sql`; the fixture pins it. Areal and volumetric are never
  converted into each other.
- [`cc_default_stage()`](https://calcofi.io/calcofi4r/reference/cc_default_stage.md)
  /
  [`cc_default_denominator()`](https://calcofi.io/calcofi4r/reference/cc_default_stage.md)
  — the picker’s rule-4 defaults (most rows with effort; most datasets
  with effort, never largest-n; `per_10m2` on a tie; `raw` only when
  nothing has effort). `CC_DENSITY_UNITS`, `CC_AREAL_GEARS` are the
  registry-owned vocabularies.

## calcofi4r 1.12.1

- `cc_get_db(local_data = TRUE)` keeps partitioned tables as remote
  views (as before 1.11.0), rather than downloading every partition —
  `obs` is all 16 datasets, and consumers rely on it being a view they
  can drop after copying one partition.

## calcofi4r 1.12.0

### The release chip

Every product on the integrated database showed *which* release its own
way (a muted span, a subtitle, a footer line, or nothing). The brand
header now carries one `release <b>vYYYY.MM.DD</b>` chip right after the
title, linking to that version’s schema and release notes.

- **[`cc_brand_header()`](https://calcofi.io/calcofi4r/reference/cc_brand_header.md)**
  gains `release =`.
- **`cc_release_chip(version)`** (new) — the chip on its own, for a
  framework-owned bar; `NULL`/`NA`/`""` renders nothing.
- **`cc_release_url(version)`** (new) —
  `https://calcofi.io/db-schema/#erd?v=…`.

## calcofi4r 1.11.0

### Content-addressed releases

- New
  [`cc_catalog()`](https://calcofi.io/calcofi4r/reference/cc_catalog.md),
  [`cc_release_sources()`](https://calcofi.io/calcofi4r/reference/cc_release_sources.md)
  and
  [`cc_read_parquet_sql()`](https://calcofi.io/calcofi4r/reference/cc_read_parquet_sql.md):
  the one place a release table is turned into parquet URLs. From the
  v2026.09 releases each table/partition is an immutable object under
  `gs://calcofi-db/ducklake/tables/{table}/{content_hash}/…` listed in
  the catalog’s `objects[]`; earlier catalogs still resolve to their
  per-release `releases/{version}/parquet/…` paths. Never build that
  path by hand — it is only guaranteed for the promoted and consolidated
  versions.
- [`cc_get_db()`](https://calcofi.io/calcofi4r/reference/cc_get_db.md)
  resolves every table through
  [`cc_release_sources()`](https://calcofi.io/calcofi4r/reference/cc_release_sources.md).
  Partitioned tables on a canonical release are read as an explicit
  https file list with `hive_partitioning = true` (no anonymous-S3
  glob), and `local_data = TRUE` keeps its content-addressed cache
  (`parquet/tables/{table}/{hash}/…`) for single-file tables, shared
  across pinned versions; a **partitioned** table stays a remote view
  under `local_data` (downloading every partition of `obs` to serve a
  pruned query is the wrong default, and consumers rely on it being a
  view).

## calcofi4r 1.10.0

### The calcofi.io brand contract, for Shiny apps

Every CalCOFI product now wears one theme (dark by default,
`?theme=dark|light` on any URL, the choice persisted across
`*.calcofi.io` by a cookie), one header (the CalCOFI logo far left
linking to calcofi.io, the app’s title beside it, a 🌓 switch) and one
favicon — see <https://calcofi.io/brand/v1/>. Three apps had been
carrying the same 30 lines of header/logo/theme CSS; one had a favicon.

- **`cc_brand_head(title, ga_app)`** (new) — `<title>`, the favicon set,
  the inline pre-paint theme snippet, `theme.css`/`theme.js`, the bslib
  bridge, and optionally
  [`cc_ga_head()`](https://calcofi.io/calcofi4r/reference/cc_ga_head.md).
- **`cc_brand_header(title, ..., subtitle, mode)`** (new) — the
  `.cc-header` bar with
  [`bslib::input_dark_mode()`](https://rstudio.github.io/bslib/reference/input_dark_mode.html)
  at the right.
- **`cc_theme(request)`** (new) — server-side theme resolution
  (`?theme=` → `cc_theme` cookie → dark) for `ui <- function(request)`,
  so the switch starts in the right state and the page never flashes the
  other colour.
- **`cc_is_dark(input)`** (new) — the switch’s state, for every
  `is_dark` argument.
- **`cc_tour_enabled(query)`** (new) — `?tour=off|false|0|no` suppresses
  a guided tour; the rule db-viz-hex had, now shared.
- **[`cc_plot_colors()`](https://calcofi.io/calcofi4r/reference/cc_plot_colors.md),
  `cc_plotly_theme(p, is_dark)`, `cc_ggplot_theme(is_dark)`** (new) — so
  plots stop lagging maps: text, grid and legend in the brand tokens,
  transparent backgrounds.
- **[`plot_ts()`](https://calcofi.io/calcofi4r/reference/plot_ts.md)**
  gains `is_dark` and `env_label` (promoted from db-viz-hex’s local
  fork, which had drifted ahead of the package); it no longer references
  the app-global `env_var_choices`.

## calcofi4r 1.9.0

### Quality flags are applied, not just carried

`obs.measurement_qual` has always been in the release and no consumer
looked at it: the station portal plotted a 1955 bottle oxygen at 2.18
ml/L between neighbours at 0.95 and 1.00 — a value the bottle database
had flagged `O_qual = 8` (suspect) all along. The vocabulary is per
dataset (bottle/CTD 8 = suspect, 9 = missing/bad; DIC WOCE 3 =
questionable, 4 = bad, 9 = missing).

- **`cc_qual_ok_sql(alias = NULL)`** (new) — the one SQL predicate to
  append to any `WHERE` over `obs` / `obs_ctd_full` /
  `sample_measurement` / `ctd_thin`; NULL-safe (an unflagged row is
  kept) and tolerant of the `"8.0"` spelling the bottle ingest used
  through v2026.08.14.
- **`cc_qual_ok(dataset_key, measurement_qual)`** (new) — its vectorised
  R twin.
- **`CC_QUAL_EXCLUDE`** (new) — the codes, per dataset.
- [`cc_match_bio_env()`](https://calcofi.io/calcofi4r/reference/cc_match_bio_env.md)’s
  env and ichthyo/zooplankton bio subqueries,
  [`cc_transect_section()`](https://calcofi.io/calcofi4r/reference/cc_transect_section.md)
  and
  [`cc_climatology()`](https://calcofi.io/calcofi4r/reference/cc_climatology.md)
  now apply it. The db-query `lib/match.js` twin is updated in step.

## calcofi4r 1.8.0

### PostgreSQL helpers for the multi-user CTD QA/QC database

New:
[`cc_pg_connect()`](https://calcofi.io/calcofi4r/reference/cc_pg_connect.md),
[`cc_pg_tunnel()`](https://calcofi.io/calcofi4r/reference/cc_pg_tunnel.md)
/
[`cc_pg_tunnel_close()`](https://calcofi.io/calcofi4r/reference/cc_pg_tunnel.md),
[`cc_pg_attach()`](https://calcofi.io/calcofi4r/reference/cc_pg_attach.md).

The public releases are Parquet and
[`cc_get_db()`](https://calcofi.io/calcofi4r/reference/cc_get_db.md)
reads them without credentials. The CTD team’s *working* database is
different: a multi-user PostgreSQL (`calcofi`) on the CalCOFI server,
reachable only through an SSH tunnel. These three helpers make that a
one-liner without putting a password in anyone’s script:

- [`cc_pg_connect()`](https://calcofi.io/calcofi4r/reference/cc_pg_connect.md)
  — `RPostgres` connection with every default resolved: host is
  `postgis` on the server (RStudio Server, Shiny) and `localhost`
  elsewhere (the tunnel), the role name is read from the `~/.pgpass`
  line you copied from the server, and the password is left to libpq,
  which reads the same file. `PGHOST`/`PGPORT`/`PGUSER` override.
- [`cc_pg_tunnel()`](https://calcofi.io/calcofi4r/reference/cc_pg_tunnel.md)
  — starts `ssh -N -L 5432:localhost:5432 calcofi` via `processx` using
  your `~/.ssh/config` alias, waits for the port, and is reused while
  alive; `cc_pg_connect(tunnel = TRUE)` calls it. Refuses to stomp on a
  port that is already open and says to use 15432 instead.
- [`cc_pg_attach()`](https://calcofi.io/calcofi4r/reference/cc_pg_attach.md)
  — `INSTALL postgres; ATTACH … (TYPE postgres)` inside a DuckDB
  connection (e.g. from
  [`cc_get_db()`](https://calcofi.io/calcofi4r/reference/cc_get_db.md)),
  so one query joins release tables with `pg.ctd.*` / `pg.work.*`;
  `read_only = FALSE` allows bulk writes from Parquet into PostgreSQL.

[`cc_db_connect()`](https://calcofi.io/calcofi4r/reference/cc_db_connect.md)
stays deprecated and now points at
[`cc_pg_connect()`](https://calcofi.io/calcofi4r/reference/cc_pg_connect.md)
(it hard-coded `gis`/`admin`/`~/.calcofi_db_pass.txt`). Account, tunnel
and `.pgpass` instructions for Mac and Windows live at
<https://calcofi.io/docs/server-access.html>.

## calcofi4r 1.7.0

### A time-series gap is drawn as a gap, not as a measured zero

[`prep_ts_sp()`](https://calcofi.io/calcofi4r/reference/prep_ts_sp.md)
now inserts `NA` rows at time steps with no observations, so the line
BREAKS there instead of running straight through.

A species series is mostly zeros and Highcharts connects consecutive
points, so an unsampled stretch rendered as a flat line along zero —
which reads as “we looked and found none” when the truth is “nobody
looked”. The two are different facts and the chart was showing the wrong
one.

The worked case is `cdfw_dungeness-crab`. Its sorted-archive effort
exists in nine years only — 1984, 1988, 1998, 2004-2009 — because the
sorting log records which archived jars have been examined, and most
have not. The chart drew a continuous zero from 1984 to 2008, asserting
measured absence across roughly 20 years in which not one jar was
opened.

Gaps are `NA` on `avg`/`std`/`upr`/`lwr` and `n = 0`, never `0` — zero
is a measurement, and collapsing the two is the bug. `n` lets a consumer
tell them apart.

Only resolutions that are a real time AXIS are filled (`year`,
`year_quarter`, `year_month`, `year_day`). `quarter`/`month`/`day` are
climatology CYCLES where every bin is populated by construction and an
absent one means something else, so they are untouched. Nothing is
padded beyond a
series`observed range, and each series gets its own range — one taxon`s
gap is never another\`s.

## calcofi4r 1.6.0

### The seafloor under a section is now sampled along the track, not at stations

New: [`cc_bathy()`](https://calcofi.io/calcofi4r/reference/cc_bathy.md),
[`cc_bathy_depth()`](https://calcofi.io/calcofi4r/reference/cc_bathy_depth.md)
and
[`cc_transect_bathy()`](https://calcofi.io/calcofi4r/reference/cc_transect_bathy.md).

Every app that draws a section draws a seafloor under it, and none of
them was drawing the seafloor that is there. `apps/ctd-viz` sampled
GEBCO **only at cast positions**; `CalCOFI/ctd-transects` sampled along
the line but at 2 km, against a grid whose own cell is ~390 m. Both
under-resolve the same real features into the same artifact — a bank
drawn as a spike, right at the depths where someone is reading the
thermocline.

Line 93.3 is the case. Fortymile Bank is a ~14 km rise from 652 m to a
**178 m** crest, with flanks on both sides. At 2 km it is four soundings
— 385, 344, 238, 370 — and draws as a triangle. Line 86.7 is the worse
version of it: station 50 sits on a Channel Islands bank at 80 m between
neighbours at 1,654 m and 1,190 m that are 37 km away, so station-only
sampling drew a mountain 74 km wide and 1.5 km tall that does not exist.

`cc_transect_bathy(lon, lat, interval_m = 500)` samples between the
positions, not only at them. The default sits just above GEBCO’s own
cell: fine enough to keep every cell the track crosses, coarse enough
not to imply detail the grid does not have.

Three things it does that the private helpers it replaces did not:

- **`dist_km`** puts the profile on the caller’s own x-axis,
  interpolated within each leg, so it is anchored exactly at every
  station and stretched between them. `ctd-transects` was doing this
  warp afterwards in Python.
- **`on_land`** is returned rather than the land samples being dropped.
  A cruise track — unlike a CalCOFI line — zigzags, and a leg between
  two casts can cross an island. That was `ctd-viz`’s stated reason for
  not sampling between casts at all; it is now a flag the consumer
  breaks its polygon on, so a crossing reads as coastline instead of
  being hidden.
- **Positions come from a great circle**, not a blend in lon/lat. The
  shortcut is within metres over a 40 km leg and not over the 400 km hop
  an arbitrary cast selection can produce.

[`cc_bathy()`](https://calcofi.io/calcofi4r/reference/cc_bathy.md)
fetches the GEBCO 2025 crop (4.3 MB, positive-down depth in metres, land
clamped to 0) from `gs://calcofi-db/bathymetry/` and caches it, so
`calcofi4r` no longer needs a sibling app checkout to know where the
bottom is. `options(calcofi4r.bathy=)` or `CALCOFI_BATHY` points it at a
local file instead.

## calcofi4r 1.5.4

### The anomaly vignette’s outlier screen is now two-sided

It screened only the upper bound, which would have sailed straight past
the cold artifact that motivated the whole exercise: a failed
temperature sensor averaged into `TempAve` produced values near **-47
degC**. A screen that can only catch the failure you already have in
front of you is not a screen.

The vignette is also rebuilt against the corrected release. The
published figure showed a -46 degC anomaly at the start of the series
and repeated +7 degC spikes in which all four depth layers moved in
lockstep — one cause with two symptoms, since a baseline cell containing
-47 degC readings is dragged so low that every other year at that
station reads as strongly positive.

The corrected series ranges **-0.97 to +1.79 degC** and reproduces
events we already know happened: 2014-15 is the marine heatwave, 2016
its El Nino tail, and 2026 is warm at every depth rather than only at
the surface. Baseline years sit near zero, as they must by construction.

## calcofi4r 1.5.3

### The getting-started vignette documented a schema that no longer exists

[`vignette("calcofi4r")`](https://calcofi.io/calcofi4r/articles/calcofi4r.md)
— the package’s front door — still described the pre-consolidation
database: `ichthyo`, `species`, `casts`, `bottle`, `bottle_measurement`,
`net`, `tow`, `site`. Every one of those tables is gone, replaced by the
core `obs` / `sample` / `obs_attribute` / `sample_measurement` family
plus the global `taxon`. It failed on `cc_describe_table("ichthyo")` and
would have failed on four more chunks.

Rewritten against the current schema, with every query run against the
live release before being committed. The schema section now explains the
consolidation rather than listing tables, and makes the point that
matters: a query written against ichthyoplankton now works unchanged
against CTD, zooplankton or seabirds.

### `cc_read_taxon()`, and `cc_read_species()` deprecated

- **[`cc_read_taxon()`](https://calcofi.io/calcofi4r/reference/cc_read_taxon.md)**
  reads the global `taxon` table — one row per taxon, keyed `worms:<id>`
  or `itis:<id>`, with cross-reference ids, `parent_taxon_key` and the
  flattened classification.
- **[`cc_read_species()`](https://calcofi.io/calcofi4r/reference/cc_read_species.md)**
  is deprecated. It called `tbl(con, "species")` and has failed with
  “Can’t query fields” against every release since the taxon
  consolidation. It now warns and forwards to
  [`cc_read_taxon()`](https://calcofi.io/calcofi4r/reference/cc_read_taxon.md),
  so existing scripts run — but check any code that joined on
  `species_id`, which is now `taxon_key`.

## calcofi4r 1.5.2

### `cc_latest_version()`, and a pkgdown build that fails when it fails

- **[`cc_latest_version()`](https://calcofi.io/calcofi4r/reference/cc_latest_version.md)**
  is now exported — it resolves the promoted release to a concrete
  version string from the same `latest.txt` every other CalCOFI consumer
  reads. Use it when you are about to pin: `version = "latest"` is not
  reproducible, but you cannot pin to a version you have not looked up.

#### The bio-env-matching vignette stopped building, and the site said it was fine

Two failures stacked, and the second hid the first.

**The pin rotted in a way a pin is not supposed to.** That vignette
pinned `v2026.05.14` to demonstrate archival reproducibility. The
release is still there — what is missing is `obs.parquet` *inside* it.
`v2026.05.14` predates the consolidation of the per-dataset tables into
the core `obs` / `sample` model, and
[`cc_match_bio_env()`](https://calcofi.io/calcofi4r/reference/cc_match_bio_env.md)
was later rewritten to query `obs`. The data was immutable exactly as
promised; the **code** moved to a schema that data does not have. The
vignette now says so, and pins to the release current at build time.

**And the site deployed anyway.** `pkgdown.yaml` carried
`continue-on-error: true`, so a failed build was a green tick. pkgdown
writes `articles/index.html` — the navbar — *before* rendering the
articles, so the site published listing every article while serving only
those built before the failure. The symptom is a 404 from a link in your
own navbar with nothing anywhere reporting it. Removed; a failed build
now fails the run and the deploy is skipped, which keeps the last good
site up instead of publishing a half-built one.

## calcofi4r 1.5.1

### New vignette: summer CTD temperature anomalies

[`vignette("ctd-temperature-anomalies")`](https://calcofi.io/calcofi4r/articles/ctd-temperature-anomalies.md)
walks a real request end to end — extract the summer cruise temperatures
and plot anomalies against a 1993-2013 baseline — using five of the
1.5.0 exports and no SQL. It doubles as their documentation, so the
functions and the worked example cannot drift apart.

It also demonstrates something worth doing regardless of the analysis:
**screening the source for physically impossible values before computing
a baseline, rather than after the result looks odd.** The released CTD
carries 18 temperature readings in 13.5 million that are not ocean
temperatures — the soak artifact, where the sensor is still warm from
the deck on the first metres of the downcast. They pass the release’s
declared bounds because those are set to “impossible” (-2 to 40 degC)
rather than “unusual”.

Eighteen rows sounds ignorable, and is not: they land on the
sparsely-sampled northern lines, where an April baseline cell may hold
2-7 observations across all of 1993-2013. One 38 degC reading moves that
cell’s mean by more than 10 degC, and every other April cruise at the
station then reads as a spurious ~11 degC *cold* anomaly. The vignette
screens them client-side and says why, rather than inheriting the
judgement invisibly.

## calcofi4r 1.5.0

### Transects, climatology and anomalies: one implementation, five new functions

Every app that draws a CalCOFI section had its own private helper.
`apps/ctd-viz` made the user click two stations on a map and ordered
them by ship track; `ctd-transects` needed the same section pre-rendered
for a browser with no R behind it. A fix to the ordering or the depth
binning had to be made twice, and the two had already drifted.

- **[`cc_transect_stations()`](https://calcofi.io/calcofi4r/reference/cc_transect_stations.md)**
  — stations on a line, ordered **nearshore → offshore by station
  number**. Well defined for every cruise with no endpoints to pick,
  which is what makes a whole archive of sections pre-renderable.
  Station order is deliberately not `order_occ`: that is the ship’s
  track, so its direction is whichever way the ship steamed, and it is
  NULL on roughly half the release’s cast rows.
- **[`cc_transect_section()`](https://calcofi.io/calcofi4r/reference/cc_transect_section.md)**
  — observations along that transect, binned by depth (default 5 m to
  500 m), long/tidy.
- **[`cc_climatology()`](https://calcofi.io/calcofi4r/reference/cc_climatology.md)**
  — a baseline mean per (station, depth bin, calendar month) over a
  stated year range, returned with `clim_n` so a thin cell can be
  filtered rather than silently trusted.
- **[`cc_anomaly()`](https://calcofi.io/calcofi4r/reference/cc_anomaly.md)**
  — `value - clim_mean`. A cell with no baseline comes back `NA`,
  **never 0**: an unsampled baseline is not a zero anomaly. Also returns
  `anomaly_sd`, the departure in baseline standard deviations, which is
  what makes 1 degC interpretable — large in the deep, unremarkable at
  the surface in spring.
- **[`cc_transect_matrix()`](https://calcofi.io/calcofi4r/reference/cc_transect_matrix.md)**
  — pivot to the station x depth matrix a heatmap wants.

These **prepare** data; they do not draw. Rendering stays with each app,
because `ctd-viz` can interpolate server-side and `ctd-transects`
cannot.

#### `x = "occupied"` vs `x = "line"`

`cc_transect_stations(x=)` chooses the horizontal ruler. `"occupied"`
(default) measures between the stations a cruise actually occupied, so
the section fills the plot; `"line"` measures along the full line
geometry, so cruises that sampled different subsets are comparable in
width.

This is not cosmetic. Line 93.3 has not been sampled past station 90
since 2025-01, though 113 of the 130 cruises before it reached station
120 — so under the default ruler a recent section spans a shorter
distance than a historical one at the same width, and comparing them by
eye overstates recent gradients.

Not to be confused with
[`buffer_transect()`](https://calcofi.io/calcofi4r/reference/buffer_transect.md),
which is a user-drawn line plus buffer corridor.

## calcofi4r 1.4.4

### `cc_tbl()` follows the spatial table rename, and keeps the old names working

Release v2026.08.02 renames `_spatial` → **`spatial`** and
`_spatial_attr` → **`spatial_attribute`**, and both now carry a real
primary key, `spatial_key = '{layer}:{id}'`. The old `id` is per-layer
sequential — a county and a sanctuary both have `id` 1 — so joining on
it alone silently mixed layers; `spatial_key` is namespaced the way
`sample_key` is.

[`cc_tbl()`](https://calcofi.io/calcofi4r/reference/cc_tbl.md) accepts
**either** name and picks the matching attribute table, so code pinned
to an older release keeps working:

``` r

cc_tbl(con, "spatial",  layer = "CA Counties")   # v2026.08.02 on
cc_tbl(con, "_spatial", layer = "CA Counties")   # older releases
```

## calcofi4r 1.4.3

*Don’t count automated browsers*

- **[`cc_ga_js()`](https://calcofi.io/calcofi4r/reference/cc_ga_js.md) /
  [`cc_ga_head()`](https://calcofi.io/calcofi4r/reference/cc_ga_head.md)
  /
  [`cc_ga_html()`](https://calcofi.io/calcofi4r/reference/cc_ga_html.md)
  skip `navigator.webdriver` clients.** GA4 filters known bots by user
  agent, so what gets through is exactly the automation that *renders* —
  Playwright, Puppeteer, Selenium, and our own `shot-scraper` screenshot
  runs — firing gtag like a real visitor. Each fetch arrives
  cookie-less, so a site sweep becomes N one-page “users” with no
  engagement; that is how `calcofi4db` came to top the usage table on
  169 users and 0% engagement. Both legs (GA4 and the Sheet) go silent
  under automation, while `window.ccTrack` stays defined and the message
  handlers still register, so a server-side
  [`cc_track()`](https://calcofi.io/calcofi4r/reference/cc_track.md) is
  a no-op rather than an unknown-message console warning.

## calcofi4r 1.4.2

*One GA4 snippet for apps that do not depend on calcofi4r*

- **[`cc_ga_html()`](https://calcofi.io/calcofi4r/reference/cc_ga_html.md)**
  New: writes \[cc_ga_js()\]’s snippet to a standalone HTML file, so an
  app can keep `includeHTML("google-analytics.html")` and gain nothing
  at runtime. Loading the calcofi4r namespace costs ~4 s of cold start
  (sf/terra/stars come with it), which a small app should not pay just
  to be counted. The file carries a banner spelling out the exact
  regeneration command — these files live in several repos and must not
  be hand-patched apart. Static by nature, so the Sheet leg and the
  per-request `ip` are off; GA4 is unaffected, since gtag resolves the
  client IP in the browser.

## calcofi4r 1.4.1

*Log the real client IP, not the proxy*

- **[`cc_ga_js()`](https://calcofi.io/calcofi4r/reference/cc_ga_js.md) /
  [`cc_ga_head()`](https://calcofi.io/calcofi4r/reference/cc_ga_head.md)
  gain `ip`** Stamps a client IP on every logged row from the **page**
  request. Behind shiny-server this is the only place a real one exists:
  shiny-server does not proxy the websocket upgrade — it opens a fresh
  localhost connection to the R worker — so `session$request` has no
  `X-Forwarded-For` and `REMOTE_ADDR` is always `127.0.0.1`, no matter
  how correctly the reverse proxy is configured. Make the app’s `ui` a
  `function(req)` and pass `ip = cc_client_ip(req)`.
- **[`cc_client_ip()`](https://calcofi.io/calcofi4r/reference/cc_client_ip.md)
  accepts a `ui(req)`** as well as a `session` — the argument is now
  `x`, and the request fields are read directly when there is no
  `$request`.
- **[`cc_track_session()`](https://calcofi.io/calcofi4r/reference/cc_track_session.md)’s
  IP is now a fallback**, not an override: the client keeps the
  page-supplied address rather than letting the session’s `127.0.0.1`
  clobber it.

## calcofi4r 1.4.0

*Non-blocking usage analytics for the Shiny apps*

- **[`cc_track()`](https://calcofi.io/calcofi4r/reference/cc_track.md)**
  New: send a usage event from the Shiny server to the browser over the
  session’s existing websocket. Makes **no HTTP request**, so
  instrumenting a hot control can never stall a reactive — replacing the
  synchronous
  [`httr2::req_perform()`](https://httr2.r-lib.org/reference/req_perform.html)
  per query that `db-viz-hex` used to run on every filter submit and
  download.
- **[`cc_track_query()`](https://calcofi.io/calcofi4r/reference/cc_track_query.md)**
  New: wrap a query expression to record its row count, duration, and
  any error (`n_rows` / `ms` / `status` / `error` get their own Sheet
  columns, so they stay numeric and chartable). The result — including a
  lazy `dbplyr` table — passes through untouched, and an error is
  re-raised after being logged.
- **[`cc_ga_head()`](https://calcofi.io/calcofi4r/reference/cc_ga_head.md)
  / [`cc_ga_js()`](https://calcofi.io/calcofi4r/reference/cc_ga_js.md)**
  New: the one `<head>` snippet every CalCOFI app installs. Emits GA4
  events for aggregate behavior and beacons full-cardinality detail to a
  Google Sheet, batched (10 events / 15 s / page-hide) via
  `navigator.sendBeacon()`. Also defines `window.ccTrack()` for pure UI
  events that never need to reach R.
- **[`cc_track_session()`](https://calcofi.io/calcofi4r/reference/cc_track_session.md),
  [`cc_client_ip()`](https://calcofi.io/calcofi4r/reference/cc_client_ip.md)**
  New: hand the browser the client IP and Shiny session token, which
  JavaScript cannot read, so the log’s `ip`/`session` columns survive
  the move to a browser-sent beacon.
- **[`cc_log_header()`](https://calcofi.io/calcofi4r/reference/cc_log_header.md),
  [`cc_apps_script()`](https://calcofi.io/calcofi4r/reference/cc_apps_script.md)**
  New: the Sheet’s column order and the generated `Code.gs` that appends
  a whole batch in one `setValues()` call — kept in one place so the
  Sheet, the Apps Script, and the client payload cannot drift.
- **testthat suite** The package now has tests (`devtools::test()`),
  starting with 18 for the analytics module.

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
  - `cc_read_bottle()`
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
